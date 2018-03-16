-module('kube-vxlan-controller').

-export([main/1]).

-define(Db, kube_vxlan_controller_db).
-define(K8s, kube_vxlan_controller_k8s).
-define(Cli, kube_vxlan_controller_cli).
-define(Net, kube_vxlan_controller_net).
-define(Pod, kube_vxlan_controller_pod).
-define(Log, kube_vxlan_controller_log).
-define(Agent, kube_vxlan_controller_agent).
-define(State, kube_vxlan_controller_state).
-define(Config, kube_vxlan_controller_config).
-define(Inspect, kube_vxlan_controller_inspect).

-define(A8nVxlanNamesSep, ", \n").

main(CliArgs) ->
    application:ensure_all_started(?MODULE),
    case ?Cli:args(CliArgs) of
        {run, Args} -> do(run, Args);
        {inspect, Subject, Args} -> do({inspect, Subject}, Args);
        version -> io:format(?Cli:version(?Config:version()));
        usage -> io:format(?Cli:usage())
    end.

do(Action, {NamedArgs, OrderedArgs}) ->
    ?Config:init(),
    case load_config(NamedArgs) of
        {ok, Config} -> do(Action, OrderedArgs, Config);
        {error, Reason} -> io:format("~p~n", [Reason])
    end.

do(run, _Args, Config) -> run(Config, #{});
do({inspect, Subject}, Args, Config) -> ?Inspect:Subject(Args, Config).

load_config(Args) -> cpf_funs:apply_while([
    {load, fun ?Config:load/1, [Args]},
    {build, fun ?Config:build/1, [{load}]},
    {validate, fun ?Config:validate/1, [{build}]}
]).

run(Config, State) ->
    ResourceVersion = ?State:resource_version(State),
    Selector = maps:get(selector, Config),

    NewState = case ?State:is_resource_version_shown(State) of
        true -> State;
        false ->
            ?Log:info("Watching pods (selector: ~s) from version: ~s",
                      [Selector, ResourceVersion]),
            ?State:set_resource_version_shown(State)
    end,

    Resource = "/api/v1/watch/pods",
    Query = [
        {"labelSelector", Selector},
        {"resourceVersion", ResourceVersion},
        {"timeoutSeconds", "10"}
    ],
    case ?K8s:http_stream_request(Resource, Query, Config) of
        {ok, Stream} -> run(Stream, Config, NewState);
        {error, Reason} -> ?Log:error(Reason)
    end.

run(Stream, Config, State) ->
    case ?K8s:http_stream_read(Stream) of
        {ok, done} -> run(Config, State);
        {ok, Events} ->
            NewState = process_events(Events, Config, State),
            run(Stream, Config, NewState);
        {error, timeout} -> run(Config, State);
        {error, Reason} -> ?Log:error(Reason)
    end.

process_events(Events, Config, State) ->
    lists:foldl(process_event_fun(Config), State, Events).

process_event_fun(Config) -> fun(Event, State) ->
    {EventType, Resource} = read_event(Event, Config),
    ?Log:info("~s~n~p", [EventType, Resource]),

    NewState = ?State:set_resource_version(Resource, State),
    process_event(EventType, Resource, Config, NewState)
end.

process_event(pod_added, Pod = #{phase := "Pending"}, Config, State) ->
    NewState = ?State:set_pod_pending(add, Pod, State),
    case maps:get(init_agent_ready, Pod) of
        true -> handle_pod_initialisation(Pod, Config, NewState);
        false -> NewState
    end;

process_event(pod_added, Pod = #{phase := "Running"}, Config, State) ->
    handle_pod_added(Pod, Config, State);

process_event(pod_modified, Pod = #{phase := "Pending"}, Config, State) ->
    case maps:get(init_agent_ready, Pod) of
        true -> handle_pod_initialisation(Pod, Config, State);
        false -> State
    end;

process_event(pod_modified, Pod = #{phase := "Running"}, Config, State) ->
    case ?State:pod_pending_action(Pod, State) of
        {ok, add} ->
            NewState = ?State:unset_pod_pending(Pod, State),
            handle_pod_added(Pod, Config, NewState);
        {ok, modify} ->
            State;
        error ->
            ?State:set_pod_pending(modify, Pod, State)
    end;

process_event(pod_deleted, Pod, Config, State) ->
    case ?State:pod_pending_action(Pod, State) of
        {ok, add} -> State;
        {ok, modify} ->
            NewPod = ?State:merge_pod_pending_info(Pod, State),
            NewState = ?State:unset_pod_pending(NewPod, State),
            handle_pod_deleted(NewPod, Config, NewState);
        error -> State
    end;

process_event(_Event, _Pod, _Config, State) -> State.

read_event(#{
  type := Type,
    object := #{
      kind := Kind,
      metadata := #{
        namespace := Namespace,
        uid := PodUid,
        name := PodName,
        annotations := Annotations,
        resourceVersion := ResourceVersion
      },
      status := Status = #{
        phase := Phase
      }
    }
}, Config) -> {
    list_to_atom(
        string:lowercase(binary_to_list(Kind)) ++
        [$_|string:lowercase(binary_to_list(Type))]
    ),
    #{resource_version => binary_to_list(ResourceVersion),
      namespace => binary_to_list(Namespace),
      pod_uid => binary_to_list(PodUid),
      pod_name => binary_to_list(PodName),
      pod_ip => binary_to_list(maps:get(podIP, Status, <<>>)),
      vxlan_names => vxlan_names(Annotations, Config),
      phase => binary_to_list(Phase),
      init_agent_ready => lists:any(
          is_init_agent_ready_fun(Config),
          maps:get(initContainerStatuses, Status, [])
      )
    }
}.

is_init_agent_ready_fun(Config) -> fun(#{name := Name, state := State}) ->
    binary_to_list(Name) == maps:get(agent_init_container_name, Config) andalso
    maps:is_key(running, State)
end.

handle_pod_initialisation(#{
    namespace := Namespace,
    pod_name := PodName,
    vxlan_names := VxlanNames
}, Config, State) ->
    ?Log:info("Pod initialisation ~p:", [{Namespace, PodName, VxlanNames}]),

    {VxlanIds, NotFoundVxlanNames} = ?Db:vxlan_ids(VxlanNames, Config),

    NotFoundVxlanNames == [] orelse
        ?Log:warning("VXLAN Id for these networks not found: ~p",
                     [NotFoundVxlanNames]),

    AgentConfig = maps:put(agent_container_name,
                  maps:get(agent_init_container_name, Config), Config),

    maps:fold(fun(VxlanName, VxlanId, _) ->
        ?Net:vxlan_init_pod(Namespace, PodName, VxlanName, VxlanId, AgentConfig)
    end, ok, VxlanIds),

    ?Agent:terminate(Namespace, PodName, AgentConfig),

    State.

handle_pod_added(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    vxlan_names := VxlanNames
}, Config, State) ->
    ?Log:info("Pod added: ~p", [{Namespace, PodName, PodIp, VxlanNames}]),

    Pods = ?Pod:get(maps:get(selector, Config), Config),

    lists:foreach(fun(VxlanName) ->
        VxlanPods = vxlan_members(VxlanName, PodName, Pods, Config),
        ?Log:info("Pods within \"~s\" to join:~n~s",
                  [VxlanName, vxlan_members_format(VxlanPods)]),
        ?Net:vxlan_add_pod(
            Namespace, PodName, PodIp, VxlanName, VxlanPods, Config
        )
    end, VxlanNames),

    State.

handle_pod_deleted(#{
    namespace := Namespace,
    pod_name := PodName,
    pod_ip := PodIp,
    vxlan_names := VxlanNames
}, Config, State) ->
    ?Log:info("Pod deleted: ~p", [{Namespace, PodName, PodIp, VxlanNames}]),

    Pods = ?Pod:get(maps:get(selector, Config), Config),

    lists:foreach(fun(VxlanName) ->
        VxlanPods = vxlan_members(VxlanName, PodName, Pods, Config),
        ?Log:info("Pods within \"~s\" to leave:~n~s",
                  [VxlanName, vxlan_members_format(VxlanPods)]),
        ?Net:vxlan_delete_pod(
            Namespace, PodName, PodIp, VxlanName, VxlanPods, Config
        )
    end, VxlanNames),

    State.

vxlan_members(VxlanName, ExcludePodName, Pods, Config) ->
    [{binary_to_list(Namespace),
      binary_to_list(Name),
      binary_to_list(PodIp)} ||
     #{metadata := #{
         namespace := Namespace,
         name := Name,
         annotations := Annotations
      },
       status := #{
         podIP := PodIp,
         phase := <<"Running">>
       }
     } <- Pods,
     lists:member(VxlanName, vxlan_names(Annotations, Config)) andalso
     binary_to_list(Name) /= ExcludePodName].

vxlan_names(Annotations, #{annotation := Annotation}) ->
    VxlanNames = binary_to_list(maps:get(Annotation, Annotations, <<>>)),
    string:lexemes(VxlanNames, ?A8nVxlanNamesSep).

vxlan_members_format(Pods) ->
    lists:flatten(io_lib:format(
        lists:flatten(lists:duplicate(length(Pods), "~p~n")), Pods)).
