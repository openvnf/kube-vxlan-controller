-module(kube_vxlan_controller_inspect).

-export([
    networks/2
]).

-define(Agent, kube_vxlan_controller_agent).
-define(Pod, kube_vxlan_controller_pod).

-define(A8nVxlanNamesSep, ", \n").

networks(VxlanNames, Config) ->
    AllPods = ?Pod:get(maps:get(selector, Config), Config),

    Pods = [
     {binary_to_list(Namespace),
      binary_to_list(Name),
      binary_to_list(PodIp),
      vxlan_names(Annotations, Config)} ||
     #{metadata := #{
         namespace := Namespace,
         name := Name,
         annotations := Annotations
      },
       status := #{
         podIP := PodIp,
         phase := <<"Running">>
       }
     } <- AllPods, any_member(vxlan_names(Annotations, Config), VxlanNames)],

    SilentConfig = maps:put(silent, true, Config),
    lists:foreach(fun(VxlanName) ->
        io:format("[~s]~n", [VxlanName]),
        lists:foreach(fun({Namespace, PodName, PodIp, PodVxlanNames}) ->
            IsMember = lists:member(VxlanName, PodVxlanNames),
            IsMember andalso begin
                io:format("~s/~s [~s]:~n", [Namespace, PodName, PodIp]),
                Command = "bridge fdb show dev " ++ VxlanName,
                Result = ?Agent:exec(Namespace, PodName, Command, SilentConfig),
                io:format("~s~n", [Result])
            end
        end, Pods)
    end, VxlanNames).
    

vxlan_names(Annotations, #{annotation := Annotation}) ->
    VxlanNames = binary_to_list(maps:get(Annotation, Annotations, <<>>)),
    string:lexemes(VxlanNames, ?A8nVxlanNamesSep).

any_member(L1, L2) ->
    lists:any(fun(X) -> lists:member(X, L2) end, L1).
