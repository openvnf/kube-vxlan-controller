-module(kube_vxlan_controller_db).

%% API
-export([nets_options/0, load_db/1]).


-include_lib("kernel/include/logger.hrl").

-define(K8s, kube_vxlan_controller_k8s).
-define(Tools, kube_vxlan_controller_tools).

-define(TABLENAME, ?MODULE).

nets_options() ->
    persistent_term:get(?TABLENAME, #{}).

load_db(#{namespace := Namespace,
	  configmap_name := ConfigMapName} = Config) ->
    Resource = "/api/v1/namespaces/" ++ Namespace ++
	"/configmaps/" ++ ConfigMapName,
    Db =
	case ?K8s:http_request(Resource, [], Config) of
	    {ok, #{data := Data}} ->
		?LOG(debug, "load VXLAN config ~p", [Data]),
		maps:fold(fun(NetName, NetOptions, Map) ->
				  maps:put(atom_to_list(NetName), net_options(NetOptions), Map)
			  end, #{}, Data);
	    _ ->
		#{}
	end,
    persistent_term:put(?TABLENAME, Db).

%%%=========================================================================
%%%  internal functions
%%%=========================================================================

net_options(Options) ->
    maps:from_list(net_id_bc([
	?Tools:pod_read_net_option(Option) ||
	Option <- string:lexemes(binary_to_list(Options), " ")
    ])).

%%% TODO: provided for BC, remove when not needed
net_id_bc(Options) ->
    lists:map(fun({Name, Value}) ->
	NameString = atom_to_list(Name),
	try list_to_integer(NameString) of
	    Id -> {id, integer_to_list(Id)}
	catch
	    _:_ -> {Name, Value}
	end
    end, Options).
%%%
