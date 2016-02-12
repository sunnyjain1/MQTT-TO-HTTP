%%%%%%%%%%%%%%%%%%%
%%%  Module  %%%%%%
%%%%%%%%%%%%%%%%%%%

-module(mqtt_to_http).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Behaviour this module is implementing  %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(auth_on_subscribe_hook).
-behaviour(auth_on_publish_hook).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%  These function can be used by other modules  %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([auth_on_publish/6,auth_on_subscribe/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%  Implementing the Hooks  %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

auth_on_publish(UserName, {_MountPoint, _ClientId} = SubscriberId, QoS, Topic, Payload, IsRetain) ->
    error_logger:info_msg("auth_on_publish: ~p ~p ~p ~p ~p ~p", [UserName, SubscriberId, QoS, Topic, Payload, IsRetain]),
	send_req(Payload,Topic),
	ok.

auth_on_subscribe(UserName, ClientId, [{_Topic, _QoS}|_] = Topics) ->
    error_logger:info_msg("auth_on_subscribe: ~p ~p ~p", [UserName, ClientId, Topics]),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%  Internal Functions  %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_req(Payload,[Topic]) ->
	Lis = [{"bye","http://www.google.com"},{"new","http://localhost:8000"}],
	T = erlang:binary_to_list(Topic),
	case find(Lis,T) of
		not_exist ->
			ok;
		URL ->
			error_logger:info_msg("Sending Request"),
			application:start(inets),
			Response = httpc:request(post, {URL, [],"application/json",Payload}, [], [{sync, false}]),
			error_logger:info_msg("Response: ~p ", [Response]),
			ok
	end.

find([],_) -> 
	not_exist;
find([{Head,URL}|Rest],Val) ->
	if
		Head == Val ->
			URL;
		true ->
			find(Rest,Val)
	end.
