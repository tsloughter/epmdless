-module(epmdless_proto_dist).

%% @doc
%% callback module for epmdless distribution protocol
%% @end


-export([listen/1, select/1, accept/1, accept_connection/5, setup/5, close/1, childspecs/0]).


listen(Name) ->
    %% loading application to get access to configuration
    application:load(epmdless),
    Port = case {os:getenv("EPMDLESS_DIST_PORT"), application:get_env(epmdless, listen_port)} of
        {false, {ok, P}}       -> P;
        {P, _} when P /= false -> list_to_integer(P);
        _                      -> 0
    end,
    %% setting mix/max distribution port, 0 - means any ephemeral port
    application:set_env(kernel, inet_dist_listen_min, Port),
    application:set_env(kernel, inet_dist_listen_max, Port),
    (get_module()):listen(Name).


select(Node) ->
    (get_module()):select(Node).


accept(Listen) ->
    (get_module()):accept(Listen).


accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    (get_module()):accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime).


setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    (get_module()):setup(Node, Type, MyNode, LongOrShortNames, SetupTime).


close(Listen) ->
    (get_module()):close(Listen).


childspecs() ->
    (get_module()):childspecs().


%% internal


get_module() ->
    application:load(epmdless),
    case application:get_env(epmdless, transport) of
        {ok, tls} -> inet_tls_dist;
        _         -> inet_tcp_dist
    end.
