-module(epmdless_client).

-behaviour(gen_server).

%% @doc
%% Module which used as a callback which passed to erlang vm via `-epmd_module` attribute
%% @end

%% epmd callbacks
-export([start_link/0, register_node/3, host_please/1, port_please/2, names/1]).
%% gen server callbacks
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).
%% db funcs
-export([add_node/2, add_node/3, remove_node/1, list_nodes/0]).
%% auxiliary funcs
-export([get_info/0]).

-record(state, {
    dist_port   :: inet:port_number(),
    nodes = #{} :: map()
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec register_node(Name, Port, Family) -> {ok, CreationId} when
      Name       :: atom(),
      Port       :: inet:port_number(),
      Family     :: atom(),
      CreationId :: 1..3.
register_node(_Name, Port, _Family) ->
    gen_server:call(?MODULE, {port, Port}, infinity),
    {ok, rand:uniform(3)}.


-spec host_please(Node) -> {host, Host} | nohost when
      Node :: atom(),
      Host :: inet:hostname() | inet:ip_address().
host_please(Node) ->
    case gen_server:call(?MODULE, {host_please, Node}, infinity) of
        {error, nohost} ->
            error_logger:info_msg("No host found for node ~p~n", [Node]),
            nohost;
        {ok, Host} ->
            error_logger:info_msg("Found host ~p for node ~p~n", [Host, Node]),
            {host, Host}
    end.


-spec port_please(Name, Host) -> {port, Port, Version} | noport when
      Name    :: atom(),
      Host    :: inet:hostname(),
      Port    :: inet:port_number(),
      Version :: 5.
%% @doc
%% request port of node `Name`
%% @end
port_please(Name, Host) ->
    case gen_server:call(?MODULE, {port_please, Name, Host}, infinity) of
        {ok, Port} ->
            error_logger:info_msg("Resolved port for ~p/~p to ~p~n", [Name, Host, Port]),
            {port, Port, 5};
        {error, noport} ->
            error_logger:info_msg("No port for ~p/~p~n", [Name, Host]),
            noport
    end.


-spec add_node(Node, Port) -> ok when
      Node :: atom(),
      Port :: inet:port_number().
add_node(Node, Port) ->
    Host = case string:tokens(atom_to_list(Node), "@") of
        [_Node, H] -> H;
        [H]        -> H
    end,
    add_node(Node, Host, Port).


-spec add_node(Node, Host, Port) -> ok when
      Node :: atom(),
      Host :: inet:hostname() | inet:ip_address(),
      Port :: inet:port_number().
add_node(Node, Host, Port) ->
    error_logger:error_msg("Adding a node: ~p~n", [Node]),
    ok = gen_server:call(?MODULE, {add_node, Node, Host, Port}, infinity).


-spec list_nodes() -> [{Node, Port}] when
      Node :: atom(),
      Port :: inet:port_number().
list_nodes() ->
    Nodes = gen_server:call(?MODULE, list_nodes, infinity),
    maps:to_list(Nodes).


-spec remove_node(Node) -> ok when
      Node :: atom().
remove_node(Node) ->
    ok = gen_server:call(?MODULE, {remove_node, Node}, infinity).


%% @doc
%% List the Erlang nodes on a certain host. We don't need that
%% @end
names(_Hostname) ->
    {error, address}.


-spec get_info() -> Info when
      Info :: [{dist_port, inet:port_number()}].
get_info() ->
    gen_server:call(?MODULE, get_info, infinity).


init([]) ->
    {ok, #state{}}.


handle_info(Msg, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.


handle_cast(Msg, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.


handle_call(list_nodes, _From, State) ->
    {reply, State#state.nodes, State};

handle_call({add_node, Node, Host, Port}, _From, State) ->
    {reply, ok, State#state{nodes = maps:put(Node, {Host, Port}, State#state.nodes)}};

handle_call({remove_node, Node}, _From, State) ->
    {reply, ok, State#state{nodes = maps:remove(Node, State#state.nodes)}};

handle_call({host_please, Node}, _From, State) ->
    Reply = case maps:find(Node, State#state.nodes) of
        error -> {error, nohost};
        {ok, {H, _P}} -> {ok, H}
    end,
    {reply, Reply, State};

handle_call({port_please, Node, Host}, _From, State) ->
    Reply = case maps:find(node_host_to_name(Node, Host), State#state.nodes) of
        error -> {error, noport};
        {ok, {_H, P}} -> {ok, P}
    end,
    {reply, Reply, State};

handle_call({port, DistPort}, _From, State) ->
    error_logger:info_msg("Starting erlang distribution at port ~p~n", [DistPort]),
    {reply, ok, State#state{dist_port = DistPort}};

handle_call(get_info, _From, State = #state{dist_port = DistPort}) ->
    {reply, [{dist_port, DistPort}], State};

handle_call(Msg, _From, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
    {reply, {error, {bad_msg, Msg}}, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_Old, State, _Extra) ->
    {ok, State}.


%% internal funcs


-spec node_host_to_name(Node, Host) -> Name when
      Node :: atom(),
      Host :: inet:hostname(),
      Name :: atom().
node_host_to_name(Node, Host) ->
    list_to_atom(lists:flatten(io_lib:format("~s@~s", [Node, Host]))).
