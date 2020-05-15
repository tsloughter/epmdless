-module(epmdless_client).

-behaviour(gen_server).

%% @doc
%% Module which used as a callback which passed to erlang vm via `-epmd_module` attribute
%% @end

%% epmd callbacks
-export([start_link/0, register_node/3, address_please/3, port_please/2, listen_port_please/2, names/1]).
%% gen server callbacks
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).
%% db funcs
-export([add_node/2, add_node/4, remove_node/1, list_nodes/0]).
%% auxiliary funcs
-export([get_info/0]).

-include_lib("kernel/include/inet.hrl").

-record(state, {
    dist_port   :: inet:port_number(),
    nodes = #{} :: map()
}).

-ifdef(OTP_VERSION).
-if(?OTP_VERSION < 23).
-define(ERL_DIST_VER, 5).  % OTP-22 or (much) older
-else.
-define(ERL_DIST_VER, 6).  % OTP-23 (or maybe newer?)
-endif.
-else.
-define(ERL_DIST_VER, 5).  % OTP-22 or (much) older
-endif.

start_link() ->
    %% new option that allows users to not have to use epmdless_proto_dist
    %% instead only set ERL_DIST_PORT
    Port = case os:getenv("ERL_DIST_PORT") of
               false ->
                   0;
               PortString ->
                   list_to_integer(PortString)
           end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

-spec register_node(Name, Port, Family) -> {ok, CreationId} when
      Name       :: atom(),
      Port       :: inet:port_number(),
      Family     :: atom(),
      CreationId :: 1..3.
register_node(_Name, Port, _Family) ->
    gen_server:call(?MODULE, {port, Port}, infinity),
    {ok, rand:uniform(3)}.

-spec port_please(Name, Host) -> {port, Port, Version} | noport when
      Name    :: atom(),
      Host    :: inet:hostname(),
      Port    :: inet:port_number(),
      Version :: 5 | 6.
%% @doc
%% request port of node `Name`
%% @end
port_please(Name, Host) ->
    case gen_server:call(?MODULE, {port_please, Name, Host}, infinity) of
        {ok, Port} ->
            {port, Port, ?ERL_DIST_VER};
        {error, noport} ->
            case os:getenv("EPMDLESS_REMSH_PORT") of
                false ->
                    %% new option that allows users to not have to use epmdless_proto_dist
                    %% instead only set ERL_DIST_PORT
                    case os:getenv("ERL_DIST_PORT") of
                        false ->
                            noport;
                        PortString ->
                            {port, list_to_integer(PortString), ?ERL_DIST_VER}
                    end;
                RemotePort ->
                    {port, list_to_integer(RemotePort), ?ERL_DIST_VER}
            end
    end.

-spec address_please(Name, Host, AddressFamily) -> Success | {error, term()} when
	  Name :: atom(),
	  Host :: string() | inet:ip_address(),
	  AddressFamily :: inet | inet6 | local,
	  Port :: non_neg_integer(),
	  Version :: non_neg_integer(),
	  Success :: {ok, inet:ip_address(), Port, Version}.
%% @doc Resolves the Host to an IP address of a remote node.
address_please(Name, Host, AddressFamily) ->
    {ok, Address} = inet:getaddr(Host, AddressFamily),
    case port_please(Name, Address) of
        {port, Port, Version} ->
            {ok, Address, Port, Version};
        noport ->
            {error, noport}
    end.

%% @doc Returns the port the local node should listen to when accepting new distribution requests.
listen_port_please(Name, Host) ->
    gen_server:call(?MODULE, {listen_port, Name, Host}, infinity).

-spec add_node(Node, Port) -> ok when
      Node :: atom(),
      Port :: inet:port_number().
add_node(Node, Port) ->
    case split_node(Node) of
        {ok, {NodeName, Host, IP}} ->
            add_node(NodeName, Host, IP, Port);
        {error, Reason} ->
            {error, Reason}
    end.

-spec add_node(NodeName, Host, IP, Port) -> ok when
      NodeName :: string(),
      Host :: inet:hostname() | inet:ip_address(),
      IP   :: inet:ip_address(),
      Port :: inet:port_number().
add_node(NodeName, Host, IP, Port) ->
    ok = gen_server:call(?MODULE, {add_node, NodeName, Host, IP, Port}, infinity).

-spec list_nodes() -> [{Node, Port}] when
      Node :: atom(),
      Port :: inet:port_number().
list_nodes() ->
    Nodes = gen_server:call(?MODULE, list_nodes, infinity),
    maps:to_list(Nodes).

-spec remove_node(Node) -> ok when
      Node :: atom().
remove_node(Node) ->
    case split_node(Node) of
        {ok, {NodeName, _Host, IP}} ->
            ok = gen_server:call(?MODULE, {remove_node, NodeName, IP}, infinity);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc
%% List the Erlang nodes on a certain host. We don't need that
%% @end
names(_Hostname) ->
    {error, address}.


-spec get_info() -> Info when
      Info :: [{dist_port, inet:port_number()}].
get_info() ->
    gen_server:call(?MODULE, get_info, infinity).


init([Port]) ->
    {ok, #state{dist_port=Port}}.


handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call(list_nodes, _From, State) ->
    {reply, State#state.nodes, State};

handle_call({add_node, NodeName, Host, IP, Port}, _From, State) ->
    {reply, ok, State#state{nodes=maps:put({NodeName, IP}, {Host, Port}, State#state.nodes)}};

handle_call({remove_node, NodeName, IP}, _From, State) ->
    {reply, ok, State#state{nodes=maps:remove({NodeName, IP}, State#state.nodes)}};

handle_call({port_please, Node, IP}, _From, State) ->
    Reply = case maps:find({Node, IP}, State#state.nodes) of
                error ->
                    {error, noport};
                {ok, {_H, P}} ->
                    {ok, P}
            end,
    {reply, Reply, State};

handle_call({port, DistPort}, _From, State) ->
    {reply, ok, State#state{dist_port=DistPort}};

handle_call({listen_port, _Name, _Host}, _From, State=#state{dist_port=DistPort}) ->
    {reply, {ok, DistPort}, State};

handle_call(get_info, _From, State=#state{dist_port=DistPort}) ->
    {reply, [{dist_port, DistPort}], State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_Old, State, _Extra) ->
    {ok, State}.

%%

%% TODO: support node's with @ using inet_db:gethostname() and inet_db:res_option(domain)
split_node(Name) ->
    case lists:splitwith(fun(C) -> C =/= $@ end, atom_to_list(Name)) of
        {NodeName, [$@ | Host]} ->
            %% TODO: support ipv6
            case inet:getaddr(Host, inet) of
                {ok, IP} ->
                    {ok, {NodeName, Host, IP}};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, {bad_name, Name}}
    end.
