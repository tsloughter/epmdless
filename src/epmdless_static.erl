%% @doc
%% Module which used as a callback which passed to erlang vm via `-epmd_module` attribute.
%%
%% Always returns the same port for any node it is asked about. Set the port either with
%% `-erl_epmd_port Port' in `vm.args' or with environment variable `ERL_DIST_PORT'
%% @end
-module(epmdless_static).

-behaviour(gen_server).

%% epmd callbacks
-export([start_link/0,
         register_node/3,
         address_please/3,
         port_please/2,
         listen_port_please/2,
         names/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include_lib("kernel/include/inet.hrl").

-record(state, {port :: inet:port_number()}).

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
    Port = case os:getenv("ERL_DIST_PORT") of
               false ->
                   try
                       %% name of this argument may change
                       {ok, [[StringPort]]} = init:get_argument(erl_epmd_port),
                       list_to_integer(StringPort)
                   catch error:_ ->
                           0
                   end;
               PortString ->
                   list_to_integer(PortString)
           end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

register_node(_Name, _Port, _Family) ->
    {ok, rand:uniform(3)}.

port_please(_Name, _Host) ->
    case gen_server:call(?MODULE, port_please, infinity) of
        {ok, Port} ->
            {port, Port, ?ERL_DIST_VER};
        _ ->
            {error, noport}
    end.

address_please(Name, Host, AddressFamily) ->
    {ok, Address} = inet:getaddr(Host, AddressFamily),
    case port_please(Name, Address) of
        {port, Port, Version} ->
            {ok, Address, Port, Version};
        noport ->
            {error, noport}
    end.

%% @doc Returns the port the local node should listen to when accepting new distribution requests.
listen_port_please(_Name, _Host) ->
    gen_server:call(?MODULE, listen_port, infinity).


names(_Hostname) ->
    {error, address}.

init([Port]) ->
    {ok, #state{port=Port}}.

handle_call(port_please, _From, State=#state{port=Port}) ->
    {reply, {ok, Port}, State};
handle_call(listen_port, _From, State=#state{port=DistPort}) ->
    {reply, {ok, DistPort}, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%

