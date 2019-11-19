-module(epmdless_SUITE).

%%% ==================================================================
%%% Common Tests Callbacks Exports
%%% ==================================================================

-export([
  all/0,
  groups/0,
  init_per_suite/1,
  end_per_suite/1
]).

%%% ==================================================================
%%% CT Exports
%%% ==================================================================

-export([
  epmdless_add_node_must_ok/1,
  epmdless_remove_node_must_ok/1
]).

%%% ==================================================================
%%% Specification
%%% ==================================================================

-type config() :: [{atom(), term()}].

%%% ==================================================================
%%% Common Tests Callbacks
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Init all groups
%% @end
%% -------------------------------------------------------------------
-spec all() -> lists:list().

all() ->
  [
    {group, epmdless}
  ].

%% -------------------------------------------------------------------
%% @doc
%% Groups
%% @end
%% -------------------------------------------------------------------
-spec groups() -> lists:list().

groups() ->
[
  {epmdless, [sequence], [
    epmdless_add_node_must_ok,
    epmdless_remove_node_must_ok
  ]}
].

%% -------------------------------------------------------------------
%% @doc
%% Init per suite
%% @end
%% -------------------------------------------------------------------
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
  Config.

%% -------------------------------------------------------------------
%% @doc
%% End per suite
%% @end
%% -------------------------------------------------------------------
-spec end_per_suite(config()) -> config().

end_per_suite(Config) ->
  Config.

%%% ==================================================================
%%% Test cases for epmdless group
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Add node
%% @end
%% -------------------------------------------------------------------
-spec epmdless_add_node_must_ok(config()) -> ok.

epmdless_add_node_must_ok(_) ->
  {ok, _} = epmdless_client:start_link(),
  {ok, Name} = application:get_env(epmdless_test, name),
  {ok, Port} = application:get_env(epmdless_test, port),
  {ok, _} = net_kernel:start([Name, shortnames]),
  Node = erlang:node(),
  ok = epmdless_client:add_node(Node, Port),
  case epmdless_client:list_nodes() of
    [{Node, {_, Port}}] = Nodes ->
      ct:comment("Node ~p was added to nodes ~p", [Node, Nodes]);
    {error, Reason} ->
      ct:fail(Reason);
    Any ->
      ct:fail("epmdless_add_node_must_ok/1 failed. Not expected: ~p", [Any])
  end.

%% -------------------------------------------------------------------
%% @doc
%% Remove node
%% @end
%% -------------------------------------------------------------------
-spec epmdless_remove_node_must_ok(config()) -> ok.

epmdless_remove_node_must_ok(_) ->
  Node = erlang:node(),
  {ok, _} = epmdless_client:start_link(),
  ok = epmdless_client:remove_node(Node),
  case epmdless_client:list_nodes() of
    [] ->
      ct:comment("Node ~p was removed", [Node]);
    {error, Reason} ->
      ct:fail(Reason);
    Any ->
      ct:fail("epmdless_remove_node_must_ok/1 failed. Not expected: ~p", [Any])
  end.
