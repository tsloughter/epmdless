%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
-module(epmdless_tls_dist).

-export([childspecs/0, listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

%% Generalized dist API
-export([gen_listen/2, gen_accept/2, gen_accept_connection/6,
	 gen_setup/6, gen_select/2]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

childspecs() ->
    {ok, [{
       epmdless_tls_dist_proxy, {epmdless_tls_dist_proxy, start_link, []},
	   permanent, infinity, worker, [epmdless_tls_dist_proxy]
    }]}.

select(Node) ->
    gen_select(inet_tcp, Node).

gen_select(Driver, Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_, Host] ->
	    case inet:getaddr(Host, Driver:family()) of
		{ok, _} -> true;
		_ -> false
	    end;
	_ -> 
	    false
    end.

is_node_name(Node) when is_atom(Node) ->
    select(Node);
is_node_name(_) ->
    false.

listen(Name) ->
    gen_listen(inet_tcp, Name).

gen_listen(Driver, Name) ->
    epmdless_tls_dist_proxy:listen(Driver, Name).

accept(Listen) ->
    gen_accept(inet_tcp, Listen).

gen_accept(Driver, Listen) ->
    epmdless_tls_dist_proxy:accept(Driver, Listen).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    gen_accept_connection(inet_tcp, AcceptPid, Socket, MyNode, Allowed, SetupTime).

gen_accept_connection(Driver, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    Kernel = self(),
    spawn_link(fun() -> do_accept(Driver, Kernel, AcceptPid, Socket,
				  MyNode, Allowed, SetupTime) end).

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    gen_setup(inet_tcp, Node, Type, MyNode, LongOrShortNames,SetupTime).

gen_setup(Driver, Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    Kernel = self(),
    spawn_opt(fun() -> do_setup(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) end, [link, {priority, max}]).
		   
do_setup(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    [Name, RawAddress] = splitnode(Driver, Node, LongOrShortNames),
    ErlEpmd = net_kernel:epmd_module(),
    Address = case ErlEpmd:host_please(Node) of
        nohost -> RawAddress;
        {host, H} -> H
    end,
    case inet:getaddr(Address, Driver:family()) of
        {ok, Ip} ->
            Timer = dist_util:start_timer(SetupTime),
            case ErlEpmd:port_please(Name, RawAddress) of
                {port, TcpPort, Version} ->
                    ?trace("port_please(~p) -> version ~p~n", 
                           [Node,Version]),
                    dist_util:reset_timer(Timer),
                    case epmdless_tls_dist_proxy:connect(Driver, Ip, TcpPort) of
                        {ok, Socket} ->
                            error_logger:info_msg("Connected to ~p~n", [{Ip, TcpPort}]),
                            HSData = connect_hs_data(Kernel, Node, MyNode, Socket, 
                                                     Timer, Version, Ip, TcpPort, RawAddress,
                                                     Type),
                            dist_util:handshake_we_started(HSData);
                        Other ->
                            error_logger:error_msg("Failed to connect to ~p with ~p~n", [{Ip, TcpPort}, Other]),
                            %% Other Node may have closed since 
                            %% port_please !
                            ?trace("other node (~p) "
                                   "closed since port_please.~n", 
                                   [Node]),
                            ?shutdown2(Node, {shutdown, {connect_failed, Other}})
                    end;
                Other ->
                    ?trace("port_please (~p) "
                           "failed.~n", [Node]),
                    ?shutdown2(Node, {shutdown, {port_please_failed, Other}})
            end;
        Other ->
            ?trace("inet_getaddr(~p) "
                   "failed (~p).~n", [Node,Other]),
            ?shutdown2(Node, {shutdown, {inet_getaddr_failed, Other}})
    end.

close(Socket) ->
    gen_tcp:close(Socket),
    ok.

do_accept(_Driver, Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    process_flag(priority, max),
    receive
	{AcceptPid, controller} ->
	    Timer = dist_util:start_timer(SetupTime),
        HSData = accept_hs_data(Kernel, MyNode, Socket, Timer, Allowed),
        dist_util:handshake_other_started(HSData)
    end.


%% If Node is illegal terminate the connection setup!!
splitnode(Driver, Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail =/= [] ->
	    Host = lists:append(Tail),
	    check_node(Driver, Name, Node, Host, LongOrShortNames);
	[_] ->
	    error_logger:error_msg("** Nodename ~p illegal, no '@' character **~n",
		      [Node]),
	    ?shutdown(Node);
	_ ->
	    error_logger:error_msg("** Nodename ~p illegal **~n", [Node]),
	    ?shutdown(Node)
    end.

check_node(Driver, Name, Node, Host, LongOrShortNames) ->
    case split_node(Host, $., []) of
	[_] when LongOrShortNames == longnames ->
	    case Driver:parse_address(Host) of
		{ok, _} ->
		    [Name, Host];
		_ ->
		    error_logger:error_msg("** System running to use "
					   "fully qualified "
					   "hostnames **~n"
					   "** Hostname ~s is illegal **~n",
					   [Host]),
		    ?shutdown(Node)
	    end;
	[_, _ | _] when LongOrShortNames == shortnames ->
	    error_logger:error_msg("** System NOT running to use fully qualified "
			      "hostnames **~n"
		      "** Hostname ~s is illegal **~n",
		      [Host]),
	    ?shutdown(Node);
	_ ->
	    [Name, Host]
    end.

split_node([Chr|T], Chr, Ack) -> 
    [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack) -> 
    split_node(T, Chr, [H|Ack]);
split_node([], _, Ack) -> 
    [lists:reverse(Ack)].

connect_hs_data(Kernel, Node, MyNode, Socket, Timer, Version, Ip, TcpPort, Address, Type) ->
    common_hs_data(Kernel, MyNode, Socket, Timer, 
		   #hs_data{other_node = Node,
			    other_version = Version,
			    f_address = 
				fun(_,_) ->
					#net_address{address = {Ip,TcpPort},
						     host = Address,
						     protocol = proxy,
						     family = inet}
				end,
			    request_type = Type
			   }).

accept_hs_data(Kernel, MyNode, Socket, Timer, Allowed) ->
    common_hs_data(Kernel, MyNode, Socket, Timer, #hs_data{
					     allowed = Allowed,
					     f_address = fun get_remote_id/2
					    }).

common_hs_data(Kernel, MyNode, Socket, Timer, HsData) ->
    HsData#hs_data{
      kernel_pid = Kernel,
      this_node = MyNode,
      socket = Socket,
      timer = Timer,
      this_flags = 0,
      f_send = 
	  fun(S,D) -> 
		  gen_tcp:send(S,D) 
	  end,
      f_recv = 
	  fun(S,N,T) -> 
		  gen_tcp:recv(S,N,T) 
	  end,
      f_setopts_pre_nodeup = 
	  fun(S) ->
		  inet:setopts(S, [{active, false}, {packet, 4}])
	  end,
		   f_setopts_post_nodeup = 
	  fun(S) -> 
		  inet:setopts(S, [{deliver, port},{active, true}])
	  end,
      f_getll = 
	  fun(S) -> 
		  inet:getll(S) 
	  end,
      mf_tick = 
	  fun(S) -> 
		  gen_tcp:send(S, <<>>)
	  end,
      mf_getstat = 
	  fun(S) ->
		  {ok, Stats} = inet:getstat(S, [recv_cnt, send_cnt, send_pend]),
		  R = proplists:get_value(recv_cnt, Stats, 0),
		  W = proplists:get_value(send_cnt, Stats, 0),
		  P = proplists:get_value(send_pend, Stats, 0),
		  {ok, R,W,P}
	  end}.

get_remote_id(Socket, _Node) ->
    case epmdless_tls_dist_proxy:get_tcp_address(Socket) of
        {ok, Address} ->
	    Address;
	{error, _Reason} ->
	    ?shutdown(no_node)
    end.
