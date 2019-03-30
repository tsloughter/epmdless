# Erlang (and Elixir) distribution without epmd, aka EPMDLESS #

Allows to connect erlang nodes, via Erlang distribution and without epmd, using tcp or tls.

## Requirements ##
 Erlang >= 19.1

## Configuration ##

### Using TCP as transport protocol ###
```
{ epmdless, [
    {transport, tcp},
    {listen_port, 17012}
    ]
}.
```

### Using TLS as transport protocol ###
```
{epmdless, [
    {transport, tls},
    {listen_port, 17012},
    {ssl_dist_opt, [
        {client, [ssl:ssl_option()]},
        {server, [ssl:ssl_option()]}
    ]}
]}
```

### VM args ###
Erlang VM needs few extra flags on start to disable epmd daemon startup and override empd client callback module.

Minimal set of options:
`erl -proto_dist epmdless_proto -start_epmd false -epmd_module epmdless_client`

Note: since epmd is disabled you need to populate epmd-client database manually. Check epmd_dist module API.

### Example usage ###
```
(app2@host.local)1> epmdless_client:add_node('app1@host.local', 17012).
ok
(app2@host.local)2> epmdless_client:list_nodes().
[{'app1@host.local',{"host.local",17012}}]
```

### Example project ###

Please also refer: https://github.com/oltarasenko/erlang_distribution_in_docker
for an example of complete project running epmdless.


## Connecting to remote nodes ##

With empdless it might be not easy to connect to remote nodes, as all normal node attach scripts are using epmd :(.
However we have developed a simple escript which allows to attach to remote nodes. Please refer node_attach for details.


Example usage:
1) Copy node_attach_template to the <release>/bin folder (rename it to node_attach);
2) Use the following command: `bin/node_attach <remote_node_name> <cookie> <distribution_port>`

```
➜  less_release bin/node_attach less@127.0.0.1 BESBOAZETBZLBZUGLUQN 17012
=INFO REPORT==== 30-Mar-2019::20:23:42 ===
Started local node: 'escript_44193@127.0.0.1'

=INFO REPORT==== 30-Mar-2019::20:23:42 ===
Starting erlang distribution at port 56793

=ERROR REPORT==== 30-Mar-2019::20:23:42 ===
Adding a node: 'less@127.0.0.1'

=INFO REPORT==== 30-Mar-2019::20:23:42 ===
Found host "127.0.0.1" for node 'less@127.0.0.1'

=INFO REPORT==== 30-Mar-2019::20:23:42 ===
Resolved port for "less"/"127.0.0.1" to 17012

=INFO REPORT==== 30-Mar-2019::20:23:42 ===
Connected to {{127,0,0,1},17012}
Erlang/OTP 20 [erts-9.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.2  (abort with ^G)
(less@127.0.0.1)1>
```




