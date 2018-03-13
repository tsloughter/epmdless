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
````

### Example project ###

Please also refer: https://github.com/oltarasenko/erlang_distribution_in_docker
for an example of complete project running epmdless.


