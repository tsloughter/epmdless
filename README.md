# Erlang (and Elixir) distribution without epmd, aka EPMDLESS #
[![Hex pm](http://img.shields.io/hexpm/v/epmdless.svg?style=flat)](https://hex.pm/packages/epmdless) 

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

### Connecting to epmdless nodes ###
It's possible to connect to epmdless nodes using remsh. The distribution port can be provided via EPMDLESS_REMSH_PORT environmental variable.

Example usage:
```
EPMDLESS_REMSH_PORT=17012 erl -name local_node@127.0.0.1 -remsh epmdless_node@127.0.0.1 -setcookie <cookie> -proto_dist epmdless_proto -epmd_module epmdless_client -pa _build/default/lib/epmdless/ebin
```

### Example project ###

Please also refer: https://github.com/oltarasenko/erlang_distribution_in_docker
for an example of complete project running epmdless.

### TLS example for Elixir ###

Here is a small project which shows how to setup EPMDLess with TLS for Elixir:
https://github.com/oltarasenko/epmdless-elixir-example

(Please also see a discussion here: https://github.com/oltarasenko/epmdless/issues/11)

