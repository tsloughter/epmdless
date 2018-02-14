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


### Example usage ###
```
(app2@host.local)1> epmdless_client:add_node('sample1@oleg.local', 17012).
ok
(app2@host.local)2> epmdless_client:list_nodes().
[{'app1@host.local',{"host.local",17012}}]
````
