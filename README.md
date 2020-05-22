# Erlang (and Elixir) distribution without EPMD, aka EPMDLESS #

![Shellcheck](https://github.com/tsloughter/epmdless/workflows/Shellcheck/badge.svg) [![Hex pm](http://img.shields.io/hexpm/v/epmdless.svg?style=flat)](https://hex.pm/packages/epmdless) 

Allows to connect erlang nodes, via Erlang distribution and without epmd, using tcp or tls.

## Requirements ##

- Erlang >= 21

## Usage ##

`epmdless` has 2 options for how ports are handled when Erlang distribution requests the port to use to connect to another node. With `epmdless_client` (variable ports) each node must first be added with a call to `epmdless_client:add_node` and this information is not automatically propagated between nodes like with EPMD, so `a` adding `b` and `c` and then connecting to them will not result in `b` and `c` creating a full mesh unless one of them has the other added to its mapping of nodes to ports with `epmdless_client:add_node`.

The other option is `epmdless_static` (static port). With this module the same port is used for every node it attempts to connect to, meaning when `a` connects to `b` and `c` a full mesh will be created because `b` will be able to look up `c`'s port and connect to it.

### Static Port ###

As of OTP-23.0 EPMD is still required to use a static port for distribution. This will be changed in an upcoming release, but for OTP releases before the upcoming support and for more dynamic options (see below) `epmdless` can be used and will continue to work on newer OTP-23.x versions.

At this time [rebar3 nightly](https://rebar3-nightly.s3.amazonaws.com/rebar3) is required for generating a release that will have working remote console and rpc.

#### Docker Usage: Connecting Nodes ####

An example project `examples/erlang_docker_example` contains a project setup to use `epmdless_static` as the `epmd_module` and a `docker-compose.yml` config that will bring up multiple nodes that all use the same port for distribution each on the same network with specific IP addresses.

The release's `vm.args.src` uses `epmdless_static` for `epmd_module` which results in the same port being used for any lookup of a node. Since the same port is used for each node the port is simply set in `vm.args.src` with `-erl_epmd_port 8001`:

```
-sname epmdless_test

-setcookie epmdless_test

-start_epmd false
-epmd_module epmdless_static
-erl_epmd_port 8001
```

In `docker-compose.yml` 3 nodes are created `node_a`, `node_b` and `node_c`: 

```yaml
  node_a:
    container_name: node_a
    hostname: node_a
    build: .
```

After running `docker-compse up` we can show that the full mesh network is created in this case when we connect `node_a` to `node_b` and `node_c`:

```
$ docker exec -ti node_a bin/epmdless_test remote_console

(epmdless_test@node_a)1> net_adm:ping(epmdless_test@node_b).
pong
(epmdless_test@node_a)2> net_adm:ping(epmdless_test@node_c).
pong
(epmdless_test@node_a)3> nodes().
[epmdless_test@node_b,epmdless_test@node_c]
```

Now open a `remote_console` on `node_c` to see that it is connected to both `node_a` and `node_b`:

```
$ docker exec -ti node_c bin/epmdless_test remote_console

(epmdless_test@node_c)1> nodes().
[epmdless_test@node_a,epmdless_test@node_b]
```

### Release: Variable Ports ###

Below in section [[Manual Usage]] you'll find details on how to use `epmdless` without building a release, this is useful for playing around or developing on `epmdless`, in this section we will be using a release built with the latest `rebar3` nightly, OTP-23 and 

The example project is under `examples/erlang_variable_ports_example`. The `rebar3` configuration shows how to include `epmdless` in your project and release:

```erlang
{erl_opts, [debug_info]}.
{deps, [epmdless]}.

{relx, [{release, {epmdless_test, "0.1.0"},
         [epmdless,
          epmdless_test]},
        {dev_mode, false},
        {include_erts, true},
        {extended_start_script, true},
        {vm_args_src, "config/vm.args.src"}]}.
```

Note that `epmdless` is kept in the release's list of applications and not in a `.app.src` list of applications. This is because it is not an actual runtime dependency of the application `epmdless_test` but a dependency of the particular release that applications is being used in.

In `vm.args.src` you'll find `epmdless_client` being setup as the `epmd_module` and the `epmd` daemon being disabled:

```
-sname ${NAME}@localhost

-setcookie epmdless_test

-start_epmd false
-epmd_module epmdless_client
```

Note that in this example there is no `-erl_epmd_port` in the `vm.args.src` because we will not use the same port for every node.

Build the release as usual:

```
$ rebar3 release
===> Plugin rebar3_hex not available. It will not be used.
===> Verifying dependencies...
===> Compiling epmdless
===> Compiling epmdless_test
===> Assembling release... epmdless_test-0.1.0
===> Warnings generating release:
*WARNING* Missing application sasl. Can not upgrade with this release
===> Release successfully assembled: _build/default/rel/epmdless_test
```

To run a release on a specific port set the `ERL_DIST_PORT` environment variable and run the console:

```
$ NAME=a ERL_DIST_PORT=8081 _build/default/rel/epmdless_test/bin/epmdless_test console
(a@localhost)1>
```

Connecting with a remote shell works the same:

```
$ NAME=a ERL_DIST_PORT=8081 _build/default/rel/epmdless_test/bin/epmdless_test remote_console
(a@localhost)1>
```

Or running an RPC:

```
$ NAME=a ERL_DIST_PORT=8081 _build/default/rel/epmdless_test/bin/epmdless_test rpc 'erlang nodes [hidden]'
[c371@rosa]
```

In this case, since the release is running on OTP-23, [erl_call](http://erlang.org/doc/man/erl_call.html) is used for `rpc` and `eval` commands and assign themselves a hidden nodename and in this case it gives itself the name `c371@rosa`.

Run another instance on a separate port and connect it to the first

```
$ NAME=b ERL_DIST_PORT=8082 _build/default/rel/epmdless_test/bin/epmdless_test console
(b@localhost)1> epmdless_client:add_node(a@localhost, 8081).
ok
(b@localhost)2> epmdless_client:list_nodes().
[{{"a",{127,0,0,1}},{"localhost",8081}}]
(b@localhost)3> net_adm:ping(a@localhost).
pong
(b@localhost)4> nodes().
[a@localhost]
```

Since the mapping of nodes to ports must be kept manually by calling `add_node/2` if a third node `c` is added and connected to `a` it will not create a full mesh as you'd expect with regular usage of Erlang distribution and EPMD. That is, unless you first use `add_node/2` to add `b` and its port to the mapping in `c`, then it will be able to automatically create the full mesh.

## Manual Usage ##

The following details how to use `epmdless` with `erl` directly.

### Example Usage with Multiple Ports ###

To play around with `epmdless` and get a feel for how it works the simplest way is to run a couple separate `erl` shells and connect them.

Only run the Erlang shell commands after havng start both `a`, `b`, and `c`, each on a different port `8001`, `8002` and `8003`:

```
ERL_DIST_PORT=8002 erl -sname b@localhost -start_epmd false -epmd_module epmdless_client -pa _build/default/lib/epmdless/ebin

> 
```

```
ERL_DIST_PORT=8001 erl -sname a@localhost -start_epmd false -epmd_module epmdless_client -pa _build/default/lib/epmdless/ebin

(a@localhost)1> epmdless_client:add_node(b@localhost, 8002).
ok
(a@localhost)2> epmdless_client:list_nodes().
[{{"b",{127,0,0,1}},{"localhost",8002}}]

```

```
ERL_DIST_PORT=8003 erl -sname c@localhost -start_epmd false -epmd_module epmdless_client -pa _build/default/lib/epmdless/ebin

(c@localhost)1> epmdless_client:add_node(a@localhost, 8001).                                                ok
(c@localhost)2> epmdless_client:list_nodes().
[{{"a",{127,0,0,1}},{"localhost",8001}}]
(c@localhost)3> net_adm:ping(a@localhost).
pong
(c@localhost)4> 2020-05-17T09:14:44.300102-06:00 error: ** Cannot get connection id for node c@localhost
2020-05-17T09:14:51.299676-06:00 warning: global: c@localhost failed to connect to b@localhost
(c@localhost)5> nodes().
[a@localhost]
(c@localhost)6> epmdless_client:list_nodes().
[{{"a",{127,0,0,1}},{"localhost",8001}}]
```

Node `a` is connected to `b` and `c` but `c` and `b` are not able to connect to each other because they have no had their respective ports added to the `epmdless_client` state with `add_node/2`. This results in the warnings about `failed to connect to b@localhost` on node `c`.

### Connecting to epmdless nodes ###

As of OTP-23 there is an option `-dist_listen` which keeps a remote shell from binding to a listen port -- and implies `-hidden` so this argument is not needed. Before this option a port had to be bound to even when wanting to simply use `erl -remsh a@localhost`. This complicated use of `epmdless` where we want to tell it the port of the remote node `a@localhost` and not care about any port for the new node that will make the remote connection.

So with OTP-23 it is possible to use the same `ERL_DIST_PORT` environment variable as used above but in this case it will not be used to listen but only to connect to `a@localhost`:

```
$ ERL_DIST_PORT=8001 erl -dist_listen false -remsh a@localhost -epmd_module epmdless_client -pa _build/default/lib/epmdless/ebin

(a@localhost)1> nodes(hidden).
['NAXDANHFJWVF@rosa']
```

For backwards compatibility reasons the environment variable `EPMDLESS_REMSH_PORT` is also still supported and can be used in place of `ERL_DIST_PORT` and will need to if on OTP before 23 because the new node will have to bind to a listen port.

Also in OTP-23 the option `-address [Hostname:]Port` was added to `erl_call`, a program for communicating with a distributed Erlang node. This is how the `relx` script does the `rpc` and `eval` commands if run on OTP-23:

```
$ _build/default/rel/epmdless_test/erts-11.0/bin/erl_call  -r -address :8081 -c epmdless_test -a 'erlang nodes [hidden]'
[c899@rosa]
```

With OTP-22 and earlier a remote shell requires setting `EPMDLESS_REMSH_PORT` to the port the node you want to connect to is using and optionally give a port for `ERL_DIST_PORT` -- if not given then `0` is uesd and a random port is chosen:

```
$ EPMDLESS_REMSH_PORT=8081 erl -sname a_remsh@localhost -remsh epmdless_test@localhost -epmd_module epmdless_client -hidden -setcookie epmdless_test -pa _build/default/checkouts/epmdless/ebin

(epmdless_test@localhost)1> nodes(hidden).
[a_remsh@localhost]
```

## Tests ##

Testing is done with [shelltestrunner](https://github.com/simonmichael/shelltestrunner/) and an example project under `shelltests/epmdless_test`. From that directory run `shelltest -c --diff --all --execdir -- epmdless_test.test`. It is also run with the latest `rebar3` and `relx` in github actions for this repo.

## Other example projects ###

Note: These will be updated for the latest `epmdless` options and moved to the `examples/` directory. But for now refer to these projects 

Please also refer: https://github.com/oltarasenko/erlang_distribution_in_docker for an example of complete project running epmdless.

### TLS example for Elixir ###

Here is a small project which shows how to setup EPMDLess with TLS for Elixir: https://github.com/oltarasenko/epmdless-elixir-example

(Please also see a discussion here: https://github.com/oltarasenko/epmdless/issues/11)
