#!/usr/bin/env bash

set -xe

cd "$(dirname "$(realpath "$0")")"

export TERM=dumb

find . -name _build -exec rm -rf {} \; || true

rebar3_dir=$(mktemp -d)

pushd "${rebar3_dir}"

wget https://rebar3-nightly.s3.amazonaws.com/rebar3
chmod +x rebar3

export PATH="${rebar3_dir}:~/.cabal/bin/:${PATH}"

popd

# for some reason in github actions this just freezes and never
# goes anywhere with no output
# So for now the "test" is just written below in this script until shelltest
# works in github actions for this again.
#shelltest -c --diff --all --debug --execdir epmdless_test/epmdless_test.test

pushd epmdless_test/
rebar3 release

ERL_DIST_PORT=9001 _build/default/rel/epmdless_test/bin/epmdless_test daemon

ERL_DIST_PORT=9001 _build/default/rel/epmdless_test/bin/epmdless_test ping

ERL_DIST_PORT=9001 _build/default/rel/epmdless_test/bin/epmdless_test stop
