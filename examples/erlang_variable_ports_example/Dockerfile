# syntax = docker/dockerfile:experimental
FROM erlang:23 as builder

WORKDIR /app/src
# create the directory to unpack the release to
RUN mkdir -p /opt/rel

ENV REBAR_BASE_DIR /app/_build

RUN rm -f /etc/apt/apt.conf.d/docker-clean; echo 'Binary::apt::APT::Keep-Downloaded-Packages "true";' > /etc/apt/apt.conf.d/keep-cache
RUN --mount=type=cache,target=/var/cache/apt --mount=type=cache,target=/var/lib/apt \
    apt update && apt-get --no-install-recommends install -y git tar

RUN wget https://rebar3-nightly.s3.amazonaws.com/rebar3 -O /tmp/rebar3 && \
    chmod +x /tmp/rebar3

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock .
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    /tmp/rebar3 compile

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    /tmp/rebar3 tar && \
    tar -zxvf $REBAR_BASE_DIR/default/rel/*/*.tar.gz -C /opt/rel

FROM ubuntu:20.04 as runner

WORKDIR /opt/epmdless_test

ENV COOKIE=epmdless_test \
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # epmdless_test specific env variables to act as defaults
    LOGGER_LEVEL=debug \
    SCHEDULERS=1

RUN rm -f /etc/apt/apt.conf.d/docker-clean; echo 'Binary::apt::APT::Keep-Downloaded-Packages "true";' > /etc/apt/apt.conf.d/keep-cache
RUN apt-get update && apt-get --no-install-recommends install -y ncurses-base

COPY --from=builder /opt/rel .

ENTRYPOINT ["/opt/epmdless_test/bin/epmdless_test"]
CMD ["foreground"]

