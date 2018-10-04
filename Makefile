include mk/Erlangbin.mk
include mk/Docker.mk

USER = openvnf

RUN_ARGS = run

DOCKER_RUN_ARGS_EXTRA = \
    -v ${PWD}/priv/$(PROJECT).config:/etc/$(PROJECT)/config \
    -v ${PWD}/pki:/var/run/$(PROJECT)/pki \
    -w /var/run/$(PROJECT)

ifdef ERLANG_VERSION
    DOCKER_BUILD_ARGS_EXTRA = \
        --build-arg ERLANG_VERSION=$(ERLANG_VERSION)
endif
