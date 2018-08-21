USER = openvnf
PROJECT = kube-vxlan-controller

DOCKER_RUN_ARGS_EXTRA = \
	-v ${PWD}/priv/$(PROJECT).config:/etc/$(PROJECT)/config \
	-v ${PWD}/pki:/var/run/$(PROJECT)/pki \
	-w /var/run/$(PROJECT)

include mk/Erlangbin.mk
include mk/Docker.mk
