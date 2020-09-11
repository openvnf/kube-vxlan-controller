# -- build-environment --
# see https://docs.docker.com/engine/userguide/eng-image/multistage-build/

FROM erlang:23.0.4-alpine AS build-env
LABEL project=kube-vxlan-controller

WORKDIR /build
RUN     apk update && apk --no-cache upgrade && \
		apk --no-cache add \
			gcc \
			git \
			libc-dev libc-utils \
			libgcc \
			linux-headers \
			make bash \
			musl-dev musl-utils \
			ncurses-dev \
			pcre2 \
			pkgconf \
			scanelf \
			zlib

ADD     . /build
RUN     rebar3 as prod release

# -- runtime image --

FROM alpine:3.11
LABEL project=kube-vxlan-controller

WORKDIR /
RUN     apk update && \
		apk --no-cache upgrade && \
		apk --no-cache add zlib ncurses-libs libcrypto1.1 tini

COPY    docker/docker-entrypoint.sh /
COPY    --from=build-env /build/_build/prod/rel/ /opt/

ENTRYPOINT ["tini", "--", "/docker-entrypoint.sh"]
CMD        ["/opt/kube-vxlan-controller/bin/kube-vxlan-controller", "foreground"]
