ARG ERLANG_VERSION=21.0.5
FROM aialferov/erlang:$ERLANG_VERSION AS builder
LABEL project=kube-vxlan-controller

COPY . src
RUN make -C src package-ready DESTDIR=/build

FROM alpine
LABEL project=kube-vxlan-controller
RUN apk add --no-cache --update ncurses

COPY --from=builder /build /

ENTRYPOINT ["/usr/local/bin/kube-vxlan-controller"]
