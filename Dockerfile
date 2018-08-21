ARG ERLANG_VERSION=21.0.5
FROM aialferov/erlang:$ERLANG_VERSION AS builder

COPY . src
RUN make -C src package-ready DESTDIR=/build

FROM aialferov/erlang-ready:ssl

COPY --from=builder /build /

ENTRYPOINT ["/usr/local/bin/kube-vxlan-controller"]
