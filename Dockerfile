FROM aialferov/r3tmpl AS builder

COPY . src
RUN make -C src docker-ready DESTDIR=/build

FROM aialferov/erlang-ready:basic

COPY --from=builder /build /

ENTRYPOINT ["/usr/local/bin/kube-vxlan-controller"]
