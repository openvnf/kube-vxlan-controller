FROM aialferov/alpinerl

ADD bin /bin

ENTRYPOINT ["/bin/kube-vxlan-controller"]
