FROM aialferov/alpinerl

ADD bin /bin

CMD /bin/kube-vxlan-controller
