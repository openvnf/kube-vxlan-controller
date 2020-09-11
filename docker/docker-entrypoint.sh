#!/bin/sh
# copy of configuration files
RELPATH=$(find /opt/kube-vxlan-controller/releases/ -mindepth 1 -maxdepth 1 -type d)
[ -f /config/kube-vxlan-controller/sys.config ] && cp /config/kube-vxlan-controller/sys.config $RELPATH/sys.config
[ -f /config/kube-vxlan-controller/vm.args ] && cp /config/kube-vxlan-controller/vm.args $RELPATH/vm.args

exec "$@"
