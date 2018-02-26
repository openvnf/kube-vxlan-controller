# Kube VXLAN Controller

Any pod in a Kubernetes cluster can become a VXLAN member by having a VXLAN type
network interface set up and L2 peer forwarding entries specified.
This can be done automatically on pod creation by using the
Kube VXLAN Controller. A pod could be configured to have any number of VXLAN
interfaces, i.e. to be a member of any number of this type networks.

## Deployment

The Controller monitors pods using the Kubernetes API and could be run as a
standalone application provided with the desired cluster API access. But the
most simple way is to run it as a part of the cluster itself, for example as
a Kubernetes Deployment. To deploy it this way execute the following command
(from a root folder of this repository copy):

```
$ kubectl apply -f k8s.yaml
```

## Usage

To make a pod VXLAN enabled it should answer the following conditions:

1. Have a `vxlan: "true"` label;
2. Have a `vxlan.travelping.com/names: <LAN list>` annotation;
3. Run a Kube VXLAN Controller Agent sidecar container with a "NET_ADMIN"
capability.

These conditions might be described in a single manifest this way:

```
spec:
  template:
    metadata:
      labels:
        vxlan: "true"
      annotations:
        vxlan.travelping.com/names: lan1, lan2
    spec:
      containers:
      - name: vxlan-controller-agent
        image: aialferov/kube-vxlan-controller-agent
        securityContext:
          capabilities:
            add:
            - NET_ADMIN
```

In this example "lan1" and "lan2" are the "LAN list" which is a list of VXLAN
names that will be set up in a pod. The VXLAN interface created in a pod will
have a specified LAN name. In this example case two interfaces "lan1" and "lan2"
will be created.

During setup process a VXLAN should be provided with an Id. The controller gets
this Id from the "kube-vxlan-controller" configmap Kubernetes object that should
exist by the time of creating a VXLAN.

The manifest menitoned in the "Deployment" section ("k8s.yaml") defines the
initial set of VXLAN name/id pairs and could be seen or edited using this
command:

```
$ kubectl -n kube-system edit configmap kube-vxlan-conrtoller
```

To edit the set the "data" section needs to be changed only. Just add or remove
another name/id pair.

The example above could be saved into a file (for example "patch.yaml") and
applied to a running deployment by patching it:

```
$ kubectl patch deployment <name> -p "$(cat patch.yaml)"
```

Or could be merged into a deployment manifest before creating it.
