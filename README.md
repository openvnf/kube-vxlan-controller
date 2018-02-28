# Kube VXLAN Controller

Any pod in a [Kubernetes](https://kubernetes.io) cluster can become a
[VXLAN](https://tools.ietf.org/html/rfc7348) overlay network member by having a
VXLAN type network interface set up and L2 peer forwarding entries specified.
This can be done automatically on pod creation by using the
Kube VXLAN Controller. A pod could be configured to have any number of VXLAN
interfaces, i.e. to be a member of any number of VXLAN Segments.

## Deployment

The controller monitors pods using the
[Kubernetes API](https://kubernetes.io/docs/reference/api-overview) and could
run as a standalone application provided with a desired cluster API access.
But the most simple way is to run it as a part of the Kubernetes cluster itself,
for example as a Kubernetes deployment. To deploy it this way execute the
following command (from a root folder of this repository copy):

```
$ kubectl apply -f k8s.yaml
```

## Usage

To make a pod VXLAN enabled it should answer the following conditions:

1. Have a `vxlan.travelping.com: "true"` label;
2. Have a `vxlan.travelping.com/names: <VXLAN name list>` annotation;
3. Run a Kube VXLAN Controller Agent sidecar container with the security context
"NET_ADMIN" capability.

These conditions might be described in a single manifest this way:

```
spec:
  template:
    metadata:
      labels:
        vxlan.travelping.com: "true"
      annotations:
        vxlan.travelping.com/names: vxeth0, vxeth1
    spec:
      containers:
      - name: vxlan-controller-agent
        image: aialferov/kube-vxlan-controller-agent
        securityContext:
          capabilities:
            add:
            - NET_ADMIN
```

This be saved into a file (for example "patch.yaml") and applied against a
running deployment by patching it:

```
$ kubectl patch deployment <name> -p "$(cat patch.yaml)"
```

Or could be merged into a deployment manifest before creating it.

In this example "vxeth0" and "vxeth1" are the list of VXLAN names that will be
set up in a pod. The network interface created in a pod will have a specified
VXLAN name. In this example case two interfaces "vxeth0" and "vxeth1" will be
created.

According to [VXLAN specification](https://tools.ietf.org/html/rfc7348#section-4)
during the setup process a VXLAN should be provided with a Segment ID or
"VXLAN Network Identifier (VNI)". The controller does that automatically using
the predefined Kubernetes configmap object that should exist by the time of
creating a VXLAN. The configmap describes relation of a VXLAN name to its VNI.

The manifest used in the "Deployment" section defines a configmap with initial
set of VXLAN name to VNI relations and could be edited using this command:

```
$ kubectl -n kube-system edit configmap kube-vxlan-conrtoller
```

To add or remove a relation the "data" section needs to be changed only.
