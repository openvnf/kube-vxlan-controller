# Kube VXLAN Controller

Any pod in a [Kubernetes] cluster can become a [VXLAN] overlay network member by
having a VXLAN type network interface set up and L2 peer forwarding entries
specified.  This can be done automatically on pod creation by using the Kube
VXLAN Controller. A pod could be configured to have any number of VXLAN
interfaces, i.e. to be a member of any number of VXLAN Segments.

## Deployment

The controller monitors pods using the [Kubernetes API] and could run as a
standalone application provided with a desired cluster API access. But the most
simple way is to run it as a part of the Kubernetes cluster itself, for example
as a Kubernetes deployment. To deploy it this way execute the following command
(from a root folder of this repository copy):

```
$ kubectl apply -f k8s.yaml
```

## Usage

To make a pod VXLAN enabled it should answer the following conditions:

* have a "vxlan.travelping.com" label set to "true"
* have a "vxlan.travelping.com/networks" annotation set to a list of networks
* run a Kube VXLAN Controller Agent init container with the security context
"NET_ADMIN" capability
* run a Kube VXLAN Controller Agent sidecar container with the security context
"NET_ADMIN" capability.

A list of networks is a comma or new line separated values representing network
name each.

These conditions could be described in a single manifest:

```
spec:
  template:
    metadata:
      labels:
        vxlan.travelping.com: "true"
      annotations:
        vxlan.travelping.com/networks: vxeth0, vxeth1
    spec:
      initContainers:
      - name: vxlan-controller-agent-init
        image: aialferov/kube-vxlan-controller-agent
        securityContext:
          capabilities:
            add: ["NET_ADMIN"]
      containers:
      - name: vxlan-controller-agent
        image: aialferov/kube-vxlan-controller-agent
        securityContext:
          capabilities:
            add: ["NET_ADMIN"]
```

This can be saved into a file (for example "patch.yaml") and applied against a
running deployment by patching it:

```
$ kubectl patch deployment <name> -p "$(cat patch.yaml)"
```

Or could be merged into a deployment manifest before creating it.

In this example "vxeth0" and "vxeth1" are the list of VXLAN names that will be
set up in a pod. The network interface created in a pod will have a specified
VXLAN name. In this example case two interfaces "vxeth0" and "vxeth1" will be
created.

### VXLAN Network Identifier

According to [VXLAN specification] during the setup process a VXLAN should be
provided with a Segment ID or "VXLAN Network Identifier (VNI)". The controller
does that automatically using the ID specified in the network options. The
options could be defined either in the "kube-vxlan-controller" configmap
or in the annotation (see "Network Configuration" below).

The manifest used in the "Deployment" section defines a configmap with initial
set of options defining VXLAN name to VNI relations and could be edited using
this command:

```
$ kubectl -n kube-system edit configmap kube-vxlan-controller
```

To add or remove a relation the "data" section needs to be changed only.

### Network Configuration

Networks could be customized with a set of parameters specified in annotation.
The following parameters are supported:

* type — network type (default: vxlan)
* id — network identifier (default: according to the configmap)
* name — network interface name (default: network name)
* dev — device used to create a network (default: eth0)

Examples:

```
anntations:
  vxlan.travelping.com/networks: |
    vxeth0
      id=1000
      dev=tun0
    vxeth1
```
```
anntations:
  vxlan.travelping.com/networks: vxeth0 id=1000 dev=tun0, vxeth1
```

When specified in annotation, a network is configured on a pod level. To set
configuration on a cluster level, the network options configmap should be
modified accordingly:

```
data:
  vxeth0: id=1000 dev=tun0
  vxeth1: id=1001
```

A pod level configuration pair overrides a cluster level one.

## Controller Workflow

The controller is subscribed to the pod events using the [Pod Watch API]. On the
"Pod added" event the controller is looking for the network list annotation and
sets up VXLAN networks according to it using the Agent init container. Thus the
other init containers available in a pod can already work with the interfaces.
Once the interfaces are set up, the controller sends a TERM signal to the main
process of the Agent to let it terminate so that the pod could proceed with its
creation.

Once a pod is running the sidecar Agent container is used to configure fdb
entries to set up configured networks peers forwarding. If added or removed pod
is a member of a certain network, the controller makes sure all the pods in
this network get the fdb entries table updated.

The controller uses the "Pod Exec API" to execute commands in a pod via [Agent]
container.

## Troubleshooting

If a pod is not becoming a VXLAN network member or hangs in the agent init
container it is possible to check if the pod answers the membership
requirements. This could be done automatically using the [scripts/check.sh]
script of this repository.

<!-- Links -->
[Kubernetes]: https://kubernetes.io
[Kubernetes API]: https://kubernetes.io/docs/reference/api-overview
[Pod Watch API]: https://v1-8.docs.kubernetes.io/docs/api-reference/v1.8/#watch-64
[VXLAN]: https://tools.ietf.org/html/rfc7348
[VXLAN specification]: https://tools.ietf.org/html/rfc7348#section-4
[Agent]: https://gitlab.tpip.net/aalferov/kube-vxlan-controller-agent
[scripts/check.sh]: https://gitlab.tpip.net/aalferov/kube-vxlan-controller/raw/master/scripts/check.sh
