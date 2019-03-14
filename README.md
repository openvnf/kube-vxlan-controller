# Kube VXLAN Controller

[![License: Apache-2.0][Apache 2.0 Badge]][Apache 2.0]
[![GitHub Release Badge]][GitHub Releases]

Any pod in a [Kubernetes] cluster can become a [VXLAN] overlay network member by
having a VXLAN type network interface set up and L2 peer forwarding entries
specified. This can be done automatically on a pod creation by using the Kube
VXLAN Controller. A pod could be configured to have any number of VXLAN
interfaces, i.e. to be a member of any number of VXLAN Segments.

## Deployment

The controller monitors pods using the [Kubernetes API] and could run as a
standalone application provided with a desired cluster API access. But the most
simple way is to run it as a part of the Kubernetes cluster itself, for example
as a Kubernetes deployment. This repository provides a [Bundle Manifest] to
create such a deployment and related workloads:

```
$ kubectl apply -f https://raw.githubusercontent.com/openvnf/kube-vxlan-controller/master/kubernetes/bundle.yaml
```

## Usage

To make a pod VXLAN enabled it should answer the following conditions:

* have a "vxlan.openvnf.org" label set to "true"
* have a "vxlan.openvnf.org/networks" annotation describing VXLAN networks
* run a Kube VXLAN Controller Agent init container with the security context
"NET_ADMIN" capability
* run a Kube VXLAN Controller Agent sidecar container with the security context
"NET_ADMIN" capability.

These conditions could be described in a single manifest:

```
spec:
  template:
    metadata:
      labels:
        vxlan.openvnf.org: "true"
      annotations:
        vxlan.openvnf.org/networks: vxeth0, vxeth1
    spec:
      initContainers:
      - name: vxlan-controller-agent-init
        image: openvnf/kube-vxlan-controller-agent
        securityContext:
          capabilities:
            add: ["NET_ADMIN"]
      containers:
      - name: vxlan-controller-agent
        image: openvnf/kube-vxlan-controller-agent
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

In this example "vxeth0" and "vxeth1" are the list of VXLAN network names that
will be created in a pod. In this case the network interfaces created in a pod
will have the same names as the specfied network names, i.e "vxeth0" and
"vxeth1". This could be configured via "Network Options" (see below).

### VXLAN Network Identifier

According to [VXLAN specification] during the setup process a VXLAN should be
provided with a Segment ID or "VXLAN Network Identifier (VNI)". The controller
does that automatically using the ID specified in the network options (see
"Network Options" below).

### Network Options

Network options define how the controller behaves setting up a particular
network interface. Options could be defined in networks defining configmap
or in a network defining annotation. Options defined in the configmap apply to
all the VXLAN enabled pods in a cluster. Options defined in a pod annotation
overrides that for the pod.

Example of the options defined in the configmap:


```
data:
  vxeth0: id=1000 dev=tun0
  vxeth1: id=1001 up
```

Example of the options defined in a pod annotation:

```
annotations:
  vxlan.openvnf.org/networks: |
    vxeth0
      ip=192.168.10.1/24
      route=192.168.10.0/24:192.168.100.1
    vxeth1
      up=false
```
```
annotations:
  vxlan.openvnf.org/networks: vxeth0 dev=tun0, vxeth1
```

The only mandatory option for now is "id" (VNI) and should be explicitly defined
in either configmap or annotation.

The manifest used in the "Deployment" section defines "kube-vxlan-controller"
configmap with some options for networks named "vxlan[0-3]" including "id".
Therefore, the example above with "vxeth0" and "vxeth1" networks works without
complains about non-existing id.

#### type

Network interface type that will be created in a pod. Currently the "vxlan" one
is only tested and supported and a default value, therefore usually should not
be specified.

#### id

Defines network identified (VNI in case of VXLAN). Mandatory, no default value.
Usually should be specified in the configmap. Specify it in a pod annotation
if you know what you do only.

#### name

Name of the actual network interface that will be created. Will be set to a
network name if not specified.

#### dev

A pod network device used to create a network interface (default: "eth0").

#### up

Defines whether a created interface should be set up. Can be set to "true" or
"false". Specifying without any value implies "true". If specified globaly in
the configmap, could be overriden with "up=false".

#### ip

Defines an IP address that will be assigned to a created network interface.

#### route

Defines a routing table rule that controller can create after an interface
setup. The following example describes how to create a rule for routing to the 
subnet "192.168.2.0/24" via the "192.168.1.1" gateway:

```
route=192.168.2.0/24:192.168.1.1
```

## Controller Workflow

The controller is subscribed to the pod events using the [Pod Watch API]. On the
"pod added" event the controller is looking for the networks annotation and
sets up networks according to it (and according to the networks configmap) using
the Agent init container. Thus the other init containers available in a pod can
already work with the interfaces.

Once the network interfaces are created and configured according to the options,
the controller sends a TERM signal to the main process of the Agent to let it
terminate so that the pod could proceed with its creation.

Once a pod is running the sidecar Agent container is used to configure fdb
entries to set up configured networks peers forwarding. If added or removed pod
is a member of a certain network, the controller makes sure all the pods in
this network get the fdb entries table updated.

The controller uses the "Pod Exec API" to execute commands in a pod via [Agent]
container.

## Example

A basic use case example demonstrates how to send packets from a pod "a" to 
a pod "b" provided that the pods are in the different VXLAN networks. This would
require a gateway pod being a member of the both networks and the routes set up
on "a" and "b". Use [Example Manifest] to create example workloads:

```
$ kubectl create -f https://raw.githubusercontent.com/openvnf/kube-vxlan-controller/master/kubernetes/example.yaml
```

This creates deployments and the corresponding pods "a", "b" and "gw" with the
VXLAN networks and the corresponding network interfaces configured the following
way:

- pod "a":
  - network: "vxeth1", IP: "192.168.11.2/29"
  - route: "192.168.12.0/29 via 192.168.11.1"
- pod "b":
  - network: "vxeth2", IP: "192.168.12.2/29"
  - route: "192.168.11.0/29 via 192.168.12.1"
- pod "gw":
  - network: "vxeth1", IP: "192.168.11.1/29"
  - network: "vxeth2", IP: "192.168.12.1/29"

To check that example works as expected we can ping one pod's VXLAN network
interface from another pod and vice versa:

```
$ POD_A=$(kubectl get po -l run=a -o jsonpath={.items[*].metadata.name})
$ POD_B=$(kubectl get po -l run=b -o jsonpath={.items[*].metadata.name})

$ kubectl exec -it $POD_A -c a ping 192.168.12.2
PING 192.168.12.2 (192.168.12.2): 56 data bytes
64 bytes from 192.168.12.2: seq=0 ttl=63 time=0.082 ms

$ kubectl exec -it $POD_B -c b ping 192.168.11.2
PING 192.168.11.2 (192.168.11.2): 56 data bytes
64 bytes from 192.168.11.2: seq=0 ttl=63 time=0.107 ms
```

## License

Copyright 2018-2019 Travelping GmbH

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

<!-- Links -->
[Kubernetes]: https://kubernetes.io
[Kubernetes API]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.10
[Pod Watch API]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.10/#watch-64
[VXLAN]: https://tools.ietf.org/html/rfc7348
[VXLAN specification]: https://tools.ietf.org/html/rfc7348#section-4
[Agent]: https://github.com/openvnf/kube-vxlan-controller-agent
[Example Manifest]: kubernetes/example.yaml
[Bundle Manifest]: kubernetes/bundle.yaml
[GitHub Releases]: https://github.com/openvnf/kube-vxlan-controller/releases

<!-- Badges -->

[Apache 2.0]: https://opensource.org/licenses/Apache-2.0
[Apache 2.0 Badge]: https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg?style=flat-square
[GitHub Releases]: https://github.com/openvnf/kube-vxlan-controller/releases
[GitHub Release Badge]: https://img.shields.io/github/release/openvnf/kube-vxlan-controller/all.svg?style=flat-square
