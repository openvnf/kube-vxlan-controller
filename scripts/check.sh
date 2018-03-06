#!/bin/sh

usage="Usage: $(basename $0) [Namespace] <Pod>"

if [ -z "$1" ]; then
    echo $usage
    exit 2
fi

if [ -n "$2" ]; then 
    namespace="-n $1"
    pod="$2"
else
    pod="$1"
fi

kubectl="kubectl $namespace get pod $pod -o jsonpath"

$kubectl='{}' > /dev/null || exit 1

function check { description="$1" spec="$2" item="$3"
    echo "$description"
    if [ -z "$($kubectl=$spec | grep $item)" ]; then
        echo "'$item' is not found in '$spec'"
        exit 1
    fi
}

check "Checking label..." \
    '{.metadata.labels.vxlan}' true

check "Checking agent init container name..." \
    '{.spec.initContainers[*].name}' vxlan-controller-agent-init

check "Checking agent init container image..." \
    '{.spec.initContainers[*].image}' aialferov/kube-vxlan-controller-agent

check "Checking agent init container command..." \
    '{.spec.initContainers[*].command}' kube-vxlan-controller-agent-init

check "Checking agent init container capabilities..." \
    '{.spec.initContainers[*].securityContext.capabilities.add}' NET_ADMIN

check "Checking agent container name..." \
    '{.spec.containers[*].name}' vxlan-controller-agent

check "Checking agent container image..." \
    '{.spec.containers[*].image}' aialferov/kube-vxlan-controller-agent

check "Checking agent container capabilities..." \
    '{.spec.containers[*].securityContext.capabilities.add}' NET_ADMIN

echo "Done."
echo
echo "The pod is VXLAN enabled and is a member of the following networks:"
$kubectl='{.metadata.annotations.vxlan\.travelping\.com\/names}'
echo
