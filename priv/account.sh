#!/bin/sh
kubectl apply -f priv/account.yaml

kubectl get -n kube-system secrets/kube-vxlan-controller-token-xbtcs -o=jsonpath='{.data.namespace}' | cut -d " " -f4 | base64 -d > pki/namespace
kubectl get -n kube-system secrets/kube-vxlan-controller-token-xbtcs -o=jsonpath='{.data.ca\.crt}' | cut -d " " -f4 | base64 -d > pki/ca.crt
kubectl get -n kube-system secrets/kube-vxlan-controller-token-xbtcs -o=jsonpath='{.data.token}' | cut -d " " -f4 | base64 -d > pki/token
