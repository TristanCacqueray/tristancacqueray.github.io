---
title: How to manually update Kubernetes Resources
date: 2022-07-15
tags: [blog, kubernetes]
---

> This post was initially published on the Software Factory blog: https://www.softwarefactory-project.io/how-to-manually-update-kubernetes-resources.html

This article demonstrates different strategies to update kubernetes
resources.

## Context and Problem Statement

Our goal is to update resource without overwritting changes made outside
of our control. For example, we would like to upgrade a container image
version or a deployment replicas count.

In the context of a kubernetes operator, there are two forms of resource
update: *replace* and *patch*. To demonstrate how the update process
works, we\'ll use the API directly by starting a proxy:

``` bash
kubectl proxy --port=8080
```

## Resource Version

Each resource is assigned a version number in its metadata. In this
example, we create a deployment named *demo-deployment* in the namespace
*tristanc*:

``` bash
curl -XPOST -H "Content-Type: application/json" http://localhost:8080/apis/apps/v1/namespaces/tristanc/deployments --data-binary @- <<EOF
{
  "metadata": {"name": "demo-deployment"},
  "spec": {
    "replicas": 1,
    "selector": {"matchLabels": {"app": "demo"}},
    "template": {
      "metadata": {"labels": {"app": "demo"}},
      "spec": {
        "containers": [{
            "name": "container-1"
            "args": ["sleep", "infinity"],
            "image": "registry.fedoraproject.org/fedora:latest",
            "imagePullPolicy": "IfNotPresent",
        }]
      }
    }
  }
}
EOF
```

The API returns the resource state with its *resourceVersion*. Each time
the resource is updated, the API automatically update the version
number.

You can run the update request below using:

``` bash
curl -XVERB -H "Content-Type: TYPE" http://localhost:8080/apis/apps/v1/namespaces/tristanc/deployments/demo-deployment --data-binary @- <<EOF
  BODY
EOF
```

## JSON Patch

A [JSON Patch RFC 6902](https://tools.ietf.org/html/rfc6902) is defined
by a **op** operation, **path** destination and a **value**.

``` bash
curl -XPATCH -H "Content-Type: application/json-patch+json"
```

### Add a container

``` json
[
  {
    "op": "add",
    "path": "/spec/template/spec/containers/-",
    "value": {
      "name": "container-2",
      "args": ["sleep", "infinity"],
      "image": "registry.fedoraproject.org/fedora:latest",
      "imagePullPolicy": "IfNotPresent"
    }
  }
]
```

### Remove a container

List indices are zero based.

``` json
[
  {
    "op": "remove",
    "path": "/spec/template/spec/containers/1"
  }
]
```

### Change the image

``` json
[
  {
    "op": "replace",
    "path": "/spec/template/spec/containers/0/image"
    "value": "registry.fedoraproject.org/fedora:36"
  }
]
```

### Test

A JSON Patch can also assert a desired state:

``` json
[
  {
    "op": "test",
    "path": "/spec/template/spec/containers/0/image",
    "value": "registry.fedoraproject.org/fedora:37"
  }
]
```

## JSON Merge Patch

A [JSON Merge Patch RFC 7396](https://tools.ietf.org/html/rfc7386) is
more similar to a diff. List elements can\'t be manipulated and the full
list needs to be provided.

``` bash
curl -XPATCH -H "Content-Type: application/merge-patch+json"
```

### Change containers

``` json
{
  "spec": {
    "template": {
      "spec": {
        "containers": [{
            "name": "container-1",
            "args": ["sleep", "infinity"],
            "image": "registry.fedoraproject.org/fedora:latest",
            "imagePullPolicy": "IfNotPresent"
        }, {
            "name": "container-2",
            "args": ["sleep", "infinity"],
            "image": "registry.fedoraproject.org/fedora:latest",
            "imagePullPolicy": "IfNotPresent"
        }]
      }
    }
  }
}
```

### Change replica count

``` json
{
  "spec": {
    "replicas": 2
  }
}
```

## Strategic Merge Patch

A strategic patch is similar to a JSON Merge Patch, but with custom
behaviors defined in the OpenAPI. For example, the pod template spec
enables adding containers to the list.

``` bash
curl -XPATCH -H "Content-Type: application/strategic-merge-patch+json"
```

### Add a container

``` json
{
  "spec": {
    "template": {
      "spec": {
        "containers": [{
            "name": "container-2",
            "args": ["sleep", "infinity"],
            "image": "registry.fedoraproject.org/fedora:latest",
            "imagePullPolicy": "IfNotPresent"
        }]
      }
    }
  }
}
```

## Server Side Apply

Since Kubernetes v1.22, a new option is available when using:

``` bash
curl -XPATCH -H "Content-Type: application/apply-patch+yaml"
```

This feature leverage a new \"field management\" mechanism, and it seems
useful when multiple clients are updating a single resource. This
feature is fairly new, and it is not yet fully supported by the
controller-runtime client.

## Replace

The other solution is to replace the resource:

``` bash
curl -XPUT -H "Content-Type: application/json"
```

The body must contains the full resource, otherwise the request will
fail.

### Get and replace

``` bash
curl http://localhost:8080/apis/apps/v1/namespaces/tristanc/deployments/demo-deployment > dep.json
# edit the file
curl -XPUT -H "Content-Type: application/json" http://localhost:8080/apis/apps/v1/namespaces/tristanc/deployments/demo-deployment -d@dep.json
```

Notice that changing the resource results in a new *resourceVersion*,
and trying to repeat the last request will fails because the version no
longer match.

## Conclusion

Kubernetes provides multiple update strategies to manage resources:

-   The easiest option seems to be the Strategic Merge Patch, or the new
    Server Side Apply, but the resulting update depend on the nature of
    the change, for example it is not clear how to remove an element
    from a list.
-   JSON Patch seems to be the most efficient, but the request body
    needs to be prepared. JSON Merge Patch is another solution, but
    removing attributes requires using a *null* value. Learn more about
    the difference in this
    [post](https://erosb.github.io/post/json-patch-vs-merge-patch/).
-   Finally replace seems to be most straightforward solution, but the
    full resources needs to be known in advance.

The controller-runtime provides a convenient Update method to use the
replace strategy: [Example Client
Update](https://pkg.go.dev/sigs.k8s.io/controller-runtime/pkg/client#example-Client-Update).
This pattern works great in the context of an operator where the state
of the resources is usually known before making creation or update
request.
