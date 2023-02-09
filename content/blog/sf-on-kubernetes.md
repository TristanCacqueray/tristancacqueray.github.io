---
title: Running Software Factory on OpenShift
date: 2022-06-08
tags: [blog, zuul, kubernetes]
---

> This post was initially published on the Software Factory blog: https://www.softwarefactory-project.io/running-software-factory-on-openshift.html

This article presents a plan for running Software Factory on OpenShift.

## Context and Problem Statement

We are looking for solutions to the following list of pain points with
the current system:

-   Where are the services secrets, and how can I rotate them? This is a
    key requirement for auditing purpose.
-   The plateform is overloaded, what is the process to increase the
    number of executors?
-   Zuul is not voting on \$change_url, what is going on? (the service
    logs are presently not centralized)
-   What base system should I use, CentOS? Fedora? Debian?
-   How can I run a pre-production deployment with a local checkout of
    the Zuul source code?
-   sfconfig execution is slow, for example sf-project.io deployment
    takes more than one hour to converge.

## Considered Options

-   \[option 1\] Modularize the Ansible roles
-   \[option 2\] Migrate to Kubernetes

## Decision Outcome

We would like to investigate if and how Kubernetes can be a solution.
Here are the condition of satisfaction for the initial architecture:

### Localhost configuration

When the user does not have a KUBECONFIG ready, sfconfig setup a single
node cluster with [Kind](https://kind.sigs.k8s.io/). Then the system
requires a working [kubectl]{.title-ref} command.

### Service deployment

Using common manifests, the user can deploy custom architecture, for
example *kubectl apply -f zuul+ci-log-processor.yaml*:

-   Deploys: opensearch, zuul and ci log processor.
-   Provides: zuul-web url and kibana url.

### Developper mode

Running *sfconfig develop
\~/src/opendev.org/openstack/ci-log-processing* restart the
ci-log-processor service using a local copy of the source. See for
example this guide:
<https://github.com/kubernetes-sigs/kind/issues/2430>

### Migration from sfconfig.yaml and arch.yaml

There is a procedure to migrate the legacy system.
