---
title: Recursive namespaces to run containers inside a container
date: 2024-02-19
tags: [blog]
---

> This post was initially published on the Software Factory blog: https://www.softwarefactory-project.io/leveraging-capn-proto-for-logreduce-reports.html

We would like to deploy a containerized workload that creates nested containers to isolate individual tasks.
This post explores the challenges of safely running a container inside a container.
In three parts, I present:

- User namespaces.
- Required capabilities.
- Procfs kernel restrictions.

> The examples in this post are using the following packages:
> - kernel-6.6.11-200.fc39.x86_64
> - selinux-policy-39.3-1.fc39.noarch
> - util-linux-core-2.39.3-1.fc39.x86_64
> - bubblewrap-0.8.0-1.fc39.x86_64
> - podman-4.8.3-1.fc39.x86_64


## Context and problem statement

The context is leveraging the [bubblewrap](https://github.com/containers/bubblewrap) tool to create
temporary sandboxes for running Ansible playbooks as part of a CI build system named `zuul-executor`.

The problem we are facing is that creating nested containers requires a privileged context from the parent container runtime.
And this is an issue when running in an environment that enforces security constraints, like OpenShift clusters managed by a third party.

The next sections describe the implications of this privileged context.

## User namespaces

Since RHEL8, regular users are allowed to create namespaces.
This used to be a privileged action that only the admin (root) could perform.
But thanks to the unprivileged user namespace, users can become root in a limited context to perform the actions required to setup a container.

We can explore this feature using the standard `unshare` utility.
As a regular user, we can create new namespaces that are isolated from the host:

```ShellSession
[tristanc@fedora ~]$ unshare --user --mount --net --pid --fork --map-root-user --mount-proc
root@fedora:~# id
uid=0(root) gid=65534(nfsnobody) groups=65534(nfsnobody) context=unconfined_u:unconfined_r:unconfined_t:s0-s0:c0.c1023
root@fedora:~# ip a
1: lo: <LOOPBACK> mtu 65536 qdisc noop state DOWN group default qlen 1000
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
root@fedora:~# ps afx
    PID TTY      STAT   TIME COMMAND
      1 pts/5    S      0:00 -bash
     79 pts/5    R+     0:00 ps afx
```

Above we can see that:

- `--user` creates a new uid mapping which lets us become root.
- `--net` creates a new network stack.
- `--pid` creates a new procfs.

To create these namespaces, the process uses the `CLONE_NEWNS|CLONE_NEWUSER|CLONE_NEWPID|CLONE_NEWNET` flags (either for the `unshare(2)` or `clone(2)` syscall).

Note that it is necessary to create a new user namespace (with `--user`), otherwise we wouldn't get the capabilities for creating the other namespaces.

We can also create nested namespaces:

```ShellSession
[tristanc@fedora ~]$ unshare --user --mount --net --pid --fork --map-root-user --mount-proc
root@fedora:~# sleep 1001 &
[1] 23
root@fedora:~# unshare --user --mount --net --pid --fork --map-root-user --mount-proc
root@fedora:~# ps afx
    PID TTY      STAT   TIME COMMAND
      1 pts/8    S      0:00 -bash
     23 pts/8    R+     0:00 ps afx
root@fedora:~# exit
root@fedora:~# ps afx
    PID TTY      STAT   TIME COMMAND
      1 pts/8    S      0:00 -bash
     23 pts/8    S      0:00 sleep 1001
     48 pts/8    R+     0:00 ps afx
```

We can also use the `bwrap` command from the bubblewrap package to achieve the same kind of isolation:

```ShellSession
[tristanc@fedora ~]$ bwrap --ro-bind /usr /usr --symlink usr/lib64 /lib64 --proc /proc --dev /dev --tmpfs /tmp --unshare-all --new-session --cap-add all --uid 0 bash
bash: cannot set terminal process group (1): Inappropriate ioctl for device
bash: no job control in this shell
bash-5.2# sleep 4242 &
[1] 7
bash-5.2# bwrap --ro-bind /usr /usr --symlink usr/lib64 /lib64 --proc /proc --dev /dev --tmpfs /tmp --unshare-all --new-session --cap-add all --uid 0 bash
bash: cannot set terminal process group (1): Inappropriate ioctl for device
bash: no job control in this shell
bash-5.2# ps afx
    PID TTY      STAT   TIME COMMAND
      1 ?        Ss     0:00 bwrap --ro-bind /usr /usr --symlink usr/lib64 /lib64 --proc /proc --dev /dev --tmpfs /tmp --unshare-all --new-session --cap-add all --uid 0 bash
      2 ?        S      0:00 bash
      3 ?        R      0:00  \_ ps afx
```

And we can confirm from the host that the namespaces are indeed nested:

```ShellSession
[tristanc@fedora ~]$ ps afx
...
 165104 pts/8    Ss     0:00  |   \_ /bin/bash --posix
 170707 pts/8    S+     0:00  |       \_ bwrap --ro-bind /usr /usr --symlink usr/lib64 /lib64 --proc /proc --dev /dev --tmpfs /tmp --unshare-all --new-session --cap-add all --uid 0 bash
 170708 ?        Ss     0:00  |           \_ bwrap --ro-bind /usr /usr --symlink usr/lib64 /lib64 --proc /proc --dev /dev --tmpfs /tmp --unshare-all --new-session --cap-add all --uid 0 bash
 170709 ?        S      0:00  |               \_ bash
 170826 ?        S      0:00  |                   \_ sleep 4242
 170827 ?        S      0:00  |                   \_ bwrap --ro-bind /usr /usr --symlink usr/lib64 /lib64 --proc /proc --dev /dev --tmpfs /tmp --unshare-all --new-session --cap-add all --uid 0 bash
 170828 ?        Ss     0:00  |                       \_ bwrap --ro-bind /usr /usr --symlink usr/lib64 /lib64 --proc /proc --dev /dev --tmpfs /tmp --unshare-all --new-session --cap-add all --uid 0 bash
 170829 ?        S      0:00  |                           \_ bash
```

In this section, we demonstrated that a regular unprivileged user is able to create namespaces recursively (up to 32 layers).
And even though the user appears to be root in the namespace, it is still a regular user from the host perspective, and the user didn't gain new privileges.

In the next section, we investigate what happens when the first namespace is created by a container runtime.


## Container runtime

In a production environment, the initial container namespaces are created by a container runtime such as [podman](https://github.com/containers/podman).
To investigate this setup, let's add some tools to the fedora's base container image:

```
[tristanc@fedora ~]$ CTX=$(buildah from fedora)
[tristanc@fedora ~]$ buildah run $CTX dnf install -y util-linux procps-ng bubblewrap
[tristanc@fedora ~]$ buildah commit --rm $CTX fedora
```

With a minimal container, using the least amount of privileges by adding `--cap-drop all`, we are not able to create the user namespace:

```
[tristanc@fedora ~]$ podman run --cap-drop all -it --rm fedora unshare --user --mount --net --pid --fork --map-root-user --mount-proc
unshare: write failed /proc/self/uid_map: Operation not permitted
```

At least, we need the `setfcap` capability which is enabled by default, but that is not enough:

```
[tristanc@fedora ~]$ podman run -it --rm fedora unshare --user --mount --net --pid --fork --map-root-user --mount-proc
unshare: mount /proc failed: Permission denied
```

It appears that we need to provide the `--privileged` flag:

```
[tristanc@fedora ~]$ podman run --privileged -it --rm fedora unshare --user --mount --net --pid --fork --map-root-user --mount-proc
-sh-5.2# unshare --user --mount --net --pid --fork --map-root-user --mount-proc
-sh-5.2#
```

Podman, as well as [cri-o](https://github.com/cri-o/cri-o), provides additional isolations.
In the next section we'll investigate what is happening.


## Procfs kernel restrictions

It appears that, for the purpose of nested containerization, the `--privileged` argument keeps the `/proc` untainted from any mountpoints.
Indeed, we can observe that a regular container does not have access to the full `/proc`:

```
[tristanc@fedora ~]$ podman run -it --rm fedora grep "^tmpfs /proc" /proc/mounts
tmpfs /proc/acpi tmpfs ro,context="system_u:object_r:container_file_t:s0:c373,c905",relatime,size=0k,uid=1000,gid=1000,inode64 0 0
tmpfs /proc/scsi tmpfs ro,context="system_u:object_r:container_file_t:s0:c373,c905",relatime,size=0k,uid=1000,gid=1000,inode64 0 0
[tristanc@fedora ~]$ podman run --privileged -it --rm fedora grep "^tmpfs /proc" /proc/mounts | wc -l
0
```

The container runtime hides some `/proc` sub directories to prevent leaking unnecessary information from the host.
We can observe the same behavior without a container runtime, similar to what we did in the first section.
For example the initial example no longer works in that situation:

```
[tristanc@fedora ~]$ sudo mount -t tmpfs none /proc/scsi
[sudo] password for tristanc:
[tristanc@fedora ~]$ unshare --user --mount --net --pid --fork --map-root-user --mount-proc
unshare: mount /proc failed: Operation not permitted
[tristanc@fedora ~]$ bwrap --ro-bind /usr /usr --symlink usr/lib64 /lib64 --proc /proc --dev /dev --tmpfs /tmp --unshare-all --new-session --cap-add all --uid 0 ps afx
bwrap: Can't mount proc on /newroot/proc: Operation not permitted
```

The same error can happen inside a privileged pod when manually hiding a directory, here `/proc/scsi`:

```
[tristanc@fedora ~]$ podman run --tmpfs /proc/scsi --privileged -it --rm fedora unshare --user --mount --net --pid --fork --map-root-user --mount-proc
unshare: mount /proc failed: Operation not permitted
```

When the procfs is not fully visible, then the kernel prevents further attempt to create a new fresh procfs, resulting in the `mount /proc failed: Operation not permitted` error.
This is unfortunate because our workload does not need a fully visible procfs, and the workload would work if the hidden paths were propagated automatically.
This is also confusing because the process is allowed to create the pid namespace with `CLONE_NEWPID`, but it is not allowed to use it when mounting the procfs.

Thankfully, as pointed out by @giuseppe from the Red Hat Container Team, there is already a [MountProc](https://github.com/kubernetes/enhancements/issues/4265)
enhancement proposed in kubernetes to enable this use-case.


## Conclusion

In conclusion, we saw that creating recursive namespaces is possible under normal conditions.
However, container runtimes are tainting the `/proc` file-system with tmpfs to prevent data
from being exposed into a container, and this alone prevents the creation of nested PID namespace.
