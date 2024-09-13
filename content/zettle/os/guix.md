---
title: GNU Guix
tags: [guix]
---

Here are my notes on [Guix](https://guix.gnu.org/).
This is an investigation to migrate from #nix to #guix for my dev environment, basically emacs and ripgrep.
Though I don't want to install the build daemon as I'm used to run nix in a namespace on Fedora.

## Setup

- Grab the qcow vm image from https://guix.gnu.org/en/download/
- Start with:
  ```ShellSession
  $ qemu-system-x86_64 -enable-kvm -m 4096 -cpu host -vga virtio \
      -device virtio-blk,drive=myhd -drive if=none,file=guix-system-vm-image-1.4.0.x86_64-linux.qcow2,id=myhd \
      -virtfs local,path=drive,mount_tag=host0,security_model=none,id=host0
  ```
- Mount the host drive with:
  ```ShellSession
  $ sudo mount -t 9p -o trans=virtio host0 /mnt -oversion=9p2000.L
  ```

## Usage

Pin the channels by running `guix describe -f channels`:

```scheme
(list (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (branch "master")
       (commit "d4869b7e43d823bccc8f6d81dd9f5d2c13cb8f25")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
```

Then run the command with:

```ShellSession
$ guix time-machine -C /mnt/channels.scm -- system reconfigure /mnt/config.scm
```

## System Config

Here is a VM config:

```scheme
;; Checkout https://github.com/SystemCrafters/guix-live-image/blob/master/config.scm
```
