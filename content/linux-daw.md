# Linux DAW

Here are my instructions to setup a [[daw]] on Linux, using a Fedora Silverblue distrib.

## Requirements

Add the following to your system:

```bash
rpm-ostree install btop gnome-session-xsession xhost realtime-setup qpwgraph pavucontrol
```

- Run `btop` to monitor global system performance.
- Run `pavucontrol` to switch the audio card configuration from `HiFi` to `Direct`.
- Use `qpwgraph` to manage connections.
- Ensure x11 client like reaper are supported.

Setup realtime group:

```bash
# Activate realtime service
for srv in setup entsk; do
    sudo systemctl enable realtime-$srv.service
    sudo systemctl start realtime-$srv.service
done

# Fix for https://docs.fedoraproject.org/en-US/fedora-silverblue/troubleshooting/#_unable_to_add_user_to_group
grep -E '^realtime:' /usr/lib/group | sudo tee -a /etc/group

sudo usermod -a -G realtime $USER
```

After reboot, you should have the following limits:

```ShellSession
$ ulimit -a
real-time non-blocking time  (microseconds, -R) unlimited
...
max locked memory           (kbytes, -l) unlimited
real-time priority                  (-r) 99
```


- [ ] try: `sudo grubby --args="preempt=full" --update-kernel=ALL`
