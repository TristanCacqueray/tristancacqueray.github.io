# Software Factory Developper Environment

Here are some notes to setup a developer environment with [[systemd-nspawn]]

## Setup chroot

```
cat <<EOF>sf.repo
[sf-master]
name=SF
baseurl=https://softwarefactory-project.io/kojifiles/repos/sf-master-el7-build/latest/x86_64/
gpgcheck=0
EOF
mkdir sfroot
sudo dnf -c sf.repo --disablerepo=* --enablerepo=sf-master --installroot $(pwd)/sfroot install systemd sf-config passwd openssh-server vim yum openssh-clients hostname selinux-policy
```

## Setup root access

```
sudo systemd-nspawn -D \$(pwd)/sfroot/
chroot> passwd -d root
chroot> rm /etc/securetty
chroot> systemctl enable sshd
```

## Boot with network

```
sudo systemd-nspawn -bD $(pwd)/sfroot -n
```

On the host:

```
sudo ip a add 192.168.42.1/24 dev ve-sfroot
sudo ip link set ve-sfroot up
sudo iptables -t nat -A POSTROUTING -s 192.168.42.42/32 -j MASQUERAD
sudo cp /etc/resolv.conf $(pwd)/sfroot/etc/
echo 1 | sudo tee /proc/sys/net/ipv4/ip_forward
echo 192.168.42.42 sftests.com | sudo tee -a /etc/hosts
```

On the container:

```
ip a add 192.168.42.42/24 dev host0
ip link set host0 up
ip route add default via 192.168.42.1
```

Stop from the host by running `killall systemd-nspawn`
