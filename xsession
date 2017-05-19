#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# Merge in defaults and keymaps
[ -f $sysresources ] && /usr/bin/xrdb -merge $sysresources
[ -f $sysmodmap ] && /usr/bin/xmodmap $sysmodmap
[ -f $userresources ] && /usr/bin/xrdb -merge $userresources
[ -f $usermodmap ] && /usr/bin/xmodmap $usermodmap

#$HOME/.screenlayout/dual.sh
compton --config ~/.config/compton.conf -b
export DE="xfce"
xgamma -bgamma 0.8 &
xbacklight -set 80 &
stalonetray &
#dunst &
xfce4-power-manager &
nitrogen --restore &
#volumeicon &
nm-applet &
#yandex-disk start &
#dropboxd &
kbdd &

# Start xmonad
exec ck-launch-session dbus-launch --exit-with-session xmonad
