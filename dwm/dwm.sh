#!/bin/sh
# dwm start file at ~/.config/dwm/dwm.sh 



nitrogen --restore &
nm-applet &
synclient TouchpadOff=1 &
emacs -daemon &
picom --experimental-backends &
setxkbmap -layout us,ara -option grp:lalt_lshift_toggle &
sxhkd -c ~/.config/sxhkd/sxhkd-dwm &
clipmenud &

exec dwm
