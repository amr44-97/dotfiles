#!/usr/bin/bash 

picom --experimental-backends &
emacs --daemon &
nm-applet &
setxkbmap -layout us,ara -option grp:lalt_lshift_toggle &
nitrogen --restore &
clipmenud 
