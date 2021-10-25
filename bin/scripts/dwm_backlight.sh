#!/bin/sh

# A dwm_bar module to display the current backlight brighness with xbacklight
# Joe Standring <git@joestandring.com>
# GNU GPLv3

# Dependencies: xbacklight

dwm_backlight () {
    #printf "^b#1e222a^^c#abb2bf^🪔%.0f%s\n" "$(xbacklight)" #"$SEP2"
    printf "^b#4a2c7d^^c#abb2bf^ %0.f%s\n" $(xbacklight)
}

dwm_backlight
