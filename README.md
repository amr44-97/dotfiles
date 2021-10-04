# Dotfiles

## dwm
---------------------
### dwm-polybar

#### Patches applied
1. dwm-cfacts
2. dwm-cool-autostart
3. dwm-anybar-polybar-tray-fix
4. dwm-ipc
5. dwm-pango
6. dwm-dwmc
7. dwm-horizgrid
8. dwm-cyclelayouts
9. dwm-bottomstack
----------------------
![screenshot1]()
---------------------------
![screenshot2]()
-------------------
## polybar

You need to compile polybar with a dwm-module so tags and other stuff work properly

follow instructions in [polybar-dwm-module](https://github.com/mihirlad55/polybar-dwm-module)

any changes to the bar (colors, tags,...) are through the polybar config file.

--------------------
add **dwm.desktop** to  /usr/share/xsessions/
---------------------
````````
[Desktop Entry]
Name=dwm
Comment=The Dynamic Window Manager
Exec=/PATH/to/dwm.sh
Type=Application
````````


