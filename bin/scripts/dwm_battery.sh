#!/bin/sh

# A dwm_bar function to read the battery level and status
# Joe Standring <git@joestandring.com>
# GNU Gt(){PLv3
bat(){
        acpi | awk '{print $4}' | sed 's/%,//'
    }

dwm_battery () {
    # Change BAT1 to whatever your battery is identified as. Typically BAT0 or BAT1
    CHARGE=$(acpi | awk '{print $4}' | sed 's/%,//')
    STATUS=$(cat /sys/class/power_supply/BAT1/status)

        if [ "$STATUS" = "Charging" ]; then
            printf "[Bat %s%% %s]" "$CHARGE" "$STATUS"
        else
            printf "[Bat %s%% %s]" "$CHARGE" "$STATUS"
        fi
}

dwm_battery

