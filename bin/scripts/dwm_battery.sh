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
            printf "^b#1e222a^^c#7eca9c^🔌 %s%% %s" "$CHARGE" "$STATUS"
        else
            printf "^b#1e222a^^c#7eca9c^ %s%% %s" "$CHARGE" "$STATUS"
        fi
    crit=$(bat)
    if [[ $crit -lt 25 ]]; then
            dunstify -u critical "☢  battery is critical"  #-h string:bgcolor:#e80e0e "battery is critical"
    fi
}

dwm_battery

