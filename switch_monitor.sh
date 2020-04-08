#!/bin/bash

DPI=166
INTERNAL=true
TRAY_GEOMETRY="4x1-280+0"
TRAY_ICON_SIZE=24
TRAY_SLOT_SIZE=24

function adjust_xresources () {
    echo "Set Xresources dpi $1"
    /usr/bin/sed -i "/Xft.dpi/c\Xft.dpi: $1" $HOME/dotfiles/Xresources
    /usr/bin/xrdb -merge $HOME/.Xresources
}

function adjust_fontconfig () {
    echo "Set fontconfig dpi $1"
    /usr/bin/sed -i "/name=\"dpi\"/c\    <edit name=\"dpi\" mode=\"assign\"><double>$1</double></edit>" $HOME/dotfiles/config/fontconfig/fonts.conf
}

function adjust_stalonetray () {
    echo "Set stalonetray settings for dpi $DPI"

    /usr/bin/sed -i "/geometry/c\geometry $1" $HOME/dotfiles/stalonetrayrc
    /usr/bin/sed -i "/icon_size/c\icon_size $2" $HOME/dotfiles/stalonetrayrc
    /usr/bin/sed -i "/slot_size/c\slot_size $3" $HOME/dotfiles/stalonetrayrc

    # Kill blueman applet
    if /usr/bin/pgrep -x "blueman-applet" > /dev/null
    then
        ps -ef | grep blueman-applet | grep -v grep | awk '{print $2}' | xargs kill
    fi

    # Kill networkmanager applet
    if /usr/bin/pgrep -x "nm-applet" > /dev/null
    then
        ps -ef | grep nm-applet | grep -v grep | awk '{print $2}' | xargs kill
    fi

    # Kill stalonetray
    if /usr/bin/pgrep -x "stalonetray" > /dev/null
    then
        ps -ef | grep stalonetray | grep -v grep | awk '{print $2}' | xargs kill
    fi
}

if [ $# -eq 0 ]; then
    if /usr/bin/xrandr --query | grep --silent '\bDP1 connected\b'; then
        echo "Connected"
        DPI=163
        INTERNAL=false
        TRAY_GEOMETRY="4x1-280+0"
        TRAY_ICON_SIZE=24
        TRAY_SLOT_SIZE=24
    fi
else
    if [ $1 == "--internal" ]; then
        echo "Switch back to internal monitor"
    else
        echo "Unknown options: $@"
        exit 1
    fi
fi

adjust_xresources $DPI
adjust_fontconfig $DPI
adjust_stalonetray $TRAY_GEOMETRY $TRAY_ICON_SIZE $TRAY_SLOT_SIZE

# Run xrandr command
if [ "$INTERNAL" == true ]; then
    /usr/bin/xrandr --output eDP1 --auto --output DP1 --off
else
    /usr/bin/xrandr --output eDP1 --off --output DP1 --auto
fi

# Recompile and restart xmonad
xmonad --restart

# Start stalonetray
if ! /usr/bin/pgrep -x "stalonetray" > /dev/null
then
    nohup /usr/bin/stalonetray > /dev/null 2>&1 &
fi

# Start networkmanager
if ! /usr/bin/pgrep -x "nm-applet" > /dev/null
then
    nohup /usr/bin/nm-applet > /dev/null 2>&1 &
fi

# Start blueman-applet
if ! /usr/bin/pgrep -x "blueman-applet" > /dev/null
then
    nohup /usr/bin/blueman-applet > /dev/null 2>&1 &
fi
