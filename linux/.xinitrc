# set a dark background
xsetroot

# remap caps lock to control
setxkbmap -option caps:ctrl_modifier

# optional: set wallpaper
feh --bg-fill ~/images/fantasia_by_luisbc-dat1z88.jpg &

# optional: start pulse-audio-systray for sound control
# pasystray &

# network manager applet
# needs network-manager-gnome for that
nm-applet &

# parcellite, to synchronize clipboards
parcellite &

# small notification tray
dunst&

exec i3
