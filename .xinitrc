# Set settings for urxvt
xrdb -merge ~/dotfiles/base16-xresources/base16-tomorrow.dark.xresources

# set a dark background
xsetroot

# remap caps lock to control
setxkbmap -option caps:ctrl_modifier

# optional: set wallpaper
feh --bg-center ~/images/fantasia_by_luisbc-dat1z88.jpg &

# optional: start pulse-audio-systray for sound control
# pasystray &

# network manager applet
# needs network-manager-gnome for that
nm-applet &

# parcellite, to synchronize clipboards
parcellite &

exec i3
