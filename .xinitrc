# Set settings for urxvt
xrdb -merge ~/dotfiles/base16-xresources/base16-tomorrow.dark.xresources

# set a dark background
xsetroot

# remap caps lock to control
setxkbmap -option caps:ctrl_modifier

# optional: set wallpaper
# feh --bg-fill Pictures/AeKQcUf.jpg

# optional: start pulse-audio-systray for sound control
# pasystray &

exec i3
