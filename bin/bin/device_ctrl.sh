#!/bin/bash
# device control commmands based on FZF

# must contain
device_control_commands=(
    "adb reboot bootloader"
    "adb reboot recovery"
    "fastboot continue"
    "fastboot flash bootloader bootloader.img"
    "fastboot flash recovery recovery.img"
    "fastboot flash boot boot.img"
    "fastboot flash system system.img"
    "fastboot update"
)

# utility function used to write the command in the shell
writecmd() {
  perl -e '$TIOCSTI = 0x5412; $l = <STDIN>; $lc = $ARGV[0] eq "-run" ? "\n" : ""; $l =~ s/\s*$/$lc/; map { ioctl STDOUT, $TIOCSTI, $_; } split "", $l;' -- $1
}

TERM=screen printf '%s\n' "${device_control_commands[@]}" | fzf | writecmd
