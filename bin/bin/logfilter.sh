#!/bin/bash

# see https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
# red: sed -e 's/.*\btoto.*/\x1b[31m&\x1b[0m/i'
black="30m"
red="31m"
green="32m"
yellow="33m"
blue="34m"
magenta="35m"
cyan="36m"
white="37m"

bright_black="30;1m"
bright_red="31;1m"
bright_green="32;1m"
bright_yellow="33;1m"
bright_blue="34;1m"
bright_magenta="35;1m"
bright_cyan="36;1m"
bright_white="37;1m"

# example of invert: -e "s/.*\\bKERNEL.*/\\x1b[${invert}&\\x1b[0m/i" \
invert="7m"

highlight=""
if [[ $# -eq 1 ]]; then
    highlight=$1
fi

#TODO: factorize code
# add possibility of adding custom patterns as arguments
if [[ $# -eq 0 ]]; then
    sed \
        -e "s/.*\\bE\\b.*/\\x1b[${bright_red}&\\x1b[0m/i" \
        -e "s/.*\\bW\\b.*/\\x1b[${blue}&\\x1b[0m/i"
fi

if [[ $# -eq 1 ]]; then
    sed \
        -e "s/.*\\bE\\b.*/\\x1b[${bright_red}&\\x1b[0m/i" \
        -e "s/.*\\bW\\b.*/\\x1b[${blue}&\\x1b[0m/i" \
        -e "s/.*\\b${highlight}\\b.*/\\x1b[${bright_yellow}&\\x1b[0m/i"
fi
