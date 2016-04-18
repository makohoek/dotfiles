#!/bin/bash
# device control commmands based on FZF

# must contain
git_shorthands=(
    "git rebase -i "
    "git rebase --continue"
    "git rebase --abort"
    "git status"
    "git log"
    "git push umg HEAD:/refs/for/integ/ndg-android"
)

# utility function used to write the command in the shell
writecmd() {
  perl -e '$TIOCSTI = 0x5412; $l = <STDIN>; $lc = $ARGV[0] eq "-run" ? "\n" : ""; $l =~ s/\s*$/$lc/; map { ioctl STDOUT, $TIOCSTI, $_; } split "", $l;' -- $1
}

TERM=screen printf '%s\n' "${git_shorthands[@]}" | fzf | writecmd
