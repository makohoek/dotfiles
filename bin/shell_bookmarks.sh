#!/bin/bash
# shell bookmarks utility from
# http://dmitryfrank.com/articles/shell_shortcuts

cdscuts_list_echo()
{
    cat $1 | sed 's/#.*//g' | sed '/^\s*$/d'
}

cdscuts_glob_echo()
{
    user_filelist=''
    if [ -r ~/.shell_bookmarks ]; then
        user_filelist=$(cdscuts_list_echo ~/.shell_bookmarks)
    fi

    echo -e "$system_wide_filelist\n$user_filelist" | sed '/^\s*$/d'
}

# Setup cdg function
# ------------------
unalias cdg 2> /dev/null
cdg() {
    local dest_dir=$(cdscuts_glob_echo | fzf )
    if [[ $dest_dir != '' ]]; then
        cd "$dest_dir"
    fi
}
export -f cdg > /dev/null
