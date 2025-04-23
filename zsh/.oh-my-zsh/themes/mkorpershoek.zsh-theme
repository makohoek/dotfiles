local blue="%{$fg_bold[blue]%}"
local reset="%{$reset_color%}"

local hostname="%m"
local hostname_output="[${blue}${hostname}${reset}]"

PROMPT='${hostname_output} %~/ $(git_prompt_info)${reset}$ '

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "