CLOCK='%D{%I:%M:%S}'
PROMPTSYMBOL='ʐ'
USERNAME='%n'
CURRENTDIRECTORY='%~'


PROMPT=$'%F{019}$CLOCK%f $(git_prompt_info) %{$fg[white]%}%{$fg_bold[yellow]%}$CURRENTDIRECTORY%{$reset_color%} %{$fg_bold[blue]%}$PROMPTSYMBOL%{$reset_color%} '


ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY_PREFIX="%{$fg[red]%}{"
ZSH_THEME_GIT_PROMPT_DIRTY_SUFFIX="}%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN_PREFIX="%{$fg[green]%}("
ZSH_THEME_GIT_PROMPT_CLEAN_SUFFIX=")%{$reset_color%}"
