CLOCK='%D{%I:%M:%S}'
PROMPTSYMBOL='ʐ'
USERNAME='%n'


PROMPT=$'%{$fg[white]%}%{$fg_bold[cyan]%}%~%{$reset_color%}$(git_prompt_info) %F{019}$CLOCK%f\
%{$fg_bold[green]%}$USERNAME%{$reset_color%} %{$fg_bold[blue]%}$PROMPTSYMBOL%{$reset_color%} '


ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[white]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="*"
ZSH_THEME_GIT_PROMPT_CLEAN=""
