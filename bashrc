# Shorter history
export HISTCONTROL=ignoredups

# Set aliases
if [[ -r ~/.aliasrc ]]; then
. ~/.aliasrc
fi

# Safe resize window
shopt -s checkwinsize

# Enable completion
source /usr/share/bash-completion/bash_completion
source /usr/share/bash-completion/completions/pass
source $HOME/.local/share/bash-completion/completions/rustup
source $HOME/.local/share/bash-completion/completions/cargo

# Set propmt
export PS1='\[\e[32m\u\]\[\e[0m\]@\[\e[34m\h\] \[\e[33m\w\]\n\[\e[33m\]\[\e[1m\]$ \[\e[0m\]'

# Colored less output
export LESS=-R
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

# Highlight syntax in less output (required package source-highlight)
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
# export LESS='-R '

# Better fonts rendering in Java applications
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd -Dswing.aatext=true'

# Silent java options
_SILENT_JAVA_OPTIONS="$_JAVA_OPTIONS"
unset _JAVA_OPTIONS
alias java='java "$_SILENT_JAVA_OPTIONS"'

# Set QT5 theme
export QT_STYLE_OVERRIDE=adwaita-dark

# Add local bin directory to PATH
export PATH="$HOME/.local/bin:$PATH"

# Add ~/.cargo/bin to PATH
export PATH="$HOME/.cargo/bin:$PATH"

# Set up virtualenvwrapper
export WORKON_HOME="$HOME/.virtualenvs"
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
source /usr/bin/virtualenvwrapper.sh

# Set default editor
export EDITOR="emacsclient -c"

# Add completion for stack tool
eval "$(stack --bash-completion-script stack)"