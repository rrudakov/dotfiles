[[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.history

setopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS

ttyctl -f

bindkey -e

if [[ "$COLORTERM" == "xfce4-terminal" ]]; then
export TERM=xterm-256color
fi

case $TERM in
	linux)
		bindkey "^[[2~" yank
		bindkey "^[[3~" delete-char
		bindkey "^[[5~" up-line-or-history
		bindkey "^[[6~" down-line-or-history
		bindkey "^[[1~" beginning-of-line
		bindkey "^[[4~" end-of-line
		bindkey "^[e" expand-cmd-path ## C-e for expanding path of typed command
		bindkey "^[[A" up-line-or-search ## up arrow for back-history-search
		bindkey "^[[B" down-line-or-search ## down arrow for fwd-history-search
		bindkey " " magic-space ## do history expansion on space
		;;
	xterm*|rxvt*|(dt|k|E)term|screen-256color)
		bindkey "^[[2~" yank
		bindkey "^[[3~" delete-char
		bindkey "^[[5~" up-line-or-history
		bindkey "^[[6~" down-line-or-history
		bindkey "^[[7~" beginning-of-line
		bindkey "^[[8~" end-of-line
		bindkey "^[e" expand-cmd-path ## C-e for expanding path of typed command
		bindkey "^[[A" up-line-or-search ## up arrow for back-history-search
		bindkey "^[[B" down-line-or-search ## down arrow for fwd-history-search
		bindkey " " magic-space ## do history expansion on space
		;;
esac

autoload -U colors && colors
PROMPT='%F{green}%n%f@%F{blue}%m%f %F{yellow}%1~%f %# '

if [[ -r ~/.aliasrc ]]; then
. ~/.aliasrc
fi

zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

export LESS=-R
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

if [ "$TERM" = "linux" ]; then
    _SEDCMD='s/.*\*color\([0-9]\{1,\}\).*#\([0-9a-fA-F]\{6\}\).*/\1 \2/p'
    for i in $(sed -n "$_SEDCMD" $HOME/.Xresources | awk '$1 < 16 {printf "\\e]P%X%s", $1, $2}'); do
        echo -en "$i"
    done
    clear
fi
BASE16_SHELL=$HOME/.config/base16-shell/

# ANDROID_HOME=$HOME/Android/Sdk
PATH=$HOME/.local/bin:$PATH
export PATH
#MANPATH=$HOME/.local/texlive/2017/texmf-dist/doc/man:$MANPATH
#export MANPATH
#INFOPATH=$HOME/.local/texlive/2017/texmf-dist/doc/info:$INFOPATH
#export INFOPATH
export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
source /usr/bin/virtualenvwrapper.sh
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"
zstyle ':completion:*' menu select
zstyle ':completion:*' rehash true
zstyle ':completion:*' use-compctl false
setopt completealiases
export QT_STYLE_OVERRIDE=adwaita-dark
export EDITOR=emacsclient

#export PATH="$HOME/.yarn/bin:$PATH"
