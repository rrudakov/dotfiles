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
PROMPT="%{$fg[green]%}%#%{$reset_color%} %{$fg[blue]%}%d %{$reset_color%}"

if [[ -r ~/.aliasrc ]]; then
. ~/.aliasrc
fi

zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# PATH=$PATH:$HOME/.stack/programs/x86_64-linux/ghc-8.0.2/bin
PATH=$PATH:$HOME/.local/bin
export PATH
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
export EDITOR=vim
export QT_STYLE_OVERRIDE=gtk
