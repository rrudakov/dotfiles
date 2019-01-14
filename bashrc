# Shorter history
export HISTCONTROL=ignoredups

if [[ -r ~/.aliasrc ]]; then
. ~/.aliasrc
fi

shopt -s checkwinsize
PS1='\[\e[32m\u\] \[\e[36m\w\] \[\e[33m\]\[\e[1m\]$ \[\e[0m\]'

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'
export QT_QPA_PLATFORMTHEME=qt5ct
export ANDROID_HOME=$HOME/Android/Sdk
PATH=$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools/bin:$HOME/.local/texlive/2017/bin/x86_64-linux:$HOME/.local/bin:$PATH
export PATH