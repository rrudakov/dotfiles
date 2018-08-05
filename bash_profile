# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

# User specific environment and startup programs

TERM=xterm-256color; export TERM

export ANDROID_HOME=$HOME/Android/Sdk
PATH=$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools/bin:$HOME/.local/texlive/2017/bin/x86_64-linux:$HOME/.local/bin:$PATH
export PATH
