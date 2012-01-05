export PATH=/opt/local/bin:${PATH}
export EDITOR=vim

export LANG=en_US.utf-8

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

# Funky git completion support
source ~/.git-completion.sh

# Funky git prompt
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1='\h:\[\033[1;34m\]\W\[\033[1;30m\]$(__git_ps1 " (%s)")\[\033[0m\]\$ '

if [ -f ~/.bashrc.local ] ; then
    source ~/.bashrc.local
fi

# Make Midnight Command black & white
alias mc='mc -b'

# Filter for less to handle some binaries files (archives and such)
export LESSOPEN='| /opt/local/bin/lesspipe.sh %s'