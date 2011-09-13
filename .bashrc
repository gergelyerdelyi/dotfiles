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
PS1='\h:\W$(__git_ps1 " (%s)")\$ '

if [ -f ~/.bashrc.local ] ; then
    source ~/.bashrc.local
fi

# Make Midnight Command black & white
alias mc='mc -b'

