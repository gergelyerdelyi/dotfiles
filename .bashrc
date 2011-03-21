export PATH=/opt/local/bin:${PATH}
export EDITOR=vim

export LANG=en_US.utf-8

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

# Funky git completion support
source ~/.git-completion.sh

# Funky git prompt
PS1='\h:\W$(__git_ps1 " (%s)")\$ '
