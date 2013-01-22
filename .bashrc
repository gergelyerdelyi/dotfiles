export PATH=${PATH}
export EDITOR=vim

export LANG=en_US.utf-8

if [ -f ~/.bashrc.local ] ; then
    source ~/.bashrc.local
fi

if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

# Funky git prompt
if [ -f `brew --prefix`/etc/bash_completion.d/git-prompt.sh ]; then
    . `brew --prefix`/etc/bash_completion.d/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    export PS1='\h:\[\033[1;34m\]\W\[\033[1;30m\]$(__git_ps1 " (%s)")\[\033[0m\]\$ '
fi

# Make Midnight Command look nice
alias mc='mc -c -S xoria256'

# Hint Emacs on which Python to use for Pymacs
export PYMACS_PYTHON=/usr/bin/python

