export PATH=/usr/local/bin:${PATH}
export EDITOR=/usr/local/bin/emacsclient
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
alias mc='mc --nosubshell --color --skin xoria256'

# Hint Emacs on which Python to use for Pymacs
export PYMACS_PYTHON=`which python`

function init_python_env ()
{
    easy_install ropemacs
    easy_install ropemode
    easy_install pylint
    easy_install elpy
}

source "$HOME/.homesick/repos/homeshick/homeshick.sh"
source "$HOME/.homesick/repos/homeshick/completions/homeshick-completion.bash"