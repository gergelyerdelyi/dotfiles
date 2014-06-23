export PATH=/usr/local/bin:${PATH}
export LANG=en_US.utf-8

if [ -f ~/.bashrc.local ] ; then
    source ~/.bashrc.local
fi

if hash brew 2>/dev/null
then
    if [ -f `brew --prefix`/etc/bash_completion ]; then
        . `brew --prefix`/etc/bash_completion
    fi

    if [ -f `brew --prefix`/etc/bash_completion.d/git-prompt.sh ]; then
        . `brew --prefix`/etc/bash_completion.d/git-prompt.sh
   fi
else
    if [ -f /etc/bash_completion.d/git ]
    then
        . /etc/bash_completion.d/git
    fi
fi

# Funky git prompt
if hash __git_ps1 2>/dev/null
then
    export GIT_PS1_SHOWDIRTYSTATE=1
    export PS1='\h:\[\033[1;34m\]\W\[\033[1;30m\]$(__git_ps1 " (%s)")\[\033[0m\]\$ '
fi
 
# Make Midnight Command look nice
alias mc='mc --nosubshell --color --skin xoria256'

# Hint Emacs on which Python to use for Pymacs
export PYMACS_PYTHON=`which python`

export GOPATH=~/work/go

if [ $(uname) == "Darwin" ] && hash go 2>/dev/null
then
    echo Darwin with Go
    launchctl setenv GOPATH $(go env GOPATH)
    launchctl setenv GOROOT $(go env GOROOT)
fi

function init_python_env ()
{
    easy_install ropemacs
    easy_install ropemode
    easy_install pylint
    easy_install elpy
}

source "$HOME/.homesick/repos/homeshick/homeshick.sh"
source "$HOME/.homesick/repos/homeshick/completions/homeshick-completion.bash"
