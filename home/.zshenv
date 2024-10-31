#setopt xtrace

export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vi'
else
  export EDITOR='vim'
fi

HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

# Make docker (-compose) understand the buildkit formats
COMPOSE_DOCKER_CLI_BUILD=1
DOCKER_BUILDKIT=1

PATH=~/.local/bin:/opt/homebrew/bin:$PATH

if [ -d $HOME/.pyenv ]; then
    PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi
