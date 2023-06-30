
# Use the Emacs way on the cmdline
bindkey -e

autoload -Uz bashcompinit compinit vcs_info
bashcompinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
if [ "$(uname)" = "Linux" ]; then
  eval "$(dircolors -b)"
fi
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

zstyle ':vcs_info:*' disable bzr cdv cvs darcs hg mtn svk svn tla
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' actionformats \
       '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{2}(%b)%f'

setopt PROMPT_SUBST

precmd() {
    # As always first run the system so everything is setup correctly.
    vcs_info
    if [[ -z ${vcs_info_msg_0_} ]]; then
		PS1='%F{blue}%m%F{5}:%F{grey}%5~ %f%# '
    else
		PS1='%F{blue}%m%F{5}:%F{grey}%3~ ${vcs_info_msg_0_} %f%# '
    fi
}

# Disable shared history
unsetopt share_history

# Load https://github.com/andsens/homeshick
if [ -f "$HOME/.homesick/repos/homeshick/homeshick.sh" ]; then
  source "$HOME/.homesick/repos/homeshick/homeshick.sh"
  source "$HOME/.homesick/repos/homeshick/completions/homeshick-completion.bash"
fi

[[ -r $HOME/.zshrc.local ]] && source $HOME/.zshrc.local

# Useful aliases
alias m='micro'

setup_homeshick () {
    git clone https://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick
	(cd $HOME/.homesick/repos/homeshick; git checkout -b v2.0.0 eea2fc572cafd900c4f8e9a49922f7902959795f)
}

setup_pyenv () {
	git clone https://github.com/pyenv/pyenv.git $HOME/.pyenv
	(cd $HOME/.pyenv; git checkout -b v2.0.4 cef86ce462674c726cb60ddd0e19da0bf39a27c9)
	git clone https://github.com/pyenv/pyenv-virtualenv.git $HOME/.pyenv/plugins/pyenv-virtualenv
	(cd $HOME/.pyenv/plugins/pyenv-virtualenv; git checkout -b v1.1.5 294f64f76b6b7fbf1a22a4ebba7710faa75c21f7)
}
