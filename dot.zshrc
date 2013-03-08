# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="dopeness"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git osx sublime brew mvn encode64 lein)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

export JAVA_HOME=$(/usr/libexec/java_home)

# ybot dev
source $HOME/ybotdev.bash

# homebrew
export PATH=/usr/local/bin:$PATH

# homebrew python
export PATH=$PATH:/usr/local/share/python

# home bin
export PATH=$PATH:~/bin

export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python2.7
export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true

if [[ -r /usr/local/share/python/virtualenvwrapper.sh ]]; then
    source /usr/local/share/python/virtualenvwrapper.sh
else
    echo "WARNING: Can't find virtualenvwrapper.sh"
fi

[[ -s "/Users/soren/.rvm/scripts/rvm" ]] && source "/Users/soren/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# ec2 api tools
export EC2_PRIVATE_KEY="$(/bin/ls "$HOME"/.ec2/pk-*.pem | /usr/bin/head -1)"
export EC2_CERT="$(/bin/ls "$HOME"/.ec2/cert-*.pem | /usr/bin/head -1)"
export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"

alias e="/usr/local/bin/emacsclient --no-wait"

function power_tail() { tail -f $1 | perl -pe "s/$2/\e[3;35;40m$&\e[0m/g"; }

function yb_emr_proxy() { ssh -i /path/to/key.pem -f -N -D 6666 -o StrictHostKeyChecking=no -o "ConnectTimeout=10" -o "ServerAliveInterval=60" -o "ControlPath=none" hadoop@$1; }

function yb_emr_ssh() { ssh -i path/to/key.pem -o StrictHostKeyChecking=no -o ServerAliveInterval=60 hadoop@$1; }
