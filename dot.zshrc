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

export TERM=xterm-256color

export JAVA_HOME=$(/usr/libexec/java_home)

# ybot dev
source $HOME/ybotdev.bash

export LEIN_USERNAME=$AWS_ACCESS_KEY_ID
export LEIN_PASSPHRASE=$AWS_SECRET_ACCESS_KEY

export AWS_ACCESS_KEY=$AWS_ACCESS_KEY_ID
export AWS_SECRET_KEY=$AWS_SECRET_ACCESS_KEY


# homebrew
export PATH=/usr/local/bin:$PATH

# homebrew python
export PATH=$PATH:/usr/local/share/python

# home bin
export PATH=$PATH:~/bin

# npm bin lol
export PATH=$PATH:/usr/local/share/npm/bin

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

# chruby lol
source /usr/local/opt/chruby/share/chruby/chruby.sh
source /usr/local/opt/chruby/share/chruby/auto.sh

chruby ruby-1.9.3

# ec2 api tools
export EC2_PRIVATE_KEY="$(/bin/ls "$HOME"/.ec2/pk-*.pem | /usr/bin/head -1)"
export EC2_CERT="$(/bin/ls "$HOME"/.ec2/cert-*.pem | /usr/bin/head -1)"
export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"

alias e="/usr/local/bin/emacsclient --no-wait"

function power_tail() { tail -f $1 | perl -pe "s/$2/\e[3;35;40m$&\e[0m/g"; }

function yb_emr_proxy() { ssh -i $1 -f -N -D 6667 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o "ConnectTimeout=10" -o "ServerAliveInterval=60" -o "ControlPath=none" hadoop@$2; }

function yb_emr_ssh() { ssh -i $1 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o ServerAliveInterval=60 hadoop@$2; }

function yb_emr_remote_repl() { ssh -i $1 -f -N -L$2:$3:$2 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o ConnectTimeout=10 -o ServerAliveInterval=60 -o ControlPath=none hadoop@$3; }

# ssh tunnels

function mktunnel {
    if [[ $* == '' ]] || [[ $1 == '-h' ]]; then
                echo 'Usage: mktunnel LOCALPORT REMOTEPORT REMOTEHOST'
        else
                ssh -fCNL $1:localhost:$2 $3;
        fi
}
 
function lstunnel { 
    if [[ $1 == '-h' ]]; then
                echo 'Usage: lstunnel [REGEX]'
        else
                count=1
                pgrep -fl "ssh -fCNL $*" | cut -f2- -d ' ' | while read LINE; do
                    echo $count: $LINE;
                    count=`expr $count + 1`;
                done
        fi      
}
 
function rmtunnel {
    if [[ $* == '' ]] || [[ $1 == '-h' ]]; then
                echo 'Usage: rmtunnel N [REGEX]'
        else
                count=1
                pgrep -f "ssh -fCNL $2" | while read LINE; do
                    if [[ $1 == $count ]]; then
                                kill -9 $LINE;
                                break
                        fi
                        count=`expr $count + 1`;
                done
        fi
}

# ssh proxies

function mkproxy {
    if [[ $* == '' ]] || [[ $1 == '-h' ]]; then
                echo 'Usage: mkproxy LOCALPORT REMOTEHOST'
        else
                ssh -fCND $1 $2;
        fi
}
 
function lsproxy { 
    if [[ $1 == '-h' ]]; then
                echo 'Usage: lsproxy [REGEX]'
        else
                count=1
                pgrep -fl "ssh -fCND $*" | cut -f2- -d ' ' | while read LINE; do
                    echo $count: $LINE;
                    count=`expr $count + 1`;
                done
        fi      
}
 
function rmproxy {
    if [[ $* == '' ]] || [[ $1 == '-h' ]]; then
                echo 'Usage: rmproxy N [REGEX]'
        else
                count=1
                pgrep -f "ssh -fCND $2" | while read LINE; do
                    if [[ $1 == $count ]]; then
                                kill -9 $LINE;
                                break
                        fi
                        count=`expr $count + 1`;
                done
        fi
}

