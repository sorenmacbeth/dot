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
# plugins=(git osx sublime brew mvn encode64 lein)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

alias e="/usr/local/bin/emacsclient --no-wait"

alias vlc='/Applications/VLC.app/Contents/MacOS/VLC'

# alias python=/usr/local/bin/python3
# alias pip=/usr/local/bin/pip3

function power_tail() { tail -f $1 | perl -pe "s/$2/\e[3;35;40m$&\e[0m/g"; }


# ssh tunnels

function mktunnel {
    if [[ $* == '' ]] || [[ $1 == '-h' ]]; then
        echo 'Usage: mktunnel LOCALPORT REMOTEPORT REMOTEHOST'
    else
        ssh -fCNL $1\:localhost\:$2 $3;
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

function cbsave {
    if [[ $* == '' ]] || [[ $1 == '-h' ]]; then
        echo 'Usage: cbsave channel output-path'
    else
        today=`date +"%m_%d_%Y"`
        streamlink -o $2/$1-$today.mpg https://chaturbate.com/$1 best;
    fi
}

