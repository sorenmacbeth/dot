export TERM=xterm-256color

export JAVA_HOME=$(/usr/libexec/java_home)

# homebrew
export PATH=/usr/local/bin:$PATH

# home bin
export PATH=$PATH:~/bin

# npm bin lol
export PATH=$PATH:/usr/local/share/npm/bin

# go stuff
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH

# my anaconda dont my anaconda dont want none unless you got buns hun
export CONDAPATH=$HOME/anaconda3
source $CONDAPATH/etc/profile.d/conda.sh

# homebrew python
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/workspace
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
source /usr/local/bin/virtualenvwrapper.sh

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/soren/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/soren/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/soren/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/soren/google-cloud-sdk/completion.zsh.inc'; fi

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh

if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

#iterm2
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# nix
. $HOME/.nix-profile/etc/profile.d/nix.sh
