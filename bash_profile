# ls highlighting 
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagacad
export PS1="\e[0;33m\u@\h:\W\e[m\$ "
export PS1='\[\e[0;33m\]\u@\h:\W\$\[\e[0m\] '  # fix the command wrapping problem

#export PS1="\e[0;33m\h:\W \u\$ \e[m"
# Tell grep to highlight matches
export GREP_OPTIONS='--color=auto'

#Encoding for SSH connection
export LC_CTYPE=en_US.UTF-8
#export LANG=en_US.UTF8

# emacs without init file
# alias emacsq='emacs -q'

# cscope editor
export EDITOR=emacsq

# P4 config
export P4CONFIG=p4.config
export P4EDITOR=emacsq
export P4DIFF=colordiff

if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi


# MacPorts
export PATH=$PATH:/opt/local/bin:/opt/local/sbin
# :/usr/local/Trolltech/Qt-4.8.1/bin

# Bash completion
if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

# Mount the SMB


##
# Your previous /Users/frinkr/.bash_profile file was backed up as /Users/frinkr/.bash_profile.macports-saved_2015-01-07_at_15:01:46
##

# MacPorts Installer addition on 2015-01-07_at_15:01:46: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

export PYTHONPATH=$PYTHONPATH:/usr/local/lib/python2.7/site-packages


# NVM
#export NVM_DIR=~/.nvm
#source $(brew --prefix nvm)/nvm.sh
#alias electron="/Applications/Electron.app/Contents/MacOS/Electron"

# auto env

auto_env()
{
    if [ -f .env/bin/activate ]; then
        source .env/bin/activate
    else
        root=`which python`
        root=${root%/.env/bin/python}
        if [[ $PWD == $root* ]]; then
            return
        fi
        if (type deactivate > /dev/null 2>&1); then
            deactivate
        fi
    fi
}

function cd
{
    builtin cd "$@"
    auto_env
}

export PROMMPT_COMMAND="auto_env"
alias brew='ALL_PROXY=http://eglbeprx001.esko-graphics.com:8080 brew'

# Docker
function docker-env()
{
    eval "$(docker-machine env $1)"
    export DOCKER_OPTS="-H $DOCKER_HOST --tls --tlskey $DOCKER_CERT_PATH/server-key.pem --tlscert $DOCKER_CERT_PATH/server.pem --tlsverify --tlscacert $DOCKER_CERT_PATH/ca.pem"
    #export DOCKER_OPTS="-H $DOCKER_HOST --insecure-registry atlantis:5000 --tls --tlskey $DOCKER_CERT_PATH/server-key.pem --tlscert $DOCKER_CERT_PATH/server.pem --tlsverify --tlscacert $DOCKER_CERT_PATH/ca.pem"
    alias docker="docker $DOCKER_OPTS"
}
