#
# If not running interactively, don't do anything
#
case $- in
    *i*) ;;
      *) return;;
esac

UNAME=uname

### THIS MACHINE SPECIFIC STUFF ####
export M2=~/.m2/repository
export WORKON_HOME=~/Envs
export ANDROID_HOME=~/workspace/java/android-sdk-linux

# Reload this file due to frequent edits
alias reload='source ~/.bashrc'

#### PATH ####
export PATH=$PATH:~/bin:$ANDROID_HOME/tools:/usr/local/go/bin
export LD_LIBRARY_PATH=`pwd`

#
# Stuff local to the environment
# This isn't checked into version control
if [ -f ~/.bash_local ]; then
   . ~/.bash_local
fi

#######################################################
#### Defaults ####
#######################################################

# Default editor
export EDITOR="emacs -nw -q"

#enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# History settings file size
HISTSIZE=1000
HISTFILESIZE=2000

# Check window size after each command and, if necessary,
# update the values of LINES and COLUMNS
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# local aliases
if [ -f ~/.bash_aliases ]; then
   . ~/.bash_aliases
fi

# Bash functions
if [ -f ~/.bash_function ]; then
   . ~/.bash_function
fi

PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

### Completion ###
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Support for multiple interpreters.

# Goodbye Grep
which ack > /dev/null
if [ $? -ne 0 ] ; then
    which ack-grep > /dev/null
    if [ $? -ne 0 ] ; then
        echo "Where the ack has ack gone?"
    else
        alias ack="ack-grep"
    fi
fi

# keybindings
bind '"\C-f\C-g": "find . | grep "';
bind '"\C-f\C-x": "find . | xargs grep "'
bind '"\C-p\C-a": "ps aux | grep "'

# Hookup Python VirtualEnvironments
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

# Sexy Bash Prompt
if [ -f ~/.bash_prompt ]; then
    . ~/.bash_prompt
fi