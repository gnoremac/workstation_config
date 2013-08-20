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
export PATH=$PATH:~/bin:$ANDROID_HOME/tools
export LD_LIBRARY_PATH=`pwd`

#### Defaults ####
# Default editor
export EDITOR="emacs -nw -q"

#enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    #eval "`dircolors -b`"
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

# Hookup Python VirtualEnvironments
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

#### Bash Functions ####
function ginit {
    mkdir $1
    cd $1
    git init
}

# Killlist
function killps {
    ps aux | grep $1 | grep -v grep | awk {'print $2'} | xargs kill -9 2> /dev/null
}

# Makes directory then moves into it
function mkcdr {
    mkdir -p -v $1
    cd $1
}

# Easy extract
extract () {
  if [ -f $1 ] ; then
      case $1 in
          *.tar.bz2)   tar xvjf $1    ;;
          *.tar.gz)    tar xvzf $1    ;;
          *.bz2)       bunzip2 $1     ;;
          *.rar)       rar x $1       ;;
          *.gz)        gunzip $1      ;;
          *.tar)       tar xvf $1     ;;
          *.tbz2)      tar xvjf $1    ;;
          *.tgz)       tar xvzf $1    ;;
          *.zip)       unzip $1       ;;
          *.Z)         uncompress $1  ;;
          *.7z)        7z x $1        ;;
          *)           echo "don't know how to extract '$1'..." ;;
      esac
  else
      echo "'$1' is not a valid file!"
  fi
}

# A frequent source of complaint from cat
# is that I've just asked it to cat a directory.
# If I have, just DWIM and do it happily.
function catorls () {
    if [ -d $1 ];
    then
        ls -l $1
    else
        cat $1
    fi
}
alias cat="catorls"

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[git-\1$(parse_git_dirty)]/"
}

function hg_dirty() {
    hg status --no-color 2> /dev/null \
    | awk '$1 == "?" { unknown = 1 } 
           $1 != "?" { changed = 1 }
           END {
             if (changed) printf "!"
             else if (unknown) printf "?" 
           }'
}

function hg_branch() {
    hg branch 2> /dev/null | sed -e '/^[.*]/d' -e "s/\(^.*\)/[hg-\1$(hg_dirty)]/"
}

#### Colours ^ Prompt ####
DEFAULT="[37;40m"
PINK="[35;40m"
GREEN="[32;40m"
ORANGE="[33;40m"
#PS1='\n\e${PINK}\u \
#\e${DEFAULT}at \e${ORANGE}\h \
#\e${DEFAULT}in \e${GREEN}\w\
#\e${ORANGE}$(parse_git_branch) \
#\e${ORANGE}$(hg_branch) \
#\n$ '

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

# Display to the terminal
function osd {
    osd_cat --align=center --pos=middle --outline=1 \
	--outlinecolour=#000000 --delay=3 --font="$font" \
	"$@"
}

