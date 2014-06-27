#
# If not running interactively, don't do anything
#
case $- in
    *i*) ;;
      *) return;;
esac

UNAME=uname

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

#
# Load all extra files
# 
# Put anything that only exists on the local environment into local
#
for file in ~/.{bash_prompt,bash_exports,bash_aliases,bash_function,bash_local}; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;


#######################################################
#### Defaults ####
#######################################################

# append to the history file, don't overwrite it
shopt -s histappend

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Check window size after each command and, if necessary,
# update the values of LINES and COLUMNS
shopt -s checkwinsize

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh;

# Enable tab completion for `g` by marking it as an alias for `git`
if type _git &> /dev/null && [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
    complete -o default -o nospace -F _git g;
fi;

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
