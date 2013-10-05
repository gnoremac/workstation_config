#### Aiases ####
#alias ll='ls -l | grep -v -E ".pyc$|~$"'
alias lx='ls -lXB'               # sort by extension
alias lk='ls -lSr'               # sort by size
alias lm='ls -alh |more'         # pipe through 'more'
alias lsd='ls -lh | grep "^d"'   # list only directories
alias lsl='ls -lh | grep "^l"'   # list only links
# list long, human readable, ignore implied~,
# ignore compiled python files
alias ll='ls -hlB --group-directories-first --hide=*.pyc'
 # list including .dotfiles
alias lsa='ls -lAh --group-directories-first'
alias lz="ls -lZ"                # SELinux display

## directory aliases
alias mkdir='mkdir -p'  #Make intermediaries
# Disk usage
alias du="du -h"
# Grepping
alias h="history | grep"

# emacs modes
alias gemacs="emacs-snapshot-gtk"
alias nemacs="emacs -nw"

# maven stuff
alias mvnc="mvn clean && mvn -Dmaven.test.skip=true install"

# moving aliases
alias ..='cd ..'
alias ...='cd ../..'
alias downloads='cd ~/downloads'

# apt aliases
alias ainstall='sudo apt-get install'

# Misc
alias rtfm='man'

# Verify mvn build
alias verify='mvn checkstyle:checkstyle checkstyle:check pmd:pmd pmd:check && mvn test -T4'
