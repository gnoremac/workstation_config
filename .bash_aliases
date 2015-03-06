#### Aiases ####
# Reload this file due to frequent edits
alias reload='source ~/.bashrc'

#enable color support of ls and also add handy aliases
#enable color support of ls and also add handy aliases
if ls --color > /dev/null 2>&1; then # GNU `ls`
   colorflag="--color=auto"
else # OS X `ls`
     colorflag="-G"
fi

alias ls='ls ${colorflag}'
alias grep='grep ${colorflag}'
alias fgrep='fgrep ${colorflag}'
alias egrep='egrep ${colorflag}'

alias lx='ls -lXB ${colorflag}'               # sort by extension
alias lk='ls -lSr ${colorflag}'               # sort by size
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
alias mvn2="mvn clean && mvn -Dmaven.test.skip=true -T 2C install"
alias mvn3="mvn clean && mvn -T 2C install"
alias mvn4="mvn clean && mvn -Dmaven.test.skip=true -Dpmd.failOnViolation=false -Dcheckstyle.skip=true -T 2C install"

# moving aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../../'
alias downloads='cd ~/downloads'

# apt aliases
alias ainstall='sudo apt-get install'

# Misc
alias rtfm='man'

# Verify mvn build
alias verify='mvn checkstyle:checkstyle checkstyle:check pmd:pmd pmd:check'

# Get OS X Software Updates, and update installed Ruby gems, Homebrew, npm
alias osx_update='sudo softwareupdate -i -a; brew update; brew upgrade; brew cleanup; npm update npm -g; npm update -g; sudo gem update --system; sudo gem update'

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# Enhanced WHOIS lookups
alias whois="whois -h whois-servers.net"

# Hide/show all desktop icons (useful when presenting)
alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"
alias showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"

# Show/hide hidden files in Finder
alias show="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
alias hide="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en0"
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"

# Top biggest files
alias biggest='du -a /home/$USER | sort -n -r | head -n 10'

# Search file types for something excluding maven target dir
maven_find() {
    find -name *.$1 | grep -v 'target' | xargs grep $2
}
alias mfind=maven_find