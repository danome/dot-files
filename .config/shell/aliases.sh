#!/usr/bin/env bash
# Shared aliases for bash/zsh

# Navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias ~='cd ~'

# List files
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias lt='ls -ltr'

# Safety nets
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# Directory creation
alias mkdir='mkdir -pv'

# Grep with color
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Git shortcuts
alias g='git'
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git log --oneline --graph --decorate'
alias gd='git diff'
alias gdc='git diff --cached'

# Emacs
alias e='emacsclient -n'
alias ec='emacsclient -c'
alias et='emacsclient -t'
alias es='emacsclient -n ~/.emacs.d/init.el'
alias emacs-config='emacsclient -n ~/.emacs.d/init.el'

# Emacs daemon management
alias eds='emacs-daemon-status'
alias edstart='emacs-daemon-start'
alias edstop='emacs-daemon-stop'
alias edrestart='emacs-daemon-restart'

# Python
alias py='python3'
alias pip='pip3'
alias venv='python3 -m venv'
alias activate='source venv/bin/activate'

# System info
alias ports='netstat -tulanp'
alias meminfo='free -h'
alias cpuinfo='lscpu'
alias df='df -h'
alias du='du -h'

# macOS specific
if [[ "$OSTYPE" == "darwin"* ]]; then
    alias showfiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder'
    alias hidefiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder'
    alias flushdns='sudo dscacheutil -flushcache'
    alias brewup='brew update && brew upgrade && brew cleanup'
fi

# Quick edit configs
alias zshconfig='${EDITOR:-vi} ~/.zshrc'
alias bashconfig='${EDITOR:-vi} ~/.bashrc'
alias sshconfig='${EDITOR:-vi} ~/.ssh/config'
alias gitconfig='${EDITOR:-vi} ~/.gitconfig'

# Misc
alias h='history'
alias j='jobs -l'
alias which='type -all'
alias path='echo -e ${PATH//:/\\n}'
alias now='date +"%T"'
alias nowdate='date +"%Y-%m-%d"'

# Claude CLI
alias claude-version='claude --version 2>/dev/null || echo "Claude CLI not found"'