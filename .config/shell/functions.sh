#!/usr/bin/env bash
# Shared functions for bash/zsh

# Emacs launch helpers
emacs-daemon-status() {
    echo "Emacs daemon is disabled"
    return 1
}

emacs-daemon-start() {
    echo "Emacs daemon is disabled"
    return 1
}

emacs-daemon-stop() {
    echo "Emacs daemon is disabled"
    return 1
}

emacs-daemon-restart() {
    echo "Emacs daemon is disabled"
    return 1
}

emacs-daemon-recover() {
    echo "Emacs daemon is disabled"
    return 1
}

emacs-client-open() {
    if [[ "$1" == "-t" ]]; then
        shift
        emacs -nw "$@"
    else
        emacs "$@"
    fi
}

emacs-gui-open() {
    if [[ -n "${DISPLAY:-}${WAYLAND_DISPLAY:-}" ]]; then
        emacs "$@" >/dev/null 2>&1 &
        disown
    else
        emacs-client-open -t "$@"
    fi
}

# Create directory and cd into it
mkd() {
    mkdir -p "$@" && cd "$_"
}

# Extract archives
extract() {
    if [ -f "$1" ]; then
        case "$1" in
            *.tar.bz2) tar xjf "$1" ;;
            *.tar.gz)  tar xzf "$1" ;;
            *.tar.xz)  tar xJf "$1" ;;
            *.bz2)     bunzip2 "$1" ;;
            *.rar)     unrar e "$1" ;;
            *.gz)      gunzip "$1" ;;
            *.tar)     tar xf "$1" ;;
            *.tbz2)    tar xjf "$1" ;;
            *.tgz)     tar xzf "$1" ;;
            *.zip)     unzip "$1" ;;
            *.Z)       uncompress "$1" ;;
            *.7z)      7z x "$1" ;;
            *)         echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Find files by name
ff() {
    find . -type f -name "*$1*"
}

# Find directories by name
fd() {
    find . -type d -name "*$1*"
}

# Quick backup of a file
backup() {
    if [ -f "$1" ]; then
        cp "$1" "$1.backup-$(date +%Y%m%d-%H%M%S)"
        echo "Backup created: $1.backup-$(date +%Y%m%d-%H%M%S)"
    else
        echo "File not found: $1"
    fi
}

# Git branch cleanup - delete merged branches
git-cleanup() {
    git branch --merged | grep -v "\*\|main\|master\|develop" | xargs -n 1 git branch -d
}

# Show git log for a specific file
gitlog() {
    git log --follow --pretty=format:'%C(yellow)%h%Creset %C(blue)%ad%Creset %C(green)%an%Creset %s' --date=short -- "$1"
}

# macOS specific functions
if [[ "$OSTYPE" == "darwin"* ]]; then
    # Open current directory in Finder
    finder() {
        open "${1:-.}"
    }

    # Empty the Trash
    emptytrash() {
        rm -rf ~/.Trash/*
        echo "Trash emptied"
    }
fi

# Show colors in terminal
colors() {
    for i in {0..255}; do
        print -Pn "%K{$i}  %k%F{$i}${(l:3::0:)i}%f " ${${(M)$((i%6)):#3}:+$'\n'}
    done
}
