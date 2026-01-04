#!/usr/bin/env bash
# Shared environment exports for bash/zsh (macOS & Linux)

# Detect OS
OS_TYPE="$(uname -s)"

# Base paths
PATH_BASE="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Homebrew (macOS)
if [ "$OS_TYPE" = "Darwin" ]; then
  if [ -d "/opt/homebrew/bin" ]; then
    PATH_HOMEBREW="/opt/homebrew/bin:/opt/homebrew/sbin"
  elif [ -d "/usr/local/homebrew/bin" ]; then
    PATH_HOMEBREW="/usr/local/homebrew/bin"
  else
    PATH_HOMEBREW=""
  fi
else
  PATH_HOMEBREW=""
fi

# User paths
PATH_USER="$HOME/bin:$HOME/.local/bin:$HOME/.cargo/bin"

# De-duplicate PATH entries
dedup_path() {
  awk -v RS=: '!seen[$0]++ {printf "%s%s", sep, $0; sep=":"}'
}

export PATH="$(printf "%s:%s:%s" "$PATH_HOMEBREW" "$PATH_USER" "$PATH_BASE" | dedup_path)"

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# Editors
export EDITOR="${EDITOR:-emacsclient -c}"
export VISUAL="${VISUAL:-emacsclient -c}"
export ALTERNATE_EDITOR=""

# History behavior
export HISTCONTROL="ignoredups:ignorespace"
export HISTSIZE=5000
export SAVEHIST=5000

# Less and man
export LESSHISTFILE=-
export PAGER="${PAGER:-less}"

# Rust defaults
export CARGO_TARGET_DIR="${CARGO_TARGET_DIR:-/tmp/rust-target}"
export CARGO_INCREMENTAL="${CARGO_INCREMENTAL:-0}"
export CARGO_BUILD_JOBS="${CARGO_BUILD_JOBS:-3}"

# XDG base
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
