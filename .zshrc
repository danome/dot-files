#!/usr/bin/env zsh
# Modern .zshrc configuration

# Enable Powerlevel10k instant prompt if available
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Zsh options
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
setopt SHARE_HISTORY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt PROMPT_SUBST
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt COMPLETE_IN_WORD
setopt NO_BEEP

# History
HISTFILE="${HISTFILE:-$HOME/.zsh_history}"
HISTSIZE=10000
SAVEHIST=10000

# Completion system
autoload -Uz compinit
if [[ -n ${ZDOTDIR}/.zcompdump(#qNmh+24) ]]; then
  compinit
else
  compinit -C
fi

# Completion options
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%B%d%b'

# Key bindings
bindkey -e  # Emacs mode
bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward
bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word

# Source shared configurations
CONFIG_DIR="${HOME}/.config/shell"
[[ -f "${CONFIG_DIR}/exports.sh" ]] && source "${CONFIG_DIR}/exports.sh"
[[ -f "${CONFIG_DIR}/aliases.sh" ]] && source "${CONFIG_DIR}/aliases.sh"
[[ -f "${CONFIG_DIR}/functions.sh" ]] && source "${CONFIG_DIR}/functions.sh"

# Local overrides for this machine
[[ -f "${CONFIG_DIR}/local.sh" ]] && source "${CONFIG_DIR}/local.sh"

# Development-specific paths (TTN, etc.)
[[ -f "${CONFIG_DIR}/development.sh" ]] && source "${CONFIG_DIR}/development.sh"

# Homebrew on macOS
if [[ "$OSTYPE" == "darwin"* ]]; then
  if [[ -f "/opt/homebrew/bin/brew" ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
  elif [[ -f "/usr/local/bin/brew" ]]; then
    eval "$(/usr/local/bin/brew shellenv)"
  fi
fi

# Cargo/Rust
[[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"

# Start Emacs daemon if not running (and not inside Emacs)
if [[ -z "$INSIDE_EMACS" ]]; then
  if ! pgrep -f "emacs --daemon" > /dev/null 2>&1; then
    emacs --daemon 2>/dev/null &
  fi
fi

# Local zsh customizations
[[ -f "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"

# Enable color support
autoload -U colors && colors
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad

# Simple prompt if no fancy prompt system
if ! command -v starship &> /dev/null; then
  PROMPT='%F{green}%n@%m%f:%F{blue}%~%f$ '
fi