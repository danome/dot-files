#!/usr/bin/env bash
# .bash_profile - Bash login shell configuration

# Source .bashrc if it exists
[[ -f "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

# Cargo/Rust environment
[[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"