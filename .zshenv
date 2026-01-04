#!/usr/bin/env zsh
# .zshenv - Zsh environment variables (loaded for all shells)

# Cargo/Rust
[[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"

# Skip global configs if needed
# export GLOBAL_RCS=0