#!/usr/bin/env bash
# kitty-shell-hooks.sh — Kitty-specific shell enhancements.
# Sourced from .bashrc when running inside kitty.
#   1. Per-OS-window background tint
#   2. Dynamic terminal title (directory at prompt, command while running)

# Only run inside kitty
[[ -z "$KITTY_PID" ]] && return 0

# ── Background tint ──────────────────────────────────────────────────
# Palette of tinted dark backgrounds (variants of the base #0f172a)
_KITTY_BG_TINTS=(
    "#0f172a"   # base blue-slate (unchanged)
    "#0b2018"   # forest green
    "#1a1030"   # deep purple
    "#201510"   # espresso brown
    "#081e24"   # deep teal
    "#220e1e"   # dark plum
)

_kitty_tint_index=$(( KITTY_PID % ${#_KITTY_BG_TINTS[@]} ))
_kitty_bg="${_KITTY_BG_TINTS[$_kitty_tint_index]}"

kitty @ --to "unix:/tmp/kitty-${KITTY_PID}" set-colors background="$_kitty_bg" 2>/dev/null

unset _KITTY_BG_TINTS _kitty_tint_index _kitty_bg

# ── Dynamic window title ─────────────────────────────────────────────
# Shows the last command run in the tab title (or "bash" if none yet)

_kitty_prompt_title() {
    # Extract last command from history, stripping the leading number
    local last
    last=$(history 1 | sed 's/^[ ]*[0-9]*[ ]*//')
    printf '\033]2;%s\007' "${last:-bash}"
}

PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}_kitty_prompt_title"
