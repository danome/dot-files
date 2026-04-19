# Changelog

All notable changes to this project will be documented in this file.

## [2.1.1] - 2026-04-19

### Changed
- Disabled the user Emacs daemon workflow on Linux/Wayland and switched shell helpers to standalone Emacs for GUI sessions
- Updated shell aliases so `e` and `ec` open standalone Emacs, while `et` uses `emacs -nw`

### Notes
- This change was made after repeated instability with daemon-backed GUI frames, including tiny startup windows and daemon crashes while using TTN observe buffers
- Re-enable `emacsclient` only after confirming a real need and a stable GUI client path
## [2.1.0] - 2026-04-18

### Added
- TTN Network Observation integration for Emacs (`ttn-observe.el`)
- Versioned support for `ttn` binary (v0.1.0, tracking `ttn-docs` 0.1.0)
- New `C-c t t` keybinding for the TTN menu

## [2.0.0] - 2025-01-04

### Added
- Modular shell configuration system in `.config/shell/`
- Modern Zsh configuration with optimizations
- Early-init.el for faster Emacs startup
- Claude CLI integration (boon-claude.el)
- Global gitignore file
- Smart installer with OS detection and backups
- Support for machine-specific overrides via local files
- Comprehensive git aliases
- Development-specific PATH management

### Changed
- Complete rewrite of shell configurations
- Modernized Emacs configuration with use-package
- Updated git configuration with better defaults
- Restructured repository for better organization
- Improved documentation

### Removed
- Legacy bash-only configurations
- Outdated Emacs customizations
- Conda and NVM specific configurations
- Vagrant-related checks
- Old GNU coreutils paths

## [1.0.0] - Previous Version

- Basic bash configuration
- Simple Emacs setup
- Minimal git configuration
