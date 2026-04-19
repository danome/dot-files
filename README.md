# Modern Dot-files

A clean, modular configuration for macOS and Linux development environments featuring:
- **Zsh** as primary shell (macOS) with bash compatibility
- **Emacs** with Magit and Claude CLI integration
- **Git** with useful aliases and ediff integration
- Modular shell configuration for easy customization

## Features

- 🚀 Fast shell startup with optimized configurations
- 📦 Modular design - mix and match components
- 🔧 Smart installer with backup and OS detection
- 🎨 Clean separation of shell-agnostic and shell-specific configs
- 💻 Developer-focused with support for Rust, Go, Python, Node.js
- 🤖 AI-powered development with Claude CLI integration

## Quick Start

```bash
# Clone the repository
git clone git@github.com:danome/dot-files.git ~/Development/repositories/dot-files

# Run installer
cd ~/Development/repositories/dot-files
chmod +x install.sh
./install.sh
```

## Structure

```
.
├── .config/
│   └── shell/           # Shared shell configurations
│       ├── aliases.sh   # Common aliases
│       ├── exports.sh   # Environment variables
│       └── functions.sh # Utility functions
├── .emacs.d/           # Emacs configuration
│   ├── init.el         # Main config
│   ├── early-init.el   # Performance optimizations
│   └── lisp/           # Custom elisp
├── .zshrc              # Zsh configuration
├── .zprofile           # Zsh login shell
├── .zshenv             # Zsh environment
├── .bashrc_modern      # Modern bash config
├── .bash_profile       # Bash login shell
├── .gitconfig          # Git configuration
├── .gitignore_global   # Global git ignores
└── install.sh          # Installation script
```

## Customization

### Machine-specific Settings
Create `~/.config/shell/local.sh` for machine-specific configurations:
```bash
# Example local.sh
export WORK_DIR="$HOME/work"
alias myproject="cd $WORK_DIR/myproject"
```

### Development Paths
Create `~/.config/shell/development.sh` for project-specific paths:
```bash
# Example development.sh
export PATH="$HOME/Development/tools:$PATH"
export GOPATH="$HOME/go"
```

### Git Customization
Create `~/.gitconfig.local` for machine-specific git settings:
```ini
[user]
    email = work@company.com
[core]
    sshCommand = /usr/bin/ssh -i ~/.ssh/work_key
```

## Shell Features

### Aliases
- `e` - Open standalone Emacs GUI
- `ec` - Open standalone Emacs GUI
- `et` - Open terminal Emacs (`emacs -nw`)
- `gs` - Git status
- `ll` - Detailed file listing
- `mkd` - Make directory and cd into it
- And many more (see `.config/shell/aliases.sh`)

### Functions
- `extract` - Extract any archive
- `backup` - Quick file backup with timestamp
- `git-cleanup` - Remove merged branches

### Emacs Note
On this Linux/Wayland setup, the Emacs daemon and `emacsclient -c` path is currently disabled because GUI client frames were unstable:
- tiny startup windows
- daemon restarts/crashes during normal GUI use
- TTN observe buffers interacting badly with daemon-backed GUI frames

The current stable baseline is standalone Emacs for GUI (`e`, `ec`) and `emacs -nw` for terminal (`et`). Re-enable the daemon only after confirming a concrete need and a stable client path.

## Emacs Integration

The configuration includes:
- **Magit** for Git integration
- **Claude CLI** helpers for AI-powered development
- **Company** for auto-completion
- **Projectile** for project management
- Language support for Python, Go, Rust, TypeScript

### Claude CLI Commands
- `C-c m m` - Claude menu
- `C-c m a` - AI commit message
- `C-c m d` - Generate documentation
- `C-c m r` - Refactor region

## Requirements

### macOS
- Zsh 5.0+ (included)
- Homebrew (recommended)
- Emacs 27+ with `emacsclient`
- Git 2.20+

### Linux
- Bash 4.0+ or Zsh 5.0+
- Emacs 27+
- Git 2.20+

## Installation Details

The installer will:
1. Detect your OS and shell
2. Create timestamped backups of existing files
3. Create symlinks to the repository files
4. Check for required tools
5. Provide next-step instructions

## Uninstalling

To uninstall, restore your original files from the backup directory shown during installation:
```bash
# The installer shows the backup location, typically:
# ~/.dotfiles-backup-YYYYMMDD-HHMMSS/
```

## Contributing

Feel free to fork and submit pull requests. Keep changes modular and well-documented.

## License

MIT License - see LICENSE file for details

## Version 2.0 Modernization Details

This repository underwent a major modernization in January 2025. Here's what was changed:

| Area | Proposed Changes | Actual Implementation |
|------|-----------------|----------------------|
| **1. Core Shell Configuration** | Create modular system with `.config/shell/` directory for shared configs | ✅ Created `.config/shell/` with:<br>• `exports.sh` - PATH management, environment variables, XDG settings<br>• `aliases.sh` - 80+ aliases for git, emacs, system commands<br>• `functions.sh` - Helper functions (extract, backup, emacs-daemon management) |
| **2. Emacs Configuration** | Modernize with use-package, include boon-claude, add early-init, remove duplicates | ✅ Created:<br>• `early-init.el` - Startup optimizations, GC tuning<br>• `init_modern.el` - Clean use-package based config<br>• `lisp/boon-claude.el` - Claude CLI integration<br>• Kept old `init.el` (not replaced yet) |
| **3. Git Configuration** | Add user-specific includes, global ignores, local settings support | ✅ Replaced `.gitconfig` with:<br>• Modern aliases (30+ shortcuts)<br>• Ediff integration for merge/diff<br>• Include for `~/.gitconfig.local`<br>• Created `.gitignore_global` with 99 lines of common ignores |
| **4. Development Tools** | Add configs for starship, tmux, SSH template | ❌ Not implemented - focused on core shell/editor configs instead |
| **5. Installation System** | Rewrite with OS detection, backups, conflict detection, idempotent | ✅ Completely rewrote `install.sh`:<br>• OS detection (macOS/Linux)<br>• Timestamped backups<br>• Color-coded output<br>• Tool checking (git, emacs, cargo, claude)<br>• Symlink management |
| **6. Files to Remove/Archive** | Remove legacy conda, vagrant, nvm configs | ✅ Removed from new configs:<br>• No conda initialization<br>• No vagrant checks<br>• No nvm references<br>• Created `.gitconfig.old`, `README.md.old` as backups |
| **7. Documentation** | Update README, add CHANGELOG, structure docs | ✅ Created:<br>• New `README.md` - Complete rewrite with features, structure, customization<br>• `CHANGELOG.md` - Version 2.0.0 documentation<br>• Clear installation/uninstallation instructions |

### Additional Changes Made
- **Zsh Configuration**: Created `.zshrc`, `.zprofile`, `.zshenv` (not in original bash-centric repo)
- **Bash Modernization**: Created `.bashrc_modern` and `.bash_profile` for Linux compatibility
- **Backup Strategy**: Old files renamed with `.old` extension rather than deleted

The implementation closely followed the plan with the exception of development tool configs (starship, tmux) which can be added in a future update if needed.
