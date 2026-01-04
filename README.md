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
- `e` - Open in emacsclient
- `gs` - Git status
- `ll` - Detailed file listing
- `mkd` - Make directory and cd into it
- And many more (see `.config/shell/aliases.sh`)

### Functions
- `extract` - Extract any archive
- `backup` - Quick file backup with timestamp
- `git-cleanup` - Remove merged branches
- `emacs-daemon-*` - Manage Emacs daemon

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
- Emacs 27+ with `emacsclient`
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