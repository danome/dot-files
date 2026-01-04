#!/usr/bin/env bash
# Modern dot-files installation script

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKUP_DIR="$HOME/.dotfiles-backup-$(date +%Y%m%d-%H%M%S)"

# Files to symlink
SHELL_FILES=(
    ".zshrc"
    ".zprofile"
    ".zshenv"
    ".bashrc_modern:.bashrc"  # Use modern bashrc as .bashrc
    ".bash_profile"
)

EMACS_FILES=(
    ".emacs.d"
)

GIT_FILES=(
    ".gitconfig"
    ".gitignore_global"
)

CONFIG_DIRS=(
    ".config/shell"
)

echo -e "${GREEN}=== Dot-files Installation ===${NC}"
echo "Source: $DOTFILES_DIR"
echo "Backup: $BACKUP_DIR"
echo ""

# Detect OS
OS_TYPE="$(uname -s)"
case "$OS_TYPE" in
    Darwin*)
        echo -e "${GREEN}Detected macOS${NC}"
        PLATFORM="macos"
        ;;
    Linux*)
        echo -e "${GREEN}Detected Linux${NC}"
        PLATFORM="linux"
        ;;
    *)
        echo -e "${YELLOW}Warning: Unknown OS type: $OS_TYPE${NC}"
        PLATFORM="unknown"
        ;;
esac

# Detect shell
if [[ -n "$ZSH_VERSION" ]]; then
    echo -e "${GREEN}Running in zsh${NC}"
elif [[ -n "$BASH_VERSION" ]]; then
    echo -e "${GREEN}Running in bash${NC}"
fi

# Create backup directory
mkdir -p "$BACKUP_DIR"

# Function to backup and symlink
backup_and_link() {
    local source_file="$1"
    local target_file="$2"
    
    # Handle renaming (e.g., .bashrc_modern:.bashrc)
    if [[ "$source_file" == *":"* ]]; then
        local actual_source="${source_file%:*}"
        target_file="${source_file#*:}"
        source_file="$actual_source"
    fi
    
    local source_path="$DOTFILES_DIR/$source_file"
    local target_path="$HOME/$target_file"
    
    # Skip if source doesn't exist
    if [[ ! -e "$source_path" ]]; then
        echo -e "${YELLOW}  Skipping $source_file (not found)${NC}"
        return
    fi
    
    # Backup existing file/directory
    if [[ -e "$target_path" ]] || [[ -L "$target_path" ]]; then
        echo "  Backing up existing $target_file"
        mv "$target_path" "$BACKUP_DIR/$(basename "$target_file")"
    fi
    
    # Create symlink
    echo "  Linking $target_file"
    ln -s "$source_path" "$target_path"
}

echo -e "\n${GREEN}Installing shell configurations...${NC}"
for file in "${SHELL_FILES[@]}"; do
    backup_and_link "$file" "$file"
done

echo -e "\n${GREEN}Installing config directories...${NC}"
for dir in "${CONFIG_DIRS[@]}"; do
    backup_and_link "$dir" "$dir"
done

echo -e "\n${GREEN}Installing Emacs configuration...${NC}"
for file in "${EMACS_FILES[@]}"; do
    backup_and_link "$file" "$file"
done

echo -e "\n${GREEN}Installing Git configuration...${NC}"
for file in "${GIT_FILES[@]}"; do
    backup_and_link "$file" "$file"
done

# Platform-specific setup
if [[ "$PLATFORM" == "macos" ]]; then
    echo -e "\n${GREEN}macOS-specific setup...${NC}"
    
    # Install Homebrew if not present
    if ! command -v brew &> /dev/null; then
        echo -e "${YELLOW}  Homebrew not found. Install from https://brew.sh${NC}"
    else
        echo "  Homebrew found at $(which brew)"
    fi
fi

# Check for required tools
echo -e "\n${GREEN}Checking required tools...${NC}"

check_tool() {
    if command -v "$1" &> /dev/null; then
        echo -e "  ✓ $1 found"
    else
        echo -e "  ${YELLOW}✗ $1 not found${NC}"
    fi
}

check_tool "git"
check_tool "emacs"
check_tool "emacsclient"
check_tool "cargo"
check_tool "claude"

echo -e "\n${GREEN}Installation complete!${NC}"
echo ""
echo "Next steps:"
echo "1. Restart your shell or run: source ~/.$(basename "$SHELL")rc"
echo "2. Review the backup at: $BACKUP_DIR"
echo "3. Create ~/.config/shell/local.sh for machine-specific settings"
echo "4. Create ~/.config/shell/development.sh for project paths"
echo ""
echo "To uninstall, restore files from: $BACKUP_DIR"
