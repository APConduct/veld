#!/bin/bash
# Install the Veld extension for Zed editor
# This script copies the extension files to Zed's extensions directory

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Determine the extensions directory based on OS
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    EXTENSIONS_DIR="$HOME/Library/Application Support/Zed/extensions"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux
    EXTENSIONS_DIR="$HOME/.config/zed/extensions"
else
    echo -e "${RED}Unsupported OS: $OSTYPE${NC}"
    exit 1
fi

VELD_EXT_DIR="$EXTENSIONS_DIR/veld"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE_DIR="$SCRIPT_DIR/zed-veld"

echo -e "${GREEN}Installing Veld extension for Zed...${NC}"

# Check if source directory exists
if [ ! -d "$SOURCE_DIR" ]; then
    echo -e "${RED}Error: Extension source not found at $SOURCE_DIR${NC}"
    exit 1
fi

# Check if extension.wasm exists
if [ ! -f "$SOURCE_DIR/extension.wasm" ]; then
    echo -e "${YELLOW}Warning: extension.wasm not found. Building now...${NC}"
    cd "$SOURCE_DIR"
    cargo build --release --target wasm32-wasip1
    if [ $? -eq 0 ]; then
        cp target/wasm32-wasip1/release/veld_zed_extension.wasm extension.wasm
        echo -e "${GREEN}✓ Built extension.wasm${NC}"
    else
        echo -e "${RED}Failed to build extension${NC}"
        exit 1
    fi
    cd - > /dev/null
fi

# Create extensions directory if it doesn't exist
mkdir -p "$EXTENSIONS_DIR"

# Check if extension is already installed
if [ -d "$VELD_EXT_DIR" ] || [ -L "$VELD_EXT_DIR" ]; then
    echo -e "${YELLOW}Veld extension already installed. Removing old version...${NC}"
    rm -rf "$VELD_EXT_DIR"
fi

# Ask user for installation type
echo ""
echo "Choose installation type:"
echo "  1) Copy (recommended for normal use)"
echo "  2) Symlink (recommended for development)"
read -p "Enter choice [1-2]: " choice

case $choice in
    1)
        echo -e "${GREEN}Copying extension files...${NC}"
        cp -r "$SOURCE_DIR" "$VELD_EXT_DIR"
        echo -e "${GREEN}✓ Extension copied to $VELD_EXT_DIR${NC}"
        ;;
    2)
        echo -e "${GREEN}Creating symlink...${NC}"
        ln -s "$SOURCE_DIR" "$VELD_EXT_DIR"
        echo -e "${GREEN}✓ Extension symlinked to $VELD_EXT_DIR${NC}"
        echo -e "${YELLOW}Note: Any changes to $SOURCE_DIR will be immediately available in Zed${NC}"
        ;;
    *)
        echo -e "${RED}Invalid choice${NC}"
        exit 1
        ;;
esac

# Check if veld-lsp is available
echo ""
echo -e "${GREEN}Checking for veld-lsp binary...${NC}"

LSP_FOUND=false

# Check PATH
if command -v veld-lsp &> /dev/null; then
    echo -e "${GREEN}✓ veld-lsp found in PATH: $(which veld-lsp)${NC}"
    LSP_FOUND=true
fi

# Check project build directory
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
if [ -f "$PROJECT_ROOT/target/release/veld-lsp" ]; then
    echo -e "${GREEN}✓ veld-lsp found at: $PROJECT_ROOT/target/release/veld-lsp${NC}"
    LSP_FOUND=true
fi

if [ "$LSP_FOUND" = false ]; then
    echo -e "${YELLOW}⚠ veld-lsp not found!${NC}"
    echo ""
    echo "The language server is required for the extension to work."
    echo "To build it, run:"
    echo ""
    echo "  cd $PROJECT_ROOT"
    echo "  cargo build --release --bin veld-lsp"
    echo ""
    echo "Or add it to your PATH if it's installed elsewhere."
fi

echo ""
echo -e "${GREEN}═══════════════════════════════════════════════════${NC}"
echo -e "${GREEN}✓ Veld extension installed successfully!${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════${NC}"
echo ""
echo "Next steps:"
echo "  1. Restart Zed editor"
echo "  2. Open a .veld file"
echo "  3. Check that syntax highlighting and LSP features work"
echo ""
echo "To view extension logs:"
echo "  - Open Zed Command Palette (Cmd+Shift+P / Ctrl+Shift+P)"
echo "  - Search for 'zed: open log'"
echo ""
echo "For troubleshooting, see: $SCRIPT_DIR/BUILD_SUCCESS.md"
echo ""
