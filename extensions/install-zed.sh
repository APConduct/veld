#!/bin/bash
# Veld Zed Extension Installation Script

set -e  # Exit on error

echo "🚀 Veld Zed Extension Installer"
echo "================================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if we're in the veld directory
if [ ! -f "Cargo.toml" ] || [ ! -d "crates/lsp" ]; then
    echo -e "${RED}❌ Error: Must run from veld repository root${NC}"
    exit 1
fi

VELD_ROOT=$(pwd)

# Step 1: Initialize submodules
echo -e "${YELLOW}📦 Step 1: Initializing git submodules...${NC}"
git submodule update --init --recursive
echo -e "${GREEN}✓ Submodules initialized${NC}"
echo ""

# Step 2: Build the LSP server
echo -e "${YELLOW}🔨 Step 2: Building Veld LSP server...${NC}"
echo "This may take a few minutes on first build..."
cargo build --release -p veld-lsp

if [ ! -f "target/release/veld-lsp" ]; then
    echo -e "${RED}❌ Error: LSP build failed${NC}"
    exit 1
fi

echo -e "${GREEN}✓ LSP server built successfully${NC}"
echo ""

# Step 3: Install the extension
echo -e "${YELLOW}📂 Step 3: Installing Zed extension...${NC}"

ZED_EXTENSIONS_DIR="$HOME/.config/zed/extensions"
EXTENSION_LINK="$ZED_EXTENSIONS_DIR/veld"

# Create extensions directory if it doesn't exist
mkdir -p "$ZED_EXTENSIONS_DIR"

# Remove old symlink if it exists
if [ -L "$EXTENSION_LINK" ]; then
    echo "Removing old extension symlink..."
    rm "$EXTENSION_LINK"
elif [ -e "$EXTENSION_LINK" ]; then
    echo -e "${RED}❌ Error: $EXTENSION_LINK exists but is not a symlink${NC}"
    echo "Please remove it manually and run this script again"
    exit 1
fi

# Create new symlink
ln -s "$VELD_ROOT/extensions/zed-veld" "$EXTENSION_LINK"
echo -e "${GREEN}✓ Extension linked to: $EXTENSION_LINK${NC}"
echo ""

# Step 4: Verify installation
echo -e "${YELLOW}🔍 Step 4: Verifying installation...${NC}"

CHECKS_PASSED=true

# Check LSP binary
if [ -f "target/release/veld-lsp" ]; then
    echo -e "${GREEN}✓ LSP binary found${NC}"
else
    echo -e "${RED}✗ LSP binary not found${NC}"
    CHECKS_PASSED=false
fi

# Check extension symlink
if [ -L "$EXTENSION_LINK" ]; then
    echo -e "${GREEN}✓ Extension symlink created${NC}"
else
    echo -e "${RED}✗ Extension symlink not found${NC}"
    CHECKS_PASSED=false
fi

# Check extension files
if [ -f "extensions/zed-veld/extension.toml" ]; then
    echo -e "${GREEN}✓ Extension files present${NC}"
else
    echo -e "${RED}✗ Extension files missing${NC}"
    CHECKS_PASSED=false
fi

echo ""

if [ "$CHECKS_PASSED" = true ]; then
    echo -e "${GREEN}✅ Installation complete!${NC}"
    echo ""
    echo "Next steps:"
    echo "  1. Restart Zed editor"
    echo "  2. Open a .veld file"
    echo "  3. Look for 'Veld LSP' in the bottom-right status bar"
    echo ""
    echo "To test:"
    echo "  - Hover over variables to see type information"
    echo "  - Ctrl/Cmd+Click to jump to definitions"
    echo "  - Type to see auto-completions"
    echo ""
    echo "Documentation:"
    echo "  - README: extensions/zed-veld/README.md"
    echo "  - LSP Features: crates/lsp/FEATURES.md"
    echo ""
else
    echo -e "${RED}❌ Installation failed - see errors above${NC}"
    exit 1
fi
