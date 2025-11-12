#!/bin/bash

# Monitor LSP activity during testing
# Usage: ./monitor_lsp.sh

echo "=========================================="
echo "Veld LSP Monitor"
echo "=========================================="
echo ""
echo "This script monitors LSP server activity in real-time."
echo "Keep this running while you test the VSCode extension."
echo ""
echo "Press Ctrl+C to stop monitoring"
echo ""
echo "=========================================="
echo ""

# Check if log file exists
if [ ! -f "lsp_server.log" ]; then
    echo "⚠️  Warning: lsp_server.log not found yet."
    echo "   The log will be created when the LSP starts."
    echo ""
    echo "Waiting for log file to be created..."
    echo ""
fi

# Function to colorize output
colorize_log() {
    while IFS= read -r line; do
        # Color ERROR lines red
        if echo "$line" | grep -q "ERROR"; then
            echo -e "\033[0;31m$line\033[0m"
        # Color WARN lines yellow
        elif echo "$line" | grep -q "WARN"; then
            echo -e "\033[0;33m$line\033[0m"
        # Color INFO lines green
        elif echo "$line" | grep -q "INFO"; then
            echo -e "\033[0;32m$line\033[0m"
        # Color DEBUG lines blue
        elif echo "$line" | grep -q "DEBUG"; then
            echo -e "\033[0;36m$line\033[0m"
        # Highlight request types
        elif echo "$line" | grep -q "textDocument/"; then
            echo -e "\033[1;35m$line\033[0m"
        # Highlight initialization
        elif echo "$line" | grep -q "initialize\|initialized"; then
            echo -e "\033[1;32m$line\033[0m"
        # Default
        else
            echo "$line"
        fi
    done
}

# Wait for file and tail it
tail -f lsp_server.log 2>/dev/null | colorize_log || {
    # If tail fails, wait for file to be created
    while [ ! -f "lsp_server.log" ]; do
        sleep 1
    done
    echo "✅ Log file created! Monitoring..."
    echo ""
    tail -f lsp_server.log | colorize_log
}
