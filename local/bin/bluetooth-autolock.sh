#!/usr/bin/env bash

# Default configuration
DEVICE_MAC="dc:10:57:6e:26:6b"  # Replace with your iPhone MAC
DEFAULT_RSSI_THRESHOLD=-10      # Default RSSI threshold in dBm
DEFAULT_CHECK_INTERVAL=10       # Default check interval in seconds

# Initialize variables with defaults
RSSI_THRESHOLD=$DEFAULT_RSSI_THRESHOLD
CHECK_INTERVAL=$DEFAULT_CHECK_INTERVAL
TEST_MODE=false

# Function to display usage
show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "OPTIONS:"
    echo "  --test                    Run in test mode (prints to console, no actual locking)"
    echo "  --interval <seconds>      Set check interval (default: $DEFAULT_CHECK_INTERVAL)"
    echo "  --threshold <-dBm>        Set RSSI threshold (default: $DEFAULT_RSSI_THRESHOLD)"
    echo "  --help                    Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 --test --interval 3 --threshold -50"
    echo "  $0 --threshold -70 --test"
    echo "  $0 --interval 10"
    echo ""
    echo "Note: RSSI threshold should be negative (e.g., -60, -70, -80)"
}

is_device_connected() {
    local connected_status
    connected_status=$(bluetoothctl info "$DEVICE_MAC" 2>/dev/null | grep "Connected:" | awk '{print $2}')
    [[ "$connected_status" == "yes" ]]
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --test)
            TEST_MODE=true
            shift
            ;;
        --interval)
            if [[ -n $2 && $2 =~ ^[0-9]+$ ]]; then
                CHECK_INTERVAL=$2
                shift 2
            else
                echo "Error: --interval requires a positive integer"
                exit 1
            fi
            ;;
        --threshold)
            if [[ -n $2 && $2 =~ ^-[0-9]+$ ]]; then
                RSSI_THRESHOLD=$2
                shift 2
            else
                echo "Error: --threshold requires a negative integer (e.g., -60)"
                exit 1
            fi
            ;;
        --help)
            show_usage
            exit 0
            ;;
        *)
            echo "Error: Unknown option '$1'"
            show_usage
            exit 1
            ;;
    esac
done

# Display configuration
if [[ "$TEST_MODE" == true ]]; then
    echo "=== BLUETOOTH AUTO-LOCK TEST MODE ==="
    echo "Device: $DEVICE_MAC"
    echo "RSSI Threshold: $RSSI_THRESHOLD dBm"
    echo "Check Interval: ${CHECK_INTERVAL}s"
    echo ""
    echo "Locking Logic:"
    echo "  ✓ Connected + Weak Signal (< $RSSI_THRESHOLD dBm) → LOCK"
    echo "  ✗ Connected + Strong Signal (≥ $RSSI_THRESHOLD dBm) → No action"
    echo "  ✗ Disconnected → No action (waiting for reconnection)"
    echo ""
    echo "Press Ctrl+C to stop"
    echo "Format: [TIME] [CONNECTION] RSSI: [VALUE] dBm - [ACTION]"
    echo "--------------------------------------------------------"
fi

# Main monitoring loop
while :; do
    TIMESTAMP=$(date '+%H:%M:%S')

    # First check if device is connected
    if is_device_connected; then
        # Device is connected, now check RSSI
        RSSI=$(hcitool rssi "$DEVICE_MAC" 2>/dev/null | awk '{print $4}')

        if [[ -n "$RSSI" && "$RSSI" -lt "$RSSI_THRESHOLD" ]]; then
            # Connected but weak signal - LOCK
            if [[ "$TEST_MODE" == true ]]; then
                echo "[$TIMESTAMP] [CONNECTED] RSSI: $RSSI dBm - WOULD LOCK (weak signal)"
            else
                if ! pgrep -x "xlock" > /dev/null; then
                    xlock -mode random &
                fi
            fi
        else
            # Connected with good signal - OK
            if [[ "$TEST_MODE" == true ]]; then
                echo "[$TIMESTAMP] [CONNECTED] RSSI: $RSSI dBm - OK (strong signal)"
            fi
        fi
    else
        # Device is disconnected - don't lock, wait for reconnection
        if [[ "$TEST_MODE" == true ]]; then
            echo "[$TIMESTAMP] [DISCONNECTED] RSSI: N/A - WAITING (no lock during reconnection)"
        fi
    fi

    sleep "$CHECK_INTERVAL"
  done
