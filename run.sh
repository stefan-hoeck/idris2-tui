#!/usr/bin/env bash

export AMPII_DB_PATH=database
export AMPII_SCALE_PATH=/dev/hidraw0
export LINES
export COLUMNS

python input-shim.py "${AMPII_SCALE_PATH}" | ./build/exec/ampii "$@" 2> debug_log
