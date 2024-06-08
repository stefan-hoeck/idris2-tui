#!/usr/bin/env bash

export AMPII_DB_PATH=database
export AMPII_SCALE_PATH=/dev/hidraw3
export LINES
export COLUMNS

if test -e "${AMPII_SCALE_PATH}"
then
    python input-shim.py \
	   "${AMPII_SCALE_PATH}" \
	   2>shim_log | ./build/exec/ampii "$@" 2> debug_log
else
    echo "Scale path does not exist"
    python input-shim.py 2>shim_log | ./build/exec/ampii "$@" 2> debug_log
fi
