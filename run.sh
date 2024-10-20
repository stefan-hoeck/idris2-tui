#!/usr/bin/env bash

# BSD 3-Clause License
#
# Copyright (c) 2023, Brandon Lewis
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


export AMPII_DB_PATH=database
export AMPII_SCALE_PATH=/dev/hidraw0

function localURL {
    echo "http://$(hostname -I | cut -d ' ' -f 1):8000"
}

function hostQR {
    qrencode -o "upload/qrcode.png" "$(localURL)"
}

function shim {
    # generate a QR code for this host
    hostQR 2>&1 > /dev/null

    # tell the app to load the image
    echo '{"tag": "Image", "contents": ["upload/qrcode.png"]}'

    # now actually start the shim
    if test -e "${AMPII_SCALE_PATH}"
    then
	python input-shim.py \
	   "${AMPII_SCALE_PATH}" \
	   2>shim_log
    else
	echo "Scale path does not exist"
	python input-shim.py 2>shim_log
    fi
}

shim | ./build/exec/ampii "$@" 2> debug_log
