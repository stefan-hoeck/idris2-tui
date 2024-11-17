#!/usr/bin/env python3

# Nothing is copied
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


"""This horrible hack reads from both stdin and the scale device,
printing a single stream of JSON-encoded packets to stdout.

It takes care of placing the terminal in raw mode and restoring its
state upon exit.

It even spawns a local web server in order to handle image upload via
smartphone.

XXX: supress the broken pipe error if user quits from idris

"""

import os
import json
import queue
import sys
import termios
import threading
import time
import tty


def readStdin(q):
    fd = sys.stdin.fileno()
    # os.set_blocking(fd, False)
    while True:
        try:
            match os.read(fd, 1):
                case None: time.sleep(1/32)
                case char: q.put({"tag": "Stdin", "contents": [chr(ord(char))]})
        except BlockingIOError:
            time.sleep(1/32)

def writeStdout(q):
    while True:
        try:
            json.dump(q.get(timeout=1), sys.stdout)
            print("")
            sys.stdout.flush()
        except queue.Empty:
            pass
        except (BrokenPipeError, IOError):
            return

def timer(q):
    while True:
        time.sleep(1.0)
        q.put({"tag": "Counter", "contents": ["Inc"]})

q = queue.Queue()
readerThread = threading.Thread(
    target=readStdin,
    args=(q,),
    daemon=True
)
writerThread = threading.Thread(
    target=writeStdout,
    args=(q,),
    daemon=True
)
timerThread = threading.Thread(
    target=timer,
    args=(q,),
    daemon=True
)

save = tty.setcbreak(sys.stdin.fileno())
try:
    readerThread.start()
    writerThread.start()
    timerThread.start()
    writeStdout(q)
except KeyboardInterrupt:
    pass
finally:
    sys.stderr.close()
    termios.tcsetattr(sys.stdin.fileno(), termios.TCSADRAIN, save)
