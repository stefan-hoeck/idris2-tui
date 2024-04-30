#!/usr/bin/env python3

"""This horrible hack reads from both stdin and the scale device,
printing a single stream of JSON-encoded packets to stdout.

It takes care of placing the terminal in raw mode and restoring its
state upon exit.

XXX: supress the broken pipe error if user quits from idris
"""

import os
import tty
import sys
import json
import queue
import threading
import termios

def readScale(path, q):
    device = open(path, "rb")
    while True:
      packet = device.read(6)
      q.put({"tag": "Scale", "contents": [list(packet)]})

def readStdin(q):
    while True:
        char = os.read(sys.stdin.fileno(), 1)
        q.put({"tag": "Stdin", "contents": [chr(ord(char))]})

try:
    save = tty.setcbreak(sys.stdin.fileno())
    q = queue.Queue()
    scaleThread = threading.Thread(
        target=readScale,
        args=(sys.argv[1], q),
        daemon=True
    )
    stdinThread = threading.Thread(
        target=readStdin,
        args=(q,),
        daemon=True
    )
    scaleThread.start()
    stdinThread.start()
    while True:
        json.dump(q.get(), sys.stdout)
        print("")
        sys.stdout.flush()
except KeyboardInterrupt:
    pass
finally:
    termios.tcsetattr(sys.stdin.fileno(), termios.TCSADRAIN, save)
