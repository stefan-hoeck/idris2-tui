#!/usr/bin/env python3

"""This horrible hack reads from both stdin and the scale device,
printing a single stream of JSON-encoded packets to stdout.

It takes care of placing the terminal in raw mode and restoring its
state upon exit.

It even spawns a local web server in order to handle image upload via
smartphone.

XXX: supress the broken pipe error if user quits from idris

"""

import os
import http.server
import json
import queue
import sys
import termios
import threading
import tty

# handles uploading images via a web interface
#
# point your phone at your hostname:8000
def server(port, q):
    class ImageHandler(http.server.SimpleHTTPRequestHandler):
        def __init__(self, *args, **kwargs):
            super().__init__(directory="web", *args, **kwargs)

        def do_PUT(self):
            print("test")
            data = self.rfile.read(int(self.headers.get('content-length')))
            q.put({"tag": "Image", "contents": [self.path, list(data)]})
            self.send_response(200, "Ok")
    s = http.server.HTTPServer(('', 8000), ImageHandler)
    s.serve_forever()

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
    serverThread = threading.Thread(
        target=server,
        args=(8000, q),
        daemon=True
    )
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
    serverThread.start()
    while True:
        json.dump(q.get(), sys.stdout)
        print("")
        sys.stdout.flush()
except KeyboardInterrupt:
    pass
finally:
    termios.tcsetattr(sys.stdin.fileno(), termios.TCSADRAIN, save)
