#!/usr/bin/env python
import re
import os
import socket
import sys

quote_map = {'&': '&&', '-': '&-', '\n': '&n', ' ': '&_'}
reverse_quote_map = {v: k for v, k in quote_map.iteritems()}
def quote(s):
    return re.sub(r'[-&\n ]', lambda m: quote_map[m.group()], s)
def unquote(s):
    return re.sub(r'&[-&n_]', lambda m: quote_map[m.group()], s)

nowait = False
if sys.argv[1:2] == ['-n']:
    del sys.argv[1:2]
    nowait = True

args = sys.argv[1:] or ['.']

with open(os.environ['remote_emacs_auth']) as infile:
    host, client_auth, tramp_prefix = infile.read().splitlines()

host, port = host.split()
sock = socket.socket()
sock.connect((host, int(port)))

pwd = os.getcwd()
tramp_args = [client_auth, '-dir', quote(tramp_prefix + pwd)]
if nowait:
    tramp_args.append('-nowait')

for arg in args:
    if arg.startswith('+'):
        tramp_args.extend(['-position', quote(arg)])
    else:
        tramp_args.extend(
            ['-file', quote(tramp_prefix + os.path.join(pwd, arg))])

sock.sendall(' '.join(tramp_args) + '\n')
buffer = ''
for line in sock.makefile():
    print unquote(line.rstrip('\n'))
