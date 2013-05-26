#!/usr/bin/env python
import errno
import os
import signal

pidfile = os.path.expanduser('~/.ssh/auth-sock-linker.pid')
try:
    with open(pidfile) as infile:
        pid = int(infile.read().strip())
except (IOError, ValueError):
    pass
else:
    try:
        os.kill(pid, signal.SIGTERM)
    except OSError, e:
        if e.errno not in (errno.ESRCH, errno.EPERM):
            raise

try:
    with open(pidfile, 'w') as outfile:
        outfile.write(str(os.getpid()))
except IOError:
    pass

if 'SSH_AUTH_SOCK' in os.environ:
    dest = os.path.expanduser('~/.ssh/auth-sock')
    try:
        os.remove(dest)
    except OSError, e:
        if e.errno != errno.ENOENT:
            raise
    os.symlink(os.environ['SSH_AUTH_SOCK'], dest)
signal.pause()
