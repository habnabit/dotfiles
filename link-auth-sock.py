#!/usr/bin/env python
import errno
import os
import signal

if 'SSH_AUTH_SOCK' in os.environ:
    dest = os.path.expanduser('~/.ssh/auth-sock')
    try:
        os.remove(dest)
    except OSError, e:
        if e.errno != errno.ENOENT:
            raise
    os.symlink(os.environ['SSH_AUTH_SOCK'], dest)
signal.pause()
