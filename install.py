#!/usr/bin/env python
import errno
import os
import shutil
import subprocess
import sys


BINS = ['hab-prompt-utils']


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    suffix = '-{0}-{4}'.format(*os.uname())
    bindir = 'bin' + suffix
    lfsdir = 'lfs' + suffix
    lfsconfig = os.path.join(here, '.lfsconfig')
    lfs_fetch = os.path.join(here, 'lfs-fetch.py')
    basedir = os.path.join(here, 'oh-my-zsh', 'helper-bins')
    real_bins = {}

    try:
        os.mkdir(os.path.join(basedir, bindir))
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise

    for binary in BINS:
        binary_lfs = os.path.join(basedir, lfsdir, binary)
        binary_bin = os.path.join(basedir, bindir, binary)
        with open(binary_lfs, 'rb') as infile:
            if infile.readline().startswith(b'version '):
                subprocess.check_call(
                    [sys.executable, lfs_fetch,
                     lfsconfig, binary_lfs, binary_bin])
            else:
                infile.seek(0)
                with open(binary_bin, 'wb') as outfile:
                    shutil.copyfileobj(infile, outfile)
        os.chmod(binary_bin, 0o755)
        real_bins[binary] = binary_bin

    subprocess.check_call(
        [real_bins['hab-prompt-utils'], 'install',
         os.path.join(here, 'Manifest'), os.path.expanduser('~')])

main()
