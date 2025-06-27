from __future__ import unicode_literals

import contextlib
import errno
import hashlib
import io
import json
import os
import posixpath
import sys
import tempfile

try:
    from urllib2 import Request, urlopen
except ImportError:
    from urllib.request import Request, urlopen
    WRAP_RESPFILE = True
else:
    WRAP_RESPFILE = False

try:
    from ConfigParser import RawConfigParser
except ImportError:
    from configparser import RawConfigParser

from fish import ProgressFish


JSON_TYPE = 'application/vnd.git-lfs+json'


def file_matches(infile, size, sha256):
    with infile:
        stat = os.fstat(infile.fileno())
        if stat.st_size != size:
            return False
        hasher = hashlib.sha256()
        for chunk in iter(lambda: infile.read(8192), b''):
            hasher.update(chunk)
        return hasher.hexdigest() == sha256


def main(config_path, desc_path, target_path):
    massaged = io.StringIO()
    with io.open(config_path, 'r') as infile:
        massaged.writelines(line.lstrip() for line in infile)
    massaged.seek(0)
    config = RawConfigParser()
    config.readfp(massaged)
    lfs_url = config.get('lfs', 'url').strip('"')
    api_url = posixpath.join(lfs_url, 'objects', 'batch')

    with io.open(desc_path, 'r') as infile:
        target = dict(line.strip().partition(' ')[::2] for line in infile)
    if target.get('version') != 'https://git-lfs.github.com/spec/v1':
        raise ValueError("can't handle lfs", target['version'])
    oid_type, sep, oid = target['oid'].partition(':')
    if oid_type != 'sha256':
        raise ValueError("can't handle oid", target['oid'])
    size = int(target['size'])

    sys.stderr.write('Fetching {!r} from lfs...\n'.format(
        os.path.basename(target_path)))
    try:
        infile = open(target_path, 'rb')
    except IOError as e:
        if e.errno != errno.ENOENT:
            raise
    else:
        if file_matches(infile, size, oid):
            sys.stderr.write('Lucky! It was already up to date.\n')
            return

    req = Request(api_url, json.dumps({
        'operation': 'download',
        'objects': [{
            'oid': oid,
            'size': size,
        }],
    }).encode(), {
        'Accept': JSON_TYPE,
        'Content-Type': JSON_TYPE,
    })
    with contextlib.closing(urlopen(req)) as respfile:
        if WRAP_RESPFILE:
            respfile = io.TextIOWrapper(respfile)
        resp = json.load(respfile)

    url = next(obj['actions']['download']['href']
               for obj in resp['objects']
               if obj['oid'] == oid)
    with contextlib.closing(urlopen(url)) as respfile:
        hasher = hashlib.sha256()
        with tempfile.NamedTemporaryFile(
                dir=os.path.dirname(target_path)) as outfile:
            fish = ProgressFish(total=size)
            fetched = 0
            for chunk in iter(lambda: respfile.read(8192), b''):
                fetched += len(chunk)
                fish.animate(amount=fetched)
                hasher.update(chunk)
                outfile.write(chunk)
            if hasher.hexdigest() != oid:
                raise ValueError('hash failure', hasher.hexdigest(), oid)
            os.rename(outfile.name, target_path)
            open(outfile.name, 'w').close()


main(*sys.argv[1:])
