import subprocess
import traceback
import urlparse
import urllib
import glib
import time
import sys
import os
from lxml import objectify

api_key = 'eba9632ddc908a8fd7ad1200d771beb7'
api_url = 'http://ws.audioscrobbler.com/2.0/'
parts = ['artist', 'name', 'album', 'date', 'url']

def lastfm_lookup():
    _, username, irssi_encoding, lastfm_strftime, lastfm_output = sys.argv
    url = urlparse.urljoin(api_url, '?' + urllib.urlencode(dict(
        method='user.getrecenttracks',
        user=username,
        api_key=api_key,
        limit='1')))
    tree = objectify.parse(url)
    assert tree.getroot().get('status') == 'ok'
    track = tree.xpath('/lfm/recenttracks/track')[0]
    parts_dict = {}
    for part in parts:
        if part == 'date':
            if track.get('nowplaying') == 'true':
                value = u''
            else:
                tt = time.localtime(int(track.date.get('uts')))
                value = time.strftime(lastfm_strftime, tt).decode(
                    irssi_encoding)
        else:
            value = unicode(getattr(track, part, u''))
        parts_dict[part] = value
    print (
        lastfm_output.decode(irssi_encoding) % parts_dict
    ).encode(irssi_encoding)

def do_now_playing(data, server, witem):
    def on_result(pid, status):
        if not os.WIFEXITED(status):
            sys.stderr.write('child %d exited abnormally: status %d\n' % (
                pid, status))
            return
        stdout, stderr = proc.stdout.read(), proc.stderr.read()
        if os.WEXITSTATUS(status):
            irssi.prnt(stderr.rstrip())
        elif irssi.settings_get_bool('lastfm_use_action'):
            witem.command('me %s' % stdout.rstrip())
        else:
            witem.command('say %s' % stdout.rstrip())
    username = data or irssi.settings_get_str('lastfm_user')
    irssi_encoding = irssi.settings_get_str('term_charset')
    lastfm_strftime = irssi.settings_get_str('lastfm_strftime')
    lastfm_output = irssi.settings_get_str('lastfm_output')
    proc = subprocess.Popen([
            sys.executable, __file__, 
            username, irssi_encoding, lastfm_strftime, lastfm_output],
        stdin=open(os.devnull), stdout=subprocess.PIPE, stderr=subprocess.PIPE, 
        close_fds=True)
    glib.child_watch_add(proc.pid, on_result)

if __name__ == '__main__':
    lastfm_lookup()
else:
    import irssi
    irssi.settings_add_str('lastfm', 'lastfm_user', '')
    irssi.settings_add_str('lastfm', 'lastfm_output', 
        'np: %(artist)s-%(name)s')
    irssi.settings_add_str('lastfm', 'lastfm_output_tab_complete', '')
    irssi.settings_add_str('lastfm', 'lastfm_strftime', 'scrobbled at: %R %Z')
    irssi.settings_add_bool('lastfm', 'lastfm_use_action', 0)
    irssi.settings_add_bool('lastfm', 'lastfm_get_player', 0)
    irssi.command_bind('np', do_now_playing)
