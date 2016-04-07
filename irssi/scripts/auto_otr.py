# -*- coding: utf-8 -*-

import irssi


log = open('/tmp/irssilog', 'a')
def irssi_privmsg(server, msg, nick, address):
    if not msg.endswith(' \t  \t\t\t\t \t \t \t   \t \t  \t   \t\t  \t '):
        return
    witem = server.window_find_item(nick)
    witem.command('say ?OTR?')

irssi.signal_add('message private', irssi_privmsg)
