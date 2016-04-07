# -*- coding: utf-8 -*-
import random

NOTES = u'♩♪♫♬'
MODIFIERS = u'♭♮♯'

def picknote():
    ret = random.choice(NOTES)
    if not random.randrange(4):
        ret = random.choice(MODIFIERS) + ret
    return ret

def notesay(s):
    return u'%s %s %s' % (picknote(), s, picknote())

def irssi_notesay(data, server, witem):
    data = data.decode('utf-8')
    witem.command('say %s' % (notesay(data).encode('utf-8'),))

if __name__ == '__main__':
    import sys
    print notesay(' '.join(sys.argv[1:]).decode('utf-8')).encode('utf-8')
else:
    import irssi
    irssi.command_bind('notesay', irssi_notesay)
