import irssi

log = open('/tmp/irssilog', 'a')
def irssi_print(dest, text, stripped):
    log.write(`dest.window, dest.server, dest.target, dest.level, text, stripped` + '\n')

irssi.signal_add('print text', irssi_print)
