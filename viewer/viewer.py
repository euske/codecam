#!/usr/bin/env python
# viewer.py

import sys
import os.path
import time
import fileinput
import re

def q(s):
    return (s.
            replace('&','&amp;').
            replace('>','&gt;').
            replace('<','&lt;').
            replace('"','&#34;').
            replace("'",'&#39;'))

def show_html_header():
    print('''<!DOCTYPE html>
<html>
<meta charset="utf-8" />
<style>
.f_anger { background: cyan; }
.f_contempt { background: cyan; }
.f_disgust { background: cyan; }
.f_fear { background: cyan; }
.f_happiness { background: yellow; }
.f_sadness { background: cyan; }
.f_surprise { background: cyan; }
.h { background: lightgreen; }
#video { position: fixed; top:0; right:0; width: 800px; padding:0; margin:0; }
</style>
<script src="player.js"></script>
<body onload="run()">
''')
    return

def getstart(path):
    (name,_) = os.path.splitext(os.path.basename(path))
    start = time.mktime(time.strptime(name[-15:], '%Y%m%d-%H%M%S'))
    return start+1

def getfeat(t, featmap, dt=1):
    i1 = len(featmap)
    i0 = 0
    while i0 < i1:
        i = (i0+i1)//2
        (v,f) = featmap[i]
        if t < v-dt:
            i1 = i
        elif v+dt <= t:
            i0 = i+1
        else:
            return f
    return None

PAT = re.compile(r'.*\bi(\d+)\.jpg')
THRESHOLD = { 'neutral':0.90 }
def loadfeats(fp):
    feats = []
    for line in fp:
        line = line.strip()
        (t,_,line) = line.partition(' ')
        m = PAT.match(t)
        t = float(m.group(1))
        fs = []
        for x in line.split(' '):
            (k,_,v) = x.partition(':')
            v = float(v)
            v -= THRESHOLD.get(k, 0)
            fs.append((v, k))
        fs.sort(reverse=True)
        f = fs[0][1]
        if f == 'neutral': continue
        #print(t,f,file=sys.stderr)
        feats.append((t-1.5, f))
    feats.sort()
    return feats

def func_text(a):
    if isinstance(a, str):
        return a
    else:
        return '['+''.join( c for (c,_,_) in a )+']'

def func_html(a):
    def z(v):
        (c,t,f) = v
        if f is None:
            return ('<a t="%s">%s</a>' % (t, q(c)))
        else:
            return ('<a t="%s" class="f_%s">%s</a>' % (t, q(f), q(c)))
    if isinstance(a, str):
        return q(a)
    else:
        #v = ','.join( str(v[0]) for (_,v) in a )
        #s = ''.join( c for (c,_) in a )
        return ''.join(map(z, a))

def show(func, maps, fp, start=0, featmap=None, debug=0):
    i1 = 0
    for line in fp:
        if debug:
            values = [ maps[i1+d] for (d,_) in enumerate(line) if i1+d in maps ]
            sys.stdout.write('# '+', '.join( repr(v) for v in values )+'\n')
        b = ''
        a = []
        for (d,c) in enumerate(line):
            v = maps.get(i1+d)
            if v is not None:
                (t,_) = v
                t -= start
                if featmap is not None:
                    f = getfeat(t, featmap)
                else:
                    f = None
                a.append((c,t,f))
            else:
                if a:
                    b += func(a)
                    a = []
                b += func(c)
        if a:
            b += func(a)
        sys.stdout.write(b)
        i1 += len(line)
    return

def main(argv):
    import getopt
    def usage():
        print('usage: %s [-d] [-s stream.mp4] [-f facemap] '
              'outfile [file ...]' % argv[0])
        return 100
    try:
        (opts, args) = getopt.getopt(argv[1:], 'ds:f:')
    except getopt.GetoptError:
        return usage()
    debug = 0
    stream = None
    featmap = None
    for (k, v) in opts:
        if k == '-d': debug += 1
        elif k == '-s': stream = v
        elif k == '-f':
            with open(v, 'r') as fp:
                featmap = loadfeats(fp)
    if not args: return usage()
    maps = {}
    with open(args.pop(0), 'r') as fp:
        for line in fp:
            # index_text c index_keylog (t,c)
            line = line.strip()
            (i2,_,line) = line.partition(' ')
            (c,_,line) = line.partition(' ')
            if not c: continue
            (i1,_,line) = line.partition(' ')
            if line:
                (t,c) = eval(line)
                maps[int(i2)] = (t,c)
    if stream is None:
        fp = fileinput.input(args)
        show(func_text, maps, fp, featmap=featmap, debug=debug)
    else:
        start = getstart(stream)
        show_html_header()
        sys.stdout.write('<video controls id="video" src="%s"></video>\n' %
                         q(stream))
        sys.stdout.write('<pre id="src">\n')
        fp = fileinput.input(args)
        show(func_html, maps, fp, start=start, featmap=featmap, debug=debug)
        sys.stdout.write('</pre>\n')
    return

if __name__ == '__main__': sys.exit(main(sys.argv))
