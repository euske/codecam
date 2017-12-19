#!/usr/bin/env python
# viewer.py

import sys
import os.path
import time
import fileinput

def q(s):
    return (s.
            replace('&','&amp;').
            replace('>','&gt;').
            replace('<','&lt;').
            replace('"','&#34;').
            replace("'",'&#39;'))

def getfeat(t, featmap, dt=1):
    i1 = len(featmap)
    i0 = 0
    while i0 < i1:
        i = (i0+i1)//2
        (v,f) = featmap[i]
        if t < v:
            i1 = i
        elif v+dt <= t:
            i0 = i+1
        else:
            return f
    return None

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
        print('usage: %s [-d] {-T|-H template.html} [-s stream.mp3] [-f facemap] '
              'outfile [file ...]' % argv[0])
        return 100
    try:
        (opts, args) = getopt.getopt(argv[1:], 'dTH:s:f:')
    except getopt.GetoptError:
        return usage()
    debug = 0
    html = None
    stream = 'stream.mp4'
    featmap = None
    for (k, v) in opts:
        if k == '-d': debug += 1
        elif k == '-T': html = None
        elif k == '-H': html = v
        elif k == '-s': stream = v
        elif k == '-f':
            featmap = []
            with open(v, 'r') as fp:
                for line in fp:
                    line = line.strip()
                    (t,_,line) = line.partition(' ')
                    (s,_,line) = line.partition(' ')
                    featmap.append((float(t), s))
            featmap.sort()
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
    if html is not None:
        with open(html, 'r') as fp:
            sys.stdout.write(fp.read())
        sys.stdout.write('<video controls id="video" src="%s"></video>\n' %
                         q(stream))
        (name,_) = os.path.splitext(os.path.basename(stream))
        start = time.mktime(time.strptime(name[-15:], '%Y%m%d-%H%M%S'))
        start += 1
        sys.stdout.write('<pre id="src">\n')
        func = func_html
    else:
        start = 0
        func = func_text
    fp = fileinput.input(args)
    show(func, maps, fp, start=start, featmap=featmap, debug=debug)
    if html is not None:
        sys.stdout.write('</pre>\n')
    return

if __name__ == '__main__': sys.exit(main(sys.argv))
