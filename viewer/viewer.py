#!/usr/bin/env python
#
# ffmpeg -r 1 -t 1 -ss 60 -i stream.mp4 -filter:v 'crop=240:180:1040:0' -f image2 snapshot-%05d.jpeg
#
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

def func_text(a):
    if isinstance(a, str):
        return a
    else:
        return '['+''.join( c for (c,_) in a )+']'

def func_html(a):
    if isinstance(a, str):
        return q(a)
    else:
        v = ','.join( str(v[0]) for (_,v) in a )
        s = ''.join( c for (c,_) in a )
        return '<a t="%s">%s</a>' % (v, q(s))

def show(func, maps, args, debug=0):
    i1 = 0
    for line in fileinput.input(args):
        if debug:
            values = [ maps[i1+d] for (d,_) in enumerate(line) if i1+d in maps ]
            sys.stdout.write('# '+', '.join( repr(v) for v in values )+'\n')
        b = ''
        a = []
        for (d,c) in enumerate(line):
            v = maps.get(i1+d)
            if v is not None:
                a.append((c,v))
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
        print('usage: %s [-d] {-T|-H template.html} [-s stream.mp3] outfile [file ...]' % argv[0])
        return 100
    try:
        (opts, args) = getopt.getopt(argv[1:], 'dTH:s:')
    except getopt.GetoptError:
        return usage()
    debug = 0
    html = None
    stream = 'stream.mp3'
    for (k, v) in opts:
        if k == '-d': debug += 1
        elif k == '-T': html = None
        elif k == '-H': html = v
        elif k == '-s': stream = v
    if not args: return usage()
    maps = {}
    with open(args.pop(0), 'r') as fp:
        for line in fp:
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
        sys.stdout.write('<video controls id="video" src="%s" width="800"></video>\n' %
                         q(stream))
        (name,_) = os.path.splitext(os.path.basename(stream))
        start = time.mktime(time.strptime(name[-15:], '%Y%m%d-%H%M%S'))
        sys.stdout.write('<pre id="src" t="%f">\n' % start)
        func = func_html
    else:
        func = func_text
    show(func, maps, args)
    if html is not None:
        sys.stdout.write('</pre>\n')
    return

if __name__ == '__main__': sys.exit(main(sys.argv))
