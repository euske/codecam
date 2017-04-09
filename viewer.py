#!/usr/bin/env python
import sys
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
        print('usage: %s [-d] {-T|-H} outfile [file ...]' % argv[0])
        return 100
    try:
        (opts, args) = getopt.getopt(argv[1:], 'dTH')
    except getopt.GetoptError:
        return usage()
    html = False
    debug = 0
    for (k, v) in opts:
        if k == '-d': debug += 1
        elif k == '-H': html = True
    if not args: return usage()
    maps = {}
    with open(args.pop(0), 'r') as fp:
        for line in fp:
            line = line.strip()
            (i2,_,line) = line.partition(' ')
            (c,_,line) = line.partition(' ')
            (i1,_,line) = line.partition(' ')
            if line:
                key = eval(line)
                maps[int(i2)] = key
    if html:
        func = func_html
        sys.stdout.write('<pre id="src">\n')
    else:
        func = func_text
    show(func, maps, args)
    if html:
        sys.stdout.write('</pre>\n')
    return

if __name__ == '__main__': sys.exit(main(sys.argv))
