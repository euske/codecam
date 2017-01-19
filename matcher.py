#!/usr/bin/env python
import sys

def readkeys(fp, title=None):
    import re
    import time
    LINE_PAT = re.compile(r'(\d+)-(\d+)-(\d+) (\d+):(\d+):(\d+\.\d+) ([a-fA-F0-9]+)')
    ok = True
    for line in fp:
        line = line.strip()
        if line.startswith('#'):
            (_,_,s) = line.partition(' ')
            ok = (title is None or s == title)
        elif line and ok:
            m = LINE_PAT.match(line)
            if m:
                (yy,mm,dd,HH,MM,SS,ch) = m.groups()
                yy = int(yy)
                mm = int(mm)
                dd = int(dd)
                HH = int(HH)
                MM = int(MM)
                SS = float(SS)
                ch = int(ch, 16)
                tm = time.mktime((yy,mm,dd,HH,MM,0,0,0,0))
                yield (tm+SS, chr(ch))
    return
                
def match(text1, text2):
    n1 = len(text1)
    n2 = len(text2)
    pairs = set()
    r = []
    for i1 in range(n1):
        i2 = 0
        while i2 < n2:
            if text1[i1] == text2[i2] and (i1,i2) not in pairs:
                ia = i1
                ib = i2
                n = 0
                while text1[ia] == text2[ib] and ia < n1 and ib < n2:
                    pairs.add((ia,ib))
                    ia += 1
                    ib += 1
                    n += 1
                r.append((n,i1,i2))
                i2 = ib
            else:
                i2 += 1
    r.sort()
    return r

def main(argv):
    import getopt
    import fileinput
    def usage():
        print('usage: %s [-d] [-t title] logfile [file ...]' % argv[0])
        return 100
    try:
        (opts, args) = getopt.getopt(argv[1:], 'dt:')
    except getopt.GetoptError:
        return usage()
    debug = 0
    title = None
    for (k, v) in opts:
        if k == '-d': debug += 1
        elif k == '-t': title = v
    if not args: return usage()
    path = args.pop(0)
    with open(path, 'r') as fp:
        keys = list(readkeys(fp, title=title))
    text1 = ''.join( c for (_,c) in keys )
    for path in args:
        with open(path, 'r') as fp:
            text2 = fp.read()
            r = match(text1, text2)
            print (len(r))
    return 0

if __name__ == '__main__': sys.exit(main(sys.argv))
