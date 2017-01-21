#!/usr/bin/env python
import sys
INF = sys.maxsize

def readkeys(fp, title=None):
    ok = True
    for line in fp:
        line = line.strip()
        if line.startswith('#'):
            (_,_,s) = line.partition(' ')
            ok = (title is None or s == title)
        elif line and ok:
            (t,_,c) = line.partition(' ')
            if t and c:
                yield (float(t), chr(int(c)))
    return

class Match:

    def __init__(self, ranges):
        self.ranges = ranges
        (_,i1,i2) = ranges[0]
        self.s1 = i1
        self.s2 = i2
        (n,i1,i2) = ranges[-1]
        self.e1 = i1+n
        self.e2 = i2+n
        assert self.s1 < self.e1
        assert self.s2 < self.e2
        self.n1 = self.e1 - self.s1
        return

    def getdist(self, m):
        # must be non-overlapping and non-crossing.
        if self.e1 <= m.s1 and self.e2 <= m.s2:
            return (m.s1-self.e1 + m.s2-self.e2)
        elif m.e1 <= self.s1 and m.e2 <= self.s2:
            return (self.s1-m.e1 + self.s2-m.e2)
        else:
            return INF

    def merge(self, m):
        if self.e1 <= m.s1 and self.e2 <= m.s2:
            return Match(self.ranges + m.ranges)
        elif m.e1 <= self.s1 and m.e2 <= self.s2:
            return Match(m.ranges + self.ranges)
        else:
            assert False
            return None

def getmatches(text1, text2):
    n1 = len(text1)
    n2 = len(text2)
    sys.stderr.write('getmatches: n1=%d, n2=%d...\n' % (n1, n2))
    pairs = set()
    matches = []
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
                if 1 < n:
                    matches.append(Match([(n,i1,i2)]))
                i2 = ib
            else:
                i2 += 1
    return matches

def cluster(matches, gap=INF):
    sys.stderr.write('cluster: %d matches' % len(matches))
    matches.sort(key=lambda m:m.n1, reverse=True)
    n = 0
    for i in range(len(matches)-1, -1, -1):
        m0 = matches[i]
        (mj,dm) = (None,gap)
        for j in range(i):
            assert j < i
            m1 = matches[j]
            d1 = m1.getdist(m0)
            if d1 < dm:
                (mj,dm) = (j,d1)
        if mj is not None:
            del matches[i]
            mm = matches[mj]
            del matches[mj]
            matches.append(m0.merge(mm))
        n += 1
        if (n % 100) == 0:
            sys.stderr.write('.')
            sys.stderr.flush()
    sys.stderr.write('\n')
    return matches

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
        matches = getmatches(text1, text2)
        n0 = INF
        n1 = len(matches)
        while n1 < n0:
            matches = cluster(matches)
            n0 = n1
            n1 = len(matches)
        print (matches)
    return 0

if __name__ == '__main__': sys.exit(main(sys.argv))
