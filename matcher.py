#!/usr/bin/env python
import sys
from bisect import bisect_left
from bisect import bisect_right
from bisect import insort
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
        self.n = sum( n for (n,_,_) in ranges )
        return

    def __repr__(self):
        return ('<Match(%d-%d, %d-%d)>' %
                (self.s1, self.e1, self.s2, self.e2))

    def getdist(self, m):
        # must be non-overlapping and non-crossing.
        if self.e1 <= m.s1 and self.e2 <= m.s2:
            return max(m.s1-self.e1, m.s2-self.e2)
        elif m.e1 <= self.s1 and m.e2 <= self.s2:
            return max(self.s1-m.e1, self.s2-m.e2)
        else:
            return INF

    def merge(self, m):
        if self.e1 <= m.s1 and self.e2 <= m.s2:
            return Match(self.ranges + m.ranges)
        elif m.e1 <= self.s1 and m.e2 <= self.s2:
            return Match(m.ranges + self.ranges)
        else:
            raise ValueError(m)

def getmatches(text1, text2, minchars=2):
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
                while ia < n1 and ib < n2 and text1[ia] == text2[ib]:
                    pairs.add((ia,ib))
                    ia += 1
                    ib += 1
                    n += 1
                if minchars <= n:
                    matches.append(Match([(n,i1,i2)]))
                i2 = ib
            else:
                i2 += 1
    return matches

class Index:

    """
    >>> idx = Index()
    >>> idx.add(1, 'moo')
    >>> idx.add(5, 'baa')
    >>> idx.search(0,2)
    {'moo'}
    >>> idx.search(1,2)
    {'moo'}
    >>> idx.search(1,1)
    {'moo'}
    >>> idx.search(2,2)
    set()
    >>> idx.search(3,10)
    {'baa'}
    >>> sorted(idx.search(0,5))
    ['baa', 'moo']
    >>> idx.search(10,5)
    set()
    >>> idx.remove(5, 'baa')
    >>> idx.search(3,10)
    set()
    """

    def __init__(self, name=None):
        self.name = name
        self.objs = {}
        self._idxs = []
        return

    def __repr__(self):
        return '<%s: %s>' % (self.__class__.__name__, self.name)

    def __len__(self):
        return sum( len(r) for r in self.objs.values() )

    def add(self, v, obj, batch=False):
        if v in self.objs:
            self.objs[v].append(obj)
        else:
            self.objs[v] = [obj]
            if batch:
                self._idxs = None
            else:
                assert v not in self._idxs
                insort(self._idxs, v)
                assert v in self._idxs
        return

    def remove(self, v, obj, batch=False):
        if v not in self.objs:
            raise KeyError(v)
        r = self.objs[v]
        r.remove(obj)
        if r: return
        del self.objs[v]
        if batch:
            self._idxs = None
        else:
            assert v in self._idxs
            i = bisect_left(self._idxs, v)
            assert self._idxs[i] == v
            del self._idxs[i]
        return

    def build(self):
        self._idxs = sorted(self.objs.keys())
        return

    def search(self, v0, v1):
        assert self._idxs is not None
        i0 = bisect_left(self._idxs, v0)
        i1 = bisect_right(self._idxs, v1)
        r = set()
        for i in range(i0, i1):
            v = self._idxs[i]
            assert v in self.objs
            r.update(self.objs[v])
        return r


def cluster(matches, mindist=INF):
    sys.stderr.write('building index...\n')
    matches.sort(key=lambda m:m.n, reverse=True)
    idx1 = Index('idx1')
    idx2 = Index('idx2')
    for m in matches:
        idx1.add(m.s1, m, batch=True)
        idx1.add(m.e1, m, batch=True)
        idx2.add(m.s2, m, batch=True)
        idx2.add(m.e2, m, batch=True)
    idx1.build()
    idx2.build()
    assert len(idx1) == len(matches)*2
    assert len(idx2) == len(matches)*2
    sys.stderr.write('clustering: %d matches' % len(matches))
    sys.stderr.flush()
    n = 0
    finished = []
    while matches:
        m0 = matches.pop(0)
        idx1.remove(m0.s1, m0)
        idx1.remove(m0.e1, m0)
        idx2.remove(m0.s2, m0)
        idx2.remove(m0.e2, m0)
        ra = idx1.search(m0.s1-mindist, m0.e1+mindist)
        rb = idx2.search(m0.s2-mindist, m0.e2+mindist)
        (m1,d1) = (None,mindist)
        for m in ra.union(rb):
            assert m is not m0
            d = m.getdist(m0)
            if d < d1:
                (m1,d1) = (m,d)
        if m1 is None:
            finished.append(m0)
        else:
            matches.remove(m1)
            idx1.remove(m1.s1, m1)
            idx1.remove(m1.e1, m1)
            idx2.remove(m1.s2, m1)
            idx2.remove(m1.e2, m1)
            m3 = m1.merge(m0)
            matches.insert(0, m3)
            idx1.add(m3.s1, m3)
            idx1.add(m3.e1, m3)
            idx2.add(m3.s2, m3)
            idx2.add(m3.e2, m3)
        n += 1
        if (n % 100) == 0:
            sys.stderr.write('.')
            sys.stderr.flush()
    sys.stderr.write('\n')
    return finished

class Taken(ValueError): pass

def fixate(matches):
    matches.sort(key=lambda m:(m.n,m.e1), reverse=True)
    maps = {}
    taken1 = set()
    taken2 = set()
    for m in matches:
        try:
            r = []
            for (n,i1,i2) in m.ranges:
                for i in range(n):
                    if i1+i in taken1: raise Taken()
                    if i2+i in taken2: raise Taken()
                    r.append((i1+i, i2+i))
            taken1.update( i1 for (i1,_) in r )
            taken2.update( i2 for (_,i2) in r )
            for (i1,i2) in r:
                maps[i2] = i1
        except Taken:
            pass
    return maps

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
    with open(args.pop(0), 'r') as fp:
        keys = list( (t,c) for (t,c) in readkeys(fp, title=title) if c.isalnum() )
    text1 = ''.join( c for (_,c) in keys )
    fp = fileinput.input(args)
    text2 = ''.join( fp )
    matches = getmatches(text1, text2)
    n0 = INF
    n1 = len(matches)
    while n1 < n0:
        matches = cluster(matches)
        n0 = n1
        n1 = len(matches)
    maps = fixate(matches)
    for (i2,c) in enumerate(text2):
        if i2 in maps:
            i1 = maps[i2]
            print(i2, c, i1, keys[i1])
    print()
    return 0

if __name__ == '__main__': sys.exit(main(sys.argv))
