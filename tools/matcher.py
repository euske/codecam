#!/usr/bin/env python
import sys
from bisect import bisect_left
from bisect import bisect_right
from bisect import insort
INF = sys.maxsize

def isok(c):
    return not c.isspace()

def readkeys(fp, title=None):
    ok = True
    for line in fp:
        line = line.strip()
        if line.startswith('#'):
            (_,_,s) = line.partition(' ')
            ok = (title is None or s == title)
        elif line and ok:
            (line,_,_) = line.partition('#')
            f = line.strip().split(' ')
            if 2 <= len(f):
                t = float(f[0])
                c = chr(int(f[1]))
                yield (t, c)
    return

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

class Corpus:

    def __init__(self, text1, text2):
        self.text1 = text1
        self.text2 = text2
        return

    def genmatches(self):
        n1 = len(self.text1)
        n2 = len(self.text2)
        sys.stderr.write('genmatches: n1=%d, n2=%d...\n' % (n1, n2))
        index1 = {}
        for (s1,c) in enumerate(self.text1):
            if not isok(c): continue
            if c in index1:
                a = index1[c]
            else:
                a = index1[c] = []
            a.append(s1)
        for (s2,c) in enumerate(self.text2):
            if c not in index1: continue
            # assert isok(c)
            # assert text1[s1] == text2[s2]
            for s1 in index1[c]:
                n = 0
                i1 = s1
                i2 = s2
                while (i1 < n1 and i2 < n2 and
                       isok(self.text1[i1]) and
                       self.text1[i1] == self.text2[i2]):
                    n += 1
                    i1 += 1
                    i2 += 1
                m = Match(self, [(n,s1,s2)])
                yield m
        return

class Match:

    def __init__(self, corpus, ranges):
        self.corpus = corpus
        self.ranges = ranges
        (_,i1,i2) = ranges[0]
        self.s1 = i1
        self.s2 = i2
        (n,i1,i2) = ranges[-1]
        self.e1 = i1+n
        self.e2 = i2+n
        assert self.s1 < self.e1
        assert self.s2 < self.e2
        common = sum( n for (n,_,_) in self.ranges )
        gap1 = (self.e1-self.s1-common)
        gap2 = (self.e2-self.s2-common)
        self.score = common*2-(gap1+gap2)
        return

    def __repr__(self):
        return ('<Match(%r) %d-%d : %d-%d>' %
                (self.ctext(), self.s1, self.e1, self.s2, self.e2))

    def ctext(self):
        return ''.join( self.corpus.text1[i1:i1+n]
                        for (n,i1,_) in self.ranges )

    def text1(self):
        i0 = None
        for (n,i1,_) in self.ranges:
            if i0 is not None:
                yield self.corpus.text1[i0:i1]
            i0 = i1+n
            yield '['+self.corpus.text1[i1:i0]+']'
        return

    def text2(self):
        i0 = None
        for (n,_,i1) in self.ranges:
            if i0 is not None:
                yield self.corpus.text2[i0:i1]
            i0 = i1+n
            yield '['+self.corpus.text2[i1:i0]+']'
        return

    def dump(self):
        print ('#', self)
        print ('#', repr(''.join(self.text1())))
        print ('#', repr(''.join(self.text2())))
        return

    def getdist(self, m):
        # must be non-overlapping and non-crossing.
        if self.e1 <= m.s1 and self.e2 <= m.s2:
            # self <= m
            return (m.s1-self.e1) + (m.s2-self.e2)
        elif m.e1 <= self.s1 and m.e2 <= self.s2:
            # m <= self
            return (self.s1-m.e1) + (self.s2-m.e2)
        else:
            return +INF

    def merge(self, m):
        if self.e1 <= m.s1 and self.e2 <= m.s2:
            return Match(self.corpus, self.ranges + m.ranges)
        elif m.e1 <= self.s1 and m.e2 <= self.s2:
            return Match(self.corpus, m.ranges + self.ranges)
        else:
            raise ValueError(m)

def cluster(matches, minscore=0, maxdist=INF):
    #sys.stderr.write('building index...\n')
    idx1 = Index('idx1')
    idx2 = Index('idx2')
    for (i,m) in enumerate(matches):
        idx1.add(m.s1, i, batch=True)
        idx1.add(m.e1, i, batch=True)
        idx2.add(m.s2, i, batch=True)
        idx2.add(m.e2, i, batch=True)
    idx1.build()
    idx2.build()
    assert len(idx1) == len(matches)*2
    assert len(idx2) == len(matches)*2
    sys.stderr.write('clustering: %d matches' % len(matches))
    sys.stderr.flush()

    pairs = []
    for (i,m0) in enumerate(matches):
        ra = idx1.search(m0.s1-maxdist, m0.e1+maxdist)
        rb = idx2.search(m0.s2-maxdist, m0.e2+maxdist)
        (m1,d1) = (None, +INF)
        for j in ra.union(rb):
            if i < j:
                m = matches[j]
                d = m.getdist(m0)
                if d < d1:
                    (m1,d1) = (m,d)
        if m1 is not None:
            assert i < matches.index(m1)
            pairs.append((d1,(m0,m1)))
        if (i % 100) == 0:
            sys.stderr.write('.')
            sys.stderr.flush()
    sys.stderr.write('\n')
    pairs.sort(key=lambda x: x[0])

    new = []
    taken = set()
    for (d,(m0,m1)) in pairs:
        m = m1.merge(m0)
        if m.score <= m0.score or m.score <= m1.score: continue
        taken.add(m0)
        taken.add(m1)
        new.append(m)
    old = [ m for m in matches if m not in taken ]
    return (new, old)

class Taken(ValueError): pass

def fixate(matches):
    matches.sort(key=lambda m:(m.score, m.e1), reverse=True)
    taken1 = set()
    taken2 = set()
    for m in matches:
        try:
            r = []
            for (n,i1,i2) in m.ranges:
                for d in range(n):
                    if i1+d in taken1: raise Taken()
                    if i2+d in taken2: raise Taken()
                    r.append((i1+d, i2+d))
            taken1.update( i1 for (i1,_) in r )
            taken2.update( i2 for (_,i2) in r )
            yield m
        except Taken:
            pass
    return

def main(argv):
    import getopt
    import fileinput
    def usage():
        print('usage: %s [-d] [-t title] [-m maxdist] [-s minscore]'
              ' [-n maxiters] [-x maxclusters] logfile [file ...]' % argv[0])
        return 100
    try:
        (opts, args) = getopt.getopt(argv[1:], 'dt:m:s:n:x:')
    except getopt.GetoptError:
        return usage()
    debug = 0
    title = None
    maxdist = 10
    minscore = 4
    maxiters = 10
    maxclusters = 1000
    for (k, v) in opts:
        if k == '-d': debug += 1
        elif k == '-t': title = v
        elif k == '-m': maxdist = int(v)
        elif k == '-s': minscore = int(v)
        elif k == '-n': maxiters = int(v)
        elif k == '-x': maxclusters = int(v)
    if maxiters == 0:
        maxiters = INF
    if maxclusters == 0:
        maxclusters = INF
    if not args: return usage()
    with open(args.pop(0), 'r') as fp:
        keys = list(readkeys(fp, title=title))
    text1 = ''.join( c for (_,c) in keys )
    fp = fileinput.input(args)
    text2 = ''.join( fp )
    corpus = Corpus(text1, text2)
    matches = list(corpus.genmatches())
    a = []
    for _ in range(maxiters):
        (new,old) = cluster(matches, minscore=minscore, maxdist=maxdist)
        a.extend(old)
        if not new: break
        matches = new
    matches = list(fixate(a))
    maps = {}
    for m in matches:
        if debug:
            m.dump()
        for (n,i1,i2) in m.ranges:
            for d in range(n):
                maps[i2+d] = i1+d
    for (i2,c) in enumerate(text2):
        if i2 in maps:
            i1 = maps[i2]
            print(i2, c, i1, keys[i1])
    print()
    return 0

if __name__ == '__main__': sys.exit(main(sys.argv))
