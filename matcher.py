#!/usr/bin/env python
import sys
import re
from bisect import bisect_left
from bisect import bisect_right
from bisect import insort
INF = sys.maxsize

PAT_CHUNK = re.compile(r'\w+|\S')
def chunk(s):
    return PAT_CHUNK.finditer(s)

def calcscore(common, gap1, gap2):
    return common*2-(gap1+gap2)

def readkeys(fp, title=None):
    ok = True
    for line in fp:
        line = line.strip()
        if line.startswith('#'):
            (_,_,s) = line.partition(' ')
            ok = (title is None or s == title)
        elif line and ok:
            (line,_,_) = line.partition('#')
            (t,_,c) = line.strip().partition(' ')
            if t and c:
                t = float(t)
                c = chr(int(c))
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

    def genmatches(self, minchars=2):
        n1 = len(self.text1)
        n2 = len(self.text2)
        sys.stderr.write('genmatches: n1=%d, n2=%d...\n' % (n1, n2))
        chunks1 = {}
        for m in chunk(self.text1):
            w = m.group(0)
            if w in chunks1:
                a = chunks1[w]
            else:
                a = chunks1[w] = []
            i1 = m.start(0)
            a.append(i1)
        for m in chunk(self.text2):
            w = m.group(0)
            if w in chunks1:
                i2 = m.start(0)
                for i1 in chunks1[w]:
                    m = Match(self, [(len(w),i1,i2)])
                    #sys.stderr.write('genmatches: %r: %r\n' % (m, w))
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
        self.common = sum( n for (n,_,_) in ranges )
        gap1 = (self.e1-self.s1-self.common)
        gap2 = (self.e2-self.s2-self.common)
        self.score = calcscore(self.common, gap1, gap2)
        return

    def __repr__(self):
        return ('<Match(%d) %d-%d : %d-%d>' %
                (self.common, self.s1, self.e1, self.s2, self.e2))

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
        print (repr(''.join(self.text1())))
        print (repr(''.join(self.text2())))
        return

    def getscore(self, m):
        # must be non-overlapping and non-crossing.
        common = self.common + m.common
        if self.e1 <= m.s1 and self.e2 <= m.s2:
            # self <= m
            return calcscore(common, m.e1-self.s1-common, m.e2-self.s2-common)
        elif m.e1 <= self.s1 and m.e2 <= self.s2:
            # m <= self
            return calcscore(common, self.e1-m.s1-common, self.e2-m.s2-common)
        else:
            return -INF

    def merge(self, m):
        if self.e1 <= m.s1 and self.e2 <= m.s2:
            return Match(self.corpus, self.ranges + m.ranges)
        elif m.e1 <= self.s1 and m.e2 <= self.s2:
            return Match(self.corpus, m.ranges + self.ranges)
        else:
            raise ValueError(m)

def cluster(matches, mindist=INF):
    #sys.stderr.write('building index...\n')
    matches.sort(key=lambda m:m.score, reverse=True)
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
    n = 0
    for (i,m0) in enumerate(matches):
        ra = idx1.search(m0.s1-mindist, m0.e1+mindist)
        rb = idx2.search(m0.s2-mindist, m0.e2+mindist)
        (m1,s1) = (None, -1)
        for j in ra.union(rb):
            if i < j:
                m = matches[j]
                s = m.getscore(m0)
                if s1 < s:
                    (m1,s1) = (m,s)
        if m1 is not None:
            if i < matches.index(m1):
                pairs.append((s1,(m0,m1)))
        n += 1
        if (n % 100) == 0:
            sys.stderr.write('.')
            sys.stderr.flush()
    sys.stderr.write('\n')
    pairs.sort(key=lambda x: x[0], reverse=True)
    
    finished = []
    taken = set()
    for (_,(m0,m1)) in pairs:
        if m0 not in taken and m1 not in taken:
            taken.add(m0)
            taken.add(m1)
            finished.append(m1.merge(m0))
    return finished + [ m for m in matches if m not in taken ]

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
        print('usage: %s [-d] [-t title] [-m mindist] [-n maxiters] logfile [file ...]' % argv[0])
        return 100
    try:
        (opts, args) = getopt.getopt(argv[1:], 'dt:m:n:')
    except getopt.GetoptError:
        return usage()
    debug = 0
    title = None
    mindist = 20
    minscore = 6
    maxiters = 10
    for (k, v) in opts:
        if k == '-d': debug += 1
        elif k == '-t': title = v
        elif k == '-m': mindist = int(v)
        elif k == '-s': minscore = int(v)
        elif k == '-n': maxiters = int(v)
    if not args: return usage()
    with open(args.pop(0), 'r') as fp:
        keys = list(readkeys(fp, title=title))
    text1 = ''.join( c for (_,c) in keys )
    fp = fileinput.input(args)
    text2 = ''.join( fp )
    corpus = Corpus(text1, text2)
    matches = list(corpus.genmatches())
    n1 = len(matches)
    for _ in range(maxiters):
        matches = cluster(matches, mindist=mindist)
        if n1 == len(matches): break
        n1 = len(matches)
    matches = [ m for m in matches if minscore <= m.score ]
    matches = list(fixate(matches))
    maps = {}
    for m in matches:
        print (m)
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
