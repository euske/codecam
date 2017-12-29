#!/usr/bin/env python
#
#  Usage:
#    $ evalprec.py reference.txt output.txt
#
import sys

def evalprec(mref, mout):
    common = 0
    for (itxt,t) in mout.items():
        if t == mref[itxt]:
            common += 1
    print ('common:', common)
    print ('maps:', len(mout))
    print ('text:', len(mref))
    print ('prec.=%.02f' % (common / len(mout)))
    print ('recall=%.02f' % (common / len(mref)))
    return

def main(argv):
    import getopt
    def usage():
        print('usage: %s reference.txt output.txt' % argv[0])
        return 100
    try:
        (opts, args) = getopt.getopt(argv[1:], '')
    except getopt.GetoptError:
        return usage()

    if not args: return usage()
    reference = args.pop(0)
    if not args: return usage()
    output = args.pop(0)

    mref = []
    with open(reference) as fp:
        for line in fp:
            (t,_,line) = line.strip().partition(' ')
            mref.append(t)

    mout = {}
    with open(output) as fp:
        for line in fp:
            (itxt,_,line) = line.strip().partition(' ')
            (c,_,line) = line.strip().partition(' ')
            if not c: continue
            (ilog,_,line) = line.strip().partition(' ')
            assert line.startswith('(')
            (t,_,_) = line[1:].partition(',')
            i = int(itxt)
            assert i not in mout
            mout[i] = t

    evalprec(mref, mout)
    return

if __name__ == '__main__': sys.exit(main(sys.argv))
