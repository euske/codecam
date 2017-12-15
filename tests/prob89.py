#!/usr/bin/env python

ROMAN2ARABIC = {
    'I':1, 'V':5, 'X':10, 'L':50, 'C':100, 'D':500, 'M':1000
}
ARABIC2ROMAN = [
    (1000,'M'), (900,'CM'), (500,'D'), (400,'CD'),
    (100,'C'), (90,'XC'), (50,'L'), (40,'XL'),
    (10,'X'), (9,'IX'), (5,'V'), (4,'IV'),
    (1,'I'),
]

def parse_roman(s):
    n = 0
    v0 = 99999
    for c in s:
        v = ROMAN2ARABIC[c]
        if v <= v0:
            n += v
        else:
            n -= v0
            n += v-v0
        v0 = v
    return n

assert (parse_roman('I') == 1)
assert (parse_roman('III') == 3)
assert (parse_roman('IV') == 4)
assert (parse_roman('VI') == 6)
assert (parse_roman('IVI') == 5)
assert (parse_roman('IIIIIIIIIIIIIIII') == 16)
assert (parse_roman('VIIIIIIIIIII') == 16)
assert (parse_roman('VVIIIIII') == 16)
assert (parse_roman('XIIIIII') == 16)
assert (parse_roman('VVVI') == 16)
assert (parse_roman('XVI') == 16)

def generate_roman(n):
    s = ''
    while 0 < n:
        for (v,c) in ARABIC2ROMAN:
            if v <= n:
                n -= v
                s += c
                break
    return s

assert (generate_roman(16) == 'XVI')
assert (generate_roman(1) == 'I')
assert (generate_roman(2) == 'II')
assert (generate_roman(3) == 'III')
assert (generate_roman(4) == 'IV')
assert (generate_roman(5) == 'V')
assert (generate_roman(6) == 'VI')

saved = 0
with open('roman.txt') as fp:
    for line in fp:
        line = line.strip()
        n = parse_roman(line)
        s = generate_roman(n)
        if line != s:
            print (line, s)
            assert len(s) <= len(line)
            saved += len(line) - len(s)
print (saved)
