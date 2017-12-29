#!/usr/bin/env python
#
#  Usage:
#    $ getface.py -K api_key images/*.jpg > faces.txt
#
#  cf. https://azure.microsoft.com/en-us/services/cognitive-services/emotion/
#
import sys
import requests
import json
import time

def send(session, data, apikey, timeout=30):
    url = 'https://westus.api.cognitive.microsoft.com/emotion/v1.0/recognize'
    headers = {
        'Content-Type': 'application/octet-stream',
        'Ocp-Apim-Subscription-Key': apikey,
    }
    req = session.post(url, data, headers=headers, timeout=timeout)
    if req.status_code != 200: raise IOError((req.status_code, url))
    objs = json.loads(req.content)
    if not objs: return None
    return objs[0]

def getvalue(props):
    maxk = None
    maxv = -1
    for (k,v) in props.items():
        if maxk is None or maxv < v:
            (maxk, maxv) = (k,v)
    return (maxk, maxv)

def main(argv):
    import getopt
    def usage():
        print('usage: %s [-d] [-K path] [-w delay] [file ...]' % argv[0])
        return 100
    try:
        (opts, args) = getopt.getopt(argv[1:], 'dK:w:')
    except getopt.GetoptError:
        return usage()
    debug = 0
    apikey = None
    delay = 1
    for (k, v) in opts:
        if k == '-d': debug += 1
        elif k == '-K':
            with open(v) as fp:
                apikey = fp.read().strip()
        elif k == '-w':
            delay = float(v)
    #
    session = requests.session()
    for path in args:
        print('sending: %r' % path, file=sys.stderr)
        with open(path, 'rb') as fp:
            data = fp.read()
            obj = send(session, data, apikey)
            if not obj: continue
            scores = obj.get('scores')
            if not scores: continue
            props = sorted(scores.items(), key=lambda x: x[1], reverse=True)
            print(path, ' '.join( '%s:%.3f' % (k,v) for (k,v) in props ))
            sys.stdout.flush()
        time.sleep(delay)
    return 0

if __name__ == '__main__': sys.exit(main(sys.argv))
