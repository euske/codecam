//  server.js
//
const http = require('http');
const url = require('url');
const fs = require('fs');
const path = require('path');

const PORT = 8080;
const DATADIR = './var';
const INTERVAL = 1000;
const TIMEOUT = 10*INTERVAL;

let sessions = {};

class Session {
  constructor(sid) {
    this.sid = sid;
    this.update();
    const path = DATADIR+'/video_'+sid+'.webm';
    this.fd = fs.openSync(path, 'w');
    this.text = {};
    this.output = {};
    console.log('start:', this);
  }

  toString() {
    return ('<Session '+this.sid+'>');
  }

  close() {
    console.log('close:', this);
    fs.closeSync(this.fd);
    const path = DATADIR+'/data_'+sid+'.json';
    const data = { text: this.text, output: this.output };
    fs.writeFileSync(path, JSON.stringify(data));
  }

  update() {
    this.lastUpdated = new Date();
  }

  isAlive(t) {
    return (t-this.lastUpdated < TIMEOUT);
  }

  addVideo(t, chunks) {
    this.update();
    for (let chunk of chunks) {
      fs.writeSync(this.fd, chunk);
    }
  }

  addText(t, chunks) {
    this.update();
    let buf = '';
    for (let chunk of chunks) {
      buf += chunk;
    }
    this.text[t] = buf;
  }

  addOutput(t, chunks) {
    this.update();
    let buf = '';
    for (let chunk of chunks) {
      buf += chunk;
    }
    this.output[t] = buf;
  }

  recvVideo(req, res, t) {
    let chunks = [];
    req.on('data', (chunk) => { chunks.push(chunk); });
    req.on('end', () => {
      this.addVideo(t, chunks);
      res.writeHead(200, {'Content-Type': 'text/html'});
      res.end('ok');
    });
  }

  recvText(req, res, t) {
    let chunks = [];
    req.on('data', (chunk) => { chunks.push(chunk); });
    req.on('end', () => {
      this.addText(t, chunks);
      res.writeHead(200, {'Content-Type': 'text/html'});
      res.end('ok');
    });
  }

  recvOutput(req, res, t) {
    let chunks = [];
    req.on('data', (chunk) => { chunks.push(chunk); });
    req.on('end', () => {
      this.addOutput(t, chunks);
      res.writeHead(200, {'Content-Type': 'text/html'});
      res.end('ok');
    });
  }
}

function idle() {
  const t = new Date();
  let removed = [];
  for (sid in sessions) {
    let session = sessions[sid];
    if (!session.isAlive(t)) {
      session.close();
      removed.push(sid);
    }
  }
  for (sid of removed) {
    delete sessions[sid];
  }
}

function serve(req, res) {
  const u = url.parse(req.url, true);
  const sid = u.query.sid;
  if (req.method == 'GET' && u.pathname == '/') {
    console.log('list');
    res.writeHead(200, {'Content-Type': 'text/html'});
    res.write('<html><body><h1>List</h1>\n');
    res.write('<table border><tr><th>Id</th><th>Date</th></tr>\n');
    for (ent of fs.readdirSync(DATADIR, {withFileTypes: true})) {
      if (ent.isFile() && ent.name.endsWith('.webm')) {
        const i = ent.name.indexOf('_');
        const id1 = ent.name.substring(i+1, ent.name.length-5);
        const path2 = path.join(DATADIR, 'data_'+id1+'.json');
        if (fs.existsSync(path2)) {
          const stat = fs.statSync(path2);
          res.write('<tr><td><a href="/play?sid='+id1+'">'+id1+'</td><td>'+
                    stat.mtime+'</td></tr>\n');
        }
      }
    }
    res.write('</table><p><form method=POST action="/rec"><input type=submit value="Record"></form>\n');
    res.end('</body></html>\n');

  } else if (req.method == 'GET' && u.pathname == '/play') {
    console.log('play:', sid);
    res.writeHead(200, {'Content-Type': 'text/html'});
    const rec = fs.readFile('player.html', (err, data) => {
      if (err) throw err;
      res.end(data);
    });

  } else if (req.method == 'GET' && u.pathname == '/text') {
    console.log('GET text:', sid);
    res.writeHead(200, {'Content-Type': 'text/json'});
    const path1 = path.join(DATADIR, 'data_'+sid+'.json');
    fs.readFile(path1, (err, data) => {
      if (err) throw err;
      res.end(data);
    });

  } else if (req.method == 'GET' && u.pathname == '/video') {
    console.log('GET video:', sid);
    res.writeHead(200, {'Content-Type': 'video/webm'});
    const path1 = path.join(DATADIR, 'video_'+sid+'.webm');
    let stream = fs.createReadStream(path1);
    stream.on('data', (chunk) => {
      res.write(chunk);
    });
    stream.on('end', () => {
      res.end('');
    });

  } else if (req.method == 'POST' && u.pathname == '/rec') {
    console.log('rec');
    res.writeHead(200, {'Content-Type': 'text/html'});
    const rec = fs.readFile('recorder.html', (err, data) => {
      if (err) throw err;
      res.end(data);
    });

  } else if (req.method == 'POST' && u.pathname == '/start') {
    let session = new Session(sid);
    if (session) {
      sessions[sid] = session;
    }

  } else if (req.method == 'POST' && u.pathname == '/stop') {
    let session = sessions[sid];
    if (session) {
      session.close();
      delete sessions[sid];
    }

  } else if (req.method == 'POST' && u.pathname == '/video') {
    //console.log('video:'+req.url, req.headers);
    let session = sessions[sid];
    const t = parseFloat(u.query.t);
    if (session) {
      session.recvVideo(req, res, t);
    }

  } else if (req.method == 'POST' && u.pathname == '/text') {
    //console.log('text:'+req.url, req.headers);
    let session = sessions[sid];
    const t = parseFloat(u.query.t);
    if (session) {
      session.recvText(req, res, t);
    }

  } else if (req.method == 'POST' && u.pathname == '/run') {
    let session = sessions[sid];
    const t = parseFloat(u.query.t);
    if (session) {
      session.recvOutput(req, res, t);
    }

  } else {
    res.writeHead(404, {'Content-Type': 'text/html'});
    res.end('<html><body>not found</body></html>');
  }
}

http.createServer(serve).listen(PORT);
setInterval(idle, INTERVAL);
console.log('Listening at '+PORT+'...');
