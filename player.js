var baseTime = 0;
var video = null;

function S(t) {
  video.currentTime = t - baseTime;
  video.play();
}

function run() {
  var src = document.getElementById('src');
  baseTime = parseFloat(src.getAttribute('t'));
  for (var a of src.getElementsByTagName('a')) {
    var ts = a.getAttribute('t').split(/,/);
    var t0 = ts[0];
    a.setAttribute('href', 'javascript:S('+t0+')');
  }
  video = document.getElementById('video');
}
