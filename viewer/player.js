//  Player.ts
//
//  Caption
//
var Caption = /** @class */ (function () {
    function Caption(elem) {
        this.elem = elem;
    }
    Caption.prototype.show = function () {
        this.elem.classList.add('h');
    };
    Caption.prototype.hide = function () {
        this.elem.classList.remove('h');
    };
    return Caption;
}());
//  Trigger
//
var Trigger = /** @class */ (function () {
    function Trigger(t, show, caption) {
        this.t = t;
        this.show = show;
        this.caption = caption;
    }
    Trigger.prototype.toString = function () {
        return ('<Trigger (' + this.t + '): show=' + this.show + ', caption=' + this.caption + '>');
    };
    return Trigger;
}());
//  Segment
//
var Segment = /** @class */ (function () {
    function Segment(t, captions) {
        this.t = t;
        this.captions = captions;
    }
    Segment.prototype.toString = function () {
        return ('<Segment (' + this.t + '): captions=' + this.captions + '>');
    };
    return Segment;
}());
//  CaptionSet
//
var CaptionSet = /** @class */ (function () {
    function CaptionSet() {
        this.present = [];
    }
    CaptionSet.prototype.setCaptions = function (captions) {
        for (var i = this.present.length - 1; 0 <= i; i--) {
            var caption = this.present[i];
            if (captions.indexOf(caption) < 0) {
                caption.hide();
                this.present.splice(i, 1);
            }
        }
        for (var _i = 0, captions_1 = captions; _i < captions_1.length; _i++) {
            var caption = captions_1[_i];
            if (this.present.indexOf(caption) < 0) {
                caption.show();
                this.present.push(caption);
            }
        }
    };
    return CaptionSet;
}());
//  Timeline
//
var Timeline = /** @class */ (function () {
    function Timeline() {
        this.triggers = [];
    }
    Timeline.prototype.add = function (t0, t1, caption) {
        this.triggers.push(new Trigger(t0, true, caption));
        this.triggers.push(new Trigger(t1, false, caption));
    };
    Timeline.prototype.build = function () {
        this.triggers.sort(function (a, b) { return a.t - b.t; });
        this.segments = [];
        var captions = [];
        this.segments.push(new Segment(-Infinity, []));
        for (var _i = 0, _a = this.triggers; _i < _a.length; _i++) {
            var trig = _a[_i];
            if (trig.show) {
                captions.push(trig.caption);
            }
            else {
                var i = captions.indexOf(trig.caption);
                captions.splice(i, 1);
            }
            this.segments.push(new Segment(trig.t, captions.slice()));
        }
        this.segments.push(new Segment(+Infinity, []));
    };
    Timeline.prototype.get = function (t) {
        var i0 = 0;
        var i1 = this.segments.length;
        while (i0 < i1) {
            var i = Math.floor((i0 + i1) / 2);
            var seg0 = this.segments[i];
            var seg1 = this.segments[i + 1];
            if (t < seg0.t) {
                i1 = i;
            }
            else if (seg1.t <= t) {
                i0 = i + 1;
            }
            else {
                // seg0.t <= t && t < seg1.t
                return seg0.captions;
            }
        }
        return [];
    };
    return Timeline;
}());
var interval = 33;
var video = null;
// seek
function S(t) {
    video.currentTime = t;
    video.play();
}
// run
function run() {
    video = document.getElementById('video');
    var src = document.getElementById('src');
    var timeline = new Timeline();
    var elems = src.getElementsByTagName('a');
    for (var i = 0; i < elems.length; i++) {
        var a = elems[i];
        var ts = a.getAttribute('t').split(/,/).map(parseFloat);
        var t0 = Math.min.apply(null, ts);
        var t1 = Math.max.apply(null, ts);
        a.setAttribute('href', 'javascript:S(' + t0 + ')');
        var caption = new Caption(a);
        timeline.add(t0, t1 + 1.0, caption);
    }
    timeline.build();
    var cset = new CaptionSet();
    window.setInterval(function () {
        var t = video.currentTime;
        cset.setCaptions(timeline.get(t));
    }, interval);
}
