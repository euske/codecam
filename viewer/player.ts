//  Player.ts
//


//  Caption
//
class Caption {

    elem: HTMLElement;

    constructor(elem: HTMLElement) {
	this.elem = elem;
    }

    show() {
	this.elem.classList.add('h');
    }
    hide() {
	this.elem.classList.remove('h');
    }
}


//  Trigger
//
class Trigger {

    t: number;
    show: boolean;
    caption: Caption;

    constructor(t: number, show: boolean, caption: Caption) {
	this.t = t;
	this.show = show;
	this.caption = caption;
    }

    toString() {
	return ('<Trigger ('+this.t+'): show='+this.show+', caption='+this.caption+'>');
    }
}


//  Segment
//
class Segment {

    t: number;
    captions: Caption[];

    constructor(t: number, captions: Caption[]) {
	this.t = t;
	this.captions = captions;
    }

    toString() {
	return ('<Segment ('+this.t+'): captions='+this.captions+'>');
    }
}


//  CaptionSet
//
class CaptionSet {

    present: Caption[] = [];

    setCaptions(captions: Caption[]) {
	for (let i = this.present.length-1; 0 <= i; i--) {
	    let caption = this.present[i];
	    if (captions.indexOf(caption) < 0) {
		caption.hide();
		this.present.splice(i, 1);
	    }
	}
	for (let caption of captions) {
	    if (this.present.indexOf(caption) < 0) {
		caption.show();
		this.present.push(caption);
	    }
	}
    }
}


//  Timeline
//
class Timeline {

    triggers: Trigger[] = [];
    segments: Segment[];

    add(t0: number, t1: number, caption: Caption) {
	this.triggers.push(new Trigger(t0, true, caption));
	this.triggers.push(new Trigger(t1, false, caption));
    }

    build() {
	this.triggers.sort((a,b) => { return a.t - b.t; });
	this.segments = [];
	let captions = [];
	this.segments.push(new Segment(-Infinity, []));
	for (let trig of this.triggers) {
	    if (trig.show) {
		captions.push(trig.caption);
	    } else {
		let i = captions.indexOf(trig.caption);
		captions.splice(i, 1);
	    }
	    this.segments.push(new Segment(trig.t, captions.slice()));
	}
	this.segments.push(new Segment(+Infinity, []));
    }

    get(t: number): Caption[] {
	let i0 = 0;
	let i1 = this.segments.length;
	while (i0 < i1) {
	    let i = Math.floor((i0+i1)/2);
	    let seg0 = this.segments[i];
	    let seg1 = this.segments[i+1];
	    if (t < seg0.t) {
		i1 = i;
	    } else if (seg1.t <= t) {
		i0 = i+1;
	    } else {
		// seg0.t <= t && t < seg1.t
		return seg0.captions;
	    }
	}
	return [];
    }
}


const interval = 33;
var video: HTMLVideoElement = null;

// seek
function S(t: number) {
    video.currentTime = t;
    video.play();
}

// run
function run() {
    video = document.getElementById('video') as HTMLVideoElement;
    let src = document.getElementById('src');
    let timeline = new Timeline();
    let elems = src.getElementsByTagName('a');
    for (let i = 0; i < elems.length; i++) {
	let a = elems[i];
	let ts = a.getAttribute('t').split(/,/).map(parseFloat);
	let t0 = Math.min.apply(null, ts);
	let t1 = Math.max.apply(null, ts);
	a.setAttribute('href', 'javascript:S('+t0+')');
	let caption = new Caption(a);
	timeline.add(t0, t1+1.0, caption);
    }
    timeline.build();
    let cset = new CaptionSet();
    window.setInterval(() => {
	let t = video.currentTime;
	cset.setCaptions(timeline.get(t));
    }, interval);
}
