#!/bin/bash
##
##  trimvideo.sh - Makes a sequence of shrinked jpegs from a video.
##
##  Usage:
##    $ trimvideo.sh input.mp4 output/ [trimsize] [videosize]
##

if [[ $# -lt 2 ]]; then
    echo "Usage: $0 input.mp4 output/ [trimsize] [videosize]"; exit 1;
fi

input="$1"
output="$2"
trimsize="${3:-320x240}"
videosize="${4:-1280x720}"
if [[ -e "$output" ]]; then
    echo "Directory exists: $output"; exit 1;
fi

trimwidth="${trimsize%x*}"
trimheight="${trimsize#*x}"
echo "trimsize: $trimwidth x $trimheight"
videowidth="${videosize%x*}"
videoheight="${videosize#*x}"
echo "videosize: $videowidth x $videoheight"
crop="$trimwidth:$trimheight:$((videowidth-trimwidth)):0"
echo "crop: $crop"
mkdir "$output" || exit 1
ffmpeg -i "$input" -filter:v "crop=$crop" -r 1 -f image2 "$output/i%05d.jpg"
