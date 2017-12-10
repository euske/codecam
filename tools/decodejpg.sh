#!/bin/sh
input="$1"
crop=320:180:960:0
ffmpeg -i "$input" -filter:v "crop=$crop" -r 1 -f image2 images/i%05d.jpg
