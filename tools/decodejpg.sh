#!/bin/sh
input="$1"
crop=480:270:800:0
ffmpeg -i "$input" -filter:v "crop=$crop" -r 1 -f image2 images/i%05d.jpg
