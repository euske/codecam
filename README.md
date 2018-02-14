# CodeCAM

A set of tools (a.k.a. "framework") to create a video-annotated source code.

## Good
 * Applicable to any language / format.
 * No need to modify the editor.

## Bad
 * Accuracy not 100%.
 * Cannot track what's not typed (e.g. moved, pasted)
 * Matching takes O(n^2).

## How to Use
 1. Obtain a source code, key logging and video.
 1. Run `matcher.py key.log sourcecode.txt > keylog.map`
 1. Run `trimvideo.sh stream.mp4 images/`
 1. Run `getface.py -K api_key images/*.jpg > faces.txt`
 1. Run `viewer.py -s stream.mp4 -f faces.txt keylog.map sourcecode.txt > viewer.html`


## Kiirogaa

### Emacs
 * Vist a file.
 * M-x `kiirogaa-start-logging`
 * M-x `kiirogaa-stop-logging`

### Windows
 * Resides on SysTray.
 * Requires Windows SDK to build.
 * `nmake`

### macOS
 * `make`
 * `sudo kiirogaa output.log`


## Tools

### matcher.py

Discovers mappings between a key log and source code.

*Usage:*
```
$ matcher.py [-d] [-t title] [-m maxdist] [-s minsocre]
  [-n maxiters] [-x maxcluters] key.log [file ...] > out.map
```

### getface.py

Label a sequence of jpeg images with face data.

*Usage:*
```
$ getface.py -K api_key images/*.jpg > faces.txt
```
cf. https://azure.microsoft.com/en-us/services/cognitive-services/emotion/

### trimvideo.sh

Makes a sequence of shrinked jpegs from a video.

*Usage:*
```
$ trimvideo.sh input.mp4 output/ [trimsize] [videosize]
```

### evalprec.py

Compares a estimated mapping with a true mapping (for debugging).

*Usage:*
```
$ evalprec.py reference.txt output.txt
```

### vierer.py

Generates a final video player w/ source code in HTML.

*Usage:*
```
$ viewer.py [-s stream.mp4] [-f faces.txt] out.map [file ...]
```
