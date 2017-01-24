# Makefile

DEL=del /f
COPY=copy /y
MT=mt -nologo
CC=cl /nologo
RC=rc
LINK=link /nologo

CFLAGS=/MD /O2 /GA /Zi
LDFLAGS=/DEBUG /OPT:REF /OPT:ICF
RCFLAGS=
DEFS_COMMON=/D WIN32 /D UNICODE /D _UNICODE
DEFS_CONSOLE=$(DEFS_COMMON) /D CONSOLE /D _CONSOLE
DEFS_WINDOWS=$(DEFS_COMMON) /D WINDOWS /D _WINDOWS
DEFS=$(DEFS_WINDOWS) /D NDEBUG
#DEFS=$(DEFS_CONSOLE)
LIBS=
INCLUDES=
TARGETS=Kiirogaa.exe Hookey.dll
DESTDIR=%UserProfile%\bin

all: $(TARGETS)

install: clean
	$(MAKE) $(TARGETS) DEFS="$(DEFS_WINDOWS)"
	$(COPY) Kiirogaa.exe $(DESTDIR)
	$(COPY) Hookey.dll $(DESTDIR)

test: $(TARGETS)
	.\Kiirogaa.exe -e

clean:
	-$(DEL) $(TARGETS)
	-$(DEL) *.lib *.exp *.obj *.res *.ilk *.pdb *.manifest

Kiirogaa.exe: Kiirogaa.obj Kiirogaa.res
	$(LINK) $(LDFLAGS) /manifest /out:$@ $** $(LIBS)
	$(MT) -manifest $@.manifest -outputresource:$@;1

Hookey.dll: Hookey.obj
	$(LINK) /DLL $(LDFLAGS) /manifest /out:$@ $** $(LIBS)
	$(MT) -manifest $@.manifest -outputresource:$@;1

Hookey.obj: Hookey.cpp
	$(CC) /LD $(CFLAGS) /Fo$@ /c $** $(DEFS) $(INCLUDES)

Kiirogaa.cpp: Resource.h
Kiirogaa.rc: Resource.h KiirogaaOn.ico KiirogaaOff.ico
Hookey.cpp: 

.cpp.obj:
	$(CC) $(CFLAGS) /Fo$@ /c $< $(DEFS) $(INCLUDES)
.rc.res:
	$(RC) $(RCFLAGS) $<
