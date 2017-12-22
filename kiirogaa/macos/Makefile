# Makefile
RM=rm -f
CC=gcc
SUDO=sudo
CFLAGS=-framework ApplicationServices -framework Carbon

SRCS=kiirogaa.c
TARGET=./kiirogaa

all: $(TARGET)

clean:
	-$(RM) $(TARGET)

test: $(TARGET)
	$(SUDO) $(TARGET)

$(TARGET): $(SRCS)
	$(CC) -o $(TARGET) $(SRCS) $(CFLAGS)
