CC=clang

all: p04 p04-slow

p04: md5test.c
	$(CC) $(CFLAGS) -D_DISPLAY_PROGRESS md5test.c md5-fast-64.S -o p04

p04-slow: md5test.c
	$(CC) $(CFLAGS) -D_SLOW_STRING_INCREMENT -D_DISPLAY_PROGRESS md5test.c md5-fast-64.S -o p04-slow

clean:
	rm -f p04 p04-slow

