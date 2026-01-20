CC = gcc
CFLAGS = -O0 -g $(shell pkg-config --cflags glfw3)
LDFLAGS = -lGL $(shell pkg-config --libs glfw3)

sources = source/cpu.c source/ppu.c source/gbe.c
objects = $(sources:.c=.o)

all: gbe clean

gbe: $(objects)
	$(CC) $(LDFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(objects)
