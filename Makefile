cc = gcc
flags = -O0 -g -std=c99 -lm

sources = source/gbe.c
objects = $(sources:.c=.o)

all: gbe clean

gbe: $(objects)
	$(cc) $(flags) $^ -o $@

%.o: %.c
	$(cc) $(flags) -c $< -o $@

clean:
	rm -f $
