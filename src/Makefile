.SUFFIXES: .erl .beam .yrl

LIBS= -l ei

CFLAGS= -Wall

.erl.beam:
	erlc -W $<

.c.o:
	gcc -o $<

MODS = inotify

all: ${MODS:%=%.beam} inotify

inotify: inotify_driver.c erl_comm.c inotify_erlang.c
	gcc -o inotify erl_comm.c inotify_driver.c inotify_erlang.c $(LIBS) $(CFLAGS)

test:
	erl -noshell -eval "inotify:test(), erlang:halt()."

clean:
	rm inotify *.beam
