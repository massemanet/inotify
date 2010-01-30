erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_inotify)/src
erlincdir = $(ERLANG_INSTALL_LIB_DIR_inotify)/src
erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_inotify)/ebin

erlsrc_DATA = \
	iwatch.erl inotify.erl inoteefy.erl

erlbeam_DATA = $(patsubst %.erl, %.beam, $(erlsrc_DATA))

EXTRA_DIST = $(erlsrc_DATA) $(erlinc_DATA)
CLEANFILES = $(erlbeam_DATA)

all: $(erlbeam_DATA)

%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) $<
