bindir = $(ERLANG_INSTALL_LIB_DIR_inotify)/priv/bin
bin_PROGRAMS = inotify
inotify_SOURCES  = inotify_driver.c erl_comm.c inotify_erlang.c
inotify_INCLUDES = inotify_erlang.h

inotify_CFLAGS = -I$(ERLANG_LIB_DIR_erl_interface)/include
inotify_LDADD  = -L$(ERLANG_LIB_DIR_erl_interface)/lib -lei

inotifydir = $(ERLANG_INSTALL_LIB_DIR_inotify)/priv/src
inotify_DATA = $(inotify_SOURCES) $(inotify_INCLUDES)

