#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/select.h>
#include <string.h>
#include <ei.h>
#include "inotify_erlang.h"
#include "erl_comm.h"

#define BUFFER_SIZE 100

int setup_select(note_t *nlist, fd_set *readfds) {
  int topfd, maxfd = 0;

  /* pselect initialisation */
  FD_ZERO(readfds);
  FD_SET(0, readfds);

  topfd = note_setup_select(nlist, readfds);
  if (topfd > maxfd)
    maxfd = topfd;

  return maxfd;
}

int process_command(const char *command, char *buf, int *index, note_t *nlist) {
  ei_x_buff  result;

  if (!strcmp("open", command)) {
    /* function open() */
    note_open(nlist, buf, index);
    return(0);
  }

  if (!strcmp("add", command)) {
    /* function int = add(int fd, char *path, int mask) */
    note_add(nlist, buf, index);
    return(0);
  }
  
  if (!strcmp("remove", command)) {
    /* function remove(fd, wd)  */
    note_remove(nlist, buf, index);
    return(0);
  }

  if (!strcmp("list", command)) {
    /* function list(nlist) */
    note_list(nlist, buf, index);
    return(0);
  }

  if (!strcmp("close", command)) {
    /* function close */
    note_close(nlist, buf, index);
    return(0);
  }

  /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
  if (ei_x_new_with_version(&result) ||
      ei_x_encode_tuple_header(&result, 2) ||
      ei_x_encode_atom(&result, "error") ||
      ei_x_encode_atom(&result, "unsupported_command"))
    return(-1);

  write_cmd(&result);
  ei_x_free(&result);
  return(0);
} 

int main() {
  /* variables for erlang interface */
  int   index, version, arity;
  int   size = BUFFER_SIZE;
  int   cmdpos, result;
  char  inbuf[BUFFER_SIZE];
  char  command[MAXATOMLEN];

  /* varables for pselect */
  int maxfd, retval;
  fd_set readfds;

  /* inotify initialisation */
  note_t *nlist;
  if ((nlist = note_init()) == NULL) {
    perror("note_init()");
    exit(1);
  
  }

  cmdpos = 0;
  maxfd = setup_select(nlist, &readfds);
  
  while ((retval = select(maxfd + 1, &readfds, NULL, NULL, NULL)) >= 0) {
    if FD_ISSET(0, &readfds) {
      memset(inbuf, 0, BUFFER_SIZE);
      index = 0;
      result = read_cmd(inbuf, &size, &cmdpos);

      if (result == 0) {
	exit(1);
      } else if (result < 0) {
	/* exit(1); */
      } else if (result == 1) {
      } else {

	/* must add two(2) to inbuf pointer to skip message length header */
	if (ei_decode_version(inbuf+2, &index, &version) ||
	    ei_decode_tuple_header(inbuf+2, &index, &arity) ||
	    ei_decode_atom(inbuf+2, &index, command)) {
	  exit(4);
	}

	process_command(command, inbuf+2, &index, nlist);

	/* reset position of inbuf */
	cmdpos = 0;
      }
    }
    /* check other file descriptors */
    note_read(nlist, &readfds);

    maxfd = setup_select(nlist, &readfds);
  }
  exit(10);
}

