#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/inotify.h>
#include <arpa/inet.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <ei.h>
#include "inotify_erlang.h"
#include "erl_comm.h"

#define EVENT_SIZE (sizeof (struct inotify_event))
#define BUF_LEN (1024 * (EVENT_SIZE + 16))
#define MSG_LEN (EVENT_SIZE + 16)

/*********************** note_send_errno **********/
int note_send_errno() {
  ei_x_buff  errmsg;

  if (ei_x_new_with_version(&errmsg) ||
      ei_x_encode_tuple_header(&errmsg, 2))
    return(-1);
  ei_x_encode_atom(&errmsg, "error");   /* element 1 */
  switch (errno) {
  case EACCES:
    ei_x_encode_atom(&errmsg, "eacces");
    break;
  case EBADF:
    ei_x_encode_atom(&errmsg, "ebadf");
    break;
  case EFAULT:
    ei_x_encode_atom(&errmsg, "efault");
    break;
  case EINVAL:
    ei_x_encode_atom(&errmsg, "einval");
    break;
  case ENOMEM:
    ei_x_encode_atom(&errmsg, "enomem");
    break;
  case ENOSPC:
    ei_x_encode_atom(&errmsg, "enospc");
    break;
  case ENOTTY:
    ei_x_encode_atom(&errmsg, "enotty");
    break;
  default:
    ei_x_encode_atom(&errmsg, "unknown_errno");
    break;
  }	  
  write_cmd(&errmsg);
  ei_x_free(&errmsg);

  return 0;
}

/*********************** note_init ****************/
note_t *note_init() {
  note_t *note;

  if ((note = calloc(1, sizeof(note_t))) == NULL) {
    return(NULL);
  }

  return(note);
}

/*********************** note_encode_mask ***********/
void note_encode_mask(ei_x_buff *outp, ulong mask) {
  /* debugging: fprintf(stderr, "note_encode_mask %ld\r\n", mask); */
  if (mask & IN_ACCESS) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "access");
  }
  if (mask & IN_ATTRIB) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "attrib");
  }
  if (mask & IN_CLOSE_WRITE) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "close_write");
  }
  if (mask & IN_CLOSE_NOWRITE) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "close_nowrite");
  }
  if (mask & IN_CREATE) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "create");
  }
  if (mask & IN_DELETE) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "delete");
  }
  if (mask & IN_DELETE_SELF) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "delete_self");
  }
  if (mask & IN_MODIFY) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "modify");
  }
  if (mask & IN_MOVE_SELF) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "move_self");
  }
  if (mask & IN_MOVED_FROM) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "move_from");
  }
  if (mask & IN_MOVED_TO) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "move_to");
  }
  if (mask & IN_OPEN) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "open");
  }
  if (mask & IN_IGNORED) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "ignored");
  }
  if (mask & IN_ISDIR) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "isdir");
  }
  if (mask & IN_Q_OVERFLOW) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "q_overflow");
  }
  if (mask & IN_UNMOUNT) {
    ei_x_encode_list_header(outp,1);
    ei_x_encode_atom(outp, "unmount");
  }
    
  ei_x_encode_empty_list(outp);
};

int note_decode_mask_atom(const char *atom, ulong *maskout) {
  *maskout = 0;
  if (!strcmp("all", atom)) {
    *maskout |= IN_ALL_EVENTS;
  } else if (!strcmp("access", atom)) {
    *maskout |= IN_ACCESS;
  } else if (!strcmp("attrib", atom)) {
    *maskout |= IN_ATTRIB;
  } else if (!strcmp("close_write", atom)) {
    *maskout |= IN_CLOSE_WRITE;
  } else if (!strcmp("close_nowrite", atom)) {
    *maskout |= IN_CLOSE_NOWRITE;
  } else if (!strcmp("close", atom)) {
    *maskout |= IN_CLOSE;
  } else if (!strcmp("create", atom)) {
    *maskout |= IN_CREATE;
  } else if (!strcmp("delete", atom)) {
    *maskout |= IN_DELETE;
  } else if (!strcmp("delete_self", atom)) {
    *maskout |= IN_DELETE_SELF;
  } else if (!strcmp("modify", atom)) {
    *maskout |= IN_MODIFY;
  } else if (!strcmp("move_self", atom)) {
    *maskout |= IN_MOVE_SELF;
  } else if (!strcmp("moved_from", atom)) {
    *maskout |= IN_MOVED_FROM;
  } else if (!strcmp("moved_to", atom)) {
    *maskout |= IN_MOVED_TO;
  } else if (!strcmp("move", atom)) {
    *maskout |= IN_MOVE;
  } else if (!strcmp("open", atom)) {
    *maskout |= IN_OPEN;
  } else if (!strcmp("dont_follow", atom)) {
    *maskout |= IN_DONT_FOLLOW;
  } else if (!strcmp("mask_add", atom)) {
    *maskout |= IN_MASK_ADD;
  } else if (!strcmp("onlydir", atom)) {
    *maskout |= IN_ONLYDIR;
  } else {
    return(-1);
  }
  return(0);
}
/*********************** note_decode_mask ***********/
int note_decode_mask(char *buf, int *index, ulong *maskout) {
  int arity, count;
  int termtype, size;
  char mstr[100];

  ei_get_type(buf, index, &termtype, &size);
  if (termtype == ERL_ATOM_EXT) {
    if (ei_decode_atom(buf, index, mstr) < 0)
	return(-1);
    
    if (note_decode_mask_atom(mstr, maskout) < 0) 
      return(-1);
  } else if (termtype == ERL_LIST_EXT) {
    if (ei_decode_list_header(buf, index, &arity) < 0)
      return(-1);
    for(count = arity; count <= arity; count++) {

      if (note_decode_mask_atom(mstr, maskout) < 0)
	return(-1);
    }
  } else {
    /* another type which is not of interest */
    return(-1);
  }
  return(0);
}
/*********************** note_setup_select ***********/
int note_setup_select(note_t *note, fd_set *readfds) {
  note_entry_t *cur;
  int maxfd = 0;

  for(cur = note->ent; cur != NULL; cur = cur->next) {
    FD_SET(cur->fd, readfds);
    if (cur->fd > maxfd)
      maxfd = cur->fd;
  }
  return maxfd;
}

/*********************** note_destroy ****************/
void note_destroy(note_t *note) {
  note_entry_t *cur, *prev;

  cur = note->ent;
  while (cur != NULL) {
    close(cur->fd);
    prev = cur;
    cur = cur->next;
    free(prev);
  }
  free(note);
}

/*********************** note_read ****************
 *
 *
 */
int note_read_send(int len, char *buf) {
  int idx = 0;
  ei_x_buff result;

  while (idx < len) {
    struct inotify_event *event;
    event = (struct inotify_event *) &buf[idx];
    
    /* encoding msg */
    /* Prepare the output buffer that will hold {event, Wd, Mask, Cookie, Name} */
    if (ei_x_new_with_version(&result) ||
	ei_x_encode_tuple_header(&result, 5)) return(-1);
    
    fprintf(stderr, "inotify_erlang:note_read  len: %d idx: %d event %d %x %x %s\r\n",
	    len, idx, event->wd, event->mask, event->cookie, event->name);
    ei_x_encode_atom(&result, "event");                         /* element 1 */
    ei_x_encode_ulong(&result, event->wd);                      /* element 2 */
    note_encode_mask(&result, event->mask);                     /* element 3 */
    ei_x_encode_ulong(&result, event->cookie);                  /* element 4 */
    ei_x_encode_string(&result, event->name);                   /* element 5 */
    
    write_cmd(&result);
    ei_x_free(&result);
    idx += EVENT_SIZE + event->len;
  }
  return 0;
}

/*********************** note_read ****************
 *  sends messages of format
 *     EVENT_MSG, size, event
 *
 */
int note_read(note_t *note, fd_set *readfds) {
  char  buf[BUF_LEN];
  int len, rc, fd = 0;
  note_entry_t *cur;

  for(cur = note->ent; cur != NULL; cur = cur->next) {
    if (FD_ISSET(cur->fd, readfds)) {
      fd = cur->fd;

      memset(buf, 0, BUF_LEN);
      rc = ioctl(fd, FIONREAD, &len );

      if (rc != 0)
	/* send error code to erlang */
	note_send_errno();

      if (len <= 0)
	continue;

      /* FIXME: potential buffer over flow bug here */
      read(fd, buf, len);

      if (len < 0) {
	 perror("note_read read()");
      } else if (len > 0) {
	if (note_read_send(len, buf) < 0)
	  return(-1);
      }
    }
  }
  return(0);
}

/*********************** note_list ****************/
int note_list(note_t *note, char *buf, int *index) {
  note_entry_t *cur;
  ei_x_buff result;

  /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
  if (ei_x_new_with_version(&result) ||
      ei_x_encode_tuple_header(&result, 2)) return(-1);
  ei_x_encode_atom(&result, "ok");

  for (cur = note->ent; cur != NULL; cur = cur->next) {
    ei_x_encode_list_header(&result,1);
    ei_x_encode_ulong(&result, cur->fd);
  }  ei_x_encode_empty_list(&result);
  write_cmd(&result);
  ei_x_free(&result);
  return(0);
}

/*********************** note_open ****************
 * buffer format is
 *    {}
 *
 * returns
 *    {ok, Fd}
 *    {error, Errno}
 */
int note_open(note_t *note, char *buf, int *count) {
  uint32_t fd;
  note_entry_t *newent;
  ei_x_buff result;

  /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
  if (ei_x_new_with_version(&result) ||
      ei_x_encode_tuple_header(&result, 2)) return(-1);

  if ((newent = (note_entry_t *)calloc(1,sizeof(note_entry_t))) == NULL) {
    if (ei_x_encode_atom(&result, "error") ||
	ei_x_encode_atom(&result, "alloc_failed"))
      return(-1);
    write_cmd(&result);
    ei_x_free(&result);
    return(0);
  }

  if ((fd = inotify_init()) < 0) {
    free(newent);
    if (ei_x_encode_atom(&result, "error") ||
	ei_x_encode_atom(&result, "inotify_init"))
      return(-1);
    write_cmd(&result);
    ei_x_free(&result);
    return(0);
  }

  newent->fd = fd;
  newent->next = note->ent;
  note->ent = newent;


  if (ei_x_encode_atom(&result, "ok") ||
      ei_x_encode_ulong(&result, fd)) return(-1);

  write_cmd(&result);
  ei_x_free(&result);
  return(0);
}
/*********************** note_close ****************/
int note_close(note_t *note, char *buf, int *count) {
  note_entry_t **curpp, *nextp;
  ulong fd;
  ei_x_buff result;

  if (ei_decode_ulong(buf, count, &fd)) return(-1);

  for(curpp = &(note->ent); *curpp != NULL; curpp = &(*curpp)->next) {
    if (fd == (*curpp)->fd) {
      close(fd);
      nextp = (*curpp)->next;
      free(*curpp);
      (*curpp) = nextp;
      /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
      if (ei_x_new_with_version(&result) ||
	  ei_x_encode_tuple_header(&result, 2)) return(-1);
      if (ei_x_encode_atom(&result, "ok") ||
	  ei_x_encode_ulong(&result, fd)) return(-1);

      write_cmd(&result);
      ei_x_free(&result);
      return(0);
    }
  }

  if (ei_x_new_with_version(&result) ||
      ei_x_encode_tuple_header(&result, 2)) return(-1);
  if (ei_x_encode_atom(&result, "error") ||
      ei_x_encode_atom(&result, "unknown")) return(-1);

  write_cmd(&result);
  ei_x_free(&result);
  return(0);
}

/*********************** note_add ****************
 * buffer format is
 *    {ulong Fd, string  Pathname, ulong Mask}
 *
 * returns
 *    {ok, Wd}
 *    {error, Errstr}
 *
 */
int note_add(note_t *note, char *buf, int *index) {
  ulong fd, mask;
  int wd;
  char pathname[512];
  ei_x_buff result;

  /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
  if (ei_x_new_with_version(&result) ||
      ei_x_encode_tuple_header(&result, 2)) return(-1);

  if (ei_decode_ulong(buf, index, &fd) ||
      ei_decode_string(buf, index, pathname)) return(-1);

  mask = 0;
  if (note_decode_mask(buf, index, &mask) < 0) {
    if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "bad_mask"))
      return(-1);
    write_cmd(&result);
    ei_x_free(&result);
    return(0);
  }

  if ((wd = inotify_add_watch(fd, pathname, mask)) < 0) {
    return note_send_errno();
  }

  if (ei_x_encode_atom(&result, "ok") || ei_x_encode_ulong(&result, wd)) return(-1);
  write_cmd(&result);
  ei_x_free(&result);
  return(0);
}

/*********************** note_remove ****************
 * buffer format is
 *    {ulong fd, ulong wd}
 *
 * returns
 *   ok
 *   {error, errno}
 */
int note_remove(note_t *note, char *buf, int *index) {
  ulong fd, wd;
  ei_x_buff result;

  if (ei_decode_ulong(buf, index, &fd) ||
      ei_decode_ulong(buf, index, &wd)) return(-1);


  if (inotify_rm_watch(fd, wd) < 0) {
  /* Prepare the output buffer that will hold {error, Reason} */
    return note_send_errno();
  }

  if (ei_x_new_with_version(&result) ||
      ei_x_encode_atom(&result, "ok")) return(-1);
  write_cmd(&result);
  ei_x_free(&result);
  return(0);
}
