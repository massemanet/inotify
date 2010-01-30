#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <ei.h>
#include "erl_comm.h"

/*
 * inputs
 *    buf     a pointer the buffer to place the message in
 *    size    size of buf
 *    curpos  current offset into buf. Should be set to 0 before
 *            initial call
 *
 * returns  < 0 on read() error
 *         == 0 on read() returning 0
 *            1 when there is more to read
 *            2 when the message is complete
 */
int read_cmd(char *buf, int *size, int *curpos)
{
  int len;
  int count;
  int desired;

  if (*curpos < 2) {
    /* read header */
    count = read(0, buf + *curpos, 2 - *curpos);

    if (count <= 0)
      return(count); /* Error or fd is closed */
    
    *curpos += count;
    if (*curpos < 2)
      return(1);
  }
  /* calculate the total message length and
   * the desired amount to read taking into account
   * the ammount already read
   */
  len = (buf[0] << 8) | buf[1];
  desired = len - *curpos + 2;

  /* check buffer size and realloc if necessary */
  if (len > *size) {
    buf = (char *) realloc(buf, len);
    if (buf == NULL)
      return -1;
    *size = len;
  }

  /* read message body */
  count = read(0, buf + *curpos, desired);
  if (count <= 0)
    return(0);

  *curpos += count;
  return(2);
}

int write_cmd(ei_x_buff *buff) {
  char li;

  li = (buff->index >> 8) & 0xff; 
  write_exact(&li, 1);
  li = buff->index & 0xff;
  write_exact(&li, 1);

  return write_exact(buff->buff, buff->index);
}

int write_exact(char *buf, int len) {
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return(i);
    wrote += i;
  } while (wrote<len);
 
  return(len);
}
