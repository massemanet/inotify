#ifndef ERL_COMM_H
#define ERL_COMM_H

int read_cmd(char *buf, int *size, int *curpos);
int read_exact(char *buf, int len);
int write_cmd(ei_x_buff *buff);
int write_exact(char *buf, int len);

#endif
