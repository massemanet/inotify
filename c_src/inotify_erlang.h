
#ifndef NOTE_H
#define NOTE_H
#include <sys/types.h>

struct note_entry_s {
  int fd;
  struct note_entry_s *next;
};

typedef struct note_entry_s note_entry_t;

struct note_s {
  note_entry_t *ent;
};
typedef struct note_s note_t;


note_t *note_init();
int note_setup_select(note_t *note, fd_set *readfds);
void note_destroy(note_t *note);
int note_read(note_t *note, fd_set *readfds);
int note_list(note_t *note, char *buf, int *index);
int note_open(note_t *note, char *buf, int *index);
int note_close(note_t *note, char *buf, int *index);
int note_add(note_t *note, char *buf, int *index);
int note_remove(note_t *note, char *buf, int *index);
#endif
