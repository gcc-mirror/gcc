/* Copyright (C) 2009-2013 Free Software Foundation, Inc.
   Contributed by Janne Blomqvist

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef GFOR_FBUF_H
#define GFOR_FBUF_H

#include "io.h"


/* Formatting buffer. This is a temporary scratch buffer used by
   formatted read and writes.  After every formatted I/O statement,
   this buffer is flushed. This buffer is needed since not all devices
   are seekable, and T or TL edit descriptors require moving backwards
   in the record.  However, advance='no' complicates the situation, so
   the buffer must only be partially flushed from the end of the last
   flush until the current position in the record. */

struct fbuf
{
  char *buf;			/* Start of buffer.  */
  int len;			/* Length of buffer.  */
  int act;			/* Active bytes in buffer.  */
  int pos;			/* Current position in buffer.  */
};

extern void fbuf_init (gfc_unit *, int);
internal_proto(fbuf_init);

extern void fbuf_destroy (gfc_unit *);
internal_proto(fbuf_destroy);

extern int fbuf_reset (gfc_unit *);
internal_proto(fbuf_reset);

extern char * fbuf_alloc (gfc_unit *, int);
internal_proto(fbuf_alloc);

extern int fbuf_flush (gfc_unit *, unit_mode);
internal_proto(fbuf_flush);

extern int fbuf_seek (gfc_unit *, int, int);
internal_proto(fbuf_seek);

extern char * fbuf_read (gfc_unit *, int *);
internal_proto(fbuf_read);

/* Never call this function, only use fbuf_getc().  */
extern int fbuf_getc_refill (gfc_unit *);
internal_proto(fbuf_getc_refill);

static inline int
fbuf_getc (gfc_unit * u)
{
  if (u->fbuf->pos < u->fbuf->act)
    return (unsigned char) u->fbuf->buf[u->fbuf->pos++];
  return fbuf_getc_refill (u);
}

static inline char *
fbuf_getptr (gfc_unit * u)
{
  return (char*) (u->fbuf->buf + u->fbuf->pos);
}

#endif
