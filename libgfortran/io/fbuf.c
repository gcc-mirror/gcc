/* Copyright (C) 2008-2017 Free Software Foundation, Inc.
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


#include "io.h"
#include "fbuf.h"
#include "unix.h"
#include <string.h>


//#define FBUF_DEBUG


void
fbuf_init (gfc_unit *u, int len)
{
  if (len == 0)
    len = 512;			/* Default size.  */

  u->fbuf = xmalloc (sizeof (struct fbuf));
  u->fbuf->buf = xmalloc (len);
  u->fbuf->len = len;
  u->fbuf->act = u->fbuf->pos = 0;
}


void
fbuf_destroy (gfc_unit *u)
{
  if (u->fbuf == NULL)
    return;
  free (u->fbuf->buf);
  free (u->fbuf);
  u->fbuf = NULL;
}


static void
#ifdef FBUF_DEBUG
fbuf_debug (gfc_unit *u, const char *format, ...)
{
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fprintf (stderr, "fbuf_debug pos: %d, act: %d, buf: ''", 
           u->fbuf->pos, u->fbuf->act);
  for (int ii = 0; ii < u->fbuf->act; ii++)
    {
      putc (u->fbuf->buf[ii], stderr);
    }
  fprintf (stderr, "''\n");
}
#else
fbuf_debug (gfc_unit *u __attribute__ ((unused)),
            const char *format __attribute__ ((unused)),
            ...) {}
#endif

  

/* You should probably call this before doing a physical seek on the
   underlying device.  Returns how much the physical position was
   modified.  */

int
fbuf_reset (gfc_unit *u)
{
  int seekval = 0;

  if (!u->fbuf)
    return 0;

  fbuf_debug (u, "fbuf_reset: ");
  fbuf_flush (u, u->mode);
  /* If we read past the current position, seek the underlying device
     back.  */
  if (u->mode == READING && u->fbuf->act > u->fbuf->pos)
    {
      seekval = - (u->fbuf->act - u->fbuf->pos);
      fbuf_debug (u, "fbuf_reset seekval %d, ", seekval);
    }
  u->fbuf->act = u->fbuf->pos = 0;
  return seekval;
}


/* Return a pointer to the current position in the buffer, and increase
   the pointer by len. Makes sure that the buffer is big enough, 
   reallocating if necessary.  */

char *
fbuf_alloc (gfc_unit *u, int len)
{
  int newlen;
  char *dest;
  fbuf_debug (u, "fbuf_alloc len %d, ", len);
  if (u->fbuf->pos + len > u->fbuf->len)
    {
      /* Round up to nearest multiple of the current buffer length.  */
      newlen = ((u->fbuf->pos + len) / u->fbuf->len + 1) *u->fbuf->len;
      u->fbuf->buf = xrealloc (u->fbuf->buf, newlen);
      u->fbuf->len = newlen;
    }

  dest = u->fbuf->buf + u->fbuf->pos;
  u->fbuf->pos += len;
  if (u->fbuf->pos > u->fbuf->act)
    u->fbuf->act = u->fbuf->pos;
  return dest;
}


/* mode argument is WRITING for write mode and READING for read
   mode. Return value is 0 for success, -1 on failure.  */

int
fbuf_flush (gfc_unit *u, unit_mode mode)
{
  int nwritten;

  if (!u->fbuf)
    return 0;

  fbuf_debug (u, "fbuf_flush with mode %d: ", mode);

  if (mode == WRITING)
    {
      if (u->fbuf->pos > 0)
	{
	  nwritten = swrite (u->s, u->fbuf->buf, u->fbuf->pos);
	  if (nwritten < 0)
	    return -1;
	}
    }
  /* Salvage remaining bytes for both reading and writing. This
     happens with the combination of advance='no' and T edit
     descriptors leaving the final position somewhere not at the end
     of the record. For reading, this also happens if we sread() past
     the record boundary.  */ 
  if (u->fbuf->act > u->fbuf->pos && u->fbuf->pos > 0)
    memmove (u->fbuf->buf, u->fbuf->buf + u->fbuf->pos, 
             u->fbuf->act - u->fbuf->pos);

  u->fbuf->act -= u->fbuf->pos;
  u->fbuf->pos = 0;

  return 0;
}


/* The mode argument is LIST_WRITING for write mode and LIST_READING for
   read.  This should only be used for list directed  I/O.
   Return value is 0 for success, -1 on failure.  */

int
fbuf_flush_list (gfc_unit *u, unit_mode mode)
{
  int nwritten;

  if (!u->fbuf)
    return 0;

  if (u->fbuf->pos < 524288) /* Upper limit for list writing.  */
    return 0;

  fbuf_debug (u, "fbuf_flush_list with mode %d: ", mode);

  if (mode == LIST_WRITING)
    {
      nwritten = swrite (u->s, u->fbuf->buf, u->fbuf->pos);
      if (nwritten < 0)
	return -1;
    }

  /* Salvage remaining bytes for both reading and writing.  */ 
  if (u->fbuf->act > u->fbuf->pos)
    memmove (u->fbuf->buf, u->fbuf->buf + u->fbuf->pos, 
             u->fbuf->act - u->fbuf->pos);

  u->fbuf->act -= u->fbuf->pos;
  u->fbuf->pos = 0;

  return 0;
}


int
fbuf_seek (gfc_unit *u, int off, int whence)
{
  if (!u->fbuf)
    return -1;

  switch (whence)
    {
    case SEEK_SET:
      break;
    case SEEK_CUR:
      off += u->fbuf->pos;
      break;
    case SEEK_END:
      off += u->fbuf->act;
      break;
    default:
      return -1;
    }

  fbuf_debug (u, "fbuf_seek, off %d ", off);
  /* The start of the buffer is always equal to the left tab
     limit. Moving to the left past the buffer is illegal in C and
     would also imply moving past the left tab limit, which is never
     allowed in Fortran. Similarly, seeking past the end of the buffer
     is not possible, in that case the user must make sure to allocate
     space with fbuf_alloc().  So return error if that is
     attempted.  */
  if (off < 0 || off > u->fbuf->act)
    return -1;
  u->fbuf->pos = off;
  return off;
}


/* Fill the buffer with bytes for reading.  Returns a pointer to start
   reading from. If we hit EOF, returns a short read count. If any
   other error occurs, return NULL.  After reading, the caller is
   expected to call fbuf_seek to update the position with the number
   of bytes actually processed. */

char *
fbuf_read (gfc_unit *u, int *len)
{
  char *ptr;
  int oldact, oldpos;
  int readlen = 0;

  fbuf_debug (u, "fbuf_read, len %d: ", *len);
  oldact = u->fbuf->act;
  oldpos = u->fbuf->pos;
  ptr = fbuf_alloc (u, *len);
  u->fbuf->pos = oldpos;
  if (oldpos + *len > oldact)
    {
      fbuf_debug (u, "reading %d bytes starting at %d ", 
                  oldpos + *len - oldact, oldact);
      readlen = sread (u->s, u->fbuf->buf + oldact, oldpos + *len - oldact);
      if (readlen < 0)
	return NULL;
      *len = oldact - oldpos + readlen;
    }
  u->fbuf->act = oldact + readlen;
  fbuf_debug (u, "fbuf_read done: ");
  return ptr;
}


/* When the fbuf_getc() inline function runs out of buffer space, it
   calls this function to fill the buffer with bytes for
   reading. Never call this function directly.  */

int
fbuf_getc_refill (gfc_unit *u)
{
  int nread;
  char *p;

  fbuf_debug (u, "fbuf_getc_refill ");

  /* Read 80 bytes (average line length?).  This is a compromise
     between not needing to call the read() syscall all the time and
     not having to memmove unnecessary stuff when switching to the
     next record.  */
  nread = 80;

  p = fbuf_read (u, &nread);

  if (p && nread > 0)
    return (unsigned char) u->fbuf->buf[u->fbuf->pos++];
  else
    return EOF;
}
