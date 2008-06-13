/* Copyright (C) 2008 Free Software Foundation, Inc.
   Contributed by Janne Blomqvist

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include "io.h"
#include <string.h>
#include <stdlib.h>


void
fbuf_init (gfc_unit * u, size_t len)
{
  if (len == 0)
    len = 512;			/* Default size.  */

  u->fbuf = get_mem (sizeof (fbuf));
  u->fbuf->buf = get_mem (len);
  u->fbuf->len = len;
  u->fbuf->act = u->fbuf->flushed = u->fbuf->pos = 0;
}


void
fbuf_reset (gfc_unit * u)
{
  u->fbuf->act = u->fbuf->flushed = u->fbuf->pos = 0;
}


void
fbuf_destroy (gfc_unit * u)
{
  if (u->fbuf == NULL)
    return;
  if (u->fbuf->buf)
    free_mem (u->fbuf->buf);
  free_mem (u->fbuf);
}


/* Return a pointer to the current position in the buffer, and increase
   the pointer by len. Makes sure that the buffer is big enough, 
   reallocating if necessary. If the buffer is not big enough, there are
   three cases to consider:
   1. If we haven't flushed anything, realloc
   2. If we have flushed enough that by discarding the flushed bytes
      the request fits into the buffer, do that.
   3. Else allocate a new buffer, memcpy unflushed active bytes from old
      buffer. */

char *
fbuf_alloc (gfc_unit * u, size_t len)
{
  size_t newlen;
  char *dest;
  if (u->fbuf->pos + len > u->fbuf->len)
    {
      if (u->fbuf->flushed == 0)
	{
	  /* Round up to nearest multiple of the current buffer length.  */
	  newlen = ((u->fbuf->pos + len) / u->fbuf->len + 1) * u->fbuf->len;
	  dest = realloc (u->fbuf->buf, newlen);
	  if (dest == NULL)
	    return NULL;
	  u->fbuf->buf = dest;
	  u->fbuf->len = newlen;
	}
      else if (u->fbuf->act - u->fbuf->flushed + len < u->fbuf->len)
	{
	  memmove (u->fbuf->buf, u->fbuf->buf + u->fbuf->flushed,
		   u->fbuf->act - u->fbuf->flushed);
	  u->fbuf->act -= u->fbuf->flushed;
	  u->fbuf->pos -= u->fbuf->flushed;
	  u->fbuf->flushed = 0;
	}
      else
	{
	  /* Most general case, flushed != 0, request doesn't fit.  */
	  newlen = ((u->fbuf->pos - u->fbuf->flushed + len)
		    / u->fbuf->len + 1) * u->fbuf->len;
	  dest = get_mem (newlen);
	  memcpy (dest, u->fbuf->buf + u->fbuf->flushed,
		  u->fbuf->act - u->fbuf->flushed);
	  u->fbuf->act -= u->fbuf->flushed;
	  u->fbuf->pos -= u->fbuf->flushed;
	  u->fbuf->flushed = 0;
	  u->fbuf->buf = dest;
	  u->fbuf->len = newlen;
	}
    }

  dest = u->fbuf->buf + u->fbuf->pos;
  u->fbuf->pos += len;
  if (u->fbuf->pos > u->fbuf->act)
    u->fbuf->act = u->fbuf->pos;
  return dest;
}




int
fbuf_flush (gfc_unit * u, int record_done)
{
  int status;
  size_t nbytes;

  if (!u->fbuf)
    return 0;
  if (u->fbuf->act - u->fbuf->flushed != 0)
    {
      if (record_done)
        nbytes = u->fbuf->act - u->fbuf->flushed;
      else	
        nbytes = u->fbuf->pos - u->fbuf->flushed;	
      status = swrite (u->s, u->fbuf->buf + u->fbuf->flushed, &nbytes);
      u->fbuf->flushed += nbytes;
    }
  else
    status = 0;
  if (record_done)
    fbuf_reset (u);
  return status;
}


int
fbuf_seek (gfc_unit * u, gfc_offset off)
{
  gfc_offset pos = u->fbuf->pos + off;
  /* Moving to the left past the flushed marked would imply moving past
     the left tab limit, which is never allowed. So return error if
     that is attempted.  */
  if (pos < (gfc_offset) u->fbuf->flushed)
    return -1;
  u->fbuf->pos = pos;
  return 0;
}
