/* 
Copyright (C) 1994 Free Software Foundation

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

#ifdef __STDC__
#include <stdlib.h>
#endif
#include "libioP.h"
#include <string.h>
#include <errno.h>

/* Read up to (and including) a TERMINATOR from FP into *LINEPTR
   (and null-terminate it).  *LINEPTR is a pointer returned from malloc (or
   NULL), pointing to *N characters of space.  It is realloc'ed as
   necessary.  Returns the number of characters read (not including the
   null terminator), or -1 on error or EOF.  */

_IO_ssize_t
DEFUN(_IO_getdelim, (lineptr, n, delimiter, fp),
      char **lineptr AND _IO_size_t *n AND int delimiter AND _IO_FILE *fp)
{
  register _IO_ssize_t cur_len = 0;
  _IO_ssize_t len;

  if (lineptr == NULL || n == NULL)
    {
#ifdef EINVAL
      errno = EINVAL;
#endif
      return -1;
    }
  CHECK_FILE (fp, -1);
  if (_IO_ferror (fp))
    return -1;

  if (*lineptr == NULL || *n == 0)
    {
      *n = 120;
      *lineptr = (char *) malloc (*n);
      if (*lineptr == NULL)
	return -1;
    }

  len = fp->_IO_read_end - fp->_IO_read_ptr;
  if (len <= 0)
    {
      if (__underflow (fp) == EOF)
	return -1;
      len = fp->_IO_read_end - fp->_IO_read_ptr;
    }

  for (;;)
    {
      _IO_size_t needed;
      char *t;
      t = (char *) memchr ((void *) fp->_IO_read_ptr, delimiter, len);
      if (t != NULL)
	len = (t - fp->_IO_read_ptr) + 1;
      /* make enough space for len+1 (for final NUL) bytes. */
      needed = cur_len + len + 1;
      if (needed > *n)
	{
	  if (t == NULL && needed < 2 * *n)
	    needed = 2 * *n;  /* Be generous. */
	  *n = needed;
	  *lineptr = (char *) realloc (*lineptr, needed);
	  if (*lineptr == NULL)
	    return -1;
	}
      memcpy (*lineptr + cur_len, (void *) fp->_IO_read_ptr, len);
      fp->_IO_read_ptr += len;
      cur_len += len;
      if (t != NULL || __underflow (fp) == EOF)
	break;
      len = fp->_IO_read_end - fp->_IO_read_ptr;
    }
  (*lineptr)[cur_len] = '\0';
  return cur_len;
}
