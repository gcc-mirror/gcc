/* Copyright (C) 1993, 1997, 1998 Free Software Foundation, Inc.
   This file is part of the GNU IO Library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#include "libioP.h"
#include <string.h>

#if defined(_LIBC) || !_G_HAVE_IO_GETLINE_INFO

_IO_size_t
_IO_getline (fp, buf, n, delim, extract_delim)
     _IO_FILE *fp;
     char *buf;
     _IO_size_t n;
     int delim;
     int extract_delim;
{
  return _IO_getline_info (fp, buf, n, delim, extract_delim, (int *) 0);
}

/* Algorithm based on that used by Berkeley pre-4.4 fgets implementation.

   Read chars into buf (of size n), until delim is seen.
   Return number of chars read (at most n).
   Does not put a terminating '\0' in buf.
   If extract_delim < 0, leave delimiter unread.
   If extract_delim > 0, insert delim in output. */

_IO_size_t
_IO_getline_info (fp, buf, n, delim, extract_delim, eof)
     _IO_FILE *fp;
     char *buf;
     _IO_size_t n;
     int delim;
     int extract_delim;
     int *eof;
{
  char *ptr = buf;
  if (eof) *eof = 0;
  while (n != 0)
    {
      _IO_ssize_t len = fp->_IO_read_end - fp->_IO_read_ptr;
      if (len <= 0)
	{
	  int c = __uflow (fp);
	  if (c == EOF)
	    {
	      if (eof) *eof = c;
	      break;
	    }
	  if (c == delim)
	    {
	      if (extract_delim > 0)
		*ptr++ = c;
	      else if (extract_delim < 0)
		_IO_sputbackc (fp, c);
	      return ptr - buf;
	    }
	  *ptr++ = c;
	  n--;
	}
	else
	  {
	    char *t;
	    if ((_IO_size_t) len >= n)
	      len = n;
	    t = (char *) memchr ((void *) fp->_IO_read_ptr, delim, len);
	    if (t != NULL)
	      {
		_IO_size_t old_len = ptr-buf;
		len = t - fp->_IO_read_ptr;
		if (extract_delim >= 0)
		  {
		    ++t;
		    if (extract_delim > 0)
		      ++len;
		  }
		memcpy ((void *) ptr, (void *) fp->_IO_read_ptr, len);
		fp->_IO_read_ptr = t;
		return old_len + len;
	      }
	    memcpy ((void *) ptr, (void *) fp->_IO_read_ptr, len);
	    fp->_IO_read_ptr += len;
	    ptr += len;
	    n -= len;
	  }
    }
  return ptr - buf;
}

#endif /* Defined(_LIBC) || !_G_HAVE_IO_GETLINE_INFO */
