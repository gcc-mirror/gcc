/* 
Copyright (C) 1993 Free Software Foundation

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

#include "libioP.h"
#include <string.h>

/* Algorithm based on that used by Berkeley pre-4.4 fgets implementation.

   Read chars into buf (of size n), until delim is seen.
   Return number of chars read (at most n).
   Does not put a terminating '\0' in buf.
   If extract_delim < 0, leave delimiter unread.
   If extract_delim > 0, insert delim in output. */

_IO_size_t
DEFUN(_IO_getline, (fp, buf, n, delim, extract_delim),
      _IO_FILE *fp AND char* buf AND _IO_size_t n
      AND int delim AND int extract_delim)
{
  register char *ptr = buf;
  do
    {
      _IO_ssize_t len = fp->_IO_read_end - fp->_IO_read_ptr;
      char *t;
      if (len <= 0)
	if (__underflow(fp) == EOF)
	  break;
	else
	  len = fp->_IO_read_end - fp->_IO_read_ptr;
      if (len >= n)
	len = n;
      t = (char*)memchr((void*)fp->_IO_read_ptr, delim, len);
      if (t != NULL)
	{
	  _IO_size_t old_len = ptr-buf;
	  len = t - fp->_IO_read_ptr;
	  if (extract_delim >= 0)
	    {
	      t++;
	      if (extract_delim > 0)
		len++;
	    }
	  memcpy((void*)ptr, (void*)fp->_IO_read_ptr, len);
	  fp->_IO_read_ptr = t;
	  return old_len + len;
	}
      memcpy((void*)ptr, (void*)fp->_IO_read_ptr, len);
      fp->_IO_read_ptr += len;
      ptr += len;
      n -= len;
    } while (n != 0);
  return ptr - buf;
}
