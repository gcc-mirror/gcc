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

#define PADSIZE 16
static char const blanks[PADSIZE] =
{' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '};
static char const zeroes[PADSIZE] =
{'0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0'};

_IO_ssize_t
DEFUN(_IO_padn, (fp, pad, count),
      _IO_FILE *fp AND int pad AND _IO_ssize_t count)
{
  char padbuf[PADSIZE];
  const char *padptr;
  register int i;
  _IO_size_t written = 0, w;
  
  if (pad == ' ')
    padptr = blanks;
  else if (pad == '0')
    padptr = zeroes;
  else
    {
      for (i = PADSIZE; --i >= 0; ) padbuf[i] = pad;
      padptr = padbuf;
    }
  for (i = count; i >= PADSIZE; i -= PADSIZE)
    {
      w = _IO_sputn(fp, padptr, PADSIZE);
      written += w;
      if (w != PADSIZE)
	return written;
    }
      
  if (i > 0)
    {
      w = _IO_sputn(fp, padptr, i);
      written += w;
    }
  return written;
}
