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
#ifdef __STDC__
#include <stdlib.h>
#endif

_IO_FILE *
DEFUN(_IO_fopen, (filename, mode),
      const char *filename AND const char *mode)
{
  struct _IO_FILE_plus *fp =
    (struct _IO_FILE_plus*)malloc(sizeof(struct _IO_FILE_plus));
  if (fp == NULL)
    return NULL;
  _IO_init(&fp->file, 0);
  _IO_JUMPS(&fp->file) = &_IO_file_jumps;
  _IO_file_init(&fp->file);
#if  !_IO_UNIFIED_JUMPTABLES
  fp->vtable = NULL;
#endif
  if (_IO_file_fopen(&fp->file, filename, mode) != NULL)
        return (_IO_FILE*)fp;
  _IO_un_link(&fp->file);
  free (fp);
  return NULL;
}
