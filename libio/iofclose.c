/* Copyright (C) 1993, 1995, 1997 Free Software Foundation, Inc.
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
#ifdef __STDC__
#include <stdlib.h>
#endif

int
_IO_fclose (fp)
     _IO_FILE *fp;
{
  int status;

  CHECK_FILE(fp, EOF);

  _IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile, fp);
  _IO_flockfile (fp);
  if (fp->_IO_file_flags & _IO_IS_FILEBUF)
    status = _IO_file_close_it (fp);
  else
    status = fp->_flags & _IO_ERR_SEEN ? -1 : 0;
  _IO_FINISH (fp);
  _IO_cleanup_region_end (1);
  if (fp != _IO_stdin && fp != _IO_stdout && fp != _IO_stderr)
    {
      fp->_IO_file_flags = 0;
      free(fp);
    }

  return status;
}

#ifdef weak_alias
weak_alias (_IO_fclose, fclose)
#elif defined(_G_STDIO_USES_LIBIO) && defined(_G_HAVE_WEAK_SYMBOL)
int fclose (_IO_FILE *) __attribute__ ((weak, alias("_IO_fclose")));
#endif
