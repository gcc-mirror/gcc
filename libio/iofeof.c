/* Copyright (C) 1993, 1996, 1997 Free Software Foundation, Inc.
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
#include "stdio.h"

int
_IO_feof (fp)
     _IO_FILE* fp;
{
  int result;
  CHECK_FILE (fp, EOF);
  _IO_flockfile (fp);
  result = _IO_feof_unlocked (fp);
  _IO_funlockfile (fp);
  return result;
}

#ifdef weak_alias
weak_alias (_IO_feof, feof)
#elif defined(_G_STDIO_USES_LIBIO) && defined(_G_HAVE_WEAK_SYMBOL)
int feof (_IO_FILE *) __attribute__ ((weak, alias("_IO_feof")));
#endif
