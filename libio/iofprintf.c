/* Copyright (C) 1993, 1997 Free Software Foundation, Inc.
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
#include <stdarg.h>
#else
#include <varargs.h>
#endif

int
#ifdef __STDC__
_IO_fprintf (_IO_FILE *fp, const char* format, ...)
#else
_IO_fprintf (fp, format, va_alist)
     _IO_FILE *fp;
     char *format;
     va_dcl
#endif
{
  int ret;
  va_list args;
  CHECK_FILE (fp, -1);
  _IO_va_start (args, format);
  ret = _IO_vfprintf (fp, format, args);
  va_end (args);
  return ret;
}
