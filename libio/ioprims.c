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

/* I/O OS-level primitives.
   Needs to be replaced if not using Unix.
   Also needs to be replaced if avoiding name-space pollution
   (in which case read would be defined in terms of _IO_read,
   rather than vice versa). */

#include "libioP.h"
#include <sys/types.h>
#include <sys/stat.h>

#ifdef TODO
/* Add open, isatty */
#endif

_IO_ssize_t
DEFUN(_IO_read, (fildes, buf, nbyte),
      int fildes AND void *buf AND _IO_size_t nbyte)
{
  return read (fildes, buf, nbyte);
}

_IO_ssize_t
DEFUN(_IO_write, (fildes, buf, nbyte),
      int fildes AND const void *buf AND _IO_size_t nbyte)
{
  return write (fildes, buf, nbyte);
}

_IO_off_t
DEFUN(_IO_lseek, (fildes, offset, whence),
      int fildes AND _IO_off_t offset AND int whence)
{
  return lseek (fildes, offset, whence);
}

int
DEFUN(_IO_close, (fildes),
      int fildes)
{
  return close (fildes);
}

int
DEFUN(_IO_fstat, (fildes, buf),
      int fildes AND struct stat *buf)
{
  return fstat (fildes, buf);
}
