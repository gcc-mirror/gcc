/* This is part of libio/iostream, providing -*- C++ -*- input/output.
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

/* Written by Per Bothner (bothner@cygnus.com). */

#ifdef __GNUG__
#pragma implementation
#endif

#define _POSIX_SOURCE
#include "libioP.h"
#include "procbuf.h"

procbuf::procbuf(const char *command, int mode) : filebuf()
{
  _IO_proc_open(this, command, (mode & ios::in) ? "r" : "w");
}

procbuf *procbuf::open(const char *command, int mode)
{
  return (procbuf*)_IO_proc_open(this, command, (mode & ios::in) ? "r" : "w");
}

/* #define USE_SIGMASK */

int procbuf::sys_close()
{
  return _IO_proc_close(this);
}

procbuf::~procbuf()
{
    close();
}
