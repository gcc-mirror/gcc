/* Copyright (C) 1993, 1994, 1996, 1997 Free Software Foundation, Inc.
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


/* This file provides definitions of _IO_stdin, _IO_stdout, and _IO_stderr
   for C code.  Compare stdstreams.cc.
   (The difference is that here the vtable field is set to 0,
   so the objects defined are not valid C++ objects.  On the other
   hand, we don't need a C++ compiler to build this file.) */

#include "libioP.h"

#ifdef _IO_MTSAFE_IO
#define DEF_STDFILE(NAME, FD, CHAIN, FLAGS) \
  static _IO_lock_t _IO_stdfile_##FD##_lock = _IO_lock_initializer; \
  struct _IO_FILE_plus NAME \
    = {FILEBUF_LITERAL(CHAIN, FLAGS, FD), &_IO_file_jumps}
#else
#define DEF_STDFILE(NAME, FD, CHAIN, FLAGS) \
  struct _IO_FILE_plus NAME \
    = {FILEBUF_LITERAL(CHAIN, FLAGS, FD), &_IO_file_jumps}
#endif

DEF_STDFILE(_IO_stdin_, 0, 0, _IO_NO_WRITES);
DEF_STDFILE(_IO_stdout_, 1, &_IO_stdin_.file, _IO_NO_READS);
DEF_STDFILE(_IO_stderr_, 2, &_IO_stdout_.file,
            _IO_NO_READS+_IO_UNBUFFERED);

_IO_FILE *_IO_list_all = &_IO_stderr_.file;
