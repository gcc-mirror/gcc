// -*- C++ -*- forwarding header.

// Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 27.8.2  C Library files
//

#ifndef _CPP_CSTDIO
#define _CPP_CSTDIO 1

#include <bits/c++config.h>
#include <bits/std_cstdarg.h>
#include <bits/std_cstddef.h>

#pragma GCC system_header
#include_next <stdio.h>

// Get rid of those macros defined in <stdio.h> in lieu of real functions.
#undef remove
#undef rename
#undef tmpfile
#undef tmpnam
#undef fclose
#undef fflush
#undef fopen
#undef freopen
#undef setbuf
#undef setvbuf
#undef fprintf
#undef fscanf
#undef printf
#undef scanf
#undef snprintf
#undef sprintf
#undef sscanf
#undef vfprintf
#undef vfscanf
#undef vprintf
#undef vscanf
#undef vsnprintf
#undef vsprintf
#undef vsscanf
#undef fgetc
#undef fgets
#undef fputc
#undef fputs
#undef getc
#undef getchar
#undef gets
#undef putc
#undef putchar
#undef puts
#undef ungetc
#undef fread
#undef fwrite
#undef fgetpos
#undef fseek
#undef fsetpos
#undef ftell
#undef rewind
#undef clearerr
#undef feof
#undef ferror
#undef perror

namespace std 
{
  using ::FILE;
  using ::fpos_t;

  using ::remove;
  using ::rename;
  using ::tmpfile;
  using ::tmpnam;
  using ::fclose;
  using ::fflush;
  using ::fopen;
  using ::freopen;
  using ::setbuf;
  using ::setvbuf;
  using ::fprintf;
  using ::fscanf;
  using ::printf;
  using ::scanf;
  using ::snprintf;
  using ::sprintf;
  using ::sscanf;
  using ::vfprintf;
  using ::vfscanf;
  using ::vprintf;
  using ::vscanf;
  using ::vsnprintf;
  using ::vsprintf;
  using ::vsscanf;
  using ::fgetc;
  using ::fgets;
  using ::fputc;
  using ::fputs;
  using ::getc;
  using ::getchar;
  using ::gets;
  using ::putc;
  using ::putchar;
  using ::puts;
  using ::ungetc;
  using ::fread;
  using ::fwrite;
  using ::fgetpos;
  using ::fseek;
  using ::fsetpos;
  using ::ftell;
  using ::rewind;
  using ::clearerr;
  using ::feof;
  using ::ferror;
  using ::perror;
}

#endif
