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

// Note: This is not a conforming implementation.

#ifndef _CPP_CSTDIO
#define _CPP_CSTDIO 1

#include <bits/c++config.h>
#include <bits/std_cstdarg.h>
#include <bits/std_cstddef.h>

#pragma GCC system_header
#include <stdio.h>

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

  extern "C" int remove(const char*); 
  extern "C" int rename(const char*, const char*); 
  extern "C" FILE* tmpfile(void); 
  extern "C" char* tmpnam(char*); 
  extern "C" int fclose(FILE*); 
  extern "C" int fflush(FILE*); 
  extern "C" FILE* fopen(const char*, const char*); 
  extern "C" FILE* freopen(const char*, const char*, FILE*); 
  extern "C" void setbuf(FILE*, char*);
  extern "C" int setvbuf(FILE*, char*, int, size_t); 
  extern "C" int fprintf(FILE*, const char*, ...); 
  extern "C" int fscanf(FILE*, const char*, ...); 
  extern "C" int printf(const char*, ...); 
  extern "C" int scanf(const char*, ...); 
  extern "C" int snprintf(char *, size_t, const char*, ...);
  extern "C" int sprintf(char *, const char*, ...); 
  extern "C" int sscanf(const char*, const char*, ...); 
  extern "C" int vfprintf(FILE*, const char*, va_list); 
  extern "C" int vfscanf(FILE*, const char*, va_list); 
  extern "C" int vprintf(const char*, va_list); 
  extern "C" int vscanf(const char*, va_list); 
  extern "C" int vsnprintf(char*, size_t, const char*, va_list); 
  extern "C" int vsprintf(char*, const char*, va_list); 
  extern "C" int vsscanf(const char*, const char*, va_list); 
  extern "C" int fgetc(FILE *); 
  extern "C" char *fgets(char*, int, FILE*); 
  extern "C" int fputc(int, FILE*); 
  extern "C" int fputs(const char*, FILE*); 
  extern "C" int getc(FILE*); 
  extern "C" int getchar(void); 
  extern "C" char *gets(char*); 
  extern "C" int putc(int, FILE*); 
  extern "C" int putchar(int); 
  extern "C" int puts(const char*); 
  extern "C" int ungetc(int, FILE*);
  extern "C" size_t fread(void*, size_t, size_t, FILE*); 
  extern "C" size_t fwrite(const void*, size_t, size_t, FILE*); 
  extern "C" int fgetpos(FILE*, fpos_t*); 
  extern "C" int fseek(FILE*, long int, int); 
  extern "C" int fsetpos(FILE*, const fpos_t*); 
  extern "C" long int ftell(FILE*); 
  extern "C" void rewind(FILE*); 
  extern "C" void clearerr(FILE*); 
  extern "C" int feof(FILE*); 
  extern "C" int ferror(FILE*); 
  extern "C" void perror(const char*);
}

#endif








