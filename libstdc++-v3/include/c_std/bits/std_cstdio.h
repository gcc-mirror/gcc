// -*- C++ -*- header wrapper.

// Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.
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

# include <bits/std_cstddef.h>  
# include <bits/std_cstdarg.h>  

namespace _C_legacy {
  extern "C" {
#     define _IN_C_LEGACY_
#     undef __need_FILE
#     pragma GCC system_header
#     include_next <stdio.h>
  }

  typedef FILE _CPP_FILE_capture;
  typedef fpos_t _CPP_fpos_t_capture;

  inline FILE* 
  _CPP_stderr_capture() { return stderr; }

  inline FILE* 
  _CPP_stdin_capture()  { return stdin; }

  inline FILE* 
  _CPP_stdout_capture() { return stdout; }

  inline int 
  _CPP_getc_capture(FILE* __f) { return getc(__f); }

  inline int 
  _CPP_getchar_capture() { return getchar(); }

  inline int 
  _CPP_putc_capture(int __c, FILE* __f) { return putc(__c, __f); }
  
  inline int 
  _CPP_putchar_capture(int __c) { return putchar(__c); }
 
  inline int 
  _CPP_feof_capture(FILE* __f) { return feof(__f); }

} // namespace _C_legacy

# undef FILE 
# undef fpos_t 

# undef remove
# undef rename
# undef tmpfile
# undef tmpnam
# undef fclose
# undef fflush
# undef fopen
# undef freopen
# undef setbuf
# undef setvbuf
# undef fprintf
# undef fscanf
# undef printf
# undef scanf
# undef sprintf
# undef sscanf
# undef vfprintf
# undef vprintf
# undef vsprintf
# undef fgetc
# undef fgets
# undef fputc
# undef fputs
# undef getc
# undef getchar
# undef gets
# undef putc
# undef putchar
# undef puts
# undef ungetc
# undef fread
# undef fwrite
# undef fgetpos
# undef fseek
# undef fsetpos
# undef ftell
# undef rewind
# undef clearerr
# undef feof
# undef ferror
# undef perror
  
# undef stderr
# define stderr std::_CPP_stderr()
# undef stdin
# define stdin  std::_CPP_stdin()
# undef stdout
# define stdout std::_CPP_stdout()

namespace std {
  struct FILE : _C_legacy::_CPP_FILE_capture { };
  struct fpos_t { _C_legacy::_CPP_fpos_t_capture _M_dummy; };

  using _C_legacy::remove;
  using _C_legacy::rename;
  using _C_legacy::tmpnam;
  using _C_legacy::printf;
  using _C_legacy::scanf;
  using _C_legacy::sprintf;
  using _C_legacy::sscanf;
  using _C_legacy::gets;
  using _C_legacy::perror;

  inline FILE* 
  _CPP_stderr()
  { return reinterpret_cast<FILE*>(_C_legacy::_CPP_stderr_capture() ); }

  inline FILE* 
  _CPP_stdin()
  { return reinterpret_cast<FILE*>(_C_legacy::_CPP_stdin_capture() ); }

  inline FILE* 
  _CPP_stdout()
  { return reinterpret_cast<FILE*>(_C_legacy::_CPP_stdout_capture() ); }

  inline FILE*
  tmpfile() { return reinterpret_cast<FILE*>(_C_legacy::tmpfile()); }

  inline int
  fclose(FILE* __f) { return _C_legacy::fclose(__f); }

  inline int
  fflush(FILE* __f) { return _C_legacy::fflush(__f); }

  inline FILE*
  fopen(char const* __name, char const* __mode) 
  { return reinterpret_cast<FILE*>(_C_legacy::fopen(__name,__mode)); }

  inline FILE*
  freopen(char const* __name, char const* __mode, FILE* __f) 
  { return reinterpret_cast<FILE*>(_C_legacy::freopen(__name,__mode,__f)); }

  inline void
  setbuf(FILE* __f, char* __buf) 
  { return _C_legacy::setbuf(__f, __buf); }

  inline int
  setvbuf(FILE* __f, char* __buf, int __mode, size_t __size) 
  { return _C_legacy::setvbuf(__f, __buf, __mode, __size); }

  inline int
  fprintf(FILE* __f, char const* __fmt, ...)
  { 
    va_list __v; 
    va_start(__v,__fmt); 
    int __i = _C_legacy::vfprintf(__f, __fmt, __v); 
    va_end(__v);
    return __i; 
  }

  inline int
  fscanf(FILE* __f, char const* __fmt, ...)
  { 
    va_list __v; 
    va_start(__v,__fmt); 
    int __i = _C_legacy::vfscanf(__f, __fmt, __v); 
    va_end(__v);
    return __i; 
  }

  inline int
  vfprintf(FILE* __f, char const* __fmt, va_list __v)
  { return _C_legacy::vfprintf(__f, __fmt, __v); }

  inline int
  vprintf(char const* __fmt, va_list __v)
  { return _C_legacy::vprintf(__fmt, __v); }

  inline int
  vsprintf(char* __buf, char const* __fmt, va_list __v)
  { return _C_legacy::vsprintf(__buf, __fmt, __v); }

  inline int
  fgetc(FILE* __f) { return _C_legacy::fgetc(__f); }

  inline char*
  fgets(char* __buf, int __n, FILE* __f) 
  { return _C_legacy::fgets(__buf, __n, __f); }

  inline int
  fputc(int __c, FILE* __f) { return _C_legacy::fputc(__c, __f); }

  inline int
  fputs(char const* __s, FILE* __f) 
  { return _C_legacy::fputs(__s, __f); }

  inline int
  getc(FILE* __f) { return _C_legacy::_CPP_getc_capture(__f); }

  inline int
  getchar() { return _C_legacy::_CPP_getchar_capture(); }

  inline int
  putc(int __c, FILE* __f) 
  { return _C_legacy::_CPP_putc_capture(__c, __f); }

  inline int
  putchar(int __c) { return _C_legacy::_CPP_putchar_capture(__c); }

  using _C_legacy::puts;

  inline int
  ungetc(int __c, FILE* __f) { return _C_legacy::ungetc(__c, __f); }

  inline size_t
  fread(void* __p, size_t __z, size_t __n, FILE* __f)
  { return _C_legacy::fread(__p,__z,__n,__f); }

  inline size_t
  fwrite(void const* __p, size_t __z, size_t __n, FILE* __f)
  { return _C_legacy::fwrite(__p,__z,__n,__f); }

  inline int
  fgetpos(FILE* __f, fpos_t* __pos)
  { return _C_legacy::fgetpos(__f,&__pos->_M_dummy); }

  inline int
  fseek(FILE* __f, long __off, int __how)
  { return _C_legacy::fseek(__f,__off,__how); }

  inline int
  fsetpos(FILE* __f, fpos_t const* __pos)
  { return _C_legacy::fsetpos(__f,&__pos->_M_dummy); }

  inline long
  ftell(FILE* __f) { return _C_legacy::ftell(__f); }

  inline void
  rewind(FILE* __f) { return _C_legacy::rewind(__f); }

  inline void
  clearerr(FILE* __f) { return _C_legacy::clearerr(__f); }

  inline int
  feof(FILE* __f) { return _C_legacy::_CPP_feof_capture(__f); }

  inline int
  ferror(FILE* __f) { return _C_legacy::ferror(__f); }
} // namespace std

# undef _IN_C_LEGACY_

#endif

