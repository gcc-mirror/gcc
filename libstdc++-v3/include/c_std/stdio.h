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

#ifndef  _INCLUDED_CPP_STDIO_H_
# define _INCLUDED_CPP_STDIO_H_ 1

# ifdef _IN_C_LEGACY_  /* sub-included by a C header */
      // get out of the "legacy"
    } // close extern "C"
  }   // close namespace _C_legacy::
#  undef _IN_C_LEGACY_
#  define _STDIO_NEED_C_LEGACY_
# endif

# include <cstdio>

  // Expose global C names, including non-standard ones, but shadow
  // some names and types with the std:: C++ version.
  using std::FILE;
  using std::fpos_t; 

  using std::remove;
  using std::rename;
  using std::tmpfile;
  using std::tmpnam;
  using std::fclose;
  using std::fflush;
  using std::fopen;
  using std::freopen;
  using std::setbuf;
  using std::setvbuf;
  using std::fprintf;
  using std::fscanf;
  using std::printf;
  using std::scanf;
  using std::sprintf;
  using std::sscanf;
  using std::vfprintf;
  using std::vprintf;
  using std::vsprintf;
  using std::fgetc;
  using std::fgets;
  using std::fputc;
  using std::fputs;
  using std::getc;
  using std::getchar;
  using std::gets;
  using std::putc;
  using std::putchar;
  using std::puts;
  using std::ungetc;
  using std::fread;
  using std::fwrite;
  using std::fgetpos;
  using std::fseek;
  using std::fsetpos;
  using std::ftell;
  using std::rewind;
  using std::clearerr;
  using std::feof;
  using std::ferror;
  using std::perror;

# ifdef _STDIO_NEED_C_LEGACY_
  // dive back into the "swamp"
  namespace _C_legacy {
    extern "C" {
#  define _IN_C_LEGACY_
#  undef _STDIO_NEED_C_LEGACY_
# endif /* _STDIO_NEED_C_LEGACY_ */
#endif /* _INCLUDED_CPP_STDIO_H_ */
