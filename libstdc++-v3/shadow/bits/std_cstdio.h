// -*- C++ -*- header wrapper.
// Copyright (C) 1997-1999 Free Software Foundation, Inc.
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

// XXX problems?  Uses size_t, va_list in interfaces.  
//   Maybe <stdio.h> needs a size_t macro defined.  Maybe it 
//   clobbers va_list.
// 
// Also,
//  uses va_start, va_end in implementation of vfprintf and
//  vfscanf.  Maybe those shouldn't be inline.

#ifndef _CPP_CSTDIO
#define _CPP_CSTDIO 1

# include <bits/std_cstddef.h>  /* pick up size_t, NULL */
# include <bits/std_cstdarg.h>  /* pick up va_list, va_start, va_end */

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <stdio.h>
    }
    // size_t handled in <cstddef>
    typedef FILE _CPP_FILE_capture;
    typedef fpos_t _CPP_fpos_t_capture;
    // NULL done in <stddef.h>
    const int _CPP_IOFBF_capture = _IOFBF;
    const int _CPP_IOLBF_capture = _IOLBF;
    const int _CPP_IONBF_capture = _IONBF;
    const int _CPP_BUFSIZ_capture = BUFSIZ;
    const int _CPP_EOF_capture = EOF;
    const int _CPP_FOPEN_MAX_capture = FOPEN_MAX;
    const int _CPP_FILENAME_MAX_capture = FILENAME_MAX;
    const int _CPP_L_tmpnam_capture = L_tmpnam;
    const int _CPP_SEEK_CUR_capture = SEEK_CUR;
    const int _CPP_SEEK_END_capture = SEEK_END;
    const int _CPP_SEEK_SET_capture = SEEK_SET;
    const int _CPP_TMP_MAX_capture = TMP_MAX;

    inline FILE* _CPP_stderr_capture() { return stderr; }
    inline FILE* _CPP_stdin_capture()  { return stdin; }
    inline FILE* _CPP_stdout_capture() { return stdout; }

    inline int _CPP_getc_capture(FILE* __f)        { return getc(__f); }
    inline int _CPP_getchar_capture()              { return getchar(); }
    inline int _CPP_putc_capture(int __c, FILE* __f) { return putc(__c, __f); }
    inline int _CPP_putchar_capture(int __c)       { return putchar(__c); }
    inline int _CPP_feof_capture(FILE* __f)        { return feof(__f); }

    namespace _C_Shadow {
    }

  } // close namespace ::_C_Swamp::

// # undef size_t  /* handled in <cstddef> */
# undef FILE 
# undef fpos_t 
# undef _IOFBF
# define _IOFBF		::_C_Swamp::_CPP__IOFBF_capture
# undef _IOLBF
# define _IOLBF		::_C_Swamp::_CPP__IOLBF_capture
# undef _IONBF	
# define _IONBF		::_C_Swamp::_CPP__IONBF_capture
# undef BUFSIZ
# define BUFSIZ		::_C_Swamp::_CPP_BUFSIZ_capture
# undef EOF
# define EOF		::_C_Swamp::_CPP_EOF_capture
# undef FOPEN_MAX
# define FOPEN_MAX	::_C_Swamp::_CPP_FOPEN_MAX_capture
# undef FILENAME_MAX
# define FILENAME_MAX	::_C_Swamp::_CPP_FILENAME_MAX_capture
# undef L_tmpnam
# define L_tmpnam	::_C_Swamp::_CPP_L_tmpnam_capture
# undef SEEK_CUR
# define SEEK_CUR	::_C_Swamp::_CPP_SEEK_CUR_capture
# undef SEEK_END
# define SEEK_END	::_C_Swamp::_CPP_SEEK_END_capture
# undef SEEK_SET
# define SEEK_SET	::_C_Swamp::_CPP_SEEK_SET_capture
# undef TMP_MAX
# define TMP_MAX	::_C_Swamp::_CPP_TMP_MAX_capture

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

  namespace _C_Swamp {
    namespace _C_Shadow {
    }
  }
  namespace std {

    // Adopt C names into std::

    struct FILE   : ::_C_Swamp::_CPP_FILE_capture { };
    struct fpos_t { ::_C_Swamp::_CPP_fpos_t_capture _M_dummy; };

    inline FILE* _CPP_stderr()
      { return reinterpret_cast<FILE*>( ::_C_Swamp::_CPP_stderr_capture() ); }
    inline FILE* _CPP_stdin()
      { return reinterpret_cast<FILE*>( ::_C_Swamp::_CPP_stdin_capture() ); }
    inline FILE* _CPP_stdout()
      { return reinterpret_cast<FILE*>( ::_C_Swamp::_CPP_stdout_capture() ); }

    using ::_C_Swamp::remove;
    using ::_C_Swamp::rename;

    inline FILE*
      tmpfile() { return reinterpret_cast<FILE*>(::_C_Swamp::tmpfile()); }

    using ::_C_Swamp::tmpnam;

    inline int
      fclose(FILE* __f) { return ::_C_Swamp::fclose(__f); }

    inline int
      fflush(FILE* __f) { return ::_C_Swamp::fflush(__f); }

    inline FILE*
      fopen(char const* __name, char const* __mode) 
        { return reinterpret_cast<FILE*>(::_C_Swamp::fopen(__name,__mode)); }

    inline FILE*
      freopen(char const* __name, char const* __mode, FILE* __f) 
        { return reinterpret_cast<FILE*>(
	  ::_C_Swamp::freopen(__name,__mode,__f)); }

    inline void
      setbuf(FILE* __f, char* __buf) 
        { return ::_C_Swamp::setbuf(__f, __buf); }

    inline int
      setvbuf(FILE* __f, char* __buf, int __mode, size_t __size) 
        { return ::_C_Swamp::setvbuf(__f, __buf, __mode, __size); }

    inline int
      fprintf(FILE* __f, char const* __fmt, ...)
        { va_list __v; va_start(__v,__fmt); 
          int __i = ::_C_Swamp::vfprintf(__f, __fmt, __v); va_end(__v);
	    return __i; }

    inline int
      fscanf(FILE* __f, char const* __fmt, ...)
        { va_list __v; va_start(__v,__fmt); 
          int __i = ::_C_Swamp::vfscanf(__f, __fmt, __v); va_end(__v);
	    return __i; }

    using ::_C_Swamp::printf;
    using ::_C_Swamp::scanf;
    using ::_C_Swamp::sprintf;
    using ::_C_Swamp::sscanf;

    // using ::_C_Swamp::vfprintf;
    inline int
      vfprintf(FILE* __f, char const* __fmt, va_list __v)
        { return ::_C_Swamp::vfprintf(__f, __fmt, __v); }

    inline int
      vprintf(char const* __fmt, va_list __v)
        { return ::_C_Swamp::vprintf(__fmt, __v); }

    inline int
      vsprintf(char* __buf, char const* __fmt, va_list __v)
        { return ::_C_Swamp::vsprintf(__buf, __fmt, __v); }

    inline int
      fgetc(FILE* __f) { return ::_C_Swamp::fgetc(__f); }

    // using ::_C_Swamp::fgets;
    inline char*
      fgets(char* __buf, int __n, FILE* __f) 
        { return ::_C_Swamp::fgets(__buf, __n, __f); }

    inline int
      fputc(int __c, FILE* __f) { return ::_C_Swamp::fputc(__c, __f); }

    inline int
      fputs(char const* __s, FILE* __f) 
        { return ::_C_Swamp::fputs(__s, __f); }

    inline int
      getc(FILE* __f) { return ::_C_Swamp::_CPP_getc_capture(__f); }

    inline int
      getchar() { return ::_C_Swamp::_CPP_getchar_capture(); }

    using ::_C_Swamp::gets;

    inline int
      putc(int __c, FILE* __f) 
        { return ::_C_Swamp::_CPP_putc_capture(__c, __f); }

    inline int
      putchar(int __c) { return ::_C_Swamp::_CPP_putchar_capture(__c); }

    using ::_C_Swamp::puts;

    // using ::_C_Swamp::ungetc;
    inline int
      ungetc(int __c, FILE* __f) { return ::_C_Swamp::ungetc(__c, __f); }

    inline size_t
      fread(void* __p, size_t __z, size_t __n, FILE* __f)
        { return ::_C_Swamp::fread(__p,__z,__n,__f); }

    inline size_t
      fwrite(void const* __p, size_t __z, size_t __n, FILE* __f)
        { return ::_C_Swamp::fwrite(__p,__z,__n,__f); }

    inline int
      fgetpos(FILE* __f, fpos_t* __pos)
        { return ::_C_Swamp::fgetpos(__f,&__pos->_M_dummy); }

    inline int
      fseek(FILE* __f, long __off, int __how)
        { return ::_C_Swamp::fseek(__f,__off,__how); }

    inline int
      fsetpos(FILE* __f, fpos_t const* __pos)
        { return ::_C_Swamp::fsetpos(__f,&__pos->_M_dummy); }

    inline long
      ftell(FILE* __f) { return ::_C_Swamp::ftell(__f); }

    inline void
      rewind(FILE* __f) { return ::_C_Swamp::rewind(__f); }

    inline void
      clearerr(FILE* __f) { return ::_C_Swamp::clearerr(__f); }

    inline int
      feof(FILE* __f) { return ::_C_Swamp::_CPP_feof_capture(__f); }

    inline int
      ferror(FILE* __f) { return ::_C_Swamp::ferror(__f); }

    using ::_C_Swamp::perror;

  } // close namespace std::
  
# undef stderr
# define stderr ::std::_CPP_stderr()
# undef stdin
# define stdin  ::std::_CPP_stdin()
# undef stdout
# define stdout ::std::_CPP_stdout()

  namespace _C_Swamp {
    namespace _C_Shadow {
      using ::std::FILE;
      using ::std::fpos_t;

      // using ::std::remove;
      // using ::std::rename;
      using ::std::tmpfile;
      // using ::std::tmpnam;
      using ::std::fclose;
      using ::std::fflush;
      using ::std::fopen;
      using ::std::freopen;
      using ::std::setbuf;
      using ::std::setvbuf;
      using ::std::fprintf;
      using ::std::fscanf;
      // using ::std::printf;
      // using ::std::scanf;
      // using ::std::sprintf;
      // using ::std::sscanf;
      using ::std::vfprintf;
      using ::std::vprintf;
      using ::std::vsprintf;  
      using ::std::fgetc;
      using ::std::fgets;
      using ::std::fputc;
      using ::std::fputs;
      using ::std::getc;
      using ::std::getchar;
      // using ::std::gets;
      using ::std::putc;
      using ::std::putchar;
      // using ::std::puts;
      using ::std::ungetc;
      using ::std::fread;
      using ::std::fwrite;
      using ::std::fgetpos;
      using ::std::fseek;
      using ::std::fsetpos;
      using ::std::ftell;
      using ::std::rewind;
      using ::std::clearerr;
      using ::std::feof;
      using ::std::ferror;
      // using ::std::perror;
    }
  }

# undef _IN_C_SWAMP_

#endif

