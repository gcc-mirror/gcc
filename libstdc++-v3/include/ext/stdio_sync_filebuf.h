// Iostreams wrapper for stdio FILE* -*- C++ -*-

// Copyright (C) 2003 Free Software Foundation, Inc.
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

/** @file ext/stdiostream.h
 *  This file is a GNU extension to the Standard C++ Library.
 */

#ifndef _EXT_STDIO_SYNC_FILEBUF
#define _EXT_STDIO_SYNC_FILEBUF

#pragma GCC system_header

#include <fstream>
#include <unistd.h>

#if defined(_GLIBCPP_HAVE_S_ISREG) || defined(_GLIBCPP_HAVE_S_IFREG)
# include <sys/stat.h>
# ifdef _GLIBCPP_HAVE_S_ISREG
#  define _GLIBCPP_ISREG(x) S_ISREG(x)
# else
#  define _GLIBCPP_ISREG(x) (((x) & S_IFMT) == S_IFREG)
# endif
#endif

#ifdef _GLIBCPP_USE_WCHAR_T
#include <cwchar>
#endif

namespace __gnu_cxx
{
  template<typename _CharT, typename _Traits = std::char_traits<_CharT> >
    class stdio_sync_filebuf : public std::basic_streambuf<_CharT, _Traits>
    {
    private:
      std::__c_file* const _M_file;

     public:
      // Types:
      typedef _CharT                  	        	char_type;
      typedef _Traits		                    	traits_type;
      typedef typename traits_type::int_type 		int_type;
      typedef typename traits_type::pos_type 		pos_type;
      typedef typename traits_type::off_type 		off_type;

      explicit 
      stdio_sync_filebuf(std::__c_file* __f) : _M_file(__f) { }

    protected:

      int_type
      syncgetc();

      int_type
      syncungetc(int_type __c);

      int_type
      syncputc(int_type __c);

      virtual int_type
      underflow()
      {
	int_type __c = this->syncgetc();
	return this->syncungetc(__c);
      }

      virtual int_type
      uflow()
      { return this->syncgetc(); }

      virtual int_type
      pbackfail(int_type __c = traits_type::eof())
      { return this->syncungetc(__c); }

      virtual std::streamsize
      xsgetn(char_type* __s, std::streamsize __n);
      
      virtual std::streamsize
      showmanyc()
      { 
#if defined(_GLIBCPP_HAVE_S_ISREG) || defined(_GLIBCPP_HAVE_S_IFREG)
	// Regular files.
	struct stat __buffer;
	int __ret = fstat(fileno(_M_file), &__buffer);
	if (!__ret && _GLIBCPP_ISREG(__buffer.st_mode))
	  return __buffer.st_size - ftell(_M_file);
#endif
	return 0; 
      }

      virtual int_type
      overflow(int_type __c = traits_type::eof())
      {
	int_type __ret;
	if (traits_type::eq_int_type(__c, traits_type::eof()))
	  {
	    if (std::fflush(_M_file))
	      __ret = traits_type::eof();
	    else
	      __ret = traits_type::not_eof(__c);
	  }
	else
	  __ret = this->syncputc(__c);
	return __ret;
      }

      virtual std::streamsize
      xsputn(const char_type* __s, std::streamsize __n);

      virtual int
      sync()
      { return std::fflush(_M_file); }

      virtual std::streampos
      seekoff(std::streamoff __off, std::ios_base::seekdir __dir,
	      std::ios_base::openmode = std::ios_base::in | std::ios_base::out)
      {
	std::streampos __ret(std::streamoff(-1));
	int __whence;
	if (__dir == std::ios_base::beg)
	  __whence = SEEK_SET;
	else if (__dir == std::ios_base::cur)
	  __whence = SEEK_CUR;
	else
	  __whence = SEEK_END;
	
	if (!fseek(_M_file, __off, __whence))
	  __ret = std::streampos(std::ftell(_M_file));
	return __ret;
      }

      virtual std::streampos
      seekpos(std::streampos __pos,
	      std::ios_base::openmode __mode =
	      std::ios_base::in | std::ios_base::out)
      { return seekoff(std::streamoff(__pos), std::ios_base::beg, __mode); }
    };

  template<>
    inline stdio_sync_filebuf<char>::int_type
    stdio_sync_filebuf<char>::syncgetc()
    { return std::getc(_M_file); }

  template<>
    inline stdio_sync_filebuf<char>::int_type
    stdio_sync_filebuf<char>::syncungetc(int_type __c)
    { return std::ungetc(__c, _M_file); }

  template<>
    inline stdio_sync_filebuf<char>::int_type
    stdio_sync_filebuf<char>::syncputc(int_type __c)
    { return std::putc(__c, _M_file); }

  template<>
    inline std::streamsize
    stdio_sync_filebuf<char>::xsgetn(char* __s, std::streamsize __n)
    { return std::fread(__s, 1, __n, _M_file); }

  template<>
    inline std::streamsize
    stdio_sync_filebuf<char>::xsputn(const char* __s, std::streamsize __n)
    { return std::fwrite(__s, 1, __n, _M_file); }

#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    inline stdio_sync_filebuf<wchar_t>::int_type
    stdio_sync_filebuf<wchar_t>::syncgetc()
    { return std::getwc(_M_file); }

  template<>
    inline stdio_sync_filebuf<wchar_t>::int_type
    stdio_sync_filebuf<wchar_t>::syncungetc(int_type __c)
    { return std::ungetwc(__c, _M_file); }

  template<>
    inline stdio_sync_filebuf<wchar_t>::int_type
    stdio_sync_filebuf<wchar_t>::syncputc(int_type __c)
    { return std::putwc(__c, _M_file); }

  template<>
    inline std::streamsize
    stdio_sync_filebuf<wchar_t>::xsgetn(wchar_t* __s, std::streamsize __n)
    {  
      std::streamsize __ret = 0;
      const int_type __eof = traits_type::eof();
      while (__n--)
	{
	  int_type __c = this->syncgetc();
	  if (traits_type::eq_int_type(__c, __eof))
	    break;
	  *__s++ = __c;
	  ++__ret;
	}
      return __ret;
    }
      
  template<>
    inline std::streamsize
    stdio_sync_filebuf<wchar_t>::xsputn(const wchar_t* __s, 
					std::streamsize __n)
    {
      std::streamsize __ret = 0;
      const int_type __eof = traits_type::eof();
      while (__n--)
	{
	  if (traits_type::eq_int_type(this->syncputc(*__s++), __eof))
	    break;
	  ++__ret;
	}
      return __ret;
    }
#endif

#if _GLIBCPP_EXTERN_TEMPLATE
  extern template class stdio_sync_filebuf<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  extern template class stdio_sync_filebuf<wchar_t>;
#endif
#endif
} // namespace __gnu_cxx

#endif 
