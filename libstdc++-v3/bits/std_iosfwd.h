// Forwarding declarations -*- C++ -*-

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
// ISO C++ 14882: 27.2  Forward declarations
//

#ifndef _CPP_IOSFWD
#define _CPP_IOSFWD 1

#include <bits/c++config.h>
#include <bits/std_cwchar.h> //For mbstate_t

namespace std {

  // Generic declarations.
  template<typename _CharT> struct char_traits;
  template<typename _Alloc> class allocator;

  // Forward declarations
  template<> class char_traits<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template<> class char_traits<wchar_t>;
#endif

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_ios;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_streambuf;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_istream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_ostream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_iostream;

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
	    typename _Alloc = allocator<_CharT> >
  class basic_stringbuf;

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
	   typename _Alloc = allocator<_CharT> >
  class basic_istringstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
	   typename _Alloc = allocator<_CharT> >
  class basic_ostringstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
	   typename _Alloc = allocator<_CharT> >
  class basic_stringstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_filebuf;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_ifstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_ofstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_fstream;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class istreambuf_iterator;

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class ostreambuf_iterator;

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
  // Not included.
  class ios_base; 
#endif

  template<class _State> struct fpos;
#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
  // Can't have self-recursive types for streampos. 
  // 21.1.3.1 char_traits sets size_type to streampos
  // 27.4.1 
  // And here, where streampos is typedefed to fpos<traits::state_type>
  typedef fpos<mbstate_t> 		streampos;
#  ifdef _GLIBCPP_USE_WCHAR_T
  typedef fpos<mbstate_t> 		wstreampos;
#  endif
#endif

  typedef basic_ios<char> 		ios;
  typedef basic_streambuf<char> 	streambuf;
  typedef basic_istream<char> 		istream;
  typedef basic_ostream<char> 		ostream;
  typedef basic_iostream<char> 		iostream;
  typedef basic_stringbuf<char> 	stringbuf;
  typedef basic_istringstream<char> 	istringstream;
  typedef basic_ostringstream<char> 	ostringstream;
  typedef basic_stringstream<char> 	stringstream;
  typedef basic_filebuf<char> 		filebuf;
  typedef basic_ifstream<char> 		ifstream;
  typedef basic_ofstream<char> 		ofstream;
  typedef basic_fstream<char> 		fstream;

#ifdef _GLIBCPP_USE_WCHAR_T
  typedef basic_ios<wchar_t> 		wios;
  typedef basic_streambuf<wchar_t> 	wstreambuf;
  typedef basic_istream<wchar_t> 	wistream;
  typedef basic_ostream<wchar_t> 	wostream;
  typedef basic_iostream<wchar_t> 	wiostream;
  typedef basic_stringbuf<wchar_t> 	wstringbuf;
  typedef basic_istringstream<wchar_t> 	wistringstream;
  typedef basic_ostringstream<wchar_t> 	wostringstream;
  typedef basic_stringstream<wchar_t> 	wstringstream;
  typedef basic_filebuf<wchar_t> 	wfilebuf;
  typedef basic_ifstream<wchar_t> 	wifstream;
  typedef basic_ofstream<wchar_t> 	wofstream;
  typedef basic_fstream<wchar_t> 	wfstream;
#endif

} // namespace std

#endif	// _CPP_IOSFWD







