// Explicit instantiation file.

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
// ISO C++ 14882:
//

#include <bits/std_string.h>
#include <bits/std_algorithm.h>
#include <bits/std_locale.h>
#include <bits/std_vector.h>
#include <bits/std_iterator.h>
#include <bits/std_streambuf.h>
#include <bits/std_sstream.h>
#include <bits/std_fstream.h>
#include <bits/std_ios.h>
#include <bits/basic_ios.tcc>
#include <bits/std_istream.h>
#include <bits/std_ostream.h>
#include <bits/std_string.h>

// NB: unnecessary if the .h headers include these
#ifndef  _GLIBCPP_FULLY_COMPLIANT_HEADERS
#include <bits/sstream.tcc>
#include <bits/fstream.tcc>
#include <bits/streambuf.tcc>
#include <bits/istream.tcc>
#include <bits/ostream.tcc>
#endif

namespace std
{

  //
  // streambuf
  // 
  template class basic_streambuf<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_streambuf<wchar_t>;
#endif


  //
  // stringstream
  //
  template class basic_stringbuf<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_stringbuf<wchar_t>;
#endif


  //
  // fstream
  //
  template class basic_filebuf<char, char_traits<char> >;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_filebuf<wchar_t, char_traits<wchar_t> >;
#endif


  //
  // basic_ios
  //
  template class basic_ios<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_ios<wchar_t>;
#endif


  //
  // istream
  //
  template class basic_istream<char>;
  template istream& ws(istream&);
  template istream& operator>>(istream&, char&);
  template istream& operator>>(istream&, unsigned char&);
  template istream& operator>>(istream&, signed char&);
  template istream& operator>>(istream&, char*);
  template istream& operator>>(istream&, unsigned char*);
  template istream& operator>>(istream&, signed char*);
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_istream<wchar_t>;
  template wistream& ws(wistream&);
  template wistream& operator>>(wistream&, wchar_t&);
  template wistream& operator>>(wistream&, wchar_t*);
#endif


  //
  // ostream
  //
  template class basic_ostream<char>;
  template ostream& endl(ostream&);
  template ostream& ends(ostream&);
  template ostream& flush(ostream&);
  template ostream& operator<<(ostream&, char);
  template ostream& operator<<(ostream&, unsigned char);
  template ostream& operator<<(ostream&, signed char);
  template ostream& operator<<(ostream&, const char*);
  template ostream& operator<<(ostream&, const unsigned char*);
  template ostream& operator<<(ostream&, const signed char*);
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_ostream<wchar_t>;
  template wostream& endl(wostream&);
  template wostream& ends(wostream&);
  template wostream& flush(wostream&);
  template wostream& operator<<(wostream&, wchar_t);
  template wostream& operator<<(wostream&, char);
  template wostream& operator<<(wostream&, const wchar_t*);
  template wostream& operator<<(wostream&, const char*);
#endif
  

  //
  // iostream
  //
  template class basic_iostream<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_iostream<wchar_t>; 
#endif


  //
  // ifstream
  //
  template class basic_ifstream<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_ifstream<wchar_t>;
#endif


  //
  // ofstream
  //
  template class basic_ofstream<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_ofstream<wchar_t>;
#endif


  //
  // istringstream
  //
  template class basic_istringstream<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_istringstream<wchar_t>; 
#endif


  //
  // ostringstream
  //
  template class basic_ostringstream<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template class basic_ostringstream<wchar_t>; 
#endif


  //
  // string related to iostreams
  //
  template 
    basic_istream<char>& 
    operator>>(basic_istream<char>&, string&);
  template 
    basic_ostream<char>& 
    operator<<(basic_ostream<char>&, const string&);
  template 
    basic_istream<char>& 
    getline(basic_istream<char>&, string&, char);
  template 
    basic_istream<char>& 
    getline(basic_istream<char>&, string&);
#ifdef _GLIBCPP_USE_WCHAR_T
  template 
    basic_istream<wchar_t>& 
    operator>>(basic_istream<wchar_t>&, wstring&);
  template 
    basic_ostream<wchar_t>& 
    operator<<(basic_ostream<wchar_t>&, const wstring&);
  template 
    basic_istream<wchar_t>& 
    getline(basic_istream<wchar_t>&, wstring&, wchar_t);
  template 
    basic_istream<wchar_t>& 
    getline(basic_istream<wchar_t>&, wstring&);
#endif

  //
  // algorithm
  //
  typedef  _Char_traits_match<char, char_traits<char> > char_match;

  template 
    const char*  
    find_if<const char *, char_match>
    (const char *, const char *, char_match, random_access_iterator_tag);

#ifdef _GLIBCPP_USE_WCHAR_T
  typedef  _Char_traits_match<wchar_t, char_traits<wchar_t> > wchar_match;

  template const wchar_t*  
    find_if<const wchar_t*, wchar_match>
    (const wchar_t*, const wchar_t*, wchar_match, random_access_iterator_tag);
#endif
  
  template 
    string* 
    __uninitialized_fill_n_aux<string*, size_t, string>
    (string*, size_t, string const &, _Bool<false>);

  template 
    string* 
    __uninitialized_copy_aux<vector<string>::const_iterator, string *>
    (vector<string>::const_iterator, vector<string>::const_iterator, 
     string*, _Bool<false>);

  template
    void 
    __pad_char(basic_ios<char>&, char*, const char*,
		const streamsize, const streamsize);
#ifdef _GLIBCPP_USE_WCHAR_T
  template
    void 
    __pad_char(basic_ios<wchar_t>&, wchar_t*, const wchar_t*,
		const streamsize, const streamsize);
#endif

  template
    ostreambuf_iterator<char>
    __pad_numeric(ostreambuf_iterator<char>, _Ios_Fmtflags, char, int,
		  const char*, const char*, const char*);
#ifdef _GLIBCPP_USE_WCHAR_T
  template
    ostreambuf_iterator<wchar_t>
    __pad_numeric(ostreambuf_iterator<wchar_t>, _Ios_Fmtflags, wchar_t, int,
		  const wchar_t*, const wchar_t*, const wchar_t*);
#endif

  template
    ostreambuf_iterator<char>
    __output_float(ostreambuf_iterator<char>, ios_base&, char, 
		   const char*, size_t);
#ifdef _GLIBCPP_USE_WCHAR_T
  template
    ostreambuf_iterator<wchar_t>
    __output_float(ostreambuf_iterator<wchar_t>, ios_base&, wchar_t, 
		   const char*, size_t);
#endif

  template
    streamsize
    __copy_streambufs(basic_ios<char>&, basic_streambuf<char>*,
		      basic_streambuf<char>*); 
#ifdef _GLIBCPP_USE_WCHAR_T
  template
    streamsize
    __copy_streambufs(basic_ios<wchar_t>&, basic_streambuf<wchar_t>*,
		      basic_streambuf<wchar_t>*); 
#endif
} //std
