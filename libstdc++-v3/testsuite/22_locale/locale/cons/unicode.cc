// { dg-do run { target c++11 } }
// { dg-require-iconv "ISO-8859-1" }

// Copyright (C) 2006-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <cwchar> // for mbstate_t
#include <locale>
#include <stdexcept>
#include <typeinfo>
#include <testsuite_hooks.h>
#include <ext/codecvt_specializations.h>

typedef std::codecvt<char, char, std::mbstate_t> 	      c_codecvt;

#ifdef _GLIBCXX_USE_WCHAR_T
typedef std::codecvt<wchar_t, char, std::mbstate_t>	      w_codecvt;
#endif

typedef std::codecvt<char16_t, char, std::mbstate_t>	      u16_codecvt;
typedef std::codecvt<char32_t, char, std::mbstate_t>	      u32_codecvt;
#ifdef _GLIBCXX_USE_CHAR8_T
typedef std::codecvt<char16_t, char8_t, std::mbstate_t>	      u16u8_codecvt;
typedef std::codecvt<char32_t, char8_t, std::mbstate_t>	      u32u8_codecvt;
#endif

class gnu_facet: public std::locale::facet
{
public:
  static std::locale::id id;
};

std::locale::id gnu_facet::id;

void test01()
{
  using namespace std;
  typedef unsigned short       				int_type;
  typedef char						ext_type;
  typedef __gnu_cxx::encoding_state	       		state_type;
  typedef codecvt<int_type, ext_type, state_type>	unicode_codecvt;

  // unicode_codecvt
  locale loc01(locale::classic());
  locale loc13(locale::classic(), new unicode_codecvt);  
  VERIFY( loc01 != loc13 );
  VERIFY( loc13.name() == "*" );
  try 
    {
      VERIFY( has_facet<c_codecvt>(loc13) );
#ifdef _GLIBCXX_USE_WCHAR_T
      VERIFY( has_facet<w_codecvt>(loc13) );
#endif
      VERIFY( has_facet<u16_codecvt>(loc13) );
      VERIFY( has_facet<u32_codecvt>(loc13) );
#ifdef _GLIBCXX_USE_CHAR8_T
      VERIFY( has_facet<u16u8_codecvt>(loc13) );
      VERIFY( has_facet<u32u8_codecvt>(loc13) );
#endif
      VERIFY( has_facet<unicode_codecvt>(loc13) );
    }
  catch(...)
    { VERIFY( false ); }

  try 
    { use_facet<gnu_facet>(loc13); }
  catch(bad_cast& obj)
    { VERIFY( true ); }
  catch(...)
    { VERIFY( false ); }
}

int main()
{
  test01();
  return 0;
}
