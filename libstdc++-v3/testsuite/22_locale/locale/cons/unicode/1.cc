// Copyright (C) 2006 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <cwchar> // for mbstate_t
#include <locale>
#include <stdexcept>
#include <testsuite_hooks.h>

#if _GLIBCXX_USE___ENC_TRAITS
typedef std::codecvt<char, char, std::mbstate_t> 	      c_codecvt;

#ifdef _GLIBCXX_USE_WCHAR_T
typedef std::codecvt<wchar_t, char, std::mbstate_t>	      w_codecvt;
#endif

class gnu_facet: public std::locale::facet
{
public:
  static std::locale::id id;
};

std::locale::id gnu_facet::id;

// Need some char_traits specializations for this to work.
typedef unsigned short			unicode_t;

namespace std
{
  template<>
    struct char_traits<unicode_t>
    {
      typedef unicode_t 	char_type;
      // Unsigned as wint_t is unsigned.
      typedef unsigned long  	int_type;
      typedef streampos 	pos_type;
      typedef streamoff 	off_type;
      typedef mbstate_t 	state_type;
      
      static void 
      assign(char_type& __c1, const char_type& __c2);

      static bool 
      eq(const char_type& __c1, const char_type& __c2);

      static bool 
      lt(const char_type& __c1, const char_type& __c2);

      static int 
      compare(const char_type* __s1, const char_type* __s2, size_t __n)
      { return memcmp(__s1, __s2, __n); }

      static size_t
      length(const char_type* __s);

      static const char_type* 
      find(const char_type* __s, size_t __n, const char_type& __a);

      static char_type* 
      move(char_type* __s1, const char_type* __s2, size_t __n);

      static char_type* 
      copy(char_type* __s1, const char_type* __s2, size_t __n)
      { return static_cast<char_type*>(memcpy(__s1, __s2, __n)); }

      static char_type* 
      assign(char_type* __s, size_t __n, char_type __a);

      static char_type 
      to_char_type(const int_type& __c);

      static int_type 
      to_int_type(const char_type& __c);

      static bool 
      eq_int_type(const int_type& __c1, const int_type& __c2);

      static int_type 
      eof(); 

      static int_type 
      not_eof(const int_type& __c);
    };
}

void test01()
{
  using namespace std;
  typedef unicode_t				int_type;
  typedef char					ext_type;
  typedef __enc_traits				enc_type;
  typedef codecvt<int_type, ext_type, enc_type>	unicode_codecvt;

  bool test __attribute__((unused)) = true;

  // unicode_codecvt
  locale loc13(locale::classic(), new unicode_codecvt);  
  VERIFY( loc01 != loc13 );
  VERIFY( loc13.name() == "*" );
  try 
    {
      VERIFY( has_facet<c_codecvt>(loc13) );
#ifdef _GLIBCXX_USE_WCHAR_T
      VERIFY( has_facet<w_codecvt>(loc13) );
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
#endif // _GLIBCXX_USE___ENC_TRAITS

int main()
{
#if _GLIBCXX_USE___ENC_TRAITS
  test01();
#endif 
  return 0;
}
