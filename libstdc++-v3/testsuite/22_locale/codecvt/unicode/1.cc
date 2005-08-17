// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003 Free Software Foundation
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

// 22.2.1.5 - Template class codecvt [lib.locale.codecvt]

#include <locale>
#include <testsuite_hooks.h>



#ifdef _GLIBCXX_USE___ENC_TRAITS

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
      {  return static_cast<char_type*>(memcpy(__s1, __s2, __n)); }

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

void
initialize_state(std::__enc_traits& state)
{ state._M_init(); }

bool length_called = false;

class length_codecvt : public std::codecvt<unicode_t, char, std::__enc_traits>
{
  typedef std::codecvt<unicode_t, char, std::__enc_traits> unicode_codecvt;

public:
  // DR75: type of first argument of do_length is state_type&
  virtual int do_length(state_type& state, const extern_type* from,
                        const extern_type* end, std::size_t max) const
  {
    length_called = true;
    return unicode_codecvt::do_length(state, from, end, max);
  }
};

// Partial specialization using __enc_traits.
// codecvt<unicode_t, char, __enc_traits>
// UNICODE - UCS2 (big endian)
void test01()
{
  using namespace std;
  typedef unicode_t				int_type;
  typedef char					ext_type;
  typedef __enc_traits				enc_type;
  typedef codecvt<int_type, ext_type, enc_type>	unicode_codecvt;

  bool test __attribute__((unused)) = true;
  const ext_type* 	e_lit = "black pearl jasmine tea";
  int 			size = strlen(e_lit);

  // construct a locale object with the specialized facet.
  locale 		loc(locale::classic(), new length_codecvt);
  // sanity check the constructed locale has the specialized facet.
  VERIFY( has_facet<unicode_codecvt>(loc) );
  const unicode_codecvt&	cvt = use_facet<unicode_codecvt>(loc); 

  unicode_codecvt::state_type state04("UCS-2BE", "ISO-8859-15", 0xfeff, 0);
  initialize_state(state04);
  length_called = false;
  cvt.length(state04, e_lit, e_lit + size, 5);
  VERIFY( length_called );
}
#endif // _GLIBCXX_USE___ENC_TRAITS

int main ()
{
#if _GLIBCXX_USE___ENC_TRAITS
  test01();
#endif 

  return 0;
}
