// { dg-require-iconv "UCS-2BE" }
// { dg-require-iconv "ISO-8859-15" }
// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003, 2004, 2005, 2006, 2007, 2009 Free Software Foundation
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

// 22.2.1.5 - Template class codecvt [lib.locale.codecvt]

#include <locale>
#include <cstring>
#include <testsuite_hooks.h>
#include <ext/codecvt_specializations.h>

// Partial specialization using encoding_state
// codecvt<unicode_t, char, encoding_state>
// UNICODE - UCS2 (big endian)
void test01()
{
  using namespace std;
  typedef unsigned short       		      		int_type;
  typedef char						ext_type;
  typedef __gnu_cxx::encoding_state    			state_type;
  typedef codecvt<int_type, ext_type, state_type>	unicode_codecvt;

  bool test __attribute__((unused)) = true;
  const ext_type* 	e_lit = "black pearl jasmine tea";
  int 			size = strlen(e_lit);

  // construct a locale object with the specialized facet.
  locale 		loc(locale::classic(), new unicode_codecvt);
  // sanity check the constructed locale has the specialized facet.
  VERIFY( has_facet<unicode_codecvt>(loc) );
  const unicode_codecvt& cvt = use_facet<unicode_codecvt>(loc); 

  unicode_codecvt::state_type state04("UCS-2BE", "ISO-8859-15", 0xfeff, 0);
  cvt.length(state04, e_lit, e_lit + size, 5);
}

int main ()
{
  test01();
  return 0;
}
