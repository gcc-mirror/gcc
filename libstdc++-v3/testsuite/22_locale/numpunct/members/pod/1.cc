// 2003-07-09  Benjamin Kosnik  <bkoz@redhat.com>

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

#include <locale>
#include <sstream>
#include <ostream>
#include <stdexcept>
#include <ext/pod_char_traits.h>
#include <testsuite_hooks.h>


// Check for numpunct and ctype dependencies. Make sure that numpunct
// can be created without ctype.
void test01()
{
  using namespace std;
  using __gnu_test::pod_type;
  using __gnu_test::value_type;
  typedef numpunct<pod_type>::string_type 	string_type;
  typedef basic_stringbuf<pod_type> 	stringbuf_type;
  typedef basic_ostream<pod_type> 		ostream_type;
  
  bool test __attribute__((unused)) = true;

  // Pre-cache sanity check.
  const locale 	loc(locale::classic(), new numpunct<pod_type>);
  const numpunct<pod_type>& np = use_facet<numpunct<pod_type> >(loc);

  pod_type dp = np.decimal_point();
  pod_type ts = np.thousands_sep();
  string g = np.grouping();
  string_type strue = np.truename();
  string_type sfalse = np.falsename();

  pod_type basedp = { value_type('.') };
  pod_type basets = { value_type(',') };

  string_type basetrue(4, pod_type());
  basetrue[0].value = value_type('t');
  basetrue[1].value = value_type('r');
  basetrue[2].value = value_type('u');
  basetrue[3].value = value_type('e');

  string_type basefalse(5, pod_type());
  basefalse[0].value = value_type('f');
  basefalse[1].value = value_type('a');
  basefalse[2].value = value_type('l');
  basefalse[3].value = value_type('s');
  basefalse[4].value = value_type('e');

  VERIFY( char_traits<pod_type>::eq(dp, basedp) );
  VERIFY( char_traits<pod_type>::eq(ts, basets) );
  VERIFY( g == "" );
  VERIFY( strue == basetrue );
  VERIFY( sfalse == basefalse );
}

int main()
{
  test01();
  return 0;
}
