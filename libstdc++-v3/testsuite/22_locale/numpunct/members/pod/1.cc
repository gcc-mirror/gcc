// 2003-07-09  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

#include <locale>
#include <sstream>
#include <ostream>
#include <stdexcept>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

// Check for numpunct and ctype dependencies. Make sure that numpunct
// can be created without ctype.
void test01()
{
  using namespace std;
  using __gnu_test::pod_ushort;
  typedef pod_ushort::value_type value_type;
  typedef numpunct<pod_ushort>::string_type 	string_type;
  typedef basic_stringbuf<pod_ushort> 	stringbuf_type;
  typedef basic_ostream<pod_ushort> 		ostream_type;

  // Pre-cache sanity check.
  const locale 	loc(locale::classic(), new numpunct<pod_ushort>);
  const numpunct<pod_ushort>& np = use_facet<numpunct<pod_ushort> >(loc);

  pod_ushort dp = np.decimal_point();
  pod_ushort ts = np.thousands_sep();
  string g = np.grouping();
  string_type strue = np.truename();
  string_type sfalse = np.falsename();

  pod_ushort basedp = { value_type('.') };
  pod_ushort basets = { value_type(',') };

  string_type basetrue(4, pod_ushort());
  basetrue[0].value = value_type('t');
  basetrue[1].value = value_type('r');
  basetrue[2].value = value_type('u');
  basetrue[3].value = value_type('e');

  string_type basefalse(5, pod_ushort());
  basefalse[0].value = value_type('f');
  basefalse[1].value = value_type('a');
  basefalse[2].value = value_type('l');
  basefalse[3].value = value_type('s');
  basefalse[4].value = value_type('e');

  VERIFY( char_traits<pod_ushort>::eq(dp, basedp) );
  VERIFY( char_traits<pod_ushort>::eq(ts, basets) );
  VERIFY( g == "" );
  VERIFY( strue == basetrue );
  VERIFY( sfalse == basefalse );
}

int main()
{
  test01();
  return 0;
}
