// 2000-08-17 Benjamin Kosnik <bkoz@cygnus.com>

// Copyright (C) 2000-2021 Free Software Foundation, Inc.
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

// Required instantiation, degenerate conversion.
// codecvt<char, char, mbstate_t>
void test01()
{
  using namespace std;
  typedef codecvt_base::result			result;
  typedef codecvt<char, char, mbstate_t> 	c_codecvt;

  int 			size = 25;
  char* 		c_arr = new char[size];
  char*                 c_ref = new char[size];
  locale 		loc = locale::classic();
  const c_codecvt* 	cvt = &use_facet<c_codecvt>(loc); 

  // According to the resolution of DR19 (see also libstd++/9168), in
  // case of degenerate conversion ('noconv'), "there are no changes to
  // the values in [to, to_limit)."
  memset(c_ref, 'X', size);

  int k = cvt->max_length();
  VERIFY( k == 1 );

  delete [] c_arr;
  delete [] c_ref;
}

int main ()
{
  test01();
  return 0;
}
