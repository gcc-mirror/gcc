// 2000-08-17 Benjamin Kosnik <bkoz@cygnus.com>

// Copyright (C) 2000-2017 Free Software Foundation, Inc.
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

  const char* 		c_lit = "black pearl jasmine tea";
  const char* 	        from_next;
  int 			size = 23;
  char* 		c_arr = new char[size];
  char*                 c_ref = new char[size];
  char*			to_next;

  locale 		loc = locale::classic();
  c_codecvt::state_type state;
  const c_codecvt* 	cvt = &use_facet<c_codecvt>(loc); 

  // According to the resolution of DR19 (see also libstd++/9168), in
  // case of degenerate conversion ('noconv'), "there are no changes to
  // the values in [to, to_limit)."
  memset(c_ref, 'X', size);

  // out
  memset(c_arr, 'X', size);
  result r2 = cvt->out(state, c_lit, c_lit + size, from_next, 
		       c_arr, c_arr + size, to_next);
  VERIFY( r2 == codecvt_base::noconv );
  VERIFY( !memcmp(c_arr, c_ref, size) ); 
  VERIFY( from_next == c_lit );
  VERIFY( to_next == c_arr );

  delete [] c_arr;
  delete [] c_ref;
}

int main ()
{
  test01();
  return 0;
}
