// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

// Need to explicitly set the state(mbstate_t) to zero.
// How to do this is not specified by the ISO C99 standard, so we
// might need to add some operators to make the intuiative case
// work:
//   w_codecvt::state_type state00;
//   state00 = 0;  
// or, can use this explicit "C" initialization:
//   w_codecvt::state_type state01 = {0, 0};
// .. except Ulrich says: Use memset. Always use memset. Feel the force...
void
zero_state(std::mbstate_t& state)
{ std::memset(&state, 0, sizeof(std::mbstate_t)); }

// Required instantiation
// codecvt<wchar_t, char, mbstate_t>
//
// Test handling of '\0' characters in input
// libstdc++/9246
void test05()
{
  using namespace std;
  typedef codecvt<wchar_t, char, mbstate_t> 	w_codecvt;
  typedef codecvt_base::result			result;
  typedef wchar_t				int_type;
  typedef char					ext_type;
  typedef char_traits<char>			ext_traits;

  bool test __attribute__((unused)) = true;
  const ext_type 	e_lit[] = { '\0', '\0', 'f', '\0' };
  const int_type 	i_lit[] = { L'\0', L'\0', L'f', L'\0' };
  const int_type*       ifrom_next;
  int 			size = sizeof(e_lit);
  ext_type* 		e_arr = new ext_type[size + 1];
  ext_type* 		e_ref = new ext_type[size + 1];
  memset(e_arr, 0xf0, size + 1);
  memset(e_ref, 0xf0, size + 1);
  ext_type*		eto_next;

  locale 		loc;
  const w_codecvt* 	cvt = &use_facet<w_codecvt>(loc); 

  // out
  w_codecvt::state_type state02;
  zero_state(state02);  
  result r2 = cvt->out(state02, i_lit, i_lit + size, ifrom_next, 
		       e_arr, e_arr + size, eto_next);
  VERIFY( r2 == codecvt_base::ok );
  VERIFY( ifrom_next == i_lit + size );
  VERIFY( eto_next == e_arr + size );
  VERIFY( !ext_traits::compare(e_arr, e_lit, size) );
  VERIFY( !ext_traits::compare(eto_next, e_ref, 1) );

  delete [] e_arr;
  delete [] e_ref;
}

int main ()
{
  test05();
  return 0;
}
