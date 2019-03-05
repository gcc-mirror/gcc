// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

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
  typedef char_traits<wchar_t>			int_traits;

  const ext_type 	e_lit[] = { '\0', '\0', 'f', '\0' };
  const ext_type*       efrom_next;
  const int_type 	i_lit[] = { L'\0', L'\0', L'f', L'\0' };
  size_t 		size = sizeof(e_lit);
  int_type* 		i_arr = new int_type[size + 1];
  int_type* 		i_ref = new int_type[size + 1];
  wmemset(i_arr, static_cast<wchar_t>(0xdeadbeef), size + 1);
  wmemset(i_ref, static_cast<wchar_t>(0xdeadbeef), size + 1);
  int_type*		ito_next;

  locale 		loc;
  const w_codecvt* 	cvt = &use_facet<w_codecvt>(loc); 

  // in
  w_codecvt::state_type state01;
  zero_state(state01);
  result r1 = cvt->in(state01, e_lit, e_lit + size, efrom_next, 
		      i_arr, i_arr + size, ito_next);
  VERIFY( r1 == codecvt_base::ok );
  VERIFY( efrom_next == e_lit + size );
  VERIFY( ito_next == i_arr + size );
  VERIFY( !int_traits::compare(i_arr, i_lit, size) ); 
  VERIFY( !int_traits::compare(ito_next, i_ref, 1) );

  delete [] i_arr;
  delete [] i_ref;
}

int main ()
{
  test05();
  return 0;
}
