// { dg-require-namedlocale "en_US.UTF-8" }

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
// Test handling of illegal input sequence in UTF-8.
void test07()
{
  using namespace std;
  typedef codecvt<wchar_t, char, mbstate_t> 	w_codecvt;
  typedef wchar_t				int_type;
  typedef char					ext_type;

  const ext_type* 	e_lit = "a\xc0\xff";
  int 			size = strlen(e_lit);

  locale loc = locale("en_US.UTF-8");
  locale::global(loc);
  const w_codecvt* 	cvt = &use_facet<w_codecvt>(loc); 

  w_codecvt::state_type state01;
  zero_state(state01);
  int i = cvt->length(state01, e_lit, e_lit + size, 1);
  VERIFY( i == 1 );

  w_codecvt::state_type state02;
  zero_state(state02);
  int j = cvt->length(state02, e_lit, e_lit + size, size);
  VERIFY( j == 1 );

  w_codecvt::state_type state03;
  zero_state(state03);
  int k = cvt->length(state03, e_lit, e_lit + size, size * 2);
  VERIFY( k == 1 );
}

int main ()
{
  test07();
  return 0;
}
