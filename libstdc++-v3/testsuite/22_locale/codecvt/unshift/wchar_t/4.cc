// { dg-require-namedlocale "en_US.UTF-8" }

// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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
void test04()
{
  using namespace std;
  typedef codecvt<wchar_t, char, mbstate_t> 	w_codecvt;
  typedef codecvt_base::result			result;
  typedef wchar_t				int_type;
  typedef char					ext_type;
  typedef char_traits<char>			ext_traits;

  const int_type 	i_lit[] = {
    0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc,
    0xd, 0xe, 0xf, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, L'!',
    L'"', L'#', L'$', L'%', L'&', L'\'', L'(', L')', L'*', L'+',
    L',', L'-', L'.', L'/', L'0', L'1', L'2', L'3', L'4', L'5',
    L'6', L'7', L'8', L'9', L':', L';', L'<', L'=', L'>', L'?',
    L'@', L'}', L'~', 0x7f, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85,
    0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99,
    0x9a, 0x9b, 0x9c, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff, 0x100,
    0x101, 0x102, 0x103, 0x104, 0x105, 0x106, 0x107, 0x108, 0x109,
    0x10a, 0x10b, 0x10c, 0x10d, 0x10e, 0x10f, 0x110, 0x111, 0x112,
    0x113, 0x114, 0x115, 0x116, 0x117, 0x118, 0x119, 0x7f8, 0x7f9,
    0x7fa, 0x7fb, 0x7fc, 0x7fd, 0x7fe, 0x7ff, 0x800, 0x801, 0x802,
    0x803, 0x804, 0x805, 0x806, 0x807, 0x808, 0x809, 0x80a, 0x80b,
    0x80c, 0x80d, 0x80e, 0x80f, 0x810, 0x811, 0x812, 0x813, 0x814,
    0x815, 0x816, 0x817, 0x1, 0x2, 0x4, 0x8, 0x10, 0x20, L'@',
    0x80, 0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000, 0x4000, 0x8000,
    0x10000, 0x20000, 0x40000, 0x80000, 0x100000, 0x0
  };

  const int_type*       ifrom_next;
  int 			size = wcslen(i_lit);
  ext_type* 		e_arr = new ext_type[size + 1];
  ext_type* 		e_ref = new ext_type[size + 1];
  memset(e_arr, 0xf0, size + 1);
  memset(e_ref, 0xf0, size + 1);
  ext_type*		eto_next;

  locale loc = locale("en_US.UTF-8");
  locale::global(loc);
  const w_codecvt* 	cvt = &use_facet<w_codecvt>(loc); 

  // unshift
  w_codecvt::state_type state01;
  zero_state(state01);
  result r1 = cvt->unshift(state01, e_arr, e_arr + size, eto_next);
  VERIFY( r1 == codecvt_base::noconv );
  VERIFY( !ext_traits::compare(e_arr, e_ref, size + 1) ); 
  VERIFY( eto_next == e_arr );

  for (int i = 0; i < size; ++i)
    {
      w_codecvt::state_type state02;
      zero_state(state02);
      cvt->out(state02, i_lit + i, i_lit + i + 1, ifrom_next, 
	       e_arr, e_arr + size, eto_next);
      memset(e_arr, 0xf0, size + 1);
      result r2 = cvt->unshift(state02, e_arr, e_arr + size, eto_next);
      VERIFY( r2 == codecvt_base::noconv );
      VERIFY( !ext_traits::compare(e_arr, e_ref, size + 1) ); 
      VERIFY( eto_next == e_arr );
    }

  delete [] e_arr;
  delete [] e_ref;
}

int main ()
{
  test04();
  return 0;
}
