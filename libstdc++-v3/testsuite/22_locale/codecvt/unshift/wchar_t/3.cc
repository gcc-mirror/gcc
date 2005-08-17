// { dg-require-namedlocale "" }

// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003, 2005 Free Software Foundation
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
void test03()
{
  using namespace std;
  typedef codecvt<wchar_t, char, mbstate_t> 	w_codecvt;
  typedef codecvt_base::result			result;
  typedef wchar_t				int_type;
  typedef char					ext_type;
  typedef char_traits<char>			ext_traits;

  bool test __attribute__((unused)) = true;
  const int_type 	i_lit[] = {
    0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc,
    0xd, 0xe, 0xf, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, L'!',
    L'"', L'#', L'$', L'%', L'&', L'\'', L'(', L')', L'*', L'+',
    L',', L'-', L'.', L'/', L'0', L'1', L'2', L'3', L'4', L'5',
    L'6', L'7', L'8', L'9', L':', L';', L'<', L'=', L'>', L'?',
    L'@', L'A', L'B', L'C', L'D', L'E', L'F', L'G', L'H', L'I',
    L'J', L'K', L'L', L'M', L'N', L'O', L'P', L'Q', L'R', L'S',
    L'T', L'U', L'V', L'W', L'X', L'Y', L'Z', L'[', L'\\', L']',
    L'^', L'_', L'`', L'a', L'b', L'c', L'd', L'e', L'f', L'g',
    L'h', L'i', L'j', L'k', L'l', L'm', L'n', L'o', L'p', L'q',
    L'r', L's', L't', L'u', L'v', L'w', L'x', L'y', L'z', L'{',
    L'|', L'}', L'~', 0x7f, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85,
    0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99,
    0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3,
    0x20ac, 0xa5, 0x160, 0xa7, 0x161, 0xa9, 0xaa, 0xab, 0xac, 0xad,
    0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0x17d, 0xb5, 0xb6, 0xb7,
    0x17e, 0xb9, 0xba, 0xbb, 0x152, 0x153, 0x178, 0xbf, 0xc0, 0xc1,
    0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb,
    0xcc, 0xcd, 0xce, 0xcf, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5,
    0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9,
    0xea, 0xeb, 0xec, 0xed, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3,
    0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd,
    0xfe, 0xff, 0x0
  };

  const int_type*       ifrom_next;
  int 			size = wcslen(i_lit);
  ext_type* 		e_arr = new ext_type[size + 1];
  ext_type* 		e_ref = new ext_type[size + 1];
  memset(e_arr, 0xf0, size + 1);
  memset(e_ref, 0xf0, size + 1);
  ext_type*		eto_next;

  locale loc = locale("en_US.ISO-8859-15");
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
  test03();
  return 0;
}
