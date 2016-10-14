// Before Solaris 11, iconv -f ISO-8859-1 -t ISO-8859-1 fails with
// Not supported ISO-8859-1 to ISO-8859-1
//
// { dg-do run { xfail *-*-solaris2.10 } }
// { dg-require-iconv "ISO-8859-1" }

// Copyright (C) 2004-2016 Free Software Foundation, Inc.
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
#include <cstring>
#include <cstddef>
#include <testsuite_hooks.h>
#include <ext/enc_filebuf.h>

int main()
{
  typedef char char_type;
  typedef __gnu_cxx::enc_filebuf<char_type> filebuf_type;
  typedef filebuf_type::state_type state_type;

  const char* str = "Hello, world!\n";
  std::locale loc(std::locale::classic(),
		  new std::codecvt<char, char, __gnu_cxx::encoding_state>());
  state_type st("ISO-8859-1", "ISO-8859-1");
  filebuf_type fb(st);
  fb.pubimbue(loc);

  fb.open("tmp_13598", std::ios_base::out);
  std::streamsize n = fb.sputn(str, std::strlen(str));
  int s = fb.pubsync();
  fb.close();

  VERIFY( std::size_t(n) == std::strlen(str) );
  VERIFY( s == 0 );
  
  return 0;
}
