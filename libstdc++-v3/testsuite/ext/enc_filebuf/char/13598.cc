// Copyright (C) 2004 Free Software Foundation
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
#include <cstring>
#include <cassert>
#ifdef _GLIBCXX_USE___ENC_TRAITS
#include <ext/enc_filebuf.h>
#endif

int main()
{
#ifdef _GLIBCXX_USE___ENC_TRAITS
  const char* str = "Hello, world!\n";

  std::locale loc(std::locale::classic(),
		  new std::codecvt<char, char, std::__enc_traits>());
  std::__enc_traits st("ISO-8859-1", "ISO-8859-1");
  __gnu_cxx::enc_filebuf<char> fb(st);
  fb.pubimbue(loc);

  fb.open("tmp_13598", std::ios_base::out);
  std::streamsize n = fb.sputn(str, std::strlen(str));
  int s = fb.pubsync();
  fb.close();
  
  assert(n == std::strlen(str));
  assert(s == 0);
#endif
  
  return 0;
}
