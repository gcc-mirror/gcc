// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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

// 27.6.1.3 unformatted input functions

#include <cwchar> // for wcslen
#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// [patch] bits/istream.tcc - getline(char_type*,streamsize,char_type)
// http://gcc.gnu.org/ml/libstdc++/2000-07/msg00003.html
void
test05()
{
  const wchar_t* charray = L"\n"
L"a\n"
L"aa\n"
L"aaa\n"
L"aaaa\n"
L"aaaaa\n"
L"aaaaaa\n"
L"aaaaaaa\n"
L"aaaaaaaa\n"
L"aaaaaaaaa\n"
L"aaaaaaaaaa\n"
L"aaaaaaaaaaa\n"
L"aaaaaaaaaaaa\n"
L"aaaaaaaaaaaaa\n"
L"aaaaaaaaaaaaaa\n";

  bool test __attribute__((unused)) = true;
  const std::streamsize it = 5;
  std::streamsize br = 0;
  wchar_t tmp[it];
  std::wstringbuf sb(charray, std::ios_base::in);
  std::wistream ifs(&sb);
  std::streamsize blen = std::wcslen(charray);
  VERIFY(!(!ifs));
  while(ifs.getline(tmp, it) || ifs.gcount())
    {
      br += ifs.gcount();
      if(ifs.eof())
        {
          // Just sanity checks to make sure we've extracted the same
          // number of chars that were in the streambuf
          VERIFY( br == blen );
          // Also, we should only set the failbit if we could
          // _extract_ no chars from the stream, i.e. the first read
          // returned EOF. 
          VERIFY( ifs.fail() && ifs.gcount() == 0 );
        }
      else if(ifs.fail())
        {
	  // delimiter not read
	  //
	  // either
	  // -> extracted no characters
	  // or
	  // -> n - 1 characters are stored
          ifs.clear(ifs.rdstate() & ~std::ios::failbit);
          VERIFY( (ifs.gcount() == 0) || (std::wcslen(tmp) == it - 1) );
          VERIFY( !(!ifs) );
          continue;
        }
      else 
        {
	  // delimiter was read.
	  //
	  // -> wcslen(__s) < n - 1 
	  // -> delimiter was seen -> gcount() > wcslen(__s)
          VERIFY( ifs.gcount() == static_cast<std::streamsize>(std::wcslen(tmp)
							       + 1) );
          continue;
        }
    }
}

int 
main()
{
  test05();
  return 0;
}
