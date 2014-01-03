// 1999-08-11 bkoz

// Copyright (C) 1999-2014 Free Software Foundation, Inc.
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

#include <cstring> // for strlen
#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// [patch] bits/istream.tcc - getline(char_type*,streamsize,char_type)
// http://gcc.gnu.org/ml/libstdc++/2000-07/msg00003.html
void
test05()
{
  const char* charray = "\n"
"a\n"
"aa\n"
"aaa\n"
"aaaa\n"
"aaaaa\n"
"aaaaaa\n"
"aaaaaaa\n"
"aaaaaaaa\n"
"aaaaaaaaa\n"
"aaaaaaaaaa\n"
"aaaaaaaaaaa\n"
"aaaaaaaaaaaa\n"
"aaaaaaaaaaaaa\n"
"aaaaaaaaaaaaaa\n";

  bool test __attribute__((unused)) = true;
  const std::streamsize it = 5;
  std::streamsize br = 0;
  char tmp[it];
  std::stringbuf sb(charray, std::ios_base::in);
  std::istream ifs(&sb);
  std::streamsize blen = std::strlen(charray);
  VERIFY(!(!ifs));
  while(ifs.getline(tmp, it) || ifs.gcount())
    {
      br += ifs.gcount();
      if(ifs.eof())
        {
          // Just sanity checks to make sure we've extracted the same
          // number of chars that were in the streambuf
          VERIFY(br == blen);
          // Also, we should only set the failbit if we could
          // _extract_ no chars from the stream, i.e. the first read
          // returned EOF. 
          VERIFY(ifs.fail() && ifs.gcount() == 0);
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
          VERIFY((ifs.gcount() == 0) || (std::strlen(tmp) == it - 1));
          VERIFY(!(!ifs));
          continue;
        }
      else 
        {
	  // delimiter was read.
	  //
	  // -> strlen(__s) < n - 1 
	  // -> delimiter was seen -> gcount() > strlen(__s)
          VERIFY(ifs.gcount() == static_cast<std::streamsize>(std::strlen(tmp) + 1) );
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
