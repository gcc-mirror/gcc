// Copyright (C) 2000-2014 Free Software Foundation, Inc.
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


// 22.2.1.3.2 ctype<char> members

#include <locale>
#include <testsuite_hooks.h>

typedef char char_type;

// Per Liboriussen <liborius@stofanet.dk>
void test03()
{
  bool test __attribute__((unused)) = true;
  std::ctype_base::mask maskdata[256];
  for (int i = 0; i < 256; ++i)
    maskdata[i] = std::ctype_base::alpha;
  std::ctype<char>* f = new std::ctype<char>(maskdata);
  std::locale loc_c = std::locale::classic();
  std::locale loc(loc_c, f);
  for (int i = 0; i < 256; ++i) 
    {
      char_type ch = i;
      VERIFY( std::isalpha(ch, loc) );
    }
}

int main() 
{
  test03();
  return 0;
}
