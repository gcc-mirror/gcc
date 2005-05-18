// { dg-require-namedlocale "" }

// Copyright (C) 2003, 2005 Free Software Foundation, Inc.
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

// 27.5.2.4.1 Locales

#include <streambuf>
#include <locale>
#include <testsuite_hooks.h>

class Buf2 : public std::streambuf
{
public:
  std::locale before;
  std::locale after;

protected:
  void imbue(const std::locale& loc)
  {
    before = getloc();

    std::streambuf::imbue(loc);

    after = getloc();
  }
};

// libstdc++/13007
void test02()
{
  bool test __attribute__((unused)) = true;

  Buf2 buf;
  std::locale loc(std::locale("en_US"));

  buf.pubimbue(loc);

  VERIFY( buf.getloc() == loc );
  VERIFY( buf.before == std::locale::classic() );
  VERIFY( buf.after == std::locale::classic() );
}

int main()
{
  test02();
  return 0;
}
