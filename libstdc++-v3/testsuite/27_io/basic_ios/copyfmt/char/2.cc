// 1999-09-20 bkoz

// Copyright (C) 1999, 2003 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// 27.4.4.2 basic_ios member functions

// NB: Don't include any other headers in this file.
#include <ios>
#include <testsuite_hooks.h>

// copyfmt and locales.
void test03()
{
  bool test __attribute__((unused)) = true;

  using namespace std;

  typedef std::ios_base::fmtflags fmtflags;
  typedef std::ios_base::iostate iostate;
  locale loc_c = locale::classic();
  locale loc_de = __gnu_test::try_named_locale("de_DE");
  std::ios ios_01(NULL);
  std::ios ios_02(NULL);
  ios_01.imbue(loc_c);
  ios_02.imbue(loc_de);
  ios_02.setstate(ios_base::badbit);
  VERIFY( loc_c == ios_01.getloc() );
  VERIFY( loc_de == ios_02.getloc() );

  iostate ios1 = ios_01.rdstate();
  iostate ios2 = ios_02.rdstate();
  streambuf* sb1 = ios_01.rdbuf();
  streambuf* sb2 = ios_02.rdbuf();
  ios_01.copyfmt(ios_02);

  VERIFY( loc_de == ios_01.getloc() );
  VERIFY( ios_01.getloc() == ios_02.getloc() );
  VERIFY( ios1 == ios_01.rdstate() );
  VERIFY( ios2 == ios_02.rdstate() );
  VERIFY( sb1 == ios_01.rdbuf() );
  VERIFY( sb2 == ios_02.rdbuf() );
}

int main() 
{
  test03();
  return 0;
}
