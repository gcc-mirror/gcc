// 1999-07-01 bkoz

// Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 21.3.7.9 inserters and extractors

// NB: This file is predicated on sstreams, istreams, and ostreams
// working, not to mention other major details like char_traits, and
// all of the string class.

#include <string>
#include <sstream>
#include <testsuite_hooks.h>

// istringstream/stringbuf extractor properly size buffer based on
// actual, not allocated contents (string.size() vs. string.capacity()).
// http://gcc.gnu.org/ml/libstdc++/1999-q4/msg00049.html
void test06(void)
{
  bool test __attribute__((unused)) = true;

  typedef std::wstring::size_type size_type;
  std::wstring str01(L"@silent");
  size_type i01 = str01.size();
  size_type i02 = str01.capacity();
  str01.erase(0, 1);
  size_type i03 = str01.size();
  size_type i04 = str01.capacity();
  VERIFY( i01 - 1 == i03 );
  VERIFY( i02 >= i04 );

  std::wistringstream is(str01);
  std::wstring str02;
  is >> str02 >> std::ws;
  size_type i05 = str02.size();
  size_type i06 = str02.capacity();
  VERIFY( i05 == i03 );
  VERIFY( i06 <= i04 );
}

int main()
{ 
  test06();
  return 0;
}
