// { dg-require-namedlocale "" }

// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <locale>
#include <stdexcept>
#include <cstdlib>
#include <testsuite_hooks.h>

class MyFacet : public std::locale::facet
{
public:
  static std::locale::id id;
};

std::locale::id MyFacet::id;

// libstdc++/12438
void test01(int iters)
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  
  for (int i = 0; i < iters; ++i)
    {
      try
	{
	  locale loc1 = locale::classic();
	  locale loc2 = locale("");
	  VERIFY( !has_facet<MyFacet>(loc2) );
	  
	  loc1.combine<MyFacet>(loc2);
	  VERIFY( false );
	}
      catch (std::runtime_error&)
	{
	}
    }
}

int main(int argc, char* argv[])
{
  // We leaked ~400-500 bytes/iter.
  __gnu_test::set_memory_limits(10.0);
  int iters = 30000;

  if (argc > 1)
    iters = atoi(argv[1]);
  if (iters < 1)
    iters = 1;
  test01(iters);

  return 0;
}
