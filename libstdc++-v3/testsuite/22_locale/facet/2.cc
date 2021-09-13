// { dg-require-namedlocale "es_MX.ISO8859-1" }

// 2000-08-31 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000-2021 Free Software Foundation, Inc.
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

// 22.1.1.1.2 - class locale::facet [lib.locale.facet]

#include <cwchar> // for mbstate_t
#include <locale>
#include <stdexcept>
#include <string>
#include <iterator>
#include <limits>
#include <testsuite_hooks.h>

// Static counter for use in checking ctors/dtors.
static std::size_t counter;

class surf : public std::locale::facet
{
public:
  static std::locale::id 	       	id;
  surf(size_t refs = 0): std::locale::facet(refs) { ++counter; }
  ~surf() { --counter; }
};

std::locale::id surf::id;

typedef surf facet_type;

void test02()
{
  using namespace std;

  // 1: Destroyed when out of scope.
  VERIFY( counter == 0 );
  {
    locale loc01(locale::classic(), new facet_type);
    VERIFY( counter == 1 );
  }
  VERIFY( counter == 0 );

  // 2: Not destroyed when out of scope, deliberately leaked.
  VERIFY( counter == 0 );
  {
    // Default refs argument is zero.
    locale loc02(locale::classic(), new facet_type(1));
    VERIFY( counter == 1 );
  }
  VERIFY( counter == 1 );

  // 3: Pathological.
  counter = 0;
  {
    // Test bounds.
    facet_type* f = new facet_type(numeric_limits<size_t>::max());
    VERIFY( counter == 1 );
    // Add a reference.
    locale loc01(locale::classic(), f);
    {
      // Add another reference...
      locale loc02(locale::classic(), f);
    }
    VERIFY( counter == 1 );
  }

  // 4: Named locale should destroy facets when it goes out of scope.
  // Not quite sure how to test for this w/o valgrind at the moment.
  {
    locale loc03 = locale(ISO_8859(1,es_MX));
  }
}

int main ()
{
  test02();
  return 0;
}
