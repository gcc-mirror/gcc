// 2000-09-13 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000, 2002, 2003, 2009 Free Software Foundation
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

// 22.1.1.5 locale static members [lib.locale.statics]

#include <cwchar> // for mbstate_t
#include <locale>
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

// Verify lifetimes of global objects.
void test03()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  string name;
  locale global_orig;
  // 1: Destroyed when out of scope.
  {
    {
      {
	VERIFY( counter == 0 );
	{
	  locale loc01(locale::classic(), new facet_type);
	  VERIFY( counter == 1 );
	  global_orig = locale::global(loc01);
	  name = loc01.name();
	}
	VERIFY( counter == 1 );
	locale loc02 = locale();
	// Weak, but it's something...
	VERIFY( loc02.name() == name );
      }
      VERIFY( counter == 1 );
      // NB: loc03 should be a copy of the previous global locale.
      locale loc03 = locale::global(global_orig);
      VERIFY( counter == 1 );
      VERIFY( loc03.name() == name );
    }
    VERIFY( counter == 0 );
    locale loc04 = locale();
    VERIFY( loc04 == global_orig );
  }

  // 2: Not destroyed when out of scope, deliberately leaked.
  {
    {
      {
	VERIFY( counter == 0 );
	{
	  locale loc01(locale::classic(), new facet_type(1));
	  VERIFY( counter == 1 );
	  global_orig = locale::global(loc01);
	  name = loc01.name();
	}
	VERIFY( counter == 1 );
	locale loc02 = locale();
	// Weak, but it's something...
	VERIFY( loc02.name() == name );
      }
      VERIFY( counter == 1 );
      // NB: loc03 should be a copy of the previous global locale.
      locale loc03 = locale::global(global_orig);
      VERIFY( counter == 1 );
      VERIFY( loc03.name() == name );
    }
    VERIFY( counter == 1 );
    locale loc04 = locale();
    VERIFY( loc04 == global_orig );
  }
  VERIFY( counter == 1 );

  // Restore global settings.
  locale::global(global_orig);
}

int main ()
{
  test03();
  return 0;
}
