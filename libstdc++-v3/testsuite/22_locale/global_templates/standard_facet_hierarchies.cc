// Copyright (C) 2007-2013 Free Software Foundation, Inc.
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

#include <string>
#include <locale>
#include <testsuite_hooks.h>

// Based on Langer Kreft "Standard C++ IOStreams and Locales" p 316-318
// PR libstdc++/30127
// PR libstdc++/34449
int main()
{
  bool test __attribute__((unused)) = true;

  using std::locale;
  using std::has_facet;
  using std::use_facet;
  typedef std::ctype<char> base_facet;
  typedef std::ctype_byname<char> derived_facet;

  locale loc_c = locale::classic();
  locale loc_base = loc_c;
  locale loc_derived(loc_c, new derived_facet("C"));

  // Standard base facet.
  VERIFY( has_facet<base_facet>(loc_c) );
  VERIFY( has_facet<base_facet>(loc_base) );
  VERIFY( has_facet<base_facet>(loc_derived) );

  // Standard derived facet.
  VERIFY( !has_facet<derived_facet>(loc_c) );
  VERIFY( !has_facet<derived_facet>(loc_base) );
  VERIFY( has_facet<derived_facet>(loc_derived) );


  // 1
  try
    {
      if (has_facet<derived_facet>(loc_base))
	{
	  use_facet<derived_facet>(loc_base).widen('k');
	  VERIFY( true );
	}
    }
  catch (...)
    { 
      // Expect no exception.
      VERIFY( true );
    }

  // 2
  try
    {
      if (has_facet<base_facet>(loc_derived))
	use_facet<base_facet>(loc_derived).widen('k');
      else
	VERIFY( true );
    }
  catch (...)
    { 
      // Expect no exception.
      VERIFY( true );
    }

  return 0;
}
