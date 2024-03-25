// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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
struct base_facet: public std::locale::facet
{
  virtual std::string msg() const
  { return "base class"; }

  static std::locale::id id;
};

std::locale::id base_facet::id;


struct derived_facet: public base_facet
{
  virtual std::string msg() const
  { return "derived class"; }

  virtual std::string msg_repeater() const
  { return "derived class derived class"; }

};

// PR libstdc++/30127
// PR libstdc++/34449
int main()
{
  using std::locale;
  using std::has_facet;
  using std::use_facet;

  locale loc_c = locale::classic();
  locale loc_base(loc_c, new base_facet);
  locale loc_derived(loc_c, new derived_facet);

  // Standard facets.
  VERIFY( has_facet<std::ctype<char> >(loc_c) );
  VERIFY( has_facet<std::ctype<char> >(loc_base) );
  VERIFY( has_facet<std::ctype<char> >(loc_derived) );

  // User defined base facet.
  VERIFY( !has_facet<base_facet>(loc_c) );
  VERIFY( has_facet<base_facet>(loc_base) );
  VERIFY( has_facet<base_facet>(loc_derived) );

  // User defined derived facet.
  VERIFY( !has_facet<derived_facet>(loc_c) );
#if __cpp_rtti
  VERIFY( !has_facet<derived_facet>(loc_base) );
  VERIFY( has_facet<derived_facet>(loc_derived) );


  // 1
  try
    {
      if (has_facet<derived_facet>(loc_base))
	{
	  use_facet<derived_facet>(loc_base).msg_repeater();
	  VERIFY( false );
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
	use_facet<base_facet>(loc_derived).msg();
      else
	VERIFY( true );
    }
  catch (...)
    { 
      // Expect no exception.
      VERIFY( true );
    }
#endif

  return 0;
}
