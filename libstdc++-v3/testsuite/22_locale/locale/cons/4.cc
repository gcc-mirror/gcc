// { dg-require-namedlocale "it_IT.ISO8859-15" }

// 2000-09-13 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000-2017 Free Software Foundation, Inc.
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

#include <cwchar> // for mbstate_t
#include <cstdlib>
#include <locale>
#include <stdexcept>
#include <testsuite_hooks.h>

// libstdc++/7811
void test03()
{
#ifdef _GLIBCXX_HAVE_SETENV 
  const char* LC_ALL_orig = getenv("LC_ALL");
  if (!setenv("LC_ALL", ISO_8859(15,it_IT), 1))
    {
      std::locale loc = std::locale(""); 
      VERIFY( loc.name() == ISO_8859(15,it_IT) );
      setenv("LC_ALL", LC_ALL_orig ? LC_ALL_orig : "", 1);
    }
#endif
}

int main()
{
  test03();
  return 0;
}
