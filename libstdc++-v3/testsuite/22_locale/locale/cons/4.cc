// 2000-09-13 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation
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

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <cwchar> // for mbstate_t
#include <locale>
#include <stdexcept>
#include <testsuite_hooks.h>

// libstdc++/7811
void test03()
{
  bool test __attribute__((unused)) = true;
#ifdef _GLIBCXX_HAVE_SETENV 
  const char* LC_ALL_orig = getenv("LC_ALL");
  if (!setenv("LC_ALL", "it_IT", 1))
    {
      std::locale loc = __gnu_test::try_named_locale(""); 
      VERIFY( loc.name() == "it_IT" );
      setenv("LC_ALL", LC_ALL_orig ? LC_ALL_orig : "", 1);
    }
#endif
}

int main()
{
  test03();
  return 0;
}
