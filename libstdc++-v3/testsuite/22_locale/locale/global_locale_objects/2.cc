// 2000-09-13 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000, 2002, 2003 Free Software Foundation
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

// 22.1.1.5 locale static members [lib.locale.statics]

#include <cwchar> // for mbstate_t
#include <locale>
#include <testsuite_hooks.h>

// Sanity check locale::global(loc) and setlocale.
void test02()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  
  const string ph("en_PH");
  const string mx("es_MX");
  const char* orig = setlocale(LC_ALL, NULL);
  const char* testph = setlocale(LC_ALL, ph.c_str());
  const char* testmx = setlocale(LC_ALL, mx.c_str());
  setlocale(LC_ALL, orig);

  // If the underlying locale doesn't support these names, setlocale
  // won't be reset. Therefore, disable unless we know these specific
  // named locales work.
  if (testph && testmx)
    {
      const locale loc_ph = __gnu_test::try_named_locale(ph.c_str());
      const locale loc_mx = __gnu_test::try_named_locale(mx.c_str());
      
      // Use setlocale between two calls to locale("")
      const locale loc_env_1 = __gnu_test::try_named_locale("");
      setlocale(LC_ALL, ph.c_str());
      const locale loc_env_2 = __gnu_test::try_named_locale("");
      VERIFY( loc_env_1 == loc_env_2 );
      
      // Change global locale.
      locale global_orig = locale::global(loc_mx);
      const char* lc_all_mx = setlocale(LC_ALL, NULL);
      if (lc_all_mx)
	{
	  VERIFY( mx == lc_all_mx );
	}
      
      // Restore global settings.
      locale::global(global_orig);
    }
}

int main ()
{
  test02();
  return 0;
}
