// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003 Free Software Foundation
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

// 22.2.1.5 - Template class codecvt [lib.locale.codecvt]

#include <locale>
#include <testsuite_hooks.h>

// Required instantiation
// codecvt<wchar_t, char, mbstate_t>
//
// Baseline test in UTF-8 locale
void test04()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  typedef codecvt<wchar_t, char, mbstate_t> 	w_codecvt;

  locale loc = __gnu_test::try_named_locale("en_US.UTF-8");
  locale::global(loc);
  const w_codecvt* 	cvt = &use_facet<w_codecvt>(loc); 

  VERIFY( !cvt->always_noconv() );
}

int main ()
{
  test04();
  return 0;
}
