// { dg-require-namedlocale "" }

// Copyright (C) 2009 Free Software Foundation
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
#include <testsuite_hooks.h>

// libstdc++/40184
void test01()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  using namespace std;
  bool test __attribute__((unused)) = true;

  locale locf(locale("C"), "ja_JP.eucjp", locale::monetary);

  const moneypunct<wchar_t, false>& mpf = 
    use_facet<moneypunct<wchar_t, false> >(locf);

  locale locf_copy(locf.name().c_str());
  const moneypunct<wchar_t, false>& mpf_copy = 
    use_facet<moneypunct<wchar_t, false> >(locf_copy);

  VERIFY( mpf.curr_symbol() == mpf_copy.curr_symbol() );

  locale loct(locale("C"), "ja_JP.eucjp", locale::monetary);

  const moneypunct<wchar_t, true>& mpt = 
    use_facet<moneypunct<wchar_t, true> >(loct);

  locale loct_copy(loct.name().c_str());
  const moneypunct<wchar_t, true>& mpt_copy = 
    use_facet<moneypunct<wchar_t, true> >(loct_copy);

  VERIFY( mpt.curr_symbol() == mpt_copy.curr_symbol() );
#endif
}

int main()
{
  test01();
  return 0;
}
