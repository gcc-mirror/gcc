// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

// Copyright (C) 2010-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 28.3 Requirements [re.req]
// 28.2(4) Table 127 - Regular expression traits class requirements
// 28.7(9) Class template regex_traits [re.traits]

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  typedef wchar_t CharT;
  typedef std::regex_traits<CharT> traits;

  wchar_t n1[] = L"lower";
  wchar_t n2[] = L"alpha";
  traits t;

#define range(s) s, s+sizeof(s)/sizeof(s[0])-1
  traits::char_class_type c1 = t.lookup_classname(range(n1));
  VERIFY( c1 != 0 );

  traits::char_class_type c2 = t.lookup_classname(range(n1), true);
  traits::char_class_type c3 = t.lookup_classname(range(n2), true);
  VERIFY( c2 == c3 );
#undef range
}

void
test04()
{
  // Transform "string" into "StRiNg"
  auto mix_case = [](std::wstring s) {
    int i = 0;
    for (auto& ch : s)
      if (++i % 2)
	ch = toupper(ch);
    return s;
  };

  typedef wchar_t CharT;
  typedef std::regex_traits<CharT> traits;
  traits t;

  auto lookup = [&t](const std::wstring& s, bool icase) {
    return t.lookup_classname(s.begin(), s.end(), icase);
  };

  VERIFY( lookup(L"", false) == 0 );
  VERIFY( lookup(L":::not a valid classname:::", false) == 0 );
  VERIFY( lookup(L":::not a valid classname:::", true) == 0 );
  VERIFY( lookup(L"alnu", false) == 0 );
  VERIFY( lookup(L"alnumb", false) == 0 );
  VERIFY( lookup(L"x", false) == 0 );
  VERIFY( lookup(L"di", false) == 0 );
  VERIFY( lookup(L"digi", false) == 0 );
  VERIFY( lookup(L"digix", false) == 0 );
  VERIFY( lookup(std::wstring{L"d\0i", 3}, false) == 0 );
  VERIFY( lookup(std::wstring{L"digit\0", 6}, false) == 0 );
  VERIFY( lookup(std::wstring{L"digit\0bad", 9}, false) == 0 );

  for (std::wstring cls : { L"alnum", L"alpha", L"blank", L"cntrl", L"digit",
			    L"graph", L"lower", L"print", L"punct", L"space",
			    L"upper", L"xdigit", L"d", L"s", L"w" })
    {
      traits::char_class_type val = lookup(cls, false);
      // val should be non-zero:
      VERIFY( val != 0 );
      // val is independent of the case of the name,
      // i.e. "alpha" and "ALPHA" and "AlPhA" give same result:
      VERIFY( lookup(mix_case(cls), false) == val );

      // Repeat same checks for icase=true.
      traits::char_class_type ival = lookup(cls, true);
      VERIFY( ival != 0 );
      VERIFY( lookup(mix_case(cls), true) == ival );

      if (cls == L"lower" || cls == L"upper") // icase=true should affect value
	{
	  VERIFY( ival != val );
	  VERIFY( ival == lookup(L"alpha", false) );
	}
      else
	VERIFY( ival == val );

      if (cls == L"d")
	VERIFY( val == lookup(L"digit", false) );
      else if (cls == L"s")
	VERIFY( val == lookup(L"space", false) );
    }
}

int main()
{
  test01();
  test04();
  return 0;
}
