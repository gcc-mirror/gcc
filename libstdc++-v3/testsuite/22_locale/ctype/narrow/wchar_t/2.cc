// 2002-05-24 bkoz

// Copyright (C) 2002-2020 Free Software Foundation, Inc.
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


// 22.2.1.3.2 ctype<char> members

#include <locale>
#include <vector>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;
  typedef wchar_t 	wide_type;

  const char dfault = '?';
  const locale loc_c = locale::classic();
  const ctype<wide_type>& ctype_c = use_facet<ctype<wide_type> >(loc_c); 

  // Construct non-asci string.
  basic_string<wide_type> 	wide(L"wibble");
  wide += wide_type(1240);
  wide += L"kibble";
  basic_string<char> 		narrow("wibble");
  narrow += dfault;
  narrow += "kibble";
  vector<char> 			narrow_chars(wide.length() + 1);

  // narrow(charT c, char dfault) const
  for (size_t i = 0; i < wide.length(); ++i)
    {
      char c = ctype_c.narrow(wide[i], dfault);
      VERIFY( c == narrow[i] );
    }

  // narrow(const charT* low, const charT* high, char dfault, char* dest) const
  ctype_c.narrow(&wide[0], &wide[0] + wide.length(), dfault, &narrow_chars[0]);
  VERIFY( narrow_chars[0] != dfault );
  for (size_t i = 0; i < wide.length(); ++i)
    VERIFY( narrow_chars[i] == narrow[i] );
}

int main() 
{
  test02();
  return 0;
}
