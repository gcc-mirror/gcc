// 1999-08-16 bkoz

// Copyright (C) 1999, 2000, 2002, 2003 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 27.6.2.5.4 basic_ostream character inserters

#include <string>
#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

// stringstream and large strings
void
test04() 
{
  bool test __attribute__((unused)) = true;
  std::wstring str_01;
  std::wstring str_tmp;
  const int i_max=250;

  std::wostringstream oss_02(str_01, std::ios_base::out);

  std::ios_base::iostate statefail;
  statefail = std::ios_base::failbit;

  // template<_CharT, _Traits>
  //  basic_ostream& operator<<(ostream&, const wchar_t*)
  for (int i = 0; i < i_max; ++i) 
    oss_02 << L"Test: " << i << std::endl;
  str_tmp = oss_02.str();
  VERIFY( !oss_02.bad() );
  VERIFY( oss_02.good() );
  VERIFY( str_tmp != str_01 );
  VERIFY( str_tmp.size() == 2390 );
}

int main()
{
  test04();
  return 0;
}
