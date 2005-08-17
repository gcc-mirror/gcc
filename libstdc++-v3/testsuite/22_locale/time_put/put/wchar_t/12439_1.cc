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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 22.2.5.3.1 time_put members

#include <locale>
#include <sstream>
#include <ctime>
#include <cstring>
#include <testsuite_hooks.h>

class TP : public std::time_put<wchar_t>
{
public:
  mutable std::wstring fill_chars;

protected:
  iter_type do_put(iter_type s, std::ios_base&, char_type fill,
		   const std::tm* t, char format, char modifier) const
  {
    fill_chars.push_back(fill);
    return s;
  }
};

// libstdc++/12439
// time_put::put doesn't pass fill character to do_put
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  
  wostringstream stream;
  time_t tt = time(NULL);
  
  const wchar_t* fmt = L"%c";
  
  TP tp;
  tp.put(TP::iter_type(stream), stream, L'W', localtime(&tt),
	 fmt, fmt + wcslen(fmt));
  VERIFY( !tp.fill_chars.empty() );
  VERIFY( tp.fill_chars[tp.fill_chars.length() - 1] == L'W' );
}

int main()
{
  test01();
  return 0;
}
