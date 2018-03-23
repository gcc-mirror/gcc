// 1999-08-16 bkoz

// Copyright (C) 1999-2018 Free Software Foundation, Inc.
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

// 27.6.2.5.4 basic_ostream character inserters

#include <string>
#include <ostream>
#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

// Global counter, needs to be reset after use.
bool used;

class gnu_ctype : public std::ctype<wchar_t>
{
protected:
  char_type        
  do_widen(char c) const
  { 
    used = true;
    return std::ctype<wchar_t>::do_widen(c);
  }

  const char*  
  do_widen(const char* low, const char* high, char_type* dest) const
  { 
    used = true;
    return std::ctype<wchar_t>::do_widen(low, high, dest);
  }
};

// 27.6.2.5.4 - Character inserter template functions 
// [lib.ostream.inserters.character]
void test07()
{
  using namespace std;

  const char* buffer = "SFPL 5th floor, outside carrol, the Asian side";

  wostringstream oss;
  oss.imbue(locale(locale::classic(), new gnu_ctype));
  
  // 1
  // template<class charT, class traits>
  // basic_ostream<charT,traits>& operator<<(basic_ostream<charT,traits>& out,
  //                                           const char* s);
  used = false;
  oss << buffer;
  VERIFY( used ); // Only required for char_type != char
  wstring str = oss.str();
  wchar_t c1 = oss.widen(buffer[0]);
  VERIFY( str[0] == c1 );
  wchar_t c2 = oss.widen(buffer[1]);
  VERIFY( str[1] == c2 );

  // 2
  // template<class charT, class traits>
  // basic_ostream<charT,traits>& operator<<(basic_ostream<charT,traits>& out,
  //                                         char c);
  used = false;
  oss.str(wstring());
  oss << 'b';
  VERIFY( used ); // Only required for char_type != char
  str = oss.str();
  wchar_t c3 = oss.widen('b');
  VERIFY( str[0] == c3 );
}

int main()
{
  test07();
  return 0;
}
