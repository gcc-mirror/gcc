// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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

// 22.2.5.3.1 time_put members

// { dg-require-time "" }

#include <locale>
#include <sstream>
#include <ctime>
#include <cstring>
#include <testsuite_hooks.h>

class TP : public std::time_put<char>
{
public:
  mutable std::string fill_chars;

protected:
  iter_type do_put(iter_type s, std::ios_base&, char_type fill,
		   const std::tm*, char, char) const
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
  
  ostringstream stream;
  time_t tt = time(0);
  
  const char* fmt = "%c";
  
  TP tp;
  tp.put(TP::iter_type(stream), stream, 'W', localtime(&tt),
	 fmt, fmt + strlen(fmt));
  VERIFY( !tp.fill_chars.empty() );
  VERIFY( tp.fill_chars[tp.fill_chars.length() - 1] == 'W' );
}

int main()
{
  test01();
  return 0;
}
