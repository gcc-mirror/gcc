// Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

// { dg-require-time "" }

// 22.2.5.3.1 time_put members

#include <locale>
#include <sstream>
#include <ctime>
#include <testsuite_hooks.h>

class TP : public std::time_put<wchar_t>
{
public:
  mutable std::string format_chars;

protected:
  iter_type do_put(iter_type s, std::ios_base&, char_type,
		   const std::tm*, char format, char) const
  {
    format_chars.push_back(format);
    return s;
  }
};

// libstdc++/12439
// time_put::put reads past end of format string
void test03()
{
  using namespace std;
  
  wostringstream stream;
  time_t tt = time(0);
  
  const wchar_t* fmt = L"%c";
  
  TP tp;
  tp.put(TP::iter_type(stream), stream, stream.fill(), localtime(&tt),
	 fmt, fmt + 1);
  VERIFY( tp.format_chars.empty() );
}

int main()
{
  test03();
  return 0;
}
