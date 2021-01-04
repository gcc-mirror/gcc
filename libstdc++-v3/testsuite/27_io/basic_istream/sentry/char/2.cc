// 1999-10-14 bkoz

// Copyright (C) 1999-2021 Free Software Foundation, Inc.
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


// 27.6.1.1.2 class basic_istream::sentry

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/944
void 
test02()
{
  using namespace std;
  istringstream in("80.21 56.89 12.3");
  int i = 0;
  double x;

  // ios_base::eof == 2
  while (in >> x)
    {
      ++i;
      if (i > 3) 
	break;
    }
  VERIFY( i == 3 );
}    

int main() 
{
  test02();
  return 0;
}
