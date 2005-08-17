// Copyright (C) 2004 Free Software Foundation
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

// 27.6.1.3 unformatted input functions

#include <istream>
#include <streambuf>
#include <testsuite_hooks.h>

// libstdc++/8258
class mybuf : public std::basic_streambuf<wchar_t> 
{ };

void test11()
{
  bool test __attribute__((unused)) = true;
  using namespace std;
  wchar_t arr[10];
  mybuf sbuf;
  basic_istream<wchar_t, char_traits<wchar_t> > istr(&sbuf);
  
  VERIFY( istr.rdstate() == ios_base::goodbit );
  VERIFY( istr.readsome(arr, 10) == 0 );
  VERIFY( istr.rdstate() == ios_base::goodbit );
}
 
int 
main()
{
  test11();
  return 0;
}
