// 1999-10-11 bkoz

// Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// 27.5.2 template class basic_streambuf

#include <streambuf>
#include <ostream>
#include <string>
#include <testsuite_hooks.h>

// test03
// http://gcc.gnu.org/ml/libstdc++/2000-q1/msg00151.html
template<typename charT, typename traits = std::char_traits<charT> >
  class basic_nullbuf : public std::basic_streambuf<charT, traits>
  {
  protected:
    typedef typename
      std::basic_streambuf<charT, traits>::int_type int_type;
    virtual int_type 
    overflow(int_type c) 
    {  return traits::not_eof(c); }
  };

typedef basic_nullbuf<wchar_t> nullbuf;

template<typename T>
  wchar_t
  print(const T& x) 
  {
   nullbuf ob;
   std::wostream out(&ob); 
   out << x << std::endl;
   return (!out ? L'0' : L'1');
 }

void test03() 
{
  bool test __attribute__((unused)) = true;
  const std::wstring control01(L"11111");
  std::wstring test01;

  test01 += print(true);
  test01 += print(3.14159);
  test01 += print(10);
  test01 += print(L'x');
  test01 += print(L"pipo");

  VERIFY( test01 == control01 );
}

int main() 
{
  test03();
  return 0;
}
