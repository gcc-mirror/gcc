// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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


// 27.6.1.3 unformatted input functions

#include <cstring> // for strlen
#include <istream>
#include <testsuite_hooks.h>

class Inbuf : public std::streambuf
{
  static const char buf[];
  const char* current;
  int size;

public:
  Inbuf()
  {
    current = buf;
    size = std::strlen(buf);
  }
  
  int_type underflow()
  {
    if (current < buf + size)
      return traits_type::to_int_type(*current);
    return traits_type::eof();
  }
  
  int_type uflow()
  {
    if (current < buf + size)
      return traits_type::to_int_type(*current++);
    return traits_type::eof();
  }
};

const char Inbuf::buf[] = "1234567890abcdefghij";

void test01()
{
  using namespace std;

  typedef char_traits<char>   traits_type;

  Inbuf inbuf1;
  istream is(&inbuf1);

  char buffer[10];
  traits_type::assign(buffer, sizeof(buffer), 'X');

  is.getline(buffer, sizeof(buffer), '0');
  VERIFY( is.rdstate() == ios_base::goodbit );
  VERIFY( !traits_type::compare(buffer, "123456789\0", sizeof(buffer)) );
  VERIFY( is.gcount() == 10 );

  is.clear();
  traits_type::assign(buffer, sizeof(buffer), 'X');
  is.getline(buffer, sizeof(buffer));
  VERIFY( is.rdstate() == ios_base::failbit );
  VERIFY( !traits_type::compare(buffer, "abcdefghi\0", sizeof(buffer)) );
  VERIFY( is.gcount() == 9 );

  is.clear();
  traits_type::assign(buffer, sizeof(buffer), 'X');
  is.getline(buffer, sizeof(buffer));
  VERIFY( is.rdstate() == ios_base::eofbit );
  VERIFY( !traits_type::compare(buffer, "j\0XXXXXXXX", sizeof(buffer)) );
  VERIFY( is.gcount() == 1 );

  is.clear();
  traits_type::assign(buffer, sizeof(buffer), 'X');
  is.getline(buffer, sizeof(buffer));
  VERIFY( is.rdstate() == (ios_base::eofbit | ios_base::failbit) );
  VERIFY( !traits_type::compare(buffer, "\0XXXXXXXXX", sizeof(buffer)) );
  VERIFY( is.gcount() == 0 );
}

int main() 
{
  test01();
  return 0;
}
