// Copyright (C) 2004 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

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
  bool test __attribute__((unused)) = true;

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
