// 1999-10-11 bkoz

// Copyright (C) 1999-2019 Free Software Foundation, Inc.
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


// 27.5.2 template class basic_streambuf

#include <streambuf>
#include <cstring>
#include <testsuite_hooks.h>

class testbuf : public std::streambuf
{
public:

  // Typedefs:
  typedef std::streambuf::traits_type traits_type;
  typedef std::streambuf::char_type char_type;

  testbuf(): std::streambuf() 
  { }

  void
  check_pointers()
  {
    VERIFY( !this->eback() );
    VERIFY( !this->gptr() );
    VERIFY( !this->egptr() );
    VERIFY( !this->pbase() );
    VERIFY( !this->pptr() );
    VERIFY( !this->epptr() );
  }

  int_type 
  pub_uflow() 
  { return (this->uflow()); }

  int_type 
  pub_overflow(int_type __c = traits_type::eof()) 
  { return (this->overflow(__c)); }

  int_type 
  pub_pbackfail(int_type __c) 
  { return (this->pbackfail(__c)); }

  void 
  pub_setg(char* beg, char* cur, char *end) 
  { this->setg(beg, cur, end); }

  void 
  pub_setp(char* beg, char* end) 
  { this->setp(beg, end); }

protected:
  int_type 
  underflow() 
  { 
    int_type __retval = traits_type::eof();
    if (this->gptr() < this->egptr())
      __retval = traits_type::not_eof(0); 
    return __retval;
  }
};

void test01()
{
  typedef testbuf::traits_type traits_type;
  typedef testbuf::int_type int_type;

  testbuf buf01;

  // sputn/xsputn
  char lit02[] = "isotope 217: the unstable molecule on thrill jockey";
  const int i02 = std::strlen(lit02);

  char carray[i02 + 1];
  std::memset(carray, 0, i02 + 1);

  buf01.pub_setp(carray, (carray + i02));
  buf01.sputn(lit02, 0);
  VERIFY( carray[0] == 0 );
  VERIFY( lit02[0] == 'i' );
  buf01.sputn(lit02, 1);
  VERIFY( lit02[0] == carray[0] );
  VERIFY( lit02[1] == 's' );
  VERIFY( carray[1] == 0 );
  buf01.sputn(lit02 + 1, 10);
  VERIFY( std::memcmp(lit02, carray, 10) == 0 );
  buf01.sputn(lit02 + 11, 20);
  VERIFY( std::memcmp(lit02, carray, 30) == 0 );
}

int main() 
{
  test01();
  return 0;
}
