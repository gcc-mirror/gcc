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

// 27.5.2 template class basic_streambuf

#include <cstring> // for memset, memcmp
#include <streambuf>
#include <sstream>
#include <ostream>
#include <testsuite_hooks.h>

// libstdc++/9424
class Outbuf_2 : public std::streambuf
{
  char buf[1];

public:
  Outbuf_2()
  {
    setp(buf, buf + 1);
  }

  int_type overflow(int_type c)
  {
    int_type eof = traits_type::eof();
    
    if (pptr() < epptr())
      {
	if (traits_type::eq_int_type(c, eof))
	  return traits_type::not_eof(c);
	
	*pptr() = traits_type::to_char_type(c);
	pbump(1);
	return c;
      }

    return eof;
  }
};

class Inbuf_2 : public std::streambuf
{
  static const char buf[];
  const char* current;
  int size;

public:
  Inbuf_2()
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

const char Inbuf_2::buf[] = "Atteivlis";

void test11()
{
  bool test __attribute__((unused)) = true;

  Inbuf_2 inbuf1;
  std::istream is(&inbuf1);
  Outbuf_2 outbuf1;
  is >> &outbuf1;
  VERIFY( inbuf1.sgetc() == 't' );
  VERIFY( is.good() );
}

int main() 
{
  test11();
  return 0;
}
