// Copyright (C) 2003 Free Software Foundation, Inc.
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

// 27.6.1.1.2 class basic_istream::sentry

#include <string>
#include <ostream>
#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  using __gnu_test::pod_type;
  typedef basic_string<pod_type> 	string_type;
  typedef basic_stringbuf<pod_type> 	stringbuf_type;
  typedef basic_ostream<pod_type> 	ostream_type;

  bool test __attribute__((unused)) = true;

  string_type str;
  stringbuf_type strbuf01;
  ostream_type stream(&strbuf01);

  try
    {
      stream << str;
    }
  catch (std::bad_cast& obj)
    {
      // Ok, throws bad_cast because locale has no ctype facet.
    }
  catch (...)
    {
      VERIFY( false );
    }

  const std::locale loc(std::locale::classic(), new std::ctype<pod_type>);
  stream.imbue(loc);
  try
    {
      stream << str;
    }
  catch (...)
    {
      VERIFY( false );
    }
}

#if !__GXX_WEAK__
// Explicitly instantiate for systems with no COMDAT or weak support.
template 
  std::basic_string<__gnu_test::pod_type>::size_type 
  std::basic_string<__gnu_test::pod_type>::_Rep::_S_max_size;

template 
  __gnu_test::pod_type
  std::basic_string<__gnu_test::pod_type>::_Rep::_S_terminal;
#endif

int main()
{
  test01();
  return 0;
}
