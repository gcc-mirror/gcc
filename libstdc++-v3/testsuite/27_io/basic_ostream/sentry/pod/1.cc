// 1999-10-14 bkoz

// Copyright (C) 1999-2017 Free Software Foundation, Inc.
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

#include <ostream>
#include <sstream>
#include <typeinfo>
#include <ext/pod_char_traits.h>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

void test01()
{
  using namespace std;
  using __gnu_test::pod_ushort;
  typedef basic_string<pod_ushort> 	string_type;
  typedef basic_stringbuf<pod_ushort> 	stringbuf_type;
  typedef basic_ostream<pod_ushort> 	ostream_type;

  const string_type str01;
  stringbuf_type* strbuf01 = 0;
  stringbuf_type strbuf02(str01);
  ostream_type ostr01(strbuf01);
  ostream_type ostr02(&strbuf02);
  
  // test negatives
  try
    {
      ostream_type::sentry sentry01(ostr01);	
    }
  catch (std::bad_cast& obj)
    {
      // Not ok, throws bad_cast because locale has no ctype facet,
      // but none is needed for ostream::sentry.
      VERIFY( false );
    }
  catch (...)
    {
      VERIFY( false );
    }

  // imbued.
  const std::locale loc(std::locale::classic(), new std::ctype<pod_ushort>);
  ostr01.imbue(loc);
  try
    {
      ostream_type::sentry sentry01(ostr01);	
      VERIFY( bool(sentry01) == false ); 
    }
  catch (...)
    {
      VERIFY( false );
    }

  // test positive 
  try
    {
      ostream_type::sentry sentry03(ostr02);	
    }
  catch (std::bad_cast& obj)
    {
      // Not ok, throws bad_cast because locale has no ctype facet,
      // but none is needed for ostream::sentry.
      VERIFY( false );
    }
  catch (...)
    {
      VERIFY( false );
    }

  // imbued.
  ostr02.clear();
  ostr02.imbue(loc);
  try
    {
      ostream_type::sentry sentry03(ostr02);	
      VERIFY( bool(sentry03) == true ); 
    }
  catch (...)
    {
      VERIFY( false );
    }
}

#if !__GXX_WEAK__
// Explicitly instantiate for systems with no COMDAT or weak support.
template 
  const std::basic_string<__gnu_test::pod_ushort>::size_type 
  std::basic_string<__gnu_test::pod_ushort>::_Rep::_S_max_size;

template 
  const __gnu_test::pod_ushort
  std::basic_string<__gnu_test::pod_ushort>::_Rep::_S_terminal;
#endif

int main() 
{
  test01();
  return 0;
}
