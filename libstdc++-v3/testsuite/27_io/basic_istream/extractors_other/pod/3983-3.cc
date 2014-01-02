// 2001-06-05 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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


// 27.4.2.1.6 class ios_base::init

#include <sstream>
#include <typeinfo>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

// libstdc++/3983
// Sentry uses locale info, so have to try one formatted input/output.
void test03()
{
  using namespace std;
  using __gnu_test::pod_ushort;
  typedef basic_stringbuf<pod_ushort> 	stringbuf_type;
  typedef basic_istream<pod_ushort> 	istream_type;

  stringbuf_type strbuf01;
  istream_type iss(&strbuf01);

  bool test __attribute__((unused)) = true;
  
  try 
    { 
      iss >> std::ws;
    }
  catch (std::bad_cast& obj)
    { }
  catch (std::exception& obj)
    { VERIFY( false ); }
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
  test03();
  return 0;
}
