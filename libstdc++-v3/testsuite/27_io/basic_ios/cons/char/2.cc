// 2001-06-05 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 27.4.2.1.6 class ios_base::init

#include <ios>
#include <testsuite_hooks.h>

// Non-required instantiations don't have the required facets inbued,
// by default, into the locale object.
// See 27.4.4.1
class gnu_ios: public std::basic_ios<char> { };

void test01() 
{
  bool test __attribute__((unused)) = true;

  // 01: Doesn't call basic_ios::init, which uses ctype<char_type>..
  // This should be unambiguously correct.
  try
    {
      gnu_ios gios;
    }
  catch(...)
    { 
      test = false; 
    }
  VERIFY( test );
}

int main()
{
  test01();
  return 0;
}
