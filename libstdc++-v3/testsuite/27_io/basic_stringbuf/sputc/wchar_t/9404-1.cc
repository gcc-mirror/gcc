// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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

// 27.7.1.3 Overridden virtual functions

#include <sstream>
#include <testsuite_hooks.h>

bool over_called;

class Derived_stringbuf : public std::wstringbuf
{
public:
  int_type overflow(int_type c)
  {
    over_called = true;
    return std::wstringbuf::overflow(c);
  }
  
  const char_type* pub_epptr() const
  {
    return epptr();
  }
  
  const char_type* pub_pptr() const
  {
    return pptr();
  }
};

// libstdc++/9404
void test04()
{
  bool test __attribute__((unused)) = true;

  bool over_expected;

  // sputc
  Derived_stringbuf dsbuf_01;
  over_called = false;
  dsbuf_01.sputc(L'i');
  VERIFY( over_called );
  over_expected = dsbuf_01.pub_epptr() == dsbuf_01.pub_pptr();
  over_called = false;
  dsbuf_01.sputc(L'v');
  VERIFY( (!over_expected && !over_called)
	  || (over_expected && over_called) );
  dsbuf_01.sputc(L'i');
  VERIFY( dsbuf_01.str() == L"ivi" ); // Sanity check.
}

int main() 
{
  test04();
  return 0;
}
