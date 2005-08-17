// 1999-09-20 bkoz

// Copyright (C) 1999, 2003 Free Software Foundation, Inc.
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

// 27.4.4.2 basic_ios member functions

// NB: Don't include any other headers in this file.
#include <ios>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::ios_base::fmtflags fmtflags;
  typedef std::ios_base::iostate iostate;
  using std::ios_base;

  // iostate exceptions() const
  iostate iostate02;
  {
    std::ios ios_01(NULL);
    VERIFY( ios_01.exceptions() == std::ios_base::goodbit );
  }

  // void exceptions(iostate except)
  {
    std::ios ios_01(NULL);
    try {
      ios_01.exceptions(std::ios_base::eofbit);
    }		 
    catch(...) {
      VERIFY( false );
    }
    iostate02 = ios_01.exceptions();
    VERIFY( static_cast<bool>(iostate02 & std::ios_base::eofbit) );
  }

  {
    std::ios ios_01(NULL);
    ios_01.clear(std::ios_base::eofbit);
    try {
      ios_01.exceptions(std::ios_base::eofbit);
      VERIFY( false );
    }		 
    catch(std::ios_base::failure& fail) {
      iostate02 = ios_01.exceptions();
      VERIFY( static_cast<bool>(iostate02 & std::ios_base::eofbit) );
    }
    catch(...) {
      VERIFY( false );
    }
  }
}

int main() 
{
  test01();
  return 0;
}
