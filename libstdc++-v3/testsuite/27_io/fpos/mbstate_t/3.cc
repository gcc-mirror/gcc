// 1999-09-20 bkoz

// Copyright (C) 1999, 2001, 2003 Free Software Foundation, Inc.
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

// 27.4.3 template class fpos

#include <cwchar> // for mbstate_t
#include <ios>
#include <testsuite_hooks.h>

void test03()
{
  bool test __attribute__((unused)) = true;

  typedef std::mbstate_t state_type;
  std::streamoff off01;
  
  // casts to const streamoff
  const std::streampos pos01 = 0;
  off01 = std::streamoff(pos01);

  // equality/inequality with const args
  const std::streampos pos02(54);
  std::streampos pos03(44);
  VERIFY( !(pos03 == pos02) );
  VERIFY( pos03 != pos02 );
  VERIFY( !(pos02 == pos03) );
  VERIFY( pos02 != pos03 );

  // default values
  std::streampos pos04;
  VERIFY( std::streamoff(pos04) == 0 ); 
} 

int main() 
{
  test03();
  return 0;
}
