// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
// Free Software Foundation, Inc.
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

#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

void
test02()
{
  bool test __attribute__((unused)) = true;
  const std::string strue("true");
  const std::string sfalse("false");
  std::string str01;
  std::string str02;

  std::locale loc_c = std::locale::classic();
  std::ostringstream ostr01;
  ostr01.imbue(loc_c);
  ostr01.flags(std::ios_base::boolalpha);

  ostr01 << true;
  str02 = ostr01.str();
  VERIFY( str02 == strue );

  ostr01.str(str01);
  ostr01 << false;
  str02 = ostr01.str();
  VERIFY( str02 == sfalse );
}

int 
main() 
{
  test02();
  return 0;
}
