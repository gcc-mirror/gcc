// 1999-08-11 bkoz

// Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation
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

// 27.6.1.3 unformatted input functions

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

// Jim Parsons <parsons at clearway dot com>
// http://gcc.gnu.org/ml/libstdc++/2000-q1/msg00177.html
void
test04()
{
  bool test __attribute__((unused)) = true;

  const std::string str_00("Red_Garland_Qunitet-Soul_Junction");
  char c_array[str_00.size() + 4];

  std::stringbuf isbuf_00(str_00, std::ios_base::in);
  std::istream is_00(&isbuf_00);
  std::ios_base::iostate state1, statefail, stateeof;
  statefail = std::ios_base::failbit;
  stateeof = std::ios_base::eofbit;

  state1 = stateeof | statefail;
  VERIFY( is_00.gcount() == 0 );
  is_00.read(c_array, str_00.size() + 1);
  VERIFY( is_00.gcount() == static_cast<std::streamsize>(str_00.size()) );
  VERIFY( is_00.rdstate() == state1 );

  is_00.read(c_array, str_00.size());
  VERIFY( is_00.rdstate() == state1 );
}
 
int 
main()
{
  test04();
  return 0;
}
