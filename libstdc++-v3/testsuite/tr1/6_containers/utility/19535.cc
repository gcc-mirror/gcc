// 2005-01-19  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2005 Free Software Foundation, Inc.
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

// tr1 additions to pair

#include <tr1/utility>

struct A { };
struct B { };

// libstdc++/19535
void test01()
{
  std::pair<A, B> p;
  std::tr1::get<1>(p);
  
  const std::pair<B, A> q;
  std::tr1::get<1>(q);
}

int main()
{
  test01();
  return 0;
}
