// 2003-02-01 Paolo Carlini <pcarlini@unitus.it>

// Copyright (C) 2003 Free Software Foundation
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

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <locale>

// Const correctness issue:
// http://gcc.gnu.org/ml/libstdc++/2003-01/msg00370.html
void
test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  const locale l1("C");
  const locale l2 =
    locale(locale::classic(), &use_facet<time_get<char> >(l1));
}


int main()
{
  test01();
  return 0;
}
