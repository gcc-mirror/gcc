// { dg-do link { target c++11 } }
// { dg-require-static-libstdcxx }
// { dg-options "-static-libstdc++" }

// Copyright (C) 2012-2019 Free Software Foundation, Inc.
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
// <http://www.gnu.org/licenses/>

// libstdc++/52689 static linking fails
#include <iostream>
#include <locale>

int main()
{
  std::locale c __attribute__((unused)) = std::locale::classic();
  std::cout << "i am old-skool\n";
  return 0;
}
// Ignore TOC warnings on AIX
// { dg-prune-output "ld: 0711-768 WARNING" }
// { dg-prune-output "recognized no-op" }
// { dg-prune-output "TOC-reload" }

