// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 25.2.1 [lib.alg.copy] Copy.

#include <algorithm>
#include <testsuite_hooks.h>

class Counting_output_iterator
: public std::iterator< std::output_iterator_tag, void, void, void, void >
{
  std::size_t c;
public:
  Counting_output_iterator() : c(0) {}
  Counting_output_iterator& operator++() { return *this; }
  Counting_output_iterator& operator*() { return *this; }
  
  template <typename T>
  void operator=(const T&) { ++c; }
  
  std::size_t current_counter() const { return c; }
};

// libstdc++/34595
void test01()
{
  bool test __attribute__((unused)) = true;
  
  int t[10] = {0,};
  Counting_output_iterator cnt;
  std::size_t res = std::copy(t+0, t+5, cnt).current_counter();

  VERIFY( res == 5 );
}

int main()
{
  test01();
  return 0;
}
