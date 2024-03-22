// { dg-do run { target c++11 } }

// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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
// <http://www.gnu.org/licenses/>.

#include <testsuite_hooks.h>
#include <unordered_set>

namespace
{
  struct hash
  {
    hash() = default;
    hash(int modulo)
      : _M_modulo(modulo)
    { }

    std::size_t operator() (int val) const noexcept
    { return val % _M_modulo; }

    int _M_modulo;
  };
}

void
test01()
{
  // static_assert(std::__cache_default<int, hash>::value,
  // 		"Unexpected default cache value");
  typedef std::unordered_set<int, hash> us_t;
  us_t us1(10, hash(13));
  us_t us2(10, hash(7));

  VERIFY( us1.hash_function()._M_modulo == 13 );
  VERIFY( us2.hash_function()._M_modulo == 7 );

  const int nb = 5;
  for (int i = 0; i != nb * us1.hash_function()._M_modulo; ++i)
    us1.insert(i);

  us_t::local_iterator lit = us1.begin(12);
  us_t::local_iterator litend = us1.end(12);
  VERIFY( std::distance(lit, litend) == nb );

  us1.swap(us2);

  VERIFY( us1.hash_function()._M_modulo == 7 );
  VERIFY( us2.hash_function()._M_modulo == 13 );

  VERIFY( std::distance(lit, litend) == nb );
}

int main()
{
  test01();
}
