// { dg-do run { target c++11 } }

// Copyright (C) 2010-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// range insert

#include <string>
#include <iterator>
#include <algorithm>
#include <unordered_set>
#include <testsuite_hooks.h>

namespace
{
  template <typename _Tp>
    std::size_t
    get_nb_bucket_elems(const std::unordered_multiset<_Tp>& us)
    {
      std::size_t nb = 0;
      for (std::size_t b = 0; b != us.bucket_count(); ++b)
	nb += us.bucket_size(b);
      return nb;
    }
}

void test01()
{
  typedef std::unordered_multiset<std::string> Set;
  Set s;
  VERIFY(s.empty());

  const int N = 10;
  const std::string A[N] = { "red", "green", "blue", "violet", "cyan",
			     "magenta", "yellow", "orange", "pink", "gray" };

  s.insert(A + 0, A + N);
  VERIFY( s.size() == static_cast<unsigned int>(N) );
  VERIFY( std::distance(s.begin(), s.end()) - N == 0 );
  VERIFY( get_nb_bucket_elems(s) - N == 0 );

  for (int i = 0; i < N; ++i)
    {
      std::string str = A[i];
      Set::iterator it = std::find(s.begin(), s.end(), str);
      VERIFY(it != s.end());
    }
}

void test02()
{
  typedef std::unordered_multiset<int> Set;
  Set s;
  VERIFY(s.empty());

  const int N = 8;
  const int A[N] = { 3, 7, 4, 8, 2, 4, 6, 7 };

  s.insert(A+0, A+N);
  VERIFY( s.size() == static_cast<unsigned int>(N) );
  VERIFY( std::distance(s.begin(), s.end()) - N == 0 );
  VERIFY( get_nb_bucket_elems(s) - N == 0 );

  VERIFY( std::count(s.begin(), s.end(), 2) == 1 );
  VERIFY( std::count(s.begin(), s.end(), 3) == 1 );
  VERIFY( std::count(s.begin(), s.end(), 4) == 2 );
  VERIFY( std::count(s.begin(), s.end(), 6) == 1 );
  VERIFY( std::count(s.begin(), s.end(), 7) == 2 );
  VERIFY( std::count(s.begin(), s.end(), 8) == 1 );
}

int main()
{
  test01();
  test02();
  return 0;
}
