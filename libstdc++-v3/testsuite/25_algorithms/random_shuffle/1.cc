// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

// 25.2.11 random_shuffle()

#include <algorithm>
#include <testsuite_hooks.h>

const int N = 200000;
int A[N], s1[N];

#if _GLIBCXX_PARALLEL
#define TAG , __gnu_parallel::sequential_tag()
#else
#define TAG
#endif

void fill_ascending()
{
  for (int i = 0; i < N; ++i)
    A[i] = i;
}

void
test01()
{
  fill_ascending();
#if _GLIBCXX_PARALLEL
  for (int num_threads = 1; num_threads <= 2; ++num_threads)
  {
    omp_set_num_threads(num_threads);
#endif
    std::copy(A, A + N, s1);
    VERIFY(std::equal(s1, s1 + N, A TAG));

    std::random_shuffle(s1, s1 + N);
    // the chance that random_shuffle leaves the order as is by coincidence
    // is negligible, so we expect it to be permuted
    VERIFY(!std::equal(s1, s1 + N, A TAG));

    std::sort(s1, s1 + N TAG);
    VERIFY(std::equal(s1, s1 + N, A TAG));
#if _GLIBCXX_PARALLEL
  }
#endif
}

int
main()
{
#if _GLIBCXX_PARALLEL
  __gnu_parallel::_Settings gpms = __gnu_parallel::_Settings::get();
  gpms.algorithm_strategy = __gnu_parallel::force_parallel;
  __gnu_parallel::_Settings::set(gpms);
#endif
  test01();
  return 0;
}
