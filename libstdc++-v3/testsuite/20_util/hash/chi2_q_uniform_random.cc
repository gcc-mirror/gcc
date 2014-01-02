// { dg-options "-std=gnu++0x" }
// Use smaller statistics when running on simulators, so it takes less time.
// For powerpc-eabi, SAMPLES=30000 fails.
// { dg-options "-std=gnu++0x -DSAMPLES=35000" { target simulator } }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "chi2_quality.h"

// Tests chi^2 for a distribution of uniformly generated random strings.
void
test_uniform_random()
{
  bool test __attribute__((unused)) = true;
  std::srand(137);
  std::unordered_set<std::string> set;
  std::string s;
  const unsigned long N = SAMPLES;
  const unsigned long k = N/100;
  const unsigned int len = 25;
  while (set.size() < N)
    {
      s.clear();
      for (unsigned int i = 0; i < len; ++i)
       s.push_back(rand() % 128);
      set.insert(s);
    }

  double chi2 = chi2_hash(set, k);
  VERIFY( chi2 < k*1.1 );
}

int
main()
{
  test_uniform_random();
  return 0;
}
