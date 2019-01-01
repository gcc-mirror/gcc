// Use smaller statistics when running on simulators, so it takes less time.
// For x86_64-linux-gnu SAMPLES=30000 fails, so increase slightly.
// { dg-options "-DSAMPLES=35000" { target simulator } }
// { dg-do run { target c++11 } }

// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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

// Tests chi^2 of a set of strings that all have a similar pattern,
// intended to mimic some sort of ID string.
void
test_numeric_pattern_set()
{
  const unsigned long N = SAMPLES;
  const unsigned long k = N/100;
  std::vector<std::string> set;
  for (unsigned long i = 0; i < N; ++i)
    {
      long i1 = i % 100000;
      long i2 = i / 100000;
      char buf[16];
      std::sprintf(buf, "XX-%05lu-%05lu", i1, i2);
      set.push_back(buf);
    }

  double chi2 = chi2_hash(set, k);
  VERIFY( chi2 < k*1.1 );
}

int
main()
{
  test_numeric_pattern_set();
  return 0;
}
