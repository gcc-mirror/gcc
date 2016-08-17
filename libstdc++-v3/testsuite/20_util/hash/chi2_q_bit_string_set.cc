// Use smaller statistics when running on simulators, so it takes less time.
// For e.g. cris-elf, mipsisa32r2el-elf, powerpc-eabi and i386-linux-gnu,
// this test fails for SAMPLES=30000.
// { dg-options "-DSAMPLES=35000" { target simulator } }
// { dg-do run { target c++11 } }

// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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

// Tests chi^2 for a set of strings that all consist of '1' and '0'.
void
test_bit_string_set()
{
  bool test __attribute__((unused)) = true;
  const unsigned long N = SAMPLES;
  const unsigned long k = N/100;
  std::vector<std::string> set;
  std::string s;
  for (unsigned long i = 0; i < N; ++i)
    {
      s.clear();
      for (unsigned int j = 0; j < sizeof(unsigned long) * 8; ++j)
        {
          const bool bit = (1UL << j) & i;
          s.push_back(bit ? '1' : '0');
        }
      set.push_back(s);
    }

  double chi2 = chi2_hash(set, k);
  VERIFY( chi2 < k*1.1 );
}

int
main()
{
  test_bit_string_set();
  return 0;
}
