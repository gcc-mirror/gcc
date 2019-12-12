// Use smaller statistics when running on simulators, so it takes less time.
// { dg-options "-DSAMPLES=30000" { target simulator } }
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

// Tests chi^2 for a distribution of strings that differ from each
// other by only a few bits. We start with an arbitrary base string, and
// flip three random bits for each member of the set.
void
test_bit_flip_set()
{
  const unsigned long N = SAMPLES;
  const unsigned long k = N/100;
  const unsigned int len = 67;
  const unsigned int bitlen = len * 8;
  const unsigned int bits_to_flip = 3;
  const char base[len+1] = "abcdefghijklmnopqrstuvwxyz"
                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                           "0123456789!@#$%";

  std::unordered_set<std::string> set;
  while (set.size() < N)
    {
      std::string s(base, base+len);
      for (unsigned int i = 0; i < bits_to_flip; ++i)
        {
          int bit = rand() % bitlen;
          s[bit/8] ^= (1 << (bit%8));
        }
      set.insert(s);
    }

  double chi2 = chi2_hash(set, k);
  VERIFY( chi2 < k*1.1 );
}

int
main()
{
  test_bit_flip_set();
  return 0;
}
