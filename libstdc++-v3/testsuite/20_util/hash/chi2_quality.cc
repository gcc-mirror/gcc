// { dg-options "-std=gnu++0x" }

// Use smaller statistics when running on simulators, so it takes less time.
// { dg-options "-std=gnu++0x -DSAMPLES=10000" { target simulator } }

// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

// This file uses the chi^2 test to measure the quality of a hash
// function, by computing the uniformity with which it distributes a set
// of N strings into k buckets (where k is significantly greater than N).
//
// Each bucket has B[i] strings in it. The expected value of each bucket
// for a uniform distribution is z = N/k, so
//   chi^2 = Sum_i (B[i] - z)^2 / z.
//
// We check whether chi^2 is small enough to be consistent with the
// hypothesis of a uniform distribution. If F(chi^2, k-1) is close to
// 0 (where F is the cumulative probability distribution), we can
// reject that hypothesis. So we don't want F to be too small, which
// for large k, means we want chi^2 to be not too much larger than k.
//
// We use the chi^2 test for several sets of strings. Any non-horrible
// hash function should do well with purely random strings. A really
// good hash function will also do well with more structured sets,
// including ones where the strings differ by only a few bits.

#include <algorithm>
#include <cstdlib>
#include <cstdio>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <string>
#include <unordered_set>
#include <vector>
#include <testsuite_hooks.h>

#ifndef SAMPLES
#define SAMPLES 300000
#endif

template <typename Container>
  double
  chi2_hash(const Container& c, long buckets)
  {
    std::vector<int> counts(buckets);
    std::hash<std::string> hasher;
    double elements = 0;
    for (auto i = c.begin(); i != c.end(); ++i)
      {
        ++counts[hasher(*i) % buckets];
        ++elements;
      }

    const double z = elements / buckets;
    double sum = 0;
    for (long i = 0; i < buckets; ++i)
      {
        double delta = counts[i] - z;
        sum += delta*delta;
      }
    return sum/z;
  }

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

// Tests chi^2 for a distribution of strings that differ from each
// other by only a few bits. We start with an arbitrary base string, and
// flip three random bits for each member of the set.
void
test_bit_flip_set()
{
  bool test __attribute__((unused)) = true;
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

// Tests chi^2 of a set of strings that all have a similar pattern,
// intended to mimic some sort of ID string.
void
test_numeric_pattern_set()
{
  bool test __attribute__((unused)) = true;
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

// Tests chi^2 for a set of words taken from a document written in English.
void
test_document_words()
{
  // That file is 187587 single-word lines.  To avoid a timeout, just skip
  // this part, which would take up to 95% of the program runtime (with
  // SAMPLES == 10000), if we're not supposed to run anywhere that long.
#if SAMPLES >= 100000
  bool test __attribute__((unused)) = true;
  const std::string f_name = "thirty_years_among_the_dead_preproc.txt";
  std::ifstream in(f_name);
  VERIFY( in.is_open() );
  std::vector<std::string> words;
  words.assign(std::istream_iterator<std::string>(in),
               std::istream_iterator<std::string>());
  VERIFY( words.size() > 100000 );
  std::sort(words.begin(), words.end());
  auto it = std::unique(words.begin(), words.end());
  words.erase(it, words.end());
  VERIFY( words.size() > 5000 );

  const unsigned long k = words.size() / 20;
  double chi2 = chi2_hash(words, k);
  VERIFY( chi2 < k*1.1 );
#endif
}

int
main()
{
  test_uniform_random();
  test_bit_flip_set();
  test_numeric_pattern_set();
  test_bit_string_set();
  test_document_words();
  return 0;
}
