// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation, Inc.
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

#include <cstdlib>
#include <unordered_set>
#include <string>
#include <functional>
#include <vector>
#include <testsuite_hooks.h>

using namespace std;

// { dg-options "-DNTESTS=1 -DNSTRINGS=100 -DSTRSIZE=21" { target simulator } }
#ifndef NTESTS
#define NTESTS 5
#endif
#ifndef NSTRINGS
#define NSTRINGS 200
#endif
#ifndef STRSIZE
#define STRSIZE 42
#endif

const unsigned int num_quality_tests = NTESTS;
const unsigned int num_strings_for_quality_tests = NSTRINGS;
const unsigned int string_size = STRSIZE;

vector<string>
random_strings(unsigned int n, unsigned int len)
{
  string s(len, '\0');
  unordered_set<string> result_set;
  while (result_set.size() < n)
    {
      result_set.insert(s);
      unsigned int tmp = rand();
      tmp %= len * 256;
      s[tmp / 256] = tmp % 256;
    }
  return vector<string>(result_set.begin(), result_set.end());
}

double
score_from_varying_position(string s, unsigned int index)
{
  bool test __attribute__((unused)) = true;
  unsigned int bits_in_hash_code = sizeof(size_t) * 8;

  // We'll iterate through all 256 vals for s[index], leaving the rest
  // of s fixed.  Then, for example, out of the 128 times that
  // s[index] has its 3rd bit equal to 0 we would like roughly half 1s
  // and half 0s in bit 9 of the hash codes.
  //
  // Bookkeeping: Conceptually we want a 3D array of ints.  We want to
  // count the number of times each output position (of which there are
  // bits_in_hash_code) is 1 for each bit position within s[index] (of 
  // which there are 8) and value of that bit (of which there are 2).
  const unsigned int jj = 2;
  const unsigned int kk = jj * bits_in_hash_code;
  const unsigned int array_size = 8 * kk;
  vector<int> ones(array_size, 0);

  for (int i = 0; i < 256; i++)
    {
      s[index] = i;
      size_t h = hash<string>()(s);
      for (int j = 0; h != 0; j++, h >>= 1)
        {
          if (h & 1)
            {
              for (int k = 0; k < 8; k++)
                ++ones[k * kk + j * jj + ((i >> k) & 1)];
            }
        }
    }

  // At most, the innermost statement in the above loop nest can
  // execute 256 * bits_in_hash_code * 8 times.  If the hash is good,
  // it'll execute about half that many times, with a pretty even
  // spread across the elements of ones[].
  VERIFY( 256 * bits_in_hash_code * 8 / array_size == 128 );
  int max_ones_possible = 128;
  int good = 0, bad = 0;
  for (int bit = 0; bit <= 1; bit++)
    {
      for (unsigned int j = 0; j < bits_in_hash_code; j++)
        {
          for (int bitpos = 0; bitpos < 8; bitpos++)
            {
              int z = ones[bitpos * kk + j * jj + bit];
              if (z <= max_ones_possible / 6
		  || z >= max_ones_possible * 5 / 6)
                {
                  // The hash function screwed up, or was just unlucky,
                  // as 128 flips of a perfect coin occasionally yield
                  // far from 64 heads.
                  bad++;
                }
              else
                good++;
            }
        }
    }
  return good / (double)(good + bad);
}

double
score_from_varying_position(const vector<string>& v, unsigned int index)
{
  double score = 0;
  for (unsigned int i = 0; i < v.size(); i++)
    score += score_from_varying_position(v[i], index);
  return score / v.size();
}

double
quality_test(unsigned int num_strings, unsigned int string_size)
{
  // Construct random strings.
  vector<string> v = random_strings(num_strings, string_size);
  double sum_of_scores = 0;
  for (unsigned int i = 0; i < string_size; i++)
    sum_of_scores += score_from_varying_position(v, i);

  // A good hash function should have a score very close to 1, and a bad
  // hash function will have a score close to 0.
  return sum_of_scores / string_size;
}

void
quality_test()
{
  bool test __attribute__((unused)) = true;
  srand(137);
  double sum_of_scores = 0;
  for (unsigned int i = 0; i < num_quality_tests; i++)
    {
      double score = quality_test(num_strings_for_quality_tests,
				  string_size);
      sum_of_scores += score;
      VERIFY( score > 0.99 );
    }

  if (num_quality_tests > 1)
    {
      double mean_quality = sum_of_scores / num_quality_tests;
      VERIFY( mean_quality > 0.9999 );
    }
}

int
main()
{
  quality_test();
  return 0;
}
