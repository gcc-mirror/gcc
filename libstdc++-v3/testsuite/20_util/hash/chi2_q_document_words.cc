// On some simulators, the workload is simply too large with values big
// enough for the test to pass the quality test, so just skip it altogether.
// { dg-do run { target { c++11 && { ! simulator } } } }

// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

// Tests chi^2 for a set of words taken from a document written in English.
void
test_document_words()
{
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
}

int
main()
{
  test_document_words();
  return 0;
}
