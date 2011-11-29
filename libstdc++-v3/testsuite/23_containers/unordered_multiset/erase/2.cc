// { dg-options "-std=gnu++0x" }

// Copyright (C) 2011 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <string>
#include <testsuite_hooks.h>

namespace
{
  std::size_t
  get_nb_bucket_elems(const std::unordered_multiset<std::string>& us)
  {
    std::size_t nb = 0;
    for (std::size_t b = 0; b != us.bucket_count(); ++b)
      nb += us.bucket_size(b);
    return nb;
  }
}

void test01()
{
  bool test __attribute__((unused)) = true;
  
  typedef std::unordered_multiset<std::string> Mset;
  typedef Mset::iterator       iterator;
  typedef Mset::const_iterator const_iterator;

  Mset ms1;
  
  ms1.insert("foo");
  ms1.insert("foo");
  ms1.insert("foo");
  ms1.insert("foo");
  ms1.insert("foo");
  ms1.insert("foo");
  ms1.insert("bar");
  ms1.insert("bar");
  ms1.insert("bar");
  ms1.insert("bar");
  ms1.insert("bar");
  ms1.insert("bar");
  VERIFY( ms1.size() == 12 );
  VERIFY( get_nb_bucket_elems(ms1) == ms1.size() );
  VERIFY( distance(ms1.begin(), ms1.end()) == ms1.size() );

  VERIFY( ms1.erase(ms1.begin()) != ms1.end() );
  VERIFY( ms1.size() == 11 );
  VERIFY( get_nb_bucket_elems(ms1) == ms1.size() );
  VERIFY( distance(ms1.begin(), ms1.end()) == ms1.size() );

  auto it = ms1.begin();
  advance(it, 2);
  VERIFY( ms1.erase(ms1.begin(), it) != ms1.end() );
  VERIFY( ms1.size() == 9 );
  VERIFY( get_nb_bucket_elems(ms1) == ms1.size() );
  VERIFY( distance(ms1.begin(), ms1.end()) == ms1.size() );

  VERIFY( ms1.erase(*(ms1.begin())) == 3 );
  VERIFY( ms1.size() == 6 );
  VERIFY( get_nb_bucket_elems(ms1) == ms1.size() );
  VERIFY( distance(ms1.begin(), ms1.end()) == ms1.size() );

  VERIFY( ms1.erase(ms1.begin(), ms1.end()) == ms1.end() );
  VERIFY( ms1.empty() );
  VERIFY( get_nb_bucket_elems(ms1) == ms1.size() );
  VERIFY( distance(ms1.begin(), ms1.end()) == ms1.size() );
}

int main()
{
  test01();
  return 0;
}
