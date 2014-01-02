// { dg-options "-std=gnu++0x" }

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <string>
#include <testsuite_hooks.h>

namespace
{
  std::size_t
  get_nb_bucket_elems(const std::unordered_multimap<std::string, int>& um)
  {
    std::size_t nb = 0;
    for (std::size_t b = 0; b != um.bucket_count(); ++b)
      nb += um.bucket_size(b);
    return nb;
  }
}

void test01()
{
  bool test __attribute__((unused)) = true;
  
  typedef std::unordered_multimap<std::string, int> Mmap;
  typedef Mmap::iterator       iterator;
  typedef Mmap::const_iterator const_iterator;
  typedef Mmap::value_type     value_type;

  Mmap mm1;
  
  mm1.insert(value_type("foo", 10));
  mm1.insert(value_type("foo", 11));
  mm1.insert(value_type("foo", 12));
  mm1.insert(value_type("foo", 13));
  mm1.insert(value_type("foo", 14));
  mm1.insert(value_type("foo", 15));
  mm1.insert(value_type("bar", 20));
  mm1.insert(value_type("bar", 21));
  mm1.insert(value_type("bar", 22));
  mm1.insert(value_type("bar", 23));
  mm1.insert(value_type("bar", 24));
  mm1.insert(value_type("bar", 25));
  VERIFY( mm1.size() == 12 );
  VERIFY( get_nb_bucket_elems(mm1) == mm1.size() );
  VERIFY( distance(mm1.begin(), mm1.end()) - mm1.size() == 0 );

  VERIFY( mm1.erase(mm1.begin()) != mm1.end() );
  VERIFY( mm1.size() == 11 );
  VERIFY( get_nb_bucket_elems(mm1) == mm1.size() );
  VERIFY( distance(mm1.begin(), mm1.end()) - mm1.size() == 0 );

  auto it = mm1.begin();
  advance(it, 2);
  VERIFY( mm1.erase(mm1.begin(), it) != mm1.end() );
  VERIFY( mm1.size() == 9 );
  VERIFY( get_nb_bucket_elems(mm1) == mm1.size() );
  VERIFY( distance(mm1.begin(), mm1.end()) - mm1.size() == 0 );

  VERIFY( mm1.erase(mm1.begin()->first) == 3 );
  VERIFY( mm1.size() == 6 );
  VERIFY( get_nb_bucket_elems(mm1) == mm1.size() );
  VERIFY( distance(mm1.begin(), mm1.end()) - mm1.size() == 0 );

  VERIFY( mm1.erase(mm1.begin(), mm1.end()) == mm1.end() );
  VERIFY( mm1.empty() );
  VERIFY( get_nb_bucket_elems(mm1) == mm1.size() );
  VERIFY( distance(mm1.begin(), mm1.end()) - mm1.size() == 0 );
}

int main()
{
  test01();
  return 0;
}
