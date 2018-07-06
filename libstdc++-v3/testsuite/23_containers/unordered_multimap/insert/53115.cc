// { dg-do run { target c++11 } }
//
// Copyright (C) 2012-2018 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

namespace
{
  std::size_t
  get_nb_bucket_elems(const std::unordered_multimap<int, int>& us)
  {
    std::size_t nb = 0;
    for (std::size_t b = 0; b != us.bucket_count(); ++b)
      nb += us.bucket_size(b);
    return nb;
  }
}

void test01()
{
  using namespace std;

  std::unordered_multimap<int, int> umm;
  umm.insert(make_pair(10, 1));
  VERIFY( umm.size() == 1 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(10, 2)); 
  VERIFY( umm.size() == 2 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(10, 3));
  VERIFY( umm.size() == 3 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(10, 4));
  VERIFY( umm.size() == 4 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(10, 5));
  VERIFY( umm.size() == 5 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(24, 6));
  VERIFY( umm.size() == 6 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(25, 7));
  VERIFY( umm.size() == 7 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(2, 8));
  VERIFY( umm.size() == 8 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(2, 9));
  VERIFY( umm.size() == 9 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(1, 10));
  VERIFY( umm.size() == 10 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );

  umm.insert(make_pair(10, 11));
  VERIFY( umm.size() == 11 );
  VERIFY( distance(umm.begin(), umm.end()) - umm.size() == 0 );
  VERIFY( get_nb_bucket_elems(umm) == umm.size() );
}

int main()
{
  test01();
  return 0;
}
