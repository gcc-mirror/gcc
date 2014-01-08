// { dg-options "-std=gnu++11" }
//
// Copyright (C) 2012-2014 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

namespace
{
  std::size_t
  get_nb_bucket_elems(const std::unordered_multiset<int>& us)
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
  bool test __attribute__((unused)) = true;

  std::unordered_multiset<int> mms;
  mms.insert(10);
  VERIFY( mms.size() == 1 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(10); 
  VERIFY( mms.size() == 2 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(10);
  VERIFY( mms.size() == 3 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(10);
  VERIFY( mms.size() == 4 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(10);
  VERIFY( mms.size() == 5 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(24);
  VERIFY( mms.size() == 6 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(25);
  VERIFY( mms.size() == 7 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(2);
  VERIFY( mms.size() == 8 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(2);
  VERIFY( mms.size() == 9 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(1);
  VERIFY( mms.size() == 10 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );

  mms.insert(10);
  VERIFY( mms.size() == 11 );
  VERIFY( distance(mms.begin(), mms.end()) - mms.size() == 0 );
  VERIFY( get_nb_bucket_elems(mms) == mms.size() );
}

int main()
{
  test01();
  return 0;
}
