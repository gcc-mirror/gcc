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
//
// { dg-options "-std=gnu++0x" }

#include <unordered_set>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  std::unordered_set<int> us;
  typedef typename std::unordered_set<int>::size_type size_type;
  bool rehashed = false;
  for (int i = 0; i != 100000; ++i)
  {
    size_type bkt_count = us.bucket_count();
    us.insert(i);
    if (bkt_count != us.bucket_count())
      {
	// Container has been rehashed, lets check that it won't be rehash again
	// if we remove and restore the last 2 inserted elements:
	rehashed = true;
	bkt_count = us.bucket_count();
	VERIFY( us.erase(i) == 1 );
	VERIFY( bkt_count == us.bucket_count() );
	if (i > 0)
	  {
	    VERIFY( us.erase(i - 1) == 1 );
	    VERIFY( bkt_count == us.bucket_count() );

	    VERIFY( us.insert(i - 1).second );
	    VERIFY( bkt_count == us.bucket_count() );
	  }
	VERIFY( us.insert(i).second );
	VERIFY( bkt_count == us.bucket_count() );
      }
  }

  // At lest we check a rehash once:
  VERIFY( rehashed );
}

int main()
{
  test01();
  return 0;
}
