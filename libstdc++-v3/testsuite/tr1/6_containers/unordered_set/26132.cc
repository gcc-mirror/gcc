// 2006-02-22  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2020 Free Software Foundation, Inc.
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

// 6.3 Unordered associative containers

#include <tr1/unordered_set>
#include <testsuite_hooks.h>

// libstdc++/26132
void test01()
{
  for (float lf = 1.0; lf < 101.0; lf *= 10.0)
    for (int size = 1; size <= 6561; size *= 3)
      {
	std::tr1::unordered_set<int> us1;
	typedef std::tr1::unordered_set<int>::size_type size_type;

	us1.max_load_factor(10.0);

	for (int i = 0; i < size; ++i)
	  us1.insert(i);

	us1.max_load_factor(lf);

	for (int i = 1; i <= 6561; i *= 81)
	  {
	    const size_type n = size * 81 / i;
	    us1.rehash(n);
	    VERIFY( us1.bucket_count() > us1.size() / us1.max_load_factor() );
	    VERIFY( us1.bucket_count() >= n );
	  }
      }
}

int main()
{
  test01();
  return 0;
}
