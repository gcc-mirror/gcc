// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

// libstdc++/23465
void test01()
{
  for (float lf = 0.1; lf < 101.0; lf *= 10.0)
    for (int size = 1; size <= 6561; size *= 3)
      {
	std::tr1::unordered_set<int> us1, us2;
	typedef std::tr1::unordered_set<int>::local_iterator local_iterator;
	typedef std::tr1::unordered_set<int>::size_type      size_type;

	us1.max_load_factor(lf);

	for (int i = 0; i < size; ++i)
	  us1.insert(i);

	us2 = us1;

	VERIFY( us2.size() == us1.size() );
	VERIFY( us2.bucket_count() == us1.bucket_count() );

	for (size_type b = 0; b < us1.bucket_count(); ++b)
	  {
	    size_type cnt = 0;
	    for (local_iterator it1 = us1.begin(b), it2 = us2.begin(b);
		 it1 != us1.end(b) && it2 != us2.end(b); ++it1, ++it2)
	      {
		VERIFY( *it1 == *it2 );
		++cnt;
	      }
	    VERIFY( cnt == us1.bucket_size(b) );
	  }
      }
}

int main()
{
  test01();
  return 0;
}
