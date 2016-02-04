// { dg-options "-std=gnu++11" }

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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
#include <limits>
#include <ext/throw_allocator.h>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::numeric_limits<std::size_t> nl_size_t;
  std::unordered_set<int, std::hash<int>, std::equal_to<int>,
		     __gnu_cxx::throw_allocator_limit<int> > us;
  int val = 0;
  for (; val != 100; ++val)
    {
      VERIFY( us.insert(val).second );
      VERIFY( us.load_factor() <= us.max_load_factor() );
    }

  float cur_max_load_factor = us.max_load_factor();
  int counter = 0;
  std::size_t thrown_exceptions = 0;

  // Reduce max load factor.
  us.max_load_factor(us.max_load_factor() / 2);

  // At this point load factor is higher than max_load_factor because we can't
  // rehash in max_load_factor call.
  VERIFY( us.load_factor() > us.max_load_factor() );

  while (true)
    {
      __gnu_cxx::limit_condition::set_limit(counter++);
      bool do_break = false;
      try
	{
	  size_t nbkts = us.bucket_count();
	  // Check that unordered_set will still be correctly resized when
	  // needed.
	  VERIFY( us.insert(val++).second );

	  VERIFY( us.bucket_count() != nbkts );
	  VERIFY( us.load_factor() <= us.max_load_factor() );
	  do_break = true;
	}
      catch (const __gnu_cxx::forced_error&)
	{
	  // max load factor doesn't change.
	  VERIFY( us.max_load_factor() == .5f );
	  ++thrown_exceptions;
	}

      if (do_break)
	break;
    }

  VERIFY( thrown_exceptions > 0 );
}

int main()
{
  test01();
  return 0;
}
