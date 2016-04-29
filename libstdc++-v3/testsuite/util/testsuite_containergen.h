// Copyright (C) 2013-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef _GLIBCXX_TESTSUITE_CONTAINER_GEN_H
#define _GLIBCXX_TESTSUITE_CONTAINER_GEN_H

#include <testsuite_container_traits.h>
#include <random>

namespace __gnu_test
{
  template<typename ContainerType, typename Tester, typename RandomGen>
    void
    test_single_container(Tester test, RandomGen& rg, int length, int domain)
    {
      std::vector<int> values;
      auto dist = std::uniform_int_distribution<>(0, domain - 1);

      for(int i = 0; i < length; ++i)
	values.push_back(dist(rg));

      ContainerType con(values.data(), values.data() + length);
      test(con, rg);
    }

  template<typename ContainerType, typename Tester, typename RandomGen>
    void
    test_special_containers(Tester test, RandomGen& rg, int length)
    {
      std::vector<int> values(length);
      ContainerType con(values.data(), values.data() + length);

      for(int i = 0; i < length; ++i)
	values[i] = 0;
      test(con, rg);

      for(int i = 0; i < length; ++i)
	values[i] = i;
      test(con, rg);

      for(int i = 0; i < length; ++i)
	values[i] = -i;
      test(con, rg);
    }

  template<typename ContainerType, typename Tester>
    void
    test_containers(Tester test)
    {
      std::mt19937_64 random_gen;

#ifdef SIMULATOR_TEST
      int loops = 10;
#else
      int loops = 1000;
#endif

      for(int i = 0; i < loops; ++i)
	test_special_containers<ContainerType>(test, random_gen, i);

      for(int i = 1; i < 100; ++i)
	for(int j = 0; j < loops; ++j)
	  test_single_container<ContainerType>(test, random_gen, i, i);

      for(int i = 0; i < loops; ++i)
	{
	  test_single_container<ContainerType>(test, random_gen, 10, 10);
	  test_single_container<ContainerType>(test, random_gen, 100, 10);
	  test_single_container<ContainerType>(test, random_gen, 1000, 10);
	  test_single_container<ContainerType>(test, random_gen, 10, 1000);
	}

#ifndef SIMULATOR_TEST
      for(int i = 0; i < 1000; ++i)
	{
	  test_single_container<ContainerType>(test, random_gen, 10000, 10);
	  test_single_container<ContainerType>(test, random_gen, 10000, 10000);
	}
#endif
    }
} // namespace __gnu_test

#endif
