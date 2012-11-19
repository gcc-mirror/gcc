// Copyright (C) 2012 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=c++11" }

#include <testsuite_performance.h>
#include <random>
#include <sstream>
#include <tr1/unordered_set>
#include<unordered_set>

struct Foo
{
  typedef std::random_device::result_type _Type;
  _Type bar;
  _Type baz;
  _Type meh;

  void
  init(std::random_device& randev)
  {
    bar = randev();
    baz = randev();
    meh = randev();
  }

  std::size_t
  hash() const noexcept
  { return std::size_t(bar ^ baz ^ meh); }

  inline bool
  operator==(const Foo& other) const
  { return other.bar == bar && other.baz == baz && other.meh == meh; }
};

struct HashFunction
{
  template<typename T>
    std::size_t operator()(const T& t) const noexcept
    { return t.hash(); }
};

template<typename _ContType>
  void bench(const char* container_desc)
  {
    using namespace __gnu_test;

    time_counter time;
    resource_counter resource;

    const int sz = 300000;

    Foo foos[sz];
    {
      std::random_device randev;
      for (int i = 0; i != sz; ++i)
	foos[i].init(randev);
    }

    _ContType s;
    start_counters(time, resource);

    for (int i = 0; i != sz ; ++i)
      s.insert(foos[i]);

    stop_counters(time, resource);
    std::ostringstream ostr;
    ostr << container_desc << sz << " Foo insertions";
    report_performance(__FILE__, ostr.str().c_str(), time, resource);

    // Try to insert again to check performance of collision detection
    
    const int nb_loop = 10;
    start_counters(time, resource);

    for (int j = 0; j != nb_loop; ++j)
      for (int i = 0; i != sz; ++i)
	s.insert(foos[i]);

    stop_counters(time, resource);
    ostr.str("");
    ostr << container_desc << nb_loop << " times insertion of "
	 << sz << " Foo";
    report_performance(__FILE__, ostr.str().c_str(), time, resource);
  }

template<bool cache>
  using __tr1_uset = std::tr1::__unordered_set<Foo, HashFunction,
					       std::equal_to<Foo>,
					       std::allocator<Foo>,
					       cache>;
template<bool cache>
  using __tr1_umset = std::tr1::__unordered_multiset<Foo, HashFunction,
						     std::equal_to<Foo>,
						     std::allocator<Foo>,
						     cache>;
template<bool cache>
  using __uset = std::__uset_hashtable<Foo, HashFunction,
				       std::equal_to<Foo>,
				       std::allocator<Foo>,
				       std::__uset_traits<cache>>;
template<bool cache>
  using __umset = std::__umset_hashtable<Foo, HashFunction,
					 std::equal_to<Foo>,
					 std::allocator<Foo>,
					 std::__uset_traits<cache>>;

int main()
{
  bench<__tr1_uset<false>>("std::tr1::unordered_set without hash code cached ");
  bench<__tr1_uset<true>>("std::tr1::unordered_set with hash code cached ");
  bench<__tr1_umset<false>>("std::tr1::unordered_multiset without hash code cached ");
  bench<__tr1_umset<true>>("std::tr1::unordered_multiset with hash code cached ");
  bench<__uset<false>>("std::unordered_set without hash code cached ");
  bench<__uset<true>>("std::unordered_set with hash code cached ");
  bench<__umset<false>>("std::unordered_multiset without hash code cached ");
  bench<__umset<true>>("std::unordered_multiset with hash code cached ");
}
