// Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <testsuite_performance.h>
#include <random>
#include <sstream>
#include <tr1/unordered_set>
#include <unordered_set>

#define USE_MY_FOO 1

struct Foo
{
#if USE_MY_FOO

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

#else

  int bar;
  int baz;
  int meh;

  Foo() 
  { bar = random(); baz = random(); meh = random(); }
  Foo(const Foo&) = default;

#endif

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

const int sz = 300000;

template<typename _ContType>
  void
  bench(const char* container_desc, const typename _ContType::value_type* foos)
  {
    using namespace __gnu_test;

    _ContType s;

    time_counter time;
    resource_counter resource;
    start_counters(time, resource);

    for (int i = 0; i != sz ; ++i)
	s.insert(foos[i]);

    stop_counters(time, resource);
    std::ostringstream ostr;
    ostr << container_desc << sz << " insertion attempts, " 
	 << s.size() << " inserted";
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
	 << sz << " elements";
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
					 std::__umset_traits<cache>>;

template<bool cache>
  using __uset2 =
	      std::_Hashtable<Foo, Foo, std::allocator<Foo>,
			      std::__detail::_Identity,
			      std::equal_to<Foo>, HashFunction,
			      std::__detail::_Mask_range_hashing,
			      std::__detail::_Default_ranged_hash,
			      std::__detail::_Power2_rehash_policy,
			      std::__uset_traits<cache>>;

template<bool cache>
  using __umset2 =
	      std::_Hashtable<Foo, Foo, std::allocator<Foo>,
			      std::__detail::_Identity,
			      std::equal_to<Foo>, HashFunction,
			      std::__detail::_Mask_range_hashing,
			      std::__detail::_Default_ranged_hash,
			      std::__detail::_Power2_rehash_policy,
			      std::__umset_traits<cache>>;

int main()
{
  using namespace __gnu_test;

  {
    int bars[sz];
    for (int i = 0; i != sz; ++i)
      bars[i] = i;
    bench<std::tr1::unordered_set<int>>(
	"std::tr1::unordered_set<int> ", bars);
    bench<std::unordered_set<int>>(
	"std::unordered_set<int> ", bars);
  }

  Foo foos[sz];
#if USE_MY_FOO
  {
    std::random_device randev;
    for (int i = 0; i != sz; ++i)
      foos[i].init(randev);
  }
#endif

  time_counter time;
  resource_counter resource;
  start_counters(time, resource);

  bench<__tr1_uset<false>>(
	"std::tr1::unordered_set without hash code cached ", foos);
  bench<__tr1_uset<true>>(
	"std::tr1::unordered_set with hash code cached ", foos);
  bench<__tr1_umset<false>>(
	"std::tr1::unordered_multiset without hash code cached ", foos);
  bench<__tr1_umset<true>>(
	"std::tr1::unordered_multiset with hash code cached ", foos);

  stop_counters(time, resource);
  report_performance(__FILE__, "tr1 benches", time, resource);

  start_counters(time, resource);
  bench<__uset<false>>(
	"std::unordered_set without hash code cached ", foos);
  bench<__uset<true>>(
	"std::unordered_set with hash code cached ", foos);
  bench<__umset<false>>(
	"std::unordered_multiset without hash code cached ", foos);
  bench<__umset<true>>(
	"std::unordered_multiset with hash code cached ", foos);

  stop_counters(time, resource);
  report_performance(__FILE__, "std benches", time, resource);

  start_counters(time, resource);
  bench<__uset2<false>>(
	"std::unordered_set2 without hash code cached ", foos);
  bench<__uset2<true>>(
	"std::unordered_set2 with hash code cached ", foos);
  bench<__umset2<false>>(
	"std::unordered_multiset2 without hash code cached ", foos);
  bench<__umset2<true>>(
	"std::unordered_multiset2 with hash code cached ", foos);

  stop_counters(time, resource);
  report_performance(__FILE__, "std2 benches", time, resource);

  bench<std::unordered_set<Foo, HashFunction>>(
	"std::unordered_set default cache ", foos);
  bench<std::unordered_multiset<Foo, HashFunction>>(
	"std::unordered_multiset default cache ", foos);
}
