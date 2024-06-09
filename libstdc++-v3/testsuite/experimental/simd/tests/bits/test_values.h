// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#ifndef SIMD_TESTS_BITS_TEST_VALUES_H_
#define SIMD_TESTS_BITS_TEST_VALUES_H_

#include "verify.h"

#include <experimental/simd>
#include <initializer_list>
#include <random>
#include <cfenv>

template <class T, class A>
  std::experimental::simd<T, A>
  iif(std::experimental::simd_mask<T, A> k,
      const typename std::experimental::simd_mask<T, A>::simd_type& t,
      const std::experimental::simd<T, A>& f)
  {
    auto r = f;
    where(k, r) = t;
    return r;
  }

template <class V>
  V
  epilogue_load(const typename V::value_type* mem, const std::size_t size)
  {
    const int rem = size % V::size();
    return where(V([](int i) { return i; }) < rem, V(0))
	     .copy_from(mem + size / V::size() * V::size(),
			std::experimental::element_aligned);
  }

template <class V, class... F>
  void
  test_values(const std::initializer_list<typename V::value_type>& inputs,
	      F&&... fun_pack)
  {
    for (auto it = inputs.begin(); it + V::size() <= inputs.end();
	 it += V::size())
      {
	[](auto...) {
	}((fun_pack(V(&it[0], std::experimental::element_aligned)), 0)...);
      }
    [](auto...) {
    }((fun_pack(epilogue_load<V>(inputs.begin(), inputs.size())), 0)...);
  }

template <class V>
  struct RandomValues
  {
    using T = typename V::value_type;
    static constexpr bool isfp = std::is_floating_point_v<T>;
    const std::size_t count;

    std::conditional_t<std::is_floating_point_v<T>,
		       std::uniform_real_distribution<T>,
		       std::uniform_int_distribution<T>>
      dist;

    const bool uniform;

    const T abs_max = std::__finite_max_v<T>;

    RandomValues(std::size_t count_, T min, T max)
    : count(count_), dist(min, max), uniform(true)
    {
      if constexpr (std::is_floating_point_v<T>)
	VERIFY(max - min <= std::__finite_max_v<T>);
    }

    RandomValues(std::size_t count_)
    : count(count_), dist(isfp ? 1 : std::__finite_min_v<T>,
			  isfp ? 2 : std::__finite_max_v<T>),
      uniform(!isfp)
    {}

    RandomValues(std::size_t count_, T abs_max_)
    : count(count_), dist(isfp ? 1 : -abs_max_, isfp ? 2 : abs_max_),
      uniform(!isfp), abs_max(abs_max_)
    {}

    template <typename URBG>
      V
      operator()(URBG& gen)
      {
	if constexpr (!isfp)
	  return V([&](int) { return dist(gen); });
	else if (uniform)
	  return V([&](int) { return dist(gen); });
	else
	  {
	    auto exp_dist
	      = std::normal_distribution<float>(0.f,
						std::__max_exponent_v<T> * .5f);
	    return V([&](int) {
		     const T mant = dist(gen);
		     T fp = 0;
		     do {
		       const int exp = exp_dist(gen);
		       fp = std::ldexp(mant, exp);
		     } while (fp >= abs_max || fp <= std::__denorm_min_v<T>);
		     fp = gen() & 0x4 ? fp : -fp;
		     return fp;
		   });
	  }
      }
  };

static std::mt19937 g_mt_gen{0};

template <class V, class... F>
  void
  test_values(const std::initializer_list<typename V::value_type>& inputs,
	      RandomValues<V> random, F&&... fun_pack)
  {
    test_values<V>(inputs, fun_pack...);
    for (size_t i = 0; i < (random.count + V::size() - 1) / V::size(); ++i)
      {
	[](auto...) {}((fun_pack(random(g_mt_gen)), 0)...);
      }
  }

template <class V, class... F>
  void
  test_values_2arg(const std::initializer_list<typename V::value_type>& inputs,
		   F&&... fun_pack)
  {
    for (auto scalar_it = inputs.begin(); scalar_it != inputs.end();
	 ++scalar_it)
      {
	for (auto it = inputs.begin(); it + V::size() <= inputs.end();
	     it += V::size())
	  {
	    [](auto...) {
	    }((fun_pack(V(&it[0], std::experimental::element_aligned),
			V(*scalar_it)),
	       0)...);
	  }
	[](auto...) {
	}((fun_pack(epilogue_load<V>(inputs.begin(), inputs.size()),
		    V(*scalar_it)),
	   0)...);
      }
  }

template <class V, class... F>
  void
  test_values_2arg(const std::initializer_list<typename V::value_type>& inputs,
		   RandomValues<V> random, F&&... fun_pack)
  {
    test_values_2arg<V>(inputs, fun_pack...);
    for (size_t i = 0; i < (random.count + V::size() - 1) / V::size(); ++i)
      {
	[](auto...) {}((fun_pack(random(g_mt_gen), random(g_mt_gen)), 0)...);
      }
  }

template <class V, class... F>
  void
  test_values_3arg(const std::initializer_list<typename V::value_type>& inputs,
		   F&&... fun_pack)
  {
    for (auto scalar_it1 = inputs.begin(); scalar_it1 != inputs.end();
	 ++scalar_it1)
      {
	for (auto scalar_it2 = inputs.begin(); scalar_it2 != inputs.end();
	     ++scalar_it2)
	  {
	    for (auto it = inputs.begin(); it + V::size() <= inputs.end();
		 it += V::size())
	      {
		[](auto...) {
		}((fun_pack(V(&it[0], std::experimental::element_aligned),
			    V(*scalar_it1), V(*scalar_it2)),
		   0)...);
	      }
	    [](auto...) {
	    }((fun_pack(epilogue_load<V>(inputs.begin(), inputs.size()),
			V(*scalar_it1), V(*scalar_it2)),
	       0)...);
	  }
      }
  }

template <class V, class... F>
  void
  test_values_3arg(const std::initializer_list<typename V::value_type>& inputs,
		   RandomValues<V> random, F&&... fun_pack)
  {
    test_values_3arg<V>(inputs, fun_pack...);
    for (size_t i = 0; i < (random.count + V::size() - 1) / V::size(); ++i)
      {
	[](auto...) {
	}((fun_pack(random(g_mt_gen), random(g_mt_gen), random(g_mt_gen)),
	   0)...);
      }
  }

#if __GCC_IEC_559 < 2
// Without IEC559 we consider -0, subnormals, +/-inf, and all NaNs to be
// invalid (potential UB when used or "produced"). This can't use isnormal (or
// any other classification function), since they know about the UB.
template <class V>
  typename V::mask_type
  isvalid(V x)
  {
    using namespace std::experimental::parallelism_v2;
    using namespace std::experimental::parallelism_v2::__proposed;
    using T = typename V::value_type;
    if constexpr (sizeof(T) <= sizeof(double))
      {
	using I = rebind_simd_t<__int_for_sizeof_t<T>, V>;
	const I abs_x = simd_bit_cast<I>(abs(x));
	const I min = simd_bit_cast<I>(V(std::__norm_min_v<T>));
	const I max = simd_bit_cast<I>(V(std::__finite_max_v<T>));
	return static_simd_cast<typename V::mask_type>(
		 simd_bit_cast<I>(x) == 0 || (abs_x >= min && abs_x <= max));
      }
    else
      {
	const V abs_x = abs(x);
	const V min = std::__norm_min_v<T>;
	// Make max non-const static to inhibit constprop. Otherwise the
	// compiler might decide `abs_x <= max` is constexpr true, by definition
	// (-ffinite-math-only)
	static V max = std::__finite_max_v<T>;
	return (x == 0 && copysign(V(1), x) == V(1))
		 || (abs_x >= min && abs_x <= max);
      }
  }

#define MAKE_TESTER_2(name_, reference_)                                       \
  [&](auto... inputs) {                                                        \
    ((where(!isvalid(inputs), inputs) = 1), ...);                              \
    const auto totest = name_(inputs...);                                      \
    using R = std::remove_const_t<decltype(totest)>;                           \
    auto&& expected = [&](const auto&... vs) -> const R {                      \
      R tmp = {};                                                              \
      for (std::size_t i = 0; i < R::size(); ++i)                              \
	tmp[i] = reference_(vs[i]...);                                         \
      return tmp;                                                              \
    };                                                                         \
    const R expect1 = expected(inputs...);                                     \
    if constexpr (std::is_floating_point_v<typename R::value_type>)            \
      {                                                                        \
	((where(!isvalid(expect1), inputs) = 1), ...);                         \
	const R expect2 = expected(inputs...);                                 \
	((FUZZY_COMPARE(name_(inputs...), expect2) << "\ninputs = ")           \
	 << ... << inputs);                                                    \
      }                                                                        \
    else                                                                       \
      ((COMPARE(name_(inputs...), expect1) << "\n" #name_ "(")                 \
       << ... << inputs)                                                       \
	<< ")";                                                                \
  }

#define MAKE_TESTER_NOFPEXCEPT(name_)                                          \
  [&](auto... inputs) {                                                        \
    ((where(!isvalid(inputs), inputs) = 1), ...);                              \
    using R = std::remove_const_t<decltype(name_(inputs...))>;                 \
    auto&& expected = [&](const auto&... vs) -> const R {                      \
      R tmp = {};                                                              \
      for (std::size_t i = 0; i < R::size(); ++i)                              \
	tmp[i] = std::name_(vs[i]...);                                         \
      return tmp;                                                              \
    };                                                                         \
    const R expect1 = expected(inputs...);                                     \
    if constexpr (std::is_floating_point_v<typename R::value_type>)            \
      {                                                                        \
	((where(!isvalid(expect1), inputs) = 1), ...);                         \
	std::feclearexcept(FE_ALL_EXCEPT);                                     \
	asm volatile("");                                                      \
	auto totest = name_(inputs...);                                        \
	asm volatile("");                                                      \
	((COMPARE(std::fetestexcept(FE_ALL_EXCEPT), 0) << "\n" #name_ "(")     \
	 << ... << inputs)                                                     \
	  << ")";                                                              \
	const R expect2 = expected(inputs...);                                 \
	std::feclearexcept(FE_ALL_EXCEPT);                                     \
	asm volatile("");                                                      \
	totest = name_(inputs...);                                             \
	asm volatile("");                                                      \
	((COMPARE(std::fetestexcept(FE_ALL_EXCEPT), 0) << "\n" #name_ "(")     \
	 << ... << inputs)                                                     \
	  << ")";                                                              \
	((FUZZY_COMPARE(totest, expect2) << "\n" #name_ "(") << ... << inputs) \
	  << ")";                                                              \
      }                                                                        \
    else                                                                       \
      {                                                                        \
	std::feclearexcept(FE_ALL_EXCEPT);                                     \
	asm volatile("");                                                      \
	auto totest = name_(inputs...);                                        \
	asm volatile("");                                                      \
	((COMPARE(std::fetestexcept(FE_ALL_EXCEPT), 0) << "\n" #name_ "(")     \
	 << ... << inputs)                                                     \
	  << ")";                                                              \
	((COMPARE(totest, expect1) << "\n" #name_ "(") << ... << inputs)       \
	  << ")";                                                              \
      }                                                                        \
  }

#else

#define MAKE_TESTER_2(name_, reference_)                                       \
  [&](auto... inputs) {                                                        \
    const auto totest = name_(inputs...);                                      \
    using R = std::remove_const_t<decltype(totest)>;                           \
    auto&& expected = [&](const auto&... vs) -> const R {                      \
      R tmp = {};                                                              \
      for (std::size_t i = 0; i < R::size(); ++i)                              \
	tmp[i] = reference_(vs[i]...);                                         \
      return tmp;                                                              \
    };                                                                         \
    const R expect1 = expected(inputs...);                                     \
    if constexpr (std::is_floating_point_v<typename R::value_type>)            \
      {                                                                        \
	((COMPARE(isnan(totest), isnan(expect1)) << #name_ "(")                \
	 << ... << inputs)                                                     \
	  << ") = " << totest << " != " << expect1;                            \
	((where(isnan(expect1), inputs) = 0), ...);                            \
	((FUZZY_COMPARE(name_(inputs...), expected(inputs...))                 \
	  << "\nclean = ")                                                     \
	 << ... << inputs);                                                    \
      }                                                                        \
    else                                                                       \
      ((COMPARE(name_(inputs...), expect1) << "\n" #name_ "(")                 \
       << ... << inputs)                                                       \
	<< ")";                                                                \
  }

#define MAKE_TESTER_NOFPEXCEPT(name_)                                          \
  [&](auto... inputs) {                                                        \
    std::feclearexcept(FE_ALL_EXCEPT);                                         \
    auto totest = name_(inputs...);                                            \
    ((COMPARE(std::fetestexcept(FE_ALL_EXCEPT), 0) << "\n" #name_ "(")         \
     << ... << inputs)                                                         \
      << ")";                                                                  \
    using R = std::remove_const_t<decltype(totest)>;                           \
    auto&& expected = [&](const auto&... vs) -> const R {                      \
      R tmp = {};                                                              \
      for (std::size_t i = 0; i < R::size(); ++i)                              \
	tmp[i] = std::name_(vs[i]...);                                         \
      return tmp;                                                              \
    };                                                                         \
    const R expect1 = expected(inputs...);                                     \
    if constexpr (std::is_floating_point_v<typename R::value_type>)            \
      {                                                                        \
	((COMPARE(isnan(totest), isnan(expect1)) << #name_ "(")                \
	 << ... << inputs)                                                     \
	  << ") = " << totest << " != " << expect1;                            \
	((where(isnan(expect1), inputs) = 0), ...);                            \
	const R expect2 = expected(inputs...);                                 \
	std::feclearexcept(FE_ALL_EXCEPT);                                     \
	asm volatile("");                                                      \
	totest = name_(inputs...);                                             \
	asm volatile("");                                                      \
	((COMPARE(std::fetestexcept(FE_ALL_EXCEPT), 0) << "\n" #name_ "(")     \
	 << ... << inputs)                                                     \
	  << ")";                                                              \
	FUZZY_COMPARE(totest, expect2);                                        \
      }                                                                        \
    else                                                                       \
      {                                                                        \
	((COMPARE(totest, expect1) << "\n" #name_ "(") << ... << inputs)       \
	  << ")";                                                              \
      }                                                                        \
  }

#endif

#define MAKE_TESTER(name_) MAKE_TESTER_2(name_, std::name_)
#endif  // SIMD_TESTS_BITS_TEST_VALUES_H_
