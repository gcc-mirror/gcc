// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#ifndef TESTS_BITS_VERIFY_H_
#define TESTS_BITS_VERIFY_H_

#include <experimental/simd>
#include <sstream>
#include <iomanip>
#include "ulp.h"

#ifdef _GLIBCXX_SIMD_HAVE_NEON
// work around PR89357:
#define alignas(...) __attribute__((aligned(__VA_ARGS__)))
#endif

using schar = signed char;
using uchar = unsigned char;
using ushort = unsigned short;
using uint = unsigned int;
using ulong = unsigned long;
using llong = long long;
using ullong = unsigned long long;
using ldouble = long double;
using wchar = wchar_t;
using char16 = char16_t;
using char32 = char32_t;

template <class T>
  T
  make_value_unknown(const T& x)
  {
    if constexpr (std::is_constructible_v<T, const volatile T&>)
      {
	const volatile T& y = x;
	return y;
      }
    else
      {
	T y = x;
	asm("" : "+m"(y));
	return y;
      }
  }

class verify
{
  const bool m_failed = false;
  size_t m_ip = 0;

  template <typename T,
	    typename = decltype(std::declval<std::stringstream&>()
				<< std::declval<const T&>())>
    void
    print(const T& x, int) const
    {
      std::stringstream ss;
      ss << x;
      __builtin_fprintf(stderr, "%s", ss.str().c_str());
    }

  template <typename T>
    void
    print(const T& x, ...) const
    {
      if constexpr (std::experimental::is_simd_v<T>)
	{
	  std::stringstream ss;
	  if constexpr (std::is_floating_point_v<typename T::value_type>)
	    {
	      ss << '(' << x[0] << " == " << std::hexfloat << x[0]
		<< std::defaultfloat << ')';
	      for (unsigned i = 1; i < x.size(); ++i)
		{
		  ss << (i % 4 == 0 ? ",\n(" : ", (") << x[i]
		    << " == " << std::hexfloat << x[i] << std::defaultfloat
		    << ')';
		}
	    }
	  else
	    {
	      ss << +x[0];
	      for (unsigned i = 1; i < x.size(); ++i)
		{
		  ss << ", " << +x[i];
		}
	    }
	  __builtin_fprintf(stderr, "%s", ss.str().c_str());
	}
      else if constexpr (std::experimental::is_simd_mask_v<T>)
	{
	  __builtin_fprintf(stderr, (x[0] ? "[1" : "[0"));
	  for (unsigned i = 1; i < x.size(); ++i)
	    {
	      __builtin_fprintf(stderr, (x[i] ? "1" : "0"));
	    }
	  __builtin_fprintf(stderr, "]");
	}
      else
	{
	  print_hex(&x, sizeof(T));
	}
    }

  void
  print_hex(const void* x, std::size_t n) const
  {
    __builtin_fprintf(stderr, "0x");
    const auto* bytes = static_cast<const unsigned char*>(x);
    for (std::size_t i = 0; i < n; ++i)
      {
	__builtin_fprintf(stderr, (i && i % 4 == 0) ? "'%02x" : "%02x",
			  bytes[i]);
      }
  }

public:
  template <typename... Ts>
    [[gnu::always_inline]]
    verify(bool ok, const char* file, const int line,
	   const char* func, const char* cond, const Ts&... extra_info)
    : m_failed(!ok), m_ip(get_ip())
    {
      if (m_failed)
	[&] {
	  __builtin_fprintf(stderr, "%s:%d: (%s):\nInstruction Pointer: %zx\n"
				    "Assertion '%s' failed.\n",
			    file, line, func, m_ip, cond);
	  (print(extra_info, int()), ...);
	}();
    }

  [[gnu::always_inline]] ~verify()
  {
    if (m_failed)
      {
	__builtin_fprintf(stderr, "\n");
	__builtin_abort();
      }
  }

  template <typename T>
    [[gnu::always_inline]]
    const verify&
    operator<<(const T& x) const
    {
      if (m_failed)
	print(x, int());
      return *this;
    }

  template <typename... Ts>
    [[gnu::always_inline]]
    const verify&
    on_failure(const Ts&... xs) const
    {
      if (m_failed)
	[&] { (print(xs, int()), ...); }();
      return *this;
    }

  [[gnu::always_inline]] static inline
  size_t
  get_ip()
  {
    size_t _ip = 0;
#ifdef __x86_64__
    asm volatile("lea 0(%%rip),%0" : "=r"(_ip));
#elif defined __i386__
    asm volatile("1: movl $1b,%0" : "=r"(_ip));
#elif defined __arm__
    asm volatile("mov %0,pc" : "=r"(_ip));
#elif defined __aarch64__
    asm volatile("adr %0,." : "=r"(_ip));
#endif
    return _ip;
  }
};

#if __FLT_EVAL_METHOD__ != 0
template <typename T>
  [[gnu::always_inline]] inline decltype(auto)
  force_fp_truncation(const T& x)
  {
    namespace stdx = std::experimental;
    if constexpr (stdx::is_simd_v<T>)
      {
	using U = typename T::value_type;
	if constexpr (std::is_floating_point_v<typename T::value_type>
		      && sizeof(U) <= 8 && (sizeof(T) < 16 || std::is_same_v<
		  T, stdx::fixed_size_simd<U, T::size()>>))
	  {
	    T y = x;
	    asm("" : "+m"(y));
	    return y;
	  }
	else
	  return x;
      }
    else if constexpr (std::is_floating_point_v<T> && sizeof(T) <= 8)
      {
	T y = x;
	asm("" : "+m"(y));
	return y;
      }
    else
      return x;
  }

#define COMPARE(_a, _b)                                                        \
  [&](auto&& _aa, auto&& _bb) {                                                \
    return verify(std::experimental::all_of(_aa == _bb), __FILE__, __LINE__,   \
		  __PRETTY_FUNCTION__, #_a " == " #_b, #_a " = ", _aa,         \
		  "\n" #_b " = ", _bb);                                        \
  }(force_fp_truncation(_a), force_fp_truncation(_b))
#else
#define COMPARE(_a, _b)                                                        \
  [&](auto&& _aa, auto&& _bb) {                                                \
    return verify(std::experimental::all_of(_aa == _bb), __FILE__, __LINE__,   \
		  __PRETTY_FUNCTION__, #_a " == " #_b, #_a " = ", _aa,         \
		  "\n" #_b " = ", _bb);                                        \
  }((_a), (_b))
#endif

#define VERIFY(_test)                                                          \
  verify(_test, __FILE__, __LINE__, __PRETTY_FUNCTION__, #_test)

  // ulp_distance_signed can raise FP exceptions and thus must be conditionally
  // executed
#define ULP_COMPARE(_a, _b, _allowed_distance)                                 \
  [&](auto&& _aa, auto&& _bb) {                                                \
    const bool success = std::experimental::all_of(                            \
      vir::test::ulp_distance(_aa, _bb) <= (_allowed_distance));               \
    return verify(success, __FILE__, __LINE__, __PRETTY_FUNCTION__,            \
		  #_a " ~~ " #_b, #_a " = ", _aa, "\n" #_b " = ", _bb,         \
		  "\ndistance = ",                                             \
		  success ? 0 : vir::test::ulp_distance_signed(_aa, _bb));     \
  }((_a), (_b))

namespace vir {
  namespace test
  {
    template <typename T>
      inline T _S_fuzzyness = 0;

    template <typename T>
      void
      setFuzzyness(T x)
      { _S_fuzzyness<T> = x; }
  } // namespace test
} // namespace vir

#define FUZZY_COMPARE(_a, _b)                                                  \
  ULP_COMPARE(                                                                 \
    _a, _b,                                                                    \
    vir::test::_S_fuzzyness<vir::test::value_type_t<decltype((_a) + (_b))>>)

template <typename V>
  void
  test();

template <typename V>
  void
  invoke_test(...)
  {}

template <typename V, typename = decltype(V())>
  void
  invoke_test(int)
  {
    test<V>();
    __builtin_fprintf(stderr, "PASS: %s\n", __PRETTY_FUNCTION__);
  }

#endif  // TESTS_BITS_VERIFY_H_
