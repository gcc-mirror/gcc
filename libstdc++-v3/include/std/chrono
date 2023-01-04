// <chrono> -*- C++ -*-

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/chrono
 *  This is a Standard C++ Library header.
 *  @ingroup chrono
 */

#ifndef _GLIBCXX_CHRONO
#define _GLIBCXX_CHRONO 1

#pragma GCC system_header

#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else

#include <ratio>
#include <type_traits>
#include <limits>
#include <ctime>
#include <bits/parse_numbers.h> // for literals support.
#if __cplusplus > 201703L
# include <concepts>
# include <compare>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#if __cplusplus >= 201703L
  namespace filesystem { struct __file_clock; };
#endif

  /**
   * @defgroup chrono Time
   * @ingroup utilities
   *
   * Classes and functions for time.
   *
   * @since C++11
   */

  /** @namespace std::chrono
   *  @brief ISO C++ 2011 namespace for date and time utilities
   *  @ingroup chrono
   */
  namespace chrono
  {
    /// @addtogroup chrono
    /// @{

    /// `chrono::duration` represents a distance between two points in time
    template<typename _Rep, typename _Period = ratio<1>>
      struct duration;

    /// `chrono::time_point` represents a point in time as measured by a clock
    template<typename _Clock, typename _Dur = typename _Clock::duration>
      struct time_point;
    /// @}
  }

  /// @addtogroup chrono
  /// @{

  // 20.11.4.3 specialization of common_type (for duration, sfinae-friendly)

  /// @cond undocumented

  template<typename _CT, typename _Period1, typename _Period2, typename = void>
    struct __duration_common_type
    { };

  template<typename _CT, typename _Period1, typename _Period2>
    struct __duration_common_type<_CT, _Period1, _Period2,
				  __void_t<typename _CT::type>>
    {
    private:
      using __gcd_num = __static_gcd<_Period1::num, _Period2::num>;
      using __gcd_den = __static_gcd<_Period1::den, _Period2::den>;
      using __cr = typename _CT::type;
      using __r = ratio<__gcd_num::value,
			(_Period1::den / __gcd_den::value) * _Period2::den>;

    public:
      using type = chrono::duration<__cr, typename __r::type>;
    };

  /// @endcond

  /// @{
  /// @relates chrono::duration

  /// Specialization of common_type for chrono::duration types.
  template<typename _Rep1, typename _Period1, typename _Rep2, typename _Period2>
    struct common_type<chrono::duration<_Rep1, _Period1>,
		       chrono::duration<_Rep2, _Period2>>
    : __duration_common_type<common_type<_Rep1, _Rep2>,
			     typename _Period1::type,
			     typename _Period2::type>
    { };

  /// Specialization of common_type for two identical chrono::duration types.
  template<typename _Rep, typename _Period>
    struct common_type<chrono::duration<_Rep, _Period>,
		       chrono::duration<_Rep, _Period>>
    {
      using type = chrono::duration<typename common_type<_Rep>::type,
				    typename _Period::type>;
    };

  /// Specialization of common_type for one chrono::duration type.
  template<typename _Rep, typename _Period>
    struct common_type<chrono::duration<_Rep, _Period>>
    {
      using type = chrono::duration<typename common_type<_Rep>::type,
				    typename _Period::type>;
    };
  /// @}

  // 20.11.4.3 specialization of common_type (for time_point, sfinae-friendly)

  /// @cond undocumented

  template<typename _CT, typename _Clock, typename = void>
    struct __timepoint_common_type
    { };

  template<typename _CT, typename _Clock>
    struct __timepoint_common_type<_CT, _Clock, __void_t<typename _CT::type>>
    {
      using type = chrono::time_point<_Clock, typename _CT::type>;
    };

  /// @endcond

  /// @{
  /// @relates chrono::time_point

  /// Specialization of common_type for chrono::time_point types.
  template<typename _Clock, typename _Duration1, typename _Duration2>
    struct common_type<chrono::time_point<_Clock, _Duration1>,
		       chrono::time_point<_Clock, _Duration2>>
    : __timepoint_common_type<common_type<_Duration1, _Duration2>, _Clock>
    { };

  /// Specialization of common_type for two identical chrono::time_point types.
  template<typename _Clock, typename _Duration>
    struct common_type<chrono::time_point<_Clock, _Duration>,
		       chrono::time_point<_Clock, _Duration>>
    { using type = chrono::time_point<_Clock, _Duration>; };

  /// Specialization of common_type for one chrono::time_point type.
  template<typename _Clock, typename _Duration>
    struct common_type<chrono::time_point<_Clock, _Duration>>
    { using type = chrono::time_point<_Clock, _Duration>; };
  /// @}

  /// @} group chrono

  namespace chrono
  {
    /// @addtogroup chrono
    /// @{

    /// @cond undocumented

    // Primary template for duration_cast impl.
    template<typename _ToDur, typename _CF, typename _CR,
	     bool _NumIsOne = false, bool _DenIsOne = false>
      struct __duration_cast_impl
      {
	template<typename _Rep, typename _Period>
	  static constexpr _ToDur
	  __cast(const duration<_Rep, _Period>& __d)
	  {
	    typedef typename _ToDur::rep			__to_rep;
	    return _ToDur(static_cast<__to_rep>(static_cast<_CR>(__d.count())
	      * static_cast<_CR>(_CF::num)
	      / static_cast<_CR>(_CF::den)));
	  }
      };

    template<typename _ToDur, typename _CF, typename _CR>
      struct __duration_cast_impl<_ToDur, _CF, _CR, true, true>
      {
	template<typename _Rep, typename _Period>
	  static constexpr _ToDur
	  __cast(const duration<_Rep, _Period>& __d)
	  {
	    typedef typename _ToDur::rep			__to_rep;
	    return _ToDur(static_cast<__to_rep>(__d.count()));
	  }
      };

    template<typename _ToDur, typename _CF, typename _CR>
      struct __duration_cast_impl<_ToDur, _CF, _CR, true, false>
      {
	template<typename _Rep, typename _Period>
	  static constexpr _ToDur
	  __cast(const duration<_Rep, _Period>& __d)
	  {
	    typedef typename _ToDur::rep			__to_rep;
	    return _ToDur(static_cast<__to_rep>(
	      static_cast<_CR>(__d.count()) / static_cast<_CR>(_CF::den)));
	  }
      };

    template<typename _ToDur, typename _CF, typename _CR>
      struct __duration_cast_impl<_ToDur, _CF, _CR, false, true>
      {
	template<typename _Rep, typename _Period>
	  static constexpr _ToDur
	  __cast(const duration<_Rep, _Period>& __d)
	  {
	    typedef typename _ToDur::rep			__to_rep;
	    return _ToDur(static_cast<__to_rep>(
	      static_cast<_CR>(__d.count()) * static_cast<_CR>(_CF::num)));
	  }
      };

    template<typename _Tp>
      struct __is_duration
      : std::false_type
      { };

    template<typename _Rep, typename _Period>
      struct __is_duration<duration<_Rep, _Period>>
      : std::true_type
      { };

    template<typename _Tp>
      using __enable_if_is_duration
	= typename enable_if<__is_duration<_Tp>::value, _Tp>::type;

    template<typename _Tp>
      using __disable_if_is_duration
	= typename enable_if<!__is_duration<_Tp>::value, _Tp>::type;

    /// @endcond

    /// duration_cast
    template<typename _ToDur, typename _Rep, typename _Period>
      constexpr __enable_if_is_duration<_ToDur>
      duration_cast(const duration<_Rep, _Period>& __d)
      {
	typedef typename _ToDur::period				__to_period;
	typedef typename _ToDur::rep				__to_rep;
	typedef ratio_divide<_Period, __to_period> 		__cf;
	typedef typename common_type<__to_rep, _Rep, intmax_t>::type
	  							__cr;
	typedef  __duration_cast_impl<_ToDur, __cf, __cr,
				      __cf::num == 1, __cf::den == 1> __dc;
	return __dc::__cast(__d);
      }

    /// treat_as_floating_point
    template<typename _Rep>
      struct treat_as_floating_point
      : is_floating_point<_Rep>
      { };

#if __cplusplus > 201402L
    template <typename _Rep>
      inline constexpr bool treat_as_floating_point_v =
        treat_as_floating_point<_Rep>::value;
#endif // C++17

#if __cplusplus > 201703L
    template<typename _Tp>
      struct is_clock;

    template<typename _Tp>
      inline constexpr bool is_clock_v = is_clock<_Tp>::value;

#if __cpp_lib_concepts
    template<typename _Tp>
      struct is_clock : false_type
      { };

    template<typename _Tp>
      requires requires {
	typename _Tp::rep;
	typename _Tp::period;
	typename _Tp::duration;
	typename _Tp::time_point::clock;
	typename _Tp::time_point::duration;
	{ &_Tp::is_steady } -> same_as<const bool*>;
	{ _Tp::now() } -> same_as<typename _Tp::time_point>;
	requires same_as<typename _Tp::duration,
			 duration<typename _Tp::rep, typename _Tp::period>>;
	requires same_as<typename _Tp::time_point::duration,
			 typename _Tp::duration>;
      }
      struct is_clock<_Tp> : true_type
      { };
#else
    template<typename _Tp, typename = void>
      struct __is_clock_impl : false_type
      { };

    template<typename _Tp>
      struct __is_clock_impl<_Tp,
			     void_t<typename _Tp::rep, typename _Tp::period,
				    typename _Tp::duration,
				    typename _Tp::time_point::duration,
				    decltype(_Tp::is_steady),
				    decltype(_Tp::now())>>
      : __and_<is_same<typename _Tp::duration,
		       duration<typename _Tp::rep, typename _Tp::period>>,
	       is_same<typename _Tp::time_point::duration,
		       typename _Tp::duration>,
	       is_same<decltype(&_Tp::is_steady), const bool*>,
	       is_same<decltype(_Tp::now()), typename _Tp::time_point>>::type
      { };

    template<typename _Tp>
      struct is_clock : __is_clock_impl<_Tp>::type
      { };
#endif
#endif // C++20

#if __cplusplus >= 201703L
# define __cpp_lib_chrono 201611

    template<typename _ToDur, typename _Rep, typename _Period>
      constexpr __enable_if_is_duration<_ToDur>
      floor(const duration<_Rep, _Period>& __d)
      {
	auto __to = chrono::duration_cast<_ToDur>(__d);
	if (__to > __d)
	  return __to - _ToDur{1};
	return __to;
      }

    template<typename _ToDur, typename _Rep, typename _Period>
      constexpr __enable_if_is_duration<_ToDur>
      ceil(const duration<_Rep, _Period>& __d)
      {
	auto __to = chrono::duration_cast<_ToDur>(__d);
	if (__to < __d)
	  return __to + _ToDur{1};
	return __to;
      }

    template <typename _ToDur, typename _Rep, typename _Period>
      constexpr enable_if_t<
	__and_<__is_duration<_ToDur>,
	       __not_<treat_as_floating_point<typename _ToDur::rep>>>::value,
	_ToDur>
      round(const duration<_Rep, _Period>& __d)
      {
	_ToDur __t0 = chrono::floor<_ToDur>(__d);
	_ToDur __t1 = __t0 + _ToDur{1};
	auto __diff0 = __d - __t0;
	auto __diff1 = __t1 - __d;
	if (__diff0 == __diff1)
	{
	    if (__t0.count() & 1)
		return __t1;
	    return __t0;
	}
	else if (__diff0 < __diff1)
	    return __t0;
	return __t1;
      }

    template<typename _Rep, typename _Period>
      constexpr
      enable_if_t<numeric_limits<_Rep>::is_signed, duration<_Rep, _Period>>
      abs(duration<_Rep, _Period> __d)
      {
	if (__d >= __d.zero())
	  return __d;
	return -__d;
      }

    // Make chrono::ceil<D> also usable as chrono::__detail::ceil<D>.
    namespace __detail { using chrono::ceil; }

#else // ! C++17

    // We want to use ceil even when compiling for earlier standards versions.
    // C++11 only allows a single statement in a constexpr function, so we
    // need to move the comparison into a separate function, __ceil_impl.
    namespace __detail
    {
      template<typename _Tp, typename _Up>
	constexpr _Tp
	__ceil_impl(const _Tp& __t, const _Up& __u)
	{
	  return (__t < __u) ? (__t + _Tp{1}) : __t;
	}

      // C++11-friendly version of std::chrono::ceil<D> for internal use.
      template<typename _ToDur, typename _Rep, typename _Period>
	constexpr _ToDur
	ceil(const duration<_Rep, _Period>& __d)
	{
	  return __detail::__ceil_impl(chrono::duration_cast<_ToDur>(__d), __d);
	}
    }
#endif // C++17

    /// duration_values
    template<typename _Rep>
      struct duration_values
      {
	static constexpr _Rep
	zero() noexcept
	{ return _Rep(0); }

	static constexpr _Rep
	max() noexcept
	{ return numeric_limits<_Rep>::max(); }

	static constexpr _Rep
	min() noexcept
	{ return numeric_limits<_Rep>::lowest(); }
      };

    /// @cond undocumented

    template<typename _Tp>
      struct __is_ratio
      : std::false_type
      { };

    template<intmax_t _Num, intmax_t _Den>
      struct __is_ratio<ratio<_Num, _Den>>
      : std::true_type
      { };

    /// @endcond

    template<typename _Rep, typename _Period>
      struct duration
      {
      private:
	template<typename _Rep2>
	  using __is_float = treat_as_floating_point<_Rep2>;

	static constexpr intmax_t
	_S_gcd(intmax_t __m, intmax_t __n) noexcept
	{
	  // Duration only allows positive periods so we don't need to
	  // handle negative values here (unlike __static_gcd and std::gcd).
#if __cplusplus >= 201402L
	  do
	    {
	      intmax_t __rem = __m % __n;
	      __m = __n;
	      __n = __rem;
	    }
	  while (__n != 0);
	  return __m;
#else
	  // C++11 doesn't allow loops in constexpr functions, but this
	  // recursive version can be more expensive to evaluate.
	  return (__n == 0) ? __m : _S_gcd(__n, __m % __n);
#endif
	}

	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 2094. overflow shouldn't participate in overload resolution
	// 3090. What is [2094] intended to mean?
	// This only produces a valid type if no overflow occurs.
	template<typename _R1, typename _R2,
		 intmax_t __gcd1 = _S_gcd(_R1::num, _R2::num),
		 intmax_t __gcd2 = _S_gcd(_R1::den, _R2::den)>
	  using __divide = ratio<(_R1::num / __gcd1) * (_R2::den / __gcd2),
				 (_R1::den / __gcd2) * (_R2::num / __gcd1)>;

	// _Period2 is an exact multiple of _Period
	template<typename _Period2>
	  using __is_harmonic
	    = __bool_constant<__divide<_Period2, _Period>::den == 1>;

      public:

	using rep = _Rep;
	using period = typename _Period::type;

	static_assert(!__is_duration<_Rep>::value, "rep cannot be a duration");
	static_assert(__is_ratio<_Period>::value,
		      "period must be a specialization of ratio");
	static_assert(_Period::num > 0, "period must be positive");

	// 20.11.5.1 construction / copy / destroy
	constexpr duration() = default;

	duration(const duration&) = default;

	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 3050. Conversion specification problem in chrono::duration
	template<typename _Rep2, typename = _Require<
		 is_convertible<const _Rep2&, rep>,
		 __or_<__is_float<rep>, __not_<__is_float<_Rep2>>>>>
	  constexpr explicit duration(const _Rep2& __rep)
	  : __r(static_cast<rep>(__rep)) { }

	template<typename _Rep2, typename _Period2, typename = _Require<
		 is_convertible<const _Rep2&, rep>,
		 __or_<__is_float<rep>,
		       __and_<__is_harmonic<_Period2>,
			      __not_<__is_float<_Rep2>>>>>>
	  constexpr duration(const duration<_Rep2, _Period2>& __d)
	  : __r(duration_cast<duration>(__d).count()) { }

	~duration() = default;
	duration& operator=(const duration&) = default;

	// 20.11.5.2 observer
	constexpr rep
	count() const
	{ return __r; }

	// 20.11.5.3 arithmetic

	constexpr duration<typename common_type<rep>::type, period>
	operator+() const
	{ return duration<typename common_type<rep>::type, period>(__r); }

	constexpr duration<typename common_type<rep>::type, period>
	operator-() const
	{ return duration<typename common_type<rep>::type, period>(-__r); }

	_GLIBCXX17_CONSTEXPR duration&
	operator++()
	{
	  ++__r;
	  return *this;
	}

	_GLIBCXX17_CONSTEXPR duration
	operator++(int)
	{ return duration(__r++); }

	_GLIBCXX17_CONSTEXPR duration&
	operator--()
	{
	  --__r;
	  return *this;
	}

	_GLIBCXX17_CONSTEXPR duration
	operator--(int)
	{ return duration(__r--); }

	_GLIBCXX17_CONSTEXPR duration&
	operator+=(const duration& __d)
	{
	  __r += __d.count();
	  return *this;
	}

	_GLIBCXX17_CONSTEXPR duration&
	operator-=(const duration& __d)
	{
	  __r -= __d.count();
	  return *this;
	}

	_GLIBCXX17_CONSTEXPR duration&
	operator*=(const rep& __rhs)
	{
	  __r *= __rhs;
	  return *this;
	}

	_GLIBCXX17_CONSTEXPR duration&
	operator/=(const rep& __rhs)
	{
	  __r /= __rhs;
	  return *this;
	}

	// DR 934.
	template<typename _Rep2 = rep>
	  _GLIBCXX17_CONSTEXPR
	  typename enable_if<!treat_as_floating_point<_Rep2>::value,
			     duration&>::type
	  operator%=(const rep& __rhs)
	  {
	    __r %= __rhs;
	    return *this;
	  }

	template<typename _Rep2 = rep>
	  _GLIBCXX17_CONSTEXPR
	  typename enable_if<!treat_as_floating_point<_Rep2>::value,
			     duration&>::type
	  operator%=(const duration& __d)
	  {
	    __r %= __d.count();
	    return *this;
	  }

	// 20.11.5.4 special values
	static constexpr duration
	zero() noexcept
	{ return duration(duration_values<rep>::zero()); }

	static constexpr duration
	min() noexcept
	{ return duration(duration_values<rep>::min()); }

	static constexpr duration
	max() noexcept
	{ return duration(duration_values<rep>::max()); }

      private:
	rep __r;
      };

    /// @{
    /// @relates std::chrono::duration

    /// The sum of two durations.
    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr typename common_type<duration<_Rep1, _Period1>,
				     duration<_Rep2, _Period2>>::type
      operator+(const duration<_Rep1, _Period1>& __lhs,
		const duration<_Rep2, _Period2>& __rhs)
      {
	typedef duration<_Rep1, _Period1>			__dur1;
	typedef duration<_Rep2, _Period2>			__dur2;
	typedef typename common_type<__dur1,__dur2>::type	__cd;
	return __cd(__cd(__lhs).count() + __cd(__rhs).count());
      }

    /// The difference between two durations.
    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr typename common_type<duration<_Rep1, _Period1>,
				     duration<_Rep2, _Period2>>::type
      operator-(const duration<_Rep1, _Period1>& __lhs,
		const duration<_Rep2, _Period2>& __rhs)
      {
	typedef duration<_Rep1, _Period1>			__dur1;
	typedef duration<_Rep2, _Period2>			__dur2;
	typedef typename common_type<__dur1,__dur2>::type	__cd;
	return __cd(__cd(__lhs).count() - __cd(__rhs).count());
      }

    /// @}

    /// @cond undocumented

    // SFINAE helper to obtain common_type<_Rep1, _Rep2> only if _Rep2
    // is implicitly convertible to it.
    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 3050. Conversion specification problem in chrono::duration constructor
    template<typename _Rep1, typename _Rep2,
	     typename _CRep = typename common_type<_Rep1, _Rep2>::type>
      using __common_rep_t = typename
	enable_if<is_convertible<const _Rep2&, _CRep>::value, _CRep>::type;

    /// @endcond

    /** @{
     * Arithmetic operators for chrono::duration
     * @relates std::chrono::duration
     */

    template<typename _Rep1, typename _Period, typename _Rep2>
      constexpr duration<__common_rep_t<_Rep1, _Rep2>, _Period>
      operator*(const duration<_Rep1, _Period>& __d, const _Rep2& __s)
      {
	typedef duration<typename common_type<_Rep1, _Rep2>::type, _Period>
	  __cd;
	return __cd(__cd(__d).count() * __s);
      }

    template<typename _Rep1, typename _Rep2, typename _Period>
      constexpr duration<__common_rep_t<_Rep2, _Rep1>, _Period>
      operator*(const _Rep1& __s, const duration<_Rep2, _Period>& __d)
      { return __d * __s; }

    template<typename _Rep1, typename _Period, typename _Rep2>
      constexpr
      duration<__common_rep_t<_Rep1, __disable_if_is_duration<_Rep2>>, _Period>
      operator/(const duration<_Rep1, _Period>& __d, const _Rep2& __s)
      {
	typedef duration<typename common_type<_Rep1, _Rep2>::type, _Period>
	  __cd;
	return __cd(__cd(__d).count() / __s);
      }

    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr typename common_type<_Rep1, _Rep2>::type
      operator/(const duration<_Rep1, _Period1>& __lhs,
		const duration<_Rep2, _Period2>& __rhs)
      {
	typedef duration<_Rep1, _Period1>			__dur1;
	typedef duration<_Rep2, _Period2>			__dur2;
	typedef typename common_type<__dur1,__dur2>::type	__cd;
	return __cd(__lhs).count() / __cd(__rhs).count();
      }

    // DR 934.
    template<typename _Rep1, typename _Period, typename _Rep2>
      constexpr
      duration<__common_rep_t<_Rep1, __disable_if_is_duration<_Rep2>>, _Period>
      operator%(const duration<_Rep1, _Period>& __d, const _Rep2& __s)
      {
	typedef duration<typename common_type<_Rep1, _Rep2>::type, _Period>
	  __cd;
	return __cd(__cd(__d).count() % __s);
      }

    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr typename common_type<duration<_Rep1, _Period1>,
				     duration<_Rep2, _Period2>>::type
      operator%(const duration<_Rep1, _Period1>& __lhs,
		const duration<_Rep2, _Period2>& __rhs)
      {
	typedef duration<_Rep1, _Period1>			__dur1;
	typedef duration<_Rep2, _Period2>			__dur2;
	typedef typename common_type<__dur1,__dur2>::type	__cd;
	return __cd(__cd(__lhs).count() % __cd(__rhs).count());
      }
    /// @}

    // comparisons

    /** @{
     * Comparisons for chrono::duration
     * @relates std::chrono::duration
     */

    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr bool
      operator==(const duration<_Rep1, _Period1>& __lhs,
		 const duration<_Rep2, _Period2>& __rhs)
      {
	typedef duration<_Rep1, _Period1>			__dur1;
	typedef duration<_Rep2, _Period2>			__dur2;
	typedef typename common_type<__dur1,__dur2>::type	__ct;
	return __ct(__lhs).count() == __ct(__rhs).count();
      }

    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr bool
      operator<(const duration<_Rep1, _Period1>& __lhs,
		const duration<_Rep2, _Period2>& __rhs)
      {
	typedef duration<_Rep1, _Period1>			__dur1;
	typedef duration<_Rep2, _Period2>			__dur2;
	typedef typename common_type<__dur1,__dur2>::type	__ct;
	return __ct(__lhs).count() < __ct(__rhs).count();
      }

#if __cpp_lib_three_way_comparison
    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      requires three_way_comparable<common_type_t<_Rep1, _Rep2>>
      constexpr auto
      operator<=>(const duration<_Rep1, _Period1>& __lhs,
		  const duration<_Rep2, _Period2>& __rhs)
      {
	using __ct = common_type_t<duration<_Rep1, _Period1>,
				   duration<_Rep2, _Period2>>;
	return __ct(__lhs).count() <=> __ct(__rhs).count();
      }
#else
    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr bool
      operator!=(const duration<_Rep1, _Period1>& __lhs,
		 const duration<_Rep2, _Period2>& __rhs)
      { return !(__lhs == __rhs); }
#endif

    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr bool
      operator<=(const duration<_Rep1, _Period1>& __lhs,
		 const duration<_Rep2, _Period2>& __rhs)
      { return !(__rhs < __lhs); }

    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr bool
      operator>(const duration<_Rep1, _Period1>& __lhs,
		const duration<_Rep2, _Period2>& __rhs)
      { return __rhs < __lhs; }

    template<typename _Rep1, typename _Period1,
	     typename _Rep2, typename _Period2>
      constexpr bool
      operator>=(const duration<_Rep1, _Period1>& __lhs,
		 const duration<_Rep2, _Period2>& __rhs)
      { return !(__lhs < __rhs); }

    /// @}

    /// @cond undocumented
#ifdef _GLIBCXX_USE_C99_STDINT_TR1
# define _GLIBCXX_CHRONO_INT64_T int64_t
#elif defined __INT64_TYPE__
# define _GLIBCXX_CHRONO_INT64_T __INT64_TYPE__
#else
    static_assert(std::numeric_limits<unsigned long long>::digits >= 64,
	"Representation type for nanoseconds must have at least 64 bits");
# define _GLIBCXX_CHRONO_INT64_T long long
#endif
    /// @endcond

    /// nanoseconds
    using nanoseconds	= duration<_GLIBCXX_CHRONO_INT64_T, nano>;

    /// microseconds
    using microseconds	= duration<_GLIBCXX_CHRONO_INT64_T, micro>;

    /// milliseconds
    using milliseconds	= duration<_GLIBCXX_CHRONO_INT64_T, milli>;

    /// seconds
    using seconds	= duration<_GLIBCXX_CHRONO_INT64_T>;

    /// minutes
    using minutes	= duration<_GLIBCXX_CHRONO_INT64_T, ratio< 60>>;

    /// hours
    using hours		= duration<_GLIBCXX_CHRONO_INT64_T, ratio<3600>>;

#if __cplusplus > 201703L
    /// days
    using days		= duration<_GLIBCXX_CHRONO_INT64_T, ratio<86400>>;

    /// weeks
    using weeks		= duration<_GLIBCXX_CHRONO_INT64_T, ratio<604800>>;

    /// years
    using years		= duration<_GLIBCXX_CHRONO_INT64_T, ratio<31556952>>;

    /// months
    using months	= duration<_GLIBCXX_CHRONO_INT64_T, ratio<2629746>>;
#endif // C++20

#undef _GLIBCXX_CHRONO_INT64_T

    template<typename _Clock, typename _Dur>
      struct time_point
      {
	static_assert(__is_duration<_Dur>::value,
	    "duration must be a specialization of std::chrono::duration");

	typedef _Clock						clock;
	typedef _Dur						duration;
	typedef typename duration::rep				rep;
	typedef typename duration::period			period;

	constexpr time_point() : __d(duration::zero())
	{ }

	constexpr explicit time_point(const duration& __dur)
	: __d(__dur)
	{ }

	// conversions
	template<typename _Dur2,
		 typename = _Require<is_convertible<_Dur2, _Dur>>>
	  constexpr time_point(const time_point<clock, _Dur2>& __t)
	  : __d(__t.time_since_epoch())
	  { }

	// observer
	constexpr duration
	time_since_epoch() const
	{ return __d; }

#if __cplusplus > 201703L
	constexpr time_point&
	operator++()
	{
	  ++__d;
	  return *this;
	}

	constexpr time_point
	operator++(int)
	{ return time_point{__d++}; }

	constexpr time_point&
	operator--()
	{
	  --__d;
	  return *this;
	}

	constexpr time_point
	operator--(int)
	{ return time_point{__d--}; }
#endif

	// arithmetic
	_GLIBCXX17_CONSTEXPR time_point&
	operator+=(const duration& __dur)
	{
	  __d += __dur;
	  return *this;
	}

	_GLIBCXX17_CONSTEXPR time_point&
	operator-=(const duration& __dur)
	{
	  __d -= __dur;
	  return *this;
	}

	// special values
	static constexpr time_point
	min() noexcept
	{ return time_point(duration::min()); }

	static constexpr time_point
	max() noexcept
	{ return time_point(duration::max()); }

      private:
	duration __d;
      };

    /// time_point_cast
    template<typename _ToDur, typename _Clock, typename _Dur>
      constexpr typename enable_if<__is_duration<_ToDur>::value,
				   time_point<_Clock, _ToDur>>::type
      time_point_cast(const time_point<_Clock, _Dur>& __t)
      {
	typedef time_point<_Clock, _ToDur>			__time_point;
	return __time_point(duration_cast<_ToDur>(__t.time_since_epoch()));
      }

#if __cplusplus > 201402L
    template<typename _ToDur, typename _Clock, typename _Dur>
      constexpr
      enable_if_t<__is_duration<_ToDur>::value, time_point<_Clock, _ToDur>>
      floor(const time_point<_Clock, _Dur>& __tp)
      {
	return time_point<_Clock, _ToDur>{
	    chrono::floor<_ToDur>(__tp.time_since_epoch())};
      }

    template<typename _ToDur, typename _Clock, typename _Dur>
      constexpr
      enable_if_t<__is_duration<_ToDur>::value, time_point<_Clock, _ToDur>>
      ceil(const time_point<_Clock, _Dur>& __tp)
      {
	return time_point<_Clock, _ToDur>{
	    chrono::ceil<_ToDur>(__tp.time_since_epoch())};
      }

    template<typename _ToDur, typename _Clock, typename _Dur>
      constexpr enable_if_t<
	__and_<__is_duration<_ToDur>,
	       __not_<treat_as_floating_point<typename _ToDur::rep>>>::value,
	time_point<_Clock, _ToDur>>
      round(const time_point<_Clock, _Dur>& __tp)
      {
	return time_point<_Clock, _ToDur>{
	    chrono::round<_ToDur>(__tp.time_since_epoch())};
      }
#endif // C++17

    /// @{
    /// @relates time_point

    /// Adjust a time point forwards by the given duration.
    template<typename _Clock, typename _Dur1,
	     typename _Rep2, typename _Period2>
      constexpr time_point<_Clock,
	typename common_type<_Dur1, duration<_Rep2, _Period2>>::type>
      operator+(const time_point<_Clock, _Dur1>& __lhs,
		const duration<_Rep2, _Period2>& __rhs)
      {
	typedef duration<_Rep2, _Period2>			__dur2;
	typedef typename common_type<_Dur1,__dur2>::type	__ct;
	typedef time_point<_Clock, __ct>			__time_point;
	return __time_point(__lhs.time_since_epoch() + __rhs);
      }

    /// Adjust a time point forwards by the given duration.
    template<typename _Rep1, typename _Period1,
	     typename _Clock, typename _Dur2>
      constexpr time_point<_Clock,
	typename common_type<duration<_Rep1, _Period1>, _Dur2>::type>
      operator+(const duration<_Rep1, _Period1>& __lhs,
		const time_point<_Clock, _Dur2>& __rhs)
      {
	typedef duration<_Rep1, _Period1>			__dur1;
	typedef typename common_type<__dur1,_Dur2>::type	__ct;
	typedef time_point<_Clock, __ct>			__time_point;
	return __time_point(__rhs.time_since_epoch() + __lhs);
      }

    /// Adjust a time point backwards by the given duration.
    template<typename _Clock, typename _Dur1,
	     typename _Rep2, typename _Period2>
      constexpr time_point<_Clock,
	typename common_type<_Dur1, duration<_Rep2, _Period2>>::type>
      operator-(const time_point<_Clock, _Dur1>& __lhs,
		const duration<_Rep2, _Period2>& __rhs)
      {
	typedef duration<_Rep2, _Period2>			__dur2;
	typedef typename common_type<_Dur1,__dur2>::type	__ct;
	typedef time_point<_Clock, __ct>			__time_point;
	return __time_point(__lhs.time_since_epoch() -__rhs);
      }

    /// The difference between two time points (as a duration)
    template<typename _Clock, typename _Dur1, typename _Dur2>
      constexpr typename common_type<_Dur1, _Dur2>::type
      operator-(const time_point<_Clock, _Dur1>& __lhs,
		const time_point<_Clock, _Dur2>& __rhs)
      { return __lhs.time_since_epoch() - __rhs.time_since_epoch(); }
    /// @}

    /** @{
     * Comparisons for time_point
     * @relates chrono::time_point
     */

    template<typename _Clock, typename _Dur1, typename _Dur2>
      constexpr bool
      operator==(const time_point<_Clock, _Dur1>& __lhs,
		 const time_point<_Clock, _Dur2>& __rhs)
      { return __lhs.time_since_epoch() == __rhs.time_since_epoch(); }

#if __cpp_lib_three_way_comparison
    template<typename _Clock, typename _Dur1,
	     three_way_comparable_with<_Dur1> _Dur2>
      constexpr auto
      operator<=>(const time_point<_Clock, _Dur1>& __lhs,
		  const time_point<_Clock, _Dur2>& __rhs)
      { return __lhs.time_since_epoch() <=> __rhs.time_since_epoch(); }
#else
    template<typename _Clock, typename _Dur1, typename _Dur2>
      constexpr bool
      operator!=(const time_point<_Clock, _Dur1>& __lhs,
		 const time_point<_Clock, _Dur2>& __rhs)
      { return !(__lhs == __rhs); }
#endif

    template<typename _Clock, typename _Dur1, typename _Dur2>
      constexpr bool
      operator<(const time_point<_Clock, _Dur1>& __lhs,
		const time_point<_Clock, _Dur2>& __rhs)
      { return  __lhs.time_since_epoch() < __rhs.time_since_epoch(); }

    template<typename _Clock, typename _Dur1, typename _Dur2>
      constexpr bool
      operator<=(const time_point<_Clock, _Dur1>& __lhs,
		 const time_point<_Clock, _Dur2>& __rhs)
      { return !(__rhs < __lhs); }

    template<typename _Clock, typename _Dur1, typename _Dur2>
      constexpr bool
      operator>(const time_point<_Clock, _Dur1>& __lhs,
		const time_point<_Clock, _Dur2>& __rhs)
      { return __rhs < __lhs; }

    template<typename _Clock, typename _Dur1, typename _Dur2>
      constexpr bool
      operator>=(const time_point<_Clock, _Dur1>& __lhs,
		 const time_point<_Clock, _Dur2>& __rhs)
      { return !(__lhs < __rhs); }

    /// @}

    // Clocks.

    // Why nanosecond resolution as the default?
    // Why have std::system_clock always count in the highest
    // resolution (ie nanoseconds), even if on some OSes the low 3
    // or 9 decimal digits will be always zero? This allows later
    // implementations to change the system_clock::now()
    // implementation any time to provide better resolution without
    // changing function signature or units.

    // To support the (forward) evolution of the library's defined
    // clocks, wrap inside inline namespace so that the current
    // defintions of system_clock, steady_clock, and
    // high_resolution_clock types are uniquely mangled. This way, new
    // code can use the latests clocks, while the library can contain
    // compatibility definitions for previous versions.  At some
    // point, when these clocks settle down, the inlined namespaces
    // can be removed.  XXX GLIBCXX_ABI Deprecated
    inline namespace _V2 {

    /**
     *  @brief System clock.
     *
     *  Time returned represents wall time from the system-wide clock.
     *  @ingroup chrono
    */
    struct system_clock
    {
      typedef chrono::nanoseconds				duration;
      typedef duration::rep					rep;
      typedef duration::period					period;
      typedef chrono::time_point<system_clock, duration> 	time_point;

      static_assert(system_clock::duration::min()
		    < system_clock::duration::zero(),
		    "a clock's minimum duration cannot be less than its epoch");

      static constexpr bool is_steady = false;

      static time_point
      now() noexcept;

      // Map to C API
      static std::time_t
      to_time_t(const time_point& __t) noexcept
      {
	return std::time_t(duration_cast<chrono::seconds>
			   (__t.time_since_epoch()).count());
      }

      static time_point
      from_time_t(std::time_t __t) noexcept
      {
	typedef chrono::time_point<system_clock, seconds>	__from;
	return time_point_cast<system_clock::duration>
	       (__from(chrono::seconds(__t)));
      }
    };


    /**
     *  @brief Monotonic clock
     *
     *  Time returned has the property of only increasing at a uniform rate.
     *  @ingroup chrono
    */
    struct steady_clock
    {
      typedef chrono::nanoseconds				duration;
      typedef duration::rep					rep;
      typedef duration::period					period;
      typedef chrono::time_point<steady_clock, duration>	time_point;

      static constexpr bool is_steady = true;

      static time_point
      now() noexcept;
    };


    /**
     *  @brief Highest-resolution clock
     *
     *  This is the clock "with the shortest tick period." Alias to
     *  std::system_clock until higher-than-nanosecond definitions
     *  become feasible.
     *  @ingroup chrono
    */
    using high_resolution_clock = system_clock;

    } // end inline namespace _V2

#if __cplusplus > 201703L
    template<typename _Duration>
      using sys_time = time_point<system_clock, _Duration>;
    using sys_seconds = sys_time<seconds>;
    using sys_days = sys_time<days>;

    using file_clock = ::std::filesystem::__file_clock;

    template<typename _Duration>
      using file_time = time_point<file_clock, _Duration>;

    template<> struct is_clock<system_clock> : true_type { };
    template<> struct is_clock<steady_clock> : true_type { };
    template<> struct is_clock<file_clock> : true_type { };

    template<> inline constexpr bool is_clock_v<system_clock> = true;
    template<> inline constexpr bool is_clock_v<steady_clock> = true;
    template<> inline constexpr bool is_clock_v<file_clock> = true;

    struct local_t { };
    template<typename _Duration>
      using local_time = time_point<local_t, _Duration>;
    using local_seconds = local_time<seconds>;
    using local_days = local_time<days>;

    class utc_clock;
    class tai_clock;
    class gps_clock;

    template<typename _Duration>
      using utc_time = time_point<utc_clock, _Duration>;
    using utc_seconds = utc_time<seconds>;

    template<typename _Duration>
      using tai_time = time_point<tai_clock, _Duration>;
    using tai_seconds = tai_time<seconds>;

    template<typename _Duration>
      using gps_time = time_point<gps_clock, _Duration>;
    using gps_seconds = gps_time<seconds>;

    template<> struct is_clock<utc_clock> : true_type { };
    template<> struct is_clock<tai_clock> : true_type { };
    template<> struct is_clock<gps_clock> : true_type { };

    template<> inline constexpr bool is_clock_v<utc_clock> = true;
    template<> inline constexpr bool is_clock_v<tai_clock> = true;
    template<> inline constexpr bool is_clock_v<gps_clock> = true;

    struct leap_second_info
    {
      bool is_leap_second;
      seconds elapsed;
    };

    // CALENDRICAL TYPES

    // CLASS DECLARATIONS
    class day;
    class month;
    class year;
    class weekday;
    class weekday_indexed;
    class weekday_last;
    class month_day;
    class month_day_last;
    class month_weekday;
    class month_weekday_last;
    class year_month;
    class year_month_day;
    class year_month_day_last;
    class year_month_weekday;
    class year_month_weekday_last;

    struct last_spec
    {
      explicit last_spec() = default;

      friend constexpr month_day_last
      operator/(int __m, last_spec) noexcept;

      friend constexpr month_day_last
      operator/(last_spec, int __m) noexcept;
    };

    inline constexpr last_spec last{};

    namespace __detail
    {
      // Compute the remainder of the Euclidean division of __n divided by __d.
      // Euclidean division truncates toward negative infinity and always
      // produces a remainder in the range of [0,__d-1] (whereas standard
      // division truncates toward zero and yields a nonpositive remainder
      // for negative __n).
      constexpr unsigned
      __modulo(long long __n, unsigned __d)
      {
	if (__n >= 0)
	  return __n % __d;
	else
	  return (__d + (__n % __d)) % __d;
      }

      inline constexpr unsigned __days_per_month[12]
	= { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    }

    // DAY

    class day
    {
    private:
      unsigned char _M_d;

    public:
      day() = default;

      explicit constexpr
      day(unsigned __d) noexcept
      : _M_d(__d)
      { }

      constexpr day&
      operator++() noexcept
      {
	++_M_d;
	return *this;
      }

      constexpr day
      operator++(int) noexcept
      {
	auto __ret = *this;
	++(*this);
	return __ret;
      }

      constexpr day&
      operator--() noexcept
      {
	--_M_d;
	return *this;
      }

      constexpr day
      operator--(int) noexcept
      {
	auto __ret = *this;
	--(*this);
	return __ret;
      }

      constexpr day&
      operator+=(const days& __d) noexcept
      {
	*this = *this + __d;
	return *this;
      }

      constexpr day&
      operator-=(const days& __d) noexcept
      {
	*this = *this - __d;
	return *this;
      }

      constexpr explicit
      operator unsigned() const noexcept
      { return _M_d; }

      constexpr bool
      ok() const noexcept
      { return 1 <= _M_d && _M_d <= 31; }

      friend constexpr bool
      operator==(const day& __x, const day& __y) noexcept
      { return unsigned{__x} == unsigned{__y}; }

      friend constexpr strong_ordering
      operator<=>(const day& __x, const day& __y) noexcept
      { return unsigned{__x} <=> unsigned{__y}; }

      friend constexpr day
      operator+(const day& __x, const days& __y) noexcept
      { return day(unsigned{__x} + __y.count()); }

      friend constexpr day
      operator+(const days& __x, const day& __y) noexcept
      { return __y + __x; }

      friend constexpr day
      operator-(const day& __x, const days& __y) noexcept
      { return __x + -__y; }

      friend constexpr days
      operator-(const day& __x, const day& __y) noexcept
      { return days{int(unsigned{__x}) - int(unsigned{__y})}; }

      friend constexpr month_day
      operator/(const month& __m, const day& __d) noexcept;

      friend constexpr month_day
      operator/(int __m, const day& __d) noexcept;

      friend constexpr month_day
      operator/(const day& __d, const month& __m) noexcept;

      friend constexpr month_day
      operator/(const day& __d, int __m) noexcept;

      friend constexpr year_month_day
      operator/(const year_month& __ym, const day& __d) noexcept;

      // TODO: Implement operator<<, to_stream, from_stream.
    };

    // MONTH

    class month
    {
    private:
      unsigned char _M_m;

    public:
      month() = default;

      explicit constexpr
      month(unsigned __m) noexcept
      : _M_m(__m)
      { }

      constexpr month&
      operator++() noexcept
      {
	*this += months{1};
	return *this;
      }

      constexpr month
      operator++(int) noexcept
      {
	auto __ret = *this;
	++(*this);
	return __ret;
      }

      constexpr month&
      operator--() noexcept
      {
	*this -= months{1};
	return *this;
      }

      constexpr month
      operator--(int) noexcept
      {
	auto __ret = *this;
	--(*this);
	return __ret;
      }

      constexpr month&
      operator+=(const months& __m) noexcept
      {
	*this = *this + __m;
	return *this;
      }

      constexpr month&
      operator-=(const months& __m) noexcept
      {
	*this = *this - __m;
	return *this;
      }

      explicit constexpr
      operator unsigned() const noexcept
      { return _M_m; }

      constexpr bool
      ok() const noexcept
      { return 1 <= _M_m && _M_m <= 12; }

      friend constexpr bool
      operator==(const month& __x, const month& __y) noexcept
      { return unsigned{__x} == unsigned{__y}; }

      friend constexpr strong_ordering
      operator<=>(const month& __x, const month& __y) noexcept
      { return unsigned{__x} <=> unsigned{__y}; }

      friend constexpr month
      operator+(const month& __x, const months& __y) noexcept
      {
	auto __n = static_cast<long long>(unsigned{__x}) + (__y.count() - 1);
	return month{__detail::__modulo(__n, 12) + 1};
      }

      friend constexpr month
      operator+(const months& __x,  const month& __y) noexcept
      { return __y + __x; }

      friend constexpr month
      operator-(const month& __x, const months& __y) noexcept
      { return __x + -__y; }

      friend constexpr months
      operator-(const month& __x,  const month& __y) noexcept
      {
	const auto __dm = int(unsigned(__x)) - int(unsigned(__y));
	return months{__dm < 0 ? 12 + __dm : __dm};
      }

      friend constexpr year_month
      operator/(const year& __y, const month& __m) noexcept;

      friend constexpr month_day
      operator/(const month& __m, int __d) noexcept;

      friend constexpr month_day_last
      operator/(const month& __m, last_spec) noexcept;

      friend constexpr month_day_last
      operator/(last_spec, const month& __m) noexcept;

      friend constexpr month_weekday
      operator/(const month& __m, const weekday_indexed& __wdi) noexcept;

      friend constexpr month_weekday
      operator/(const weekday_indexed& __wdi, const month& __m) noexcept;

      friend constexpr month_weekday_last
      operator/(const month& __m, const weekday_last& __wdl) noexcept;

      friend constexpr month_weekday_last
      operator/(const weekday_last& __wdl, const month& __m) noexcept;

      // TODO: Implement operator<<, to_stream, from_stream.
    };

    inline constexpr month January{1};
    inline constexpr month February{2};
    inline constexpr month March{3};
    inline constexpr month April{4};
    inline constexpr month May{5};
    inline constexpr month June{6};
    inline constexpr month July{7};
    inline constexpr month August{8};
    inline constexpr month September{9};
    inline constexpr month October{10};
    inline constexpr month November{11};
    inline constexpr month December{12};

    // YEAR

    class year
    {
    private:
      short _M_y;

    public:
      year() = default;

      explicit constexpr
      year(int __y) noexcept
      : _M_y{static_cast<short>(__y)}
      { }

      static constexpr year
      min() noexcept
      { return year{-32767}; }

      static constexpr year
      max() noexcept
      { return year{32767}; }

      constexpr year&
      operator++() noexcept
      {
	++_M_y;
	return *this;
      }

      constexpr year
      operator++(int) noexcept
      {
	auto __ret = *this;
	++(*this);
	return __ret;
      }

      constexpr year&
      operator--() noexcept
      {
	--_M_y;
	return *this;
      }

      constexpr year
      operator--(int) noexcept
      {
	auto __ret = *this;
	--(*this);
	return __ret;
      }

      constexpr year&
      operator+=(const years& __y) noexcept
      {
	*this = *this + __y;
	return *this;
      }

      constexpr year&
      operator-=(const years& __y) noexcept
      {
	*this = *this - __y;
	return *this;
      }

      constexpr year
      operator+() const noexcept
      { return *this; }

      constexpr year
      operator-() const noexcept
      { return year{-_M_y}; }

      constexpr bool
      is_leap() const noexcept
      {
	// Testing divisibility by 100 first gives better performance, that is,
	// return (_M_y % 100 != 0 || _M_y % 400 == 0) && _M_y % 4 == 0;

	// It gets even faster if _M_y is in [-536870800, 536870999]
	// (which is the case here) and _M_y % 100 is replaced by
	// __is_multiple_of_100 below.

	// References:
	// [1] https://github.com/cassioneri/calendar
	// [2] https://accu.org/journals/overload/28/155/overload155.pdf#page=16

	constexpr uint32_t __multiplier   = 42949673;
	constexpr uint32_t __bound        = 42949669;
	constexpr uint32_t __max_dividend = 1073741799;
	constexpr uint32_t __offset       = __max_dividend / 2 / 100 * 100;
	const bool __is_multiple_of_100
	  = __multiplier * (_M_y + __offset) < __bound;
	return (!__is_multiple_of_100 || _M_y % 400 == 0) && _M_y % 4 == 0;
      }

      explicit constexpr
      operator int() const noexcept
      { return _M_y; }

      constexpr bool
      ok() const noexcept
      { return min()._M_y <= _M_y && _M_y <= max()._M_y; }

      friend constexpr bool
      operator==(const year& __x, const year& __y) noexcept
      { return int{__x} == int{__y}; }

      friend constexpr strong_ordering
      operator<=>(const year& __x, const year& __y) noexcept
      { return int{__x} <=> int{__y}; }

      friend constexpr year
      operator+(const year& __x, const years& __y) noexcept
      { return year{int{__x} + static_cast<int>(__y.count())}; }

      friend constexpr year
      operator+(const years& __x, const year& __y) noexcept
      { return __y + __x; }

      friend constexpr year
      operator-(const year& __x, const years& __y) noexcept
      { return __x + -__y; }

      friend constexpr years
      operator-(const year& __x, const year& __y) noexcept
      { return years{int{__x} - int{__y}}; }

      friend constexpr year_month
      operator/(const year& __y, int __m) noexcept;

      friend constexpr year_month_day
      operator/(const year& __y, const month_day& __md) noexcept;

      friend constexpr year_month_day
      operator/(const month_day& __md, const year& __y) noexcept;

      friend constexpr year_month_day_last
      operator/(const year& __y, const month_day_last& __mdl) noexcept;

      friend constexpr year_month_day_last
      operator/(const month_day_last& __mdl, const year& __y) noexcept;

      friend constexpr year_month_weekday
      operator/(const year& __y, const month_weekday& __mwd) noexcept;

      friend constexpr year_month_weekday
      operator/(const month_weekday& __mwd, const year& __y) noexcept;

      friend constexpr year_month_weekday_last
      operator/(const year& __y, const month_weekday_last& __mwdl) noexcept;

      friend constexpr year_month_weekday_last
      operator/(const month_weekday_last& __mwdl, const year& __y) noexcept;

      // TODO: Implement operator<<, to_stream, from_stream.
    };

    // WEEKDAY

    class weekday
    {
    private:
      unsigned char _M_wd;

      static constexpr weekday
      _S_from_days(const days& __d)
      {
	auto __n = __d.count();
	return weekday(__n >= -4 ? (__n + 4) % 7 : (__n + 5) % 7 + 6);
      }

    public:
      weekday() = default;

      explicit constexpr
      weekday(unsigned __wd) noexcept
      : _M_wd(__wd == 7 ? 0 : __wd) // __wd % 7 ?
      { }

      constexpr
      weekday(const sys_days& __dp) noexcept
      : weekday{_S_from_days(__dp.time_since_epoch())}
      { }

      explicit constexpr
      weekday(const local_days& __dp) noexcept
      : weekday{sys_days{__dp.time_since_epoch()}}
      { }

      constexpr weekday&
      operator++() noexcept
      {
	*this += days{1};
	return *this;
      }

      constexpr weekday
      operator++(int) noexcept
      {
	auto __ret = *this;
	++(*this);
	return __ret;
      }

      constexpr weekday&
      operator--() noexcept
      {
	*this -= days{1};
	return *this;
      }

      constexpr weekday
      operator--(int) noexcept
      {
	auto __ret = *this;
	--(*this);
	return __ret;
      }

      constexpr weekday&
      operator+=(const days& __d) noexcept
      {
	*this = *this + __d;
	return *this;
      }

      constexpr weekday&
      operator-=(const days& __d) noexcept
      {
	*this = *this - __d;
	return *this;
      }

      constexpr unsigned
      c_encoding() const noexcept
      { return _M_wd; }

      constexpr unsigned
      iso_encoding() const noexcept
      { return _M_wd == 0u ? 7u : _M_wd; }

      constexpr bool
      ok() const noexcept
      { return _M_wd <= 6; }

      constexpr weekday_indexed
      operator[](unsigned __index) const noexcept;

      constexpr weekday_last
      operator[](last_spec) const noexcept;

      friend constexpr bool
      operator==(const weekday& __x, const weekday& __y) noexcept
      { return __x._M_wd == __y._M_wd; }

      friend constexpr weekday
      operator+(const weekday& __x, const days& __y) noexcept
      {
	auto __n = static_cast<long long>(__x._M_wd) + __y.count();
	return weekday{__detail::__modulo(__n, 7)};
      }

      friend constexpr weekday
      operator+(const days& __x, const weekday& __y) noexcept
      { return __y + __x; }

      friend constexpr weekday
      operator-(const weekday& __x, const days& __y) noexcept
      { return __x + -__y; }

      friend constexpr days
      operator-(const weekday& __x, const weekday& __y) noexcept
      {
	auto __n = static_cast<long long>(__x._M_wd) - __y._M_wd;
	return days{__detail::__modulo(__n, 7)};
      }

      // TODO: operator<<, from_stream.
    };

    inline constexpr weekday Sunday{0};
    inline constexpr weekday Monday{1};
    inline constexpr weekday Tuesday{2};
    inline constexpr weekday Wednesday{3};
    inline constexpr weekday Thursday{4};
    inline constexpr weekday Friday{5};
    inline constexpr weekday Saturday{6};

    // WEEKDAY_INDEXED

    class weekday_indexed
    {
    private:
      chrono::weekday _M_wd;
      unsigned char _M_index;

    public:
      weekday_indexed() = default;

      constexpr
      weekday_indexed(const chrono::weekday& __wd, unsigned __index) noexcept
      : _M_wd(__wd), _M_index(__index)
      { }

      constexpr chrono::weekday
      weekday() const noexcept
      { return _M_wd; }

      constexpr unsigned
      index() const noexcept
      { return _M_index; };

      constexpr bool
      ok() const noexcept
      { return _M_wd.ok() && 1 <= _M_index && _M_index <= 5; }

      friend constexpr bool
      operator==(const weekday_indexed& __x, const weekday_indexed& __y) noexcept
      { return __x.weekday() == __y.weekday() && __x.index() == __y.index(); }

      friend constexpr month_weekday
      operator/(const month& __m, const weekday_indexed& __wdi) noexcept;

      friend constexpr month_weekday
      operator/(int __m, const weekday_indexed& __wdi) noexcept;

      friend constexpr month_weekday
      operator/(const weekday_indexed& __wdi, const month& __m) noexcept;

      friend constexpr month_weekday
      operator/(const weekday_indexed& __wdi, int __m) noexcept;

      friend constexpr year_month_weekday
      operator/(const year_month& __ym, const weekday_indexed& __wdi) noexcept;

      // TODO: Implement operator<<.
    };

    constexpr weekday_indexed
    weekday::operator[](unsigned __index) const noexcept
    { return {*this, __index}; }

    // WEEKDAY_LAST

    class weekday_last
    {
    private:
      chrono::weekday _M_wd;

    public:
      explicit constexpr
      weekday_last(const chrono::weekday& __wd) noexcept
      : _M_wd{__wd}
      { }

      constexpr chrono::weekday
      weekday() const noexcept
      { return _M_wd; }

      constexpr bool
      ok() const noexcept
      { return _M_wd.ok(); }

      friend constexpr bool
      operator==(const weekday_last& __x, const weekday_last& __y) noexcept
      { return __x.weekday() == __y.weekday(); }

      friend constexpr month_weekday_last
      operator/(int __m, const weekday_last& __wdl) noexcept;

      friend constexpr month_weekday_last
      operator/(const weekday_last& __wdl, int __m) noexcept;

      friend constexpr year_month_weekday_last
      operator/(const year_month& __ym, const weekday_last& __wdl) noexcept;

      // TODO: Implement operator<<.
    };

    constexpr weekday_last
    weekday::operator[](last_spec) const noexcept
    { return weekday_last{*this}; }

    // MONTH_DAY

    class month_day
    {
    private:
      chrono::month _M_m;
      chrono::day _M_d;

    public:
      month_day() = default;

      constexpr
      month_day(const chrono::month& __m, const chrono::day& __d) noexcept
      : _M_m{__m}, _M_d{__d}
      { }

      constexpr chrono::month
      month() const noexcept
      { return _M_m; }

      constexpr chrono::day
      day() const noexcept
      { return _M_d; }

      constexpr bool
      ok() const noexcept
      {
	return _M_m.ok()
	  && 1u <= unsigned(_M_d)
	  && unsigned(_M_d) <= __detail::__days_per_month[unsigned(_M_m) - 1];
      }

      friend constexpr bool
      operator==(const month_day& __x, const month_day& __y) noexcept
      { return __x.month() == __y.month() && __x.day() == __y.day(); }

      friend constexpr strong_ordering
      operator<=>(const month_day& __x, const month_day& __y) noexcept
	= default;

      friend constexpr month_day
      operator/(const chrono::month& __m, const chrono::day& __d) noexcept
      { return {__m, __d}; }

      friend constexpr month_day
      operator/(const chrono::month& __m, int __d) noexcept
      { return {__m, chrono::day(unsigned(__d))}; }

      friend constexpr month_day
      operator/(int __m, const chrono::day& __d) noexcept
      { return {chrono::month(unsigned(__m)), __d}; }

      friend constexpr month_day
      operator/(const chrono::day& __d, const chrono::month& __m) noexcept
      { return {__m, __d}; }

      friend constexpr month_day
      operator/(const chrono::day& __d, int __m) noexcept
      { return {chrono::month(unsigned(__m)), __d}; }

      friend constexpr year_month_day
      operator/(int __y, const month_day& __md) noexcept;

      friend constexpr year_month_day
      operator/(const month_day& __md, int __y) noexcept;

      // TODO: Implement operator<<, from_stream.
    };

    // MONTH_DAY_LAST

    class month_day_last
    {
    private:
      chrono::month _M_m;

    public:
      explicit constexpr
      month_day_last(const chrono::month& __m) noexcept
      : _M_m{__m}
      { }

      constexpr chrono::month
      month() const noexcept
      { return _M_m; }

      constexpr bool
      ok() const noexcept
      { return _M_m.ok(); }

      friend constexpr bool
      operator==(const month_day_last& __x, const month_day_last& __y) noexcept
      { return __x.month() == __y.month(); }

      friend constexpr strong_ordering
      operator<=>(const month_day_last& __x, const month_day_last& __y) noexcept
	= default;

      friend constexpr month_day_last
      operator/(const chrono::month& __m, last_spec) noexcept
      { return month_day_last{__m}; }

      friend constexpr month_day_last
      operator/(int __m, last_spec) noexcept
      { return chrono::month(unsigned(__m)) / last; }

      friend constexpr month_day_last
      operator/(last_spec, const chrono::month& __m) noexcept
      { return __m / last; }

      friend constexpr month_day_last
      operator/(last_spec, int __m) noexcept
      { return __m / last; }

      friend constexpr year_month_day_last
      operator/(int __y, const month_day_last& __mdl) noexcept;

      friend constexpr year_month_day_last
      operator/(const month_day_last& __mdl, int __y) noexcept;

      // TODO: Implement operator<<.
    };

    // MONTH_WEEKDAY

    class month_weekday
    {
    private:
      chrono::month _M_m;
      chrono::weekday_indexed _M_wdi;

    public:
      constexpr
      month_weekday(const chrono::month& __m,
		    const chrono::weekday_indexed& __wdi) noexcept
      : _M_m{__m}, _M_wdi{__wdi}
      { }

      constexpr chrono::month
      month() const noexcept
      { return _M_m; }

      constexpr chrono::weekday_indexed
      weekday_indexed() const noexcept
      { return _M_wdi; }

      constexpr bool
      ok() const noexcept
      { return _M_m.ok() && _M_wdi.ok(); }

      friend constexpr bool
      operator==(const month_weekday& __x, const month_weekday& __y) noexcept
      {
	return __x.month() == __y.month()
	  && __x.weekday_indexed() == __y.weekday_indexed();
      }

      friend constexpr month_weekday
      operator/(const chrono::month& __m,
		const chrono::weekday_indexed& __wdi) noexcept
      { return {__m, __wdi}; }

      friend constexpr month_weekday
      operator/(int __m, const chrono::weekday_indexed& __wdi) noexcept
      { return chrono::month(unsigned(__m)) / __wdi; }

      friend constexpr month_weekday
      operator/(const chrono::weekday_indexed& __wdi,
		const chrono::month& __m) noexcept
      { return __m / __wdi; }

      friend constexpr month_weekday
      operator/(const chrono::weekday_indexed& __wdi, int __m) noexcept
      { return __m / __wdi; }

      friend constexpr year_month_weekday
      operator/(int __y, const month_weekday& __mwd) noexcept;

      friend constexpr year_month_weekday
      operator/(const month_weekday& __mwd, int __y) noexcept;

      // TODO: Implement operator<<.
    };

    // MONTH_WEEKDAY_LAST

    class month_weekday_last
    {
    private:
      chrono::month _M_m;
      chrono::weekday_last _M_wdl;

    public:
      constexpr
      month_weekday_last(const chrono::month& __m,
			 const chrono::weekday_last& __wdl) noexcept
      :_M_m{__m}, _M_wdl{__wdl}
      { }

      constexpr chrono::month
      month() const noexcept
      { return _M_m; }

      constexpr chrono::weekday_last
      weekday_last() const noexcept
      { return _M_wdl; }

      constexpr bool
      ok() const noexcept
      { return _M_m.ok() && _M_wdl.ok(); }

      friend constexpr bool
      operator==(const month_weekday_last& __x,
		 const month_weekday_last& __y) noexcept
      {
	return __x.month() == __y.month()
	  && __x.weekday_last() == __y.weekday_last();
      }

      friend constexpr month_weekday_last
      operator/(const chrono::month& __m,
		const chrono::weekday_last& __wdl) noexcept
      { return {__m, __wdl}; }

      friend constexpr month_weekday_last
      operator/(int __m, const chrono::weekday_last& __wdl) noexcept
      { return chrono::month(unsigned(__m)) / __wdl; }

      friend constexpr month_weekday_last
      operator/(const chrono::weekday_last& __wdl,
		const chrono::month& __m) noexcept
      { return __m / __wdl; }

      friend constexpr month_weekday_last
      operator/(const chrono::weekday_last& __wdl, int __m) noexcept
      { return chrono::month(unsigned(__m)) / __wdl; }

      friend constexpr year_month_weekday_last
      operator/(int __y, const month_weekday_last& __mwdl) noexcept;

      friend constexpr year_month_weekday_last
      operator/(const month_weekday_last& __mwdl, int __y) noexcept;

      // TODO: Implement operator<<.
    };

    // YEAR_MONTH

    namespace __detail
    {
      // [time.cal.ym], [time.cal.ymd], etc constrain the 'months'-based
      // addition/subtraction operator overloads like so:
      //
      //   Constraints: if the argument supplied by the caller for the months
      //   parameter is convertible to years, its implicit conversion sequence
      //   to years is worse than its implicit conversion sequence to months.
      //
      // We realize this constraint by templatizing the 'months'-based
      // overloads (using a dummy defaulted template parameter), so that
      // overload resolution doesn't select the 'months'-based overload unless
      // the implicit conversion sequence to 'months' is better than that to
      // 'years'.
      using __months_years_conversion_disambiguator = void;
    }

    class year_month
    {
    private:
      chrono::year _M_y;
      chrono::month _M_m;

    public:
      year_month() = default;

      constexpr
      year_month(const chrono::year& __y, const chrono::month& __m) noexcept
      : _M_y{__y}, _M_m{__m}
      { }

      constexpr chrono::year
      year() const noexcept
      { return _M_y; }

      constexpr chrono::month
      month() const noexcept
      { return _M_m; }

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month&
	operator+=(const months& __dm) noexcept
	{
	  *this = *this + __dm;
	  return *this;
	}

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month&
	operator-=(const months& __dm) noexcept
	{
	  *this = *this - __dm;
	  return *this;
	}

      constexpr year_month&
      operator+=(const years& __dy)  noexcept
      {
	*this = *this + __dy;
	return *this;
      }

      constexpr year_month&
      operator-=(const years& __dy)  noexcept
      {
	*this = *this - __dy;
	return *this;
      }

      constexpr bool
      ok() const noexcept
      { return _M_y.ok() && _M_m.ok(); }

      friend constexpr bool
      operator==(const year_month& __x, const year_month& __y) noexcept
      { return __x.year() == __y.year() && __x.month() == __y.month(); }

      friend constexpr strong_ordering
      operator<=>(const year_month& __x, const year_month& __y) noexcept
	= default;

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month
	operator+(const year_month& __ym, const months& __dm) noexcept
	{
	  // TODO: Optimize?
	  auto __m = __ym.month() + __dm;
	  auto __i = int(unsigned(__ym.month())) - 1 + __dm.count();
	  auto __y = (__i < 0
		      ? __ym.year() + years{(__i - 11) / 12}
		      : __ym.year() + years{__i / 12});
	  return __y / __m;
	}

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month
	operator+(const months& __dm, const year_month& __ym) noexcept
	{ return __ym + __dm; }

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month
	operator-(const year_month& __ym, const months& __dm) noexcept
	{ return __ym + -__dm; }

      friend constexpr months
      operator-(const year_month& __x, const year_month& __y) noexcept
      {
	return (__x.year() - __y.year()
		+ months{static_cast<int>(unsigned{__x.month()})
			 - static_cast<int>(unsigned{__y.month()})});
      }

      friend constexpr year_month
      operator+(const year_month& __ym, const years& __dy) noexcept
      { return (__ym.year() + __dy) / __ym.month(); }

      friend constexpr year_month
      operator+(const years& __dy, const year_month& __ym) noexcept
      { return __ym + __dy; }

      friend constexpr year_month
      operator-(const year_month& __ym, const years& __dy) noexcept
      { return __ym + -__dy; }

      friend constexpr year_month
      operator/(const chrono::year& __y, const chrono::month& __m) noexcept
      { return {__y, __m}; }

      friend constexpr year_month
      operator/(const chrono::year& __y, int __m) noexcept
      { return {__y, chrono::month(unsigned(__m))}; }

      friend constexpr year_month_day
      operator/(const year_month& __ym, int __d) noexcept;

      friend constexpr year_month_day_last
      operator/(const year_month& __ym, last_spec) noexcept;

      // TODO: Implement operator<<, from_stream.
    };

    // YEAR_MONTH_DAY

    class year_month_day
    {
    private:
      chrono::year _M_y;
      chrono::month _M_m;
      chrono::day _M_d;

      static constexpr year_month_day _S_from_days(const days& __dp) noexcept;

      constexpr days _M_days_since_epoch() const noexcept;

    public:
      year_month_day() = default;

      constexpr
      year_month_day(const chrono::year& __y, const chrono::month& __m,
		     const chrono::day& __d) noexcept
      : _M_y{__y}, _M_m{__m}, _M_d{__d}
      { }

      constexpr
      year_month_day(const year_month_day_last& __ymdl) noexcept;

      constexpr
      year_month_day(const sys_days& __dp) noexcept
      : year_month_day(_S_from_days(__dp.time_since_epoch()))
      { }

      explicit constexpr
      year_month_day(const local_days& __dp) noexcept
      : year_month_day(sys_days{__dp.time_since_epoch()})
      { }

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month_day&
	operator+=(const months& __m) noexcept
	{
	  *this = *this + __m;
	  return *this;
	}

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month_day&
	operator-=(const months& __m) noexcept
	{
	  *this = *this - __m;
	  return *this;
	}

      constexpr year_month_day&
      operator+=(const years& __y) noexcept
      {
	*this = *this + __y;
	return *this;
      }

      constexpr year_month_day&
      operator-=(const years& __y) noexcept
      {
	*this = *this - __y;
	return *this;
      }

      constexpr chrono::year
      year() const noexcept
      { return _M_y; }

      constexpr chrono::month
      month() const noexcept
      { return _M_m; }

      constexpr chrono::day
      day() const noexcept
      { return _M_d; }

      constexpr
      operator sys_days() const noexcept
      { return sys_days{_M_days_since_epoch()}; }

      explicit constexpr
      operator local_days() const noexcept
      { return local_days{sys_days{*this}.time_since_epoch()}; }

      constexpr bool ok() const noexcept;

      friend constexpr bool
      operator==(const year_month_day& __x, const year_month_day& __y) noexcept
      {
	return __x.year() == __y.year()
	  && __x.month() == __y.month()
	  && __x.day() == __y.day();
      }

      friend constexpr strong_ordering
      operator<=>(const year_month_day& __x, const year_month_day& __y) noexcept
	= default;

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_day
	operator+(const year_month_day& __ymd, const months& __dm) noexcept
	{ return (__ymd.year() / __ymd.month() + __dm) / __ymd.day(); }

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_day
	operator+(const months& __dm, const year_month_day& __ymd) noexcept
	{ return __ymd + __dm; }

      friend constexpr year_month_day
      operator+(const year_month_day& __ymd, const years& __dy) noexcept
      { return (__ymd.year() + __dy) / __ymd.month() / __ymd.day(); }

      friend constexpr year_month_day
      operator+(const years& __dy, const year_month_day& __ymd) noexcept
      { return __ymd + __dy; }

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_day
	operator-(const year_month_day& __ymd, const months& __dm) noexcept
	{ return __ymd + -__dm; }

      friend constexpr year_month_day
      operator-(const year_month_day& __ymd, const years& __dy) noexcept
      { return __ymd + -__dy; }

      friend constexpr year_month_day
      operator/(const year_month& __ym, const chrono::day& __d) noexcept
      { return {__ym.year(), __ym.month(), __d}; }

      friend constexpr year_month_day
      operator/(const year_month& __ym, int __d) noexcept
      { return __ym / chrono::day{unsigned(__d)}; }

      friend constexpr year_month_day
      operator/(const chrono::year& __y, const month_day& __md) noexcept
      { return __y / __md.month() / __md.day(); }

      friend constexpr year_month_day
      operator/(int __y, const month_day& __md) noexcept
      { return chrono::year{__y} / __md; }

      friend constexpr year_month_day
      operator/(const month_day& __md, const chrono::year& __y) noexcept
      { return __y / __md; }

      friend constexpr year_month_day
      operator/(const month_day& __md, int __y) noexcept
      { return chrono::year(__y) / __md; }

      // TODO: Implement operator<<, from_stream.
    };

    // Construct from days since 1970/01/01.
    // Proposition 6.3 of Neri and Schneider,
    // "Euclidean Affine Functions and Applications to Calendar Algorithms".
    // https://arxiv.org/abs/2102.06959
    constexpr year_month_day
    year_month_day::_S_from_days(const days& __dp) noexcept
    {
      constexpr auto __z2    = static_cast<uint32_t>(-1468000);
      constexpr auto __r2_e3 = static_cast<uint32_t>(536895458);

      const auto __r0 = static_cast<uint32_t>(__dp.count()) + __r2_e3;

      const auto __n1 = 4 * __r0 + 3;
      const auto __q1 = __n1 / 146097;
      const auto __r1 = __n1 % 146097 / 4;

      constexpr auto __p32 = static_cast<uint64_t>(1) << 32;
      const auto __n2 = 4 * __r1 + 3;
      const auto __u2 = static_cast<uint64_t>(2939745) * __n2;
      const auto __q2 = static_cast<uint32_t>(__u2 / __p32);
      const auto __r2 = static_cast<uint32_t>(__u2 % __p32) / 2939745 / 4;

      constexpr auto __p16 = static_cast<uint32_t>(1) << 16;
      const auto __n3 = 2141 * __r2 + 197913;
      const auto __q3 = __n3 / __p16;
      const auto __r3 = __n3 % __p16 / 2141;

      const auto __y0 = 100 * __q1 + __q2;
      const auto __m0 = __q3;
      const auto __d0 = __r3;

      const auto __j  = __r2 >= 306;
      const auto __y1 = __y0 + __j;
      const auto __m1 = __j ? __m0 - 12 : __m0;
      const auto __d1 = __d0 + 1;

      return year_month_day{chrono::year{static_cast<int>(__y1 + __z2)},
			    chrono::month{__m1}, chrono::day{__d1}};
    }

    // Days since 1970/01/01.
    // Proposition 6.2 of Neri and Schneider,
    // "Euclidean Affine Functions and Applications to Calendar Algorithms".
    // https://arxiv.org/abs/2102.06959
    constexpr days
    year_month_day::_M_days_since_epoch() const noexcept
    {
      auto constexpr __z2    = static_cast<uint32_t>(-1468000);
      auto constexpr __r2_e3 = static_cast<uint32_t>(536895458);

      const auto __y1 = static_cast<uint32_t>(static_cast<int>(_M_y)) - __z2;
      const auto __m1 = static_cast<uint32_t>(static_cast<unsigned>(_M_m));
      const auto __d1 = static_cast<uint32_t>(static_cast<unsigned>(_M_d));

      const auto __j  = static_cast<uint32_t>(__m1 < 3);
      const auto __y0 = __y1 - __j;
      const auto __m0 = __j ? __m1 + 12 : __m1;
      const auto __d0 = __d1 - 1;

      const auto __q1 = __y0 / 100;
      const auto __yc = 1461 * __y0 / 4 - __q1 + __q1 / 4;
      const auto __mc = (979 *__m0 - 2919) / 32;
      const auto __dc = __d0;

      return days{static_cast<int32_t>(__yc + __mc + __dc - __r2_e3)};
    }

    // YEAR_MONTH_DAY_LAST

    class year_month_day_last
    {
    private:
      chrono::year _M_y;
      chrono::month_day_last _M_mdl;

    public:
      constexpr
      year_month_day_last(const chrono::year& __y,
			  const chrono::month_day_last& __mdl) noexcept
      : _M_y{__y}, _M_mdl{__mdl}
      { }

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month_day_last&
	operator+=(const months& __m) noexcept
	{
	  *this = *this + __m;
	  return *this;
	}

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month_day_last&
	operator-=(const months& __m) noexcept
	{
	  *this = *this - __m;
	  return *this;
	}

      constexpr year_month_day_last&
      operator+=(const years& __y)  noexcept
      {
	*this = *this + __y;
	return *this;
      }

      constexpr year_month_day_last&
      operator-=(const years& __y)  noexcept
      {
	*this = *this - __y;
	return *this;
      }

      constexpr chrono::year
      year() const noexcept
      { return _M_y; }

      constexpr chrono::month
      month() const noexcept
      { return _M_mdl.month(); }

      constexpr chrono::month_day_last
      month_day_last() const noexcept
      { return _M_mdl; }

      // Return A day representing the last day of this year, month pair.
      constexpr chrono::day
      day() const noexcept
      {
	const auto __m = static_cast<unsigned>(month());

	// Excluding February, the last day of month __m is either 30 or 31 or,
	// in another words, it is 30 + b = 30 | b, where b is in {0, 1}.

	// If __m in {1, 3, 4, 5, 6, 7}, then b is 1 if, and only if __m is odd.
	// Hence, b = __m & 1 = (__m ^ 0) & 1.

	// If __m in {8, 9, 10, 11, 12}, then b is 1 if, and only if __m is even.
	// Hence, b = (__m ^ 1) & 1.

	// Therefore, b = (__m ^ c) & 1, where c = 0, if __m < 8, or c = 1 if
	// __m >= 8, that is, c = __m >> 3.

	// The above mathematically justifies this implementation whose
	// performance does not depend on look-up tables being on the L1 cache.
	return chrono::day{__m != 2 ? ((__m ^ (__m >> 3)) & 1) | 30
				    : _M_y.is_leap() ? 29 : 28};
      }

      constexpr
      operator sys_days() const noexcept
      { return sys_days{year() / month() / day()}; }

      explicit constexpr
      operator local_days() const noexcept
      { return local_days{sys_days{*this}.time_since_epoch()}; }

      constexpr bool
      ok() const noexcept
      { return _M_y.ok() && _M_mdl.ok(); }

      friend constexpr bool
      operator==(const year_month_day_last& __x,
		 const year_month_day_last& __y) noexcept
      {
	return __x.year() == __y.year()
	  && __x.month_day_last() == __y.month_day_last();
      }

      friend constexpr strong_ordering
      operator<=>(const year_month_day_last& __x,
		  const year_month_day_last& __y) noexcept
	= default;

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_day_last
	operator+(const year_month_day_last& __ymdl,
		  const months& __dm) noexcept
	{ return (__ymdl.year() / __ymdl.month() + __dm) / last; }

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_day_last
	operator+(const months& __dm,
		  const year_month_day_last& __ymdl) noexcept
	{ return __ymdl + __dm; }

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_day_last
	operator-(const year_month_day_last& __ymdl,
		  const months& __dm) noexcept
	{ return __ymdl + -__dm; }

      friend constexpr year_month_day_last
      operator+(const year_month_day_last& __ymdl,
		const years& __dy) noexcept
      { return {__ymdl.year() + __dy, __ymdl.month_day_last()}; }

      friend constexpr year_month_day_last
      operator+(const years& __dy,
		const year_month_day_last& __ymdl) noexcept
      { return __ymdl + __dy; }

      friend constexpr year_month_day_last
      operator-(const year_month_day_last& __ymdl,
		const years& __dy) noexcept
      { return __ymdl + -__dy; }

      friend constexpr year_month_day_last
      operator/(const year_month& __ym, last_spec) noexcept
      { return {__ym.year(), chrono::month_day_last{__ym.month()}}; }

      friend constexpr year_month_day_last
      operator/(const chrono::year& __y,
		const chrono::month_day_last& __mdl) noexcept
      { return {__y, __mdl}; }

      friend constexpr year_month_day_last
      operator/(int __y, const chrono::month_day_last& __mdl) noexcept
      { return chrono::year(__y) / __mdl; }

      friend constexpr year_month_day_last
      operator/(const chrono::month_day_last& __mdl,
		const chrono::year& __y) noexcept
      { return __y / __mdl; }

      friend constexpr year_month_day_last
      operator/(const chrono::month_day_last& __mdl, int __y) noexcept
      { return chrono::year(__y) / __mdl; }

      // TODO: Implement operator<<.
    };

    // year_month_day ctor from year_month_day_last
    constexpr
    year_month_day::year_month_day(const year_month_day_last& __ymdl) noexcept
    : _M_y{__ymdl.year()}, _M_m{__ymdl.month()}, _M_d{__ymdl.day()}
    { }

    constexpr bool
    year_month_day::ok() const noexcept
    {
      if (!_M_y.ok() || !_M_m.ok())
	return false;
      return chrono::day{1} <= _M_d && _M_d <= (_M_y / _M_m / last).day();
    }

    // YEAR_MONTH_WEEKDAY

    class year_month_weekday
    {
    private:
      chrono::year _M_y;
      chrono::month _M_m;
      chrono::weekday_indexed _M_wdi;

      static constexpr year_month_weekday
      _S_from_sys_days(const sys_days& __dp)
      {
	year_month_day __ymd{__dp};
	chrono::weekday __wd{__dp};
	auto __index = __wd[(unsigned{__ymd.day()} - 1) / 7 + 1];
	return {__ymd.year(), __ymd.month(), __index};
      }

    public:
      year_month_weekday() = default;

      constexpr
      year_month_weekday(const chrono::year& __y, const chrono::month& __m,
			 const chrono::weekday_indexed& __wdi) noexcept
      : _M_y{__y}, _M_m{__m}, _M_wdi{__wdi}
      { }

      constexpr
      year_month_weekday(const sys_days& __dp) noexcept
      : year_month_weekday{_S_from_sys_days(__dp)}
      { }

      explicit constexpr
      year_month_weekday(const local_days& __dp) noexcept
      : year_month_weekday{sys_days{__dp.time_since_epoch()}}
      { }

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month_weekday&
	operator+=(const months& __m) noexcept
	{
	  *this = *this + __m;
	  return *this;
	}

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month_weekday&
	operator-=(const months& __m) noexcept
	{
	  *this = *this - __m;
	  return *this;
	}

      constexpr year_month_weekday&
      operator+=(const years& __y) noexcept
      {
	*this = *this + __y;
	return *this;
      }

      constexpr year_month_weekday&
      operator-=(const years& __y) noexcept
      {
	*this = *this - __y;
	return *this;
      }

      constexpr chrono::year
      year() const noexcept
      { return _M_y; }

      constexpr chrono::month
      month() const noexcept
      { return _M_m; }

      constexpr chrono::weekday
      weekday() const noexcept
      { return _M_wdi.weekday(); }

      constexpr unsigned
      index() const noexcept
      { return _M_wdi.index(); }

      constexpr chrono::weekday_indexed
      weekday_indexed() const noexcept
      { return _M_wdi; }

      constexpr
      operator sys_days() const noexcept
      {
	auto __d = sys_days{year() / month() / 1};
	return __d + (weekday() - chrono::weekday(__d)
		      + days{(static_cast<int>(index())-1)*7});
      }

      explicit constexpr
      operator local_days() const noexcept
      { return local_days{sys_days{*this}.time_since_epoch()}; }

      constexpr bool
      ok() const noexcept
      {
	if (!_M_y.ok() || !_M_m.ok() || !_M_wdi.ok())
	  return false;
	if (_M_wdi.index() <= 4)
	  return true;
	days __d = (_M_wdi.weekday()
		    - chrono::weekday{sys_days{_M_y / _M_m / 1}}
		    + days((_M_wdi.index()-1)*7 + 1));
	__glibcxx_assert(__d.count() >= 1);
	return __d.count() <= unsigned{(_M_y / _M_m / last).day()};
      }

      friend constexpr bool
      operator==(const year_month_weekday& __x,
		 const year_month_weekday& __y) noexcept
      {
	return __x.year() == __y.year()
	  && __x.month() == __y.month()
	  && __x.weekday_indexed() == __y.weekday_indexed();
      }

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_weekday
	operator+(const year_month_weekday& __ymwd, const months& __dm) noexcept
	{
	  return ((__ymwd.year() / __ymwd.month() + __dm)
		  / __ymwd.weekday_indexed());
	}

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_weekday
	operator+(const months& __dm, const year_month_weekday& __ymwd) noexcept
	{ return __ymwd + __dm; }

      friend constexpr year_month_weekday
      operator+(const year_month_weekday& __ymwd, const years& __dy) noexcept
      { return {__ymwd.year() + __dy, __ymwd.month(), __ymwd.weekday_indexed()}; }

      friend constexpr year_month_weekday
      operator+(const years& __dy, const year_month_weekday& __ymwd) noexcept
      { return __ymwd + __dy; }

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_weekday
	operator-(const year_month_weekday& __ymwd, const months& __dm) noexcept
	{ return __ymwd + -__dm; }

      friend constexpr year_month_weekday
      operator-(const year_month_weekday& __ymwd, const years& __dy) noexcept
      { return __ymwd + -__dy; }

      friend constexpr year_month_weekday
      operator/(const year_month& __ym,
		const chrono::weekday_indexed& __wdi) noexcept
      { return {__ym.year(), __ym.month(), __wdi}; }

      friend constexpr year_month_weekday
      operator/(const chrono::year& __y, const month_weekday& __mwd) noexcept
      { return {__y, __mwd.month(), __mwd.weekday_indexed()}; }

      friend constexpr year_month_weekday
      operator/(int __y, const month_weekday& __mwd) noexcept
      { return chrono::year(__y) / __mwd; }

      friend constexpr year_month_weekday
      operator/(const month_weekday& __mwd, const chrono::year& __y) noexcept
      { return __y / __mwd; }

      friend constexpr year_month_weekday
      operator/(const month_weekday& __mwd, int __y) noexcept
      { return chrono::year(__y) / __mwd; }

      // TODO: Implement operator<<.
    };

    // YEAR_MONTH_WEEKDAY_LAST

    class year_month_weekday_last
    {
    private:
      chrono::year _M_y;
      chrono::month _M_m;
      chrono::weekday_last _M_wdl;

    public:
      constexpr
      year_month_weekday_last(const chrono::year& __y, const chrono::month& __m,
			      const chrono::weekday_last& __wdl) noexcept
      : _M_y{__y}, _M_m{__m}, _M_wdl{__wdl}
      { }

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month_weekday_last&
	operator+=(const months& __m) noexcept
	{
	  *this = *this + __m;
	  return *this;
	}

      template<typename = __detail::__months_years_conversion_disambiguator>
	constexpr year_month_weekday_last&
	operator-=(const months& __m) noexcept
	{
	  *this = *this - __m;
	  return *this;
	}

      constexpr year_month_weekday_last&
      operator+=(const years& __y)  noexcept
      {
	*this = *this + __y;
	return *this;
      }

      constexpr year_month_weekday_last&
      operator-=(const years& __y)  noexcept
      {
	*this = *this - __y;
	return *this;
      }

      constexpr chrono::year
      year() const noexcept
      { return _M_y; }

      constexpr chrono::month
      month() const noexcept
      { return _M_m; }

      constexpr chrono::weekday
      weekday() const noexcept
      { return _M_wdl.weekday(); }

      constexpr chrono::weekday_last
      weekday_last() const noexcept
      { return _M_wdl; }

      constexpr
      operator sys_days() const noexcept
      {
	const auto __d = sys_days{_M_y / _M_m / last};
	return sys_days{(__d - (chrono::weekday{__d}
				- _M_wdl.weekday())).time_since_epoch()};
      }

      explicit constexpr
      operator local_days() const noexcept
      { return local_days{sys_days{*this}.time_since_epoch()}; }

      constexpr bool
      ok() const noexcept
      { return _M_y.ok() && _M_m.ok() && _M_wdl.ok(); }

      friend constexpr bool
      operator==(const year_month_weekday_last& __x,
		 const year_month_weekday_last& __y) noexcept
      {
	return __x.year() == __y.year()
	  && __x.month() == __y.month()
	  && __x.weekday_last() == __y.weekday_last();
      }

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_weekday_last
	operator+(const year_month_weekday_last& __ymwdl,
		  const months& __dm) noexcept
	{
	  return ((__ymwdl.year() / __ymwdl.month() + __dm)
		  / __ymwdl.weekday_last());
	}

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_weekday_last
	operator+(const months& __dm,
		  const year_month_weekday_last& __ymwdl) noexcept
	{ return __ymwdl + __dm; }

      friend constexpr year_month_weekday_last
      operator+(const year_month_weekday_last& __ymwdl,
		const years& __dy) noexcept
      { return {__ymwdl.year() + __dy, __ymwdl.month(), __ymwdl.weekday_last()}; }

      friend constexpr year_month_weekday_last
      operator+(const years& __dy,
		const year_month_weekday_last& __ymwdl) noexcept
      { return __ymwdl + __dy; }

      template<typename = __detail::__months_years_conversion_disambiguator>
	friend constexpr year_month_weekday_last
	operator-(const year_month_weekday_last& __ymwdl,
		  const months& __dm) noexcept
	{ return __ymwdl + -__dm; }

      friend constexpr year_month_weekday_last
      operator-(const year_month_weekday_last& __ymwdl,
		const years& __dy) noexcept
      { return __ymwdl + -__dy; }

      friend constexpr year_month_weekday_last
      operator/(const year_month& __ym,
		const chrono::weekday_last& __wdl) noexcept
      { return {__ym.year(), __ym.month(), __wdl}; }

      friend constexpr year_month_weekday_last
      operator/(const chrono::year& __y,
		const chrono::month_weekday_last& __mwdl) noexcept
      { return {__y, __mwdl.month(), __mwdl.weekday_last()}; }

      friend constexpr year_month_weekday_last
      operator/(int __y, const chrono::month_weekday_last& __mwdl) noexcept
      { return chrono::year(__y) / __mwdl; }

      friend constexpr year_month_weekday_last
      operator/(const chrono::month_weekday_last& __mwdl,
		const chrono::year& __y) noexcept
      { return __y / __mwdl; }

      friend constexpr year_month_weekday_last
      operator/(const chrono::month_weekday_last& __mwdl, int __y) noexcept
      { return chrono::year(__y) / __mwdl; }

      // TODO: Implement operator<<.
    };

    // HH_MM_SS

    namespace __detail
    {
      consteval long long
      __pow10(unsigned __n)
      {
	long long __r = 1;
	while (__n-- > 0)
	  __r *= 10;
	return __r;
      }
    }

    template<typename _Duration>
      class hh_mm_ss
      {
      private:
	static constexpr int
	_S_fractional_width()
	{
	  int __multiplicity_2 = 0;
	  int __multiplicity_5 = 0;
	  auto __den = _Duration::period::den;
	  while ((__den % 2) == 0)
	    {
	      ++__multiplicity_2;
	      __den /= 2;
	    }
	  while ((__den % 5) == 0)
	    {
	      ++__multiplicity_5;
	      __den /= 5;
	    }
	  if (__den != 1)
	    return 6;

	  int __width = (__multiplicity_2 > __multiplicity_5
			 ? __multiplicity_2 : __multiplicity_5);
	  if (__width > 18)
	    __width = 18;
	  return __width;
	}

	constexpr
	hh_mm_ss(_Duration __d, bool __is_neg)
	: _M_is_neg(__is_neg),
	  _M_h (duration_cast<chrono::hours>(__d)),
	  _M_m (duration_cast<chrono::minutes>(__d - hours())),
	  _M_s (duration_cast<chrono::seconds>(__d - hours() - minutes()))
	{
	  auto __ss = __d - hours() - minutes() - seconds();
	  if constexpr (treat_as_floating_point_v<typename precision::rep>)
	    _M_ss = __ss;
	  else
	    _M_ss = duration_cast<precision>(__ss);
	}

	static constexpr _Duration
	_S_abs(_Duration __d)
	{
	  if constexpr (numeric_limits<typename _Duration::rep>::is_signed)
	    return chrono::abs(__d);
	  else
	    return __d;
	}

      public:
	static constexpr unsigned fractional_width = {_S_fractional_width()};

	using precision
	  = duration<common_type_t<typename _Duration::rep,
				   chrono::seconds::rep>,
		     ratio<1, __detail::__pow10(fractional_width)>>;

	constexpr
	hh_mm_ss() noexcept
	: hh_mm_ss{_Duration::zero()}
	{ }

	constexpr explicit
	hh_mm_ss(_Duration __d)
	: hh_mm_ss(_S_abs(__d), __d < _Duration::zero())
	{ }

	constexpr bool
	is_negative() const noexcept
	{ return _M_is_neg; }

	constexpr chrono::hours
	hours() const noexcept
	{ return _M_h; }

	constexpr chrono::minutes
	minutes() const noexcept
	{ return _M_m; }

	constexpr chrono::seconds
	seconds() const noexcept
	{ return _M_s; }

	constexpr precision
	subseconds() const noexcept
	{ return _M_ss; }

	constexpr explicit
	operator precision() const noexcept
	{ return to_duration(); }

	constexpr precision
	to_duration() const noexcept
	{
	  if (_M_is_neg)
	    return -(_M_h + _M_m + _M_s + _M_ss);
	  else
	    return _M_h + _M_m + _M_s + _M_ss;
	}

	// TODO: Implement operator<<.

      private:
	bool _M_is_neg;
	chrono::hours _M_h;
	chrono::minutes _M_m;
	chrono::seconds _M_s;
	precision _M_ss;
      };
#endif // C++20

    /// @} group chrono
  } // namespace chrono

#if __cplusplus > 201103L

#define __cpp_lib_chrono_udls 201304

  inline namespace literals
  {
  /** ISO C++ 2014  namespace for suffixes for duration literals.
   *
   * These suffixes can be used to create `chrono::duration` values with
   * tick periods of hours, minutes, seconds, milliseconds, microseconds
   * or nanoseconds. For example, `std::chrono::seconds(5)` can be written
   * as `5s` after making the suffix visible in the current scope.
   * The suffixes can be made visible by a using-directive or
   * using-declaration such as:
   *  - `using namespace std::chrono_literals;`
   *  - `using namespace std::literals;`
   *  - `using namespace std::chrono;`
   *  - `using namespace std;`
   *  - `using std::chrono_literals::operator""s;`
   *
   * The result of these suffixes on an integer literal is one of the
   * standard typedefs such as `std::chrono::hours`.
   * The result on a floating-point literal is a duration type with the
   * specified tick period and an unspecified floating-point representation,
   * for example `1.5e2ms` might be equivalent to
   * `chrono::duration<long double, chrono::milli>(1.5e2)`.
   *
   * @ingroup chrono
   */
  inline namespace chrono_literals
  {
    /// @addtogroup chrono
    /// @{

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wliteral-suffix"
    /// @cond undocumented
    template<typename _Dur, char... _Digits>
      constexpr _Dur __check_overflow()
      {
	using _Val = __parse_int::_Parse_int<_Digits...>;
	constexpr typename _Dur::rep __repval = _Val::value;
	static_assert(__repval >= 0 && __repval == _Val::value,
		      "literal value cannot be represented by duration type");
	return _Dur(__repval);
      }
    /// @endcond

    /// Literal suffix for durations representing non-integer hours
    constexpr chrono::duration<long double, ratio<3600,1>>
    operator""h(long double __hours)
    { return chrono::duration<long double, ratio<3600,1>>{__hours}; }

    /// Literal suffix for durations of type `std::chrono::hours`
    template <char... _Digits>
      constexpr chrono::hours
      operator""h()
      { return __check_overflow<chrono::hours, _Digits...>(); }

    /// Literal suffix for durations representing non-integer minutes
    constexpr chrono::duration<long double, ratio<60,1>>
    operator""min(long double __mins)
    { return chrono::duration<long double, ratio<60,1>>{__mins}; }

    /// Literal suffix for durations of type `std::chrono::minutes`
    template <char... _Digits>
      constexpr chrono::minutes
      operator""min()
      { return __check_overflow<chrono::minutes, _Digits...>(); }

    /// Literal suffix for durations representing non-integer seconds
    constexpr chrono::duration<long double>
    operator""s(long double __secs)
    { return chrono::duration<long double>{__secs}; }

    /// Literal suffix for durations of type `std::chrono::seconds`
    template <char... _Digits>
      constexpr chrono::seconds
      operator""s()
      { return __check_overflow<chrono::seconds, _Digits...>(); }

    /// Literal suffix for durations representing non-integer milliseconds
    constexpr chrono::duration<long double, milli>
    operator""ms(long double __msecs)
    { return chrono::duration<long double, milli>{__msecs}; }

    /// Literal suffix for durations of type `std::chrono::milliseconds`
    template <char... _Digits>
      constexpr chrono::milliseconds
      operator""ms()
      { return __check_overflow<chrono::milliseconds, _Digits...>(); }

    /// Literal suffix for durations representing non-integer microseconds
    constexpr chrono::duration<long double, micro>
    operator""us(long double __usecs)
    { return chrono::duration<long double, micro>{__usecs}; }

    /// Literal suffix for durations of type `std::chrono::microseconds`
    template <char... _Digits>
      constexpr chrono::microseconds
      operator""us()
      { return __check_overflow<chrono::microseconds, _Digits...>(); }

    /// Literal suffix for durations representing non-integer nanoseconds
    constexpr chrono::duration<long double, nano>
    operator""ns(long double __nsecs)
    { return chrono::duration<long double, nano>{__nsecs}; }

    /// Literal suffix for durations of type `std::chrono::nanoseconds`
    template <char... _Digits>
      constexpr chrono::nanoseconds
      operator""ns()
      { return __check_overflow<chrono::nanoseconds, _Digits...>(); }

#if __cplusplus > 201703L
    constexpr chrono::day
    operator""d(unsigned long long __d) noexcept
    { return chrono::day{static_cast<unsigned>(__d)}; }

    constexpr chrono::year
    operator""y(unsigned long long __y) noexcept
    { return chrono::year{static_cast<int>(__y)}; }
#endif // C++20

#pragma GCC diagnostic pop
    /// @}
  } // inline namespace chrono_literals
  } // inline namespace literals

  namespace chrono
  {
    using namespace literals::chrono_literals;
  } // namespace chrono

#if __cplusplus > 201703L
  namespace chrono
  {
    /// @addtogroup chrono
    /// @{

    // 12/24 HOURS FUNCTIONS

    constexpr bool
    is_am(const hours& __h) noexcept
    { return 0h <= __h && __h <= 11h; }

    constexpr bool
    is_pm(const hours& __h) noexcept
    { return 12h <= __h && __h <= 23h; }

    constexpr hours
    make12(const hours& __h) noexcept
    {
      if (__h == 0h)
	return 12h;
      else if (__h > 12h)
	return __h - 12h;
      return __h;
    }

    constexpr hours
    make24(const hours& __h, bool __is_pm) noexcept
    {
      if (!__is_pm)
	{
	  if (__h == 12h)
	    return 0h;
	  else
	    return __h;
	}
      else
	{
	  if (__h == 12h)
	    return __h;
	  else
	    return __h + 12h;
	}
    }
    /// @} group chrono
  } // namespace chrono
#endif

#if __cplusplus >= 201703L
  namespace filesystem
  {
    struct __file_clock
    {
      using duration                  = chrono::nanoseconds;
      using rep                       = duration::rep;
      using period                    = duration::period;
      using time_point                = chrono::time_point<__file_clock>;
      static constexpr bool is_steady = false;

      static time_point
      now() noexcept
      { return _S_from_sys(chrono::system_clock::now()); }

#if __cplusplus > 201703L
      template<typename _Dur>
	static
	chrono::file_time<_Dur>
	from_sys(const chrono::sys_time<_Dur>& __t) noexcept
	{ return _S_from_sys(__t); }

      // For internal use only
      template<typename _Dur>
	static
	chrono::sys_time<_Dur>
	to_sys(const chrono::file_time<_Dur>& __t) noexcept
	{ return _S_to_sys(__t); }
#endif // C++20

    private:
      using __sys_clock = chrono::system_clock;

      // This clock's (unspecified) epoch is 2174-01-01 00:00:00 UTC.
      // A signed 64-bit duration with nanosecond resolution gives roughly
      // +/- 292 years, which covers the 1901-2446 date range for ext4.
      static constexpr chrono::seconds _S_epoch_diff{6437664000};

    protected:
      // For internal use only
      template<typename _Dur>
	static
	chrono::time_point<__file_clock, _Dur>
	_S_from_sys(const chrono::time_point<__sys_clock, _Dur>& __t) noexcept
	{
	  using __file_time = chrono::time_point<__file_clock, _Dur>;
	  return __file_time{__t.time_since_epoch()} - _S_epoch_diff;
	}

      // For internal use only
      template<typename _Dur>
	static
	chrono::time_point<__sys_clock, _Dur>
	_S_to_sys(const chrono::time_point<__file_clock, _Dur>& __t) noexcept
	{
	  using __sys_time = chrono::time_point<__sys_clock, _Dur>;
	  return __sys_time{__t.time_since_epoch()} + _S_epoch_diff;
	}
    };
  } // namespace filesystem
#endif // C++17
#endif // C++14

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // C++11

#endif //_GLIBCXX_CHRONO
