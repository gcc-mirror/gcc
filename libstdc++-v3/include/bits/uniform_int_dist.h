// Class template uniform_int_distribution -*- C++ -*-

// Copyright (C) 2009-2026 Free Software Foundation, Inc.
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

/**
 * @file bits/uniform_int_dist.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{random}
 */

#ifndef _GLIBCXX_BITS_UNIFORM_INT_DIST_H
#define _GLIBCXX_BITS_UNIFORM_INT_DIST_H

#include <type_traits>
#include <ext/numeric_traits.h>
#if __cplusplus > 201703L
# include <concepts>
#endif
#include <bits/concept_check.h> // __glibcxx_function_requires

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#ifdef __cpp_lib_concepts
  /// Requirements for a uniform random bit generator.
  /**
   * @ingroup random_distributions_uniform
   * @headerfile random
   * @since C++20
   */
  template<typename _Gen>
    concept uniform_random_bit_generator
      = invocable<_Gen&> && unsigned_integral<invoke_result_t<_Gen&>>
      && requires
      {
	{ _Gen::min() } -> same_as<invoke_result_t<_Gen&>>;
	{ _Gen::max() } -> same_as<invoke_result_t<_Gen&>>;
	requires bool_constant<(_Gen::min() < _Gen::max())>::value;
      };
#endif

  /// @cond undocumented
  namespace __detail
  {
    // Determine whether number is a power of two.
    // This is true for zero, which is OK because we want _Power_of_2(n+1)
    // to be true if n==numeric_limits<_Tp>::max() and so n+1 wraps around.
    template<typename _Tp>
      constexpr bool
      _Power_of_2(_Tp __x)
      {
	return ((__x - 1) & __x) == 0;
      }

    template<int __s,
	     int __which = ((__s <= __CHAR_BIT__ * sizeof (int))
			    + (__s <= __CHAR_BIT__ * sizeof (long))
			    + (__s <= __CHAR_BIT__ * sizeof (long long))
			    /* assume long long no bigger than __int128 */
			    + (__s <= 128))>
      struct _Select_uint_least_t
      {
	static_assert(__which < 0, /* needs to be dependent */
		      "sorry, would be too much trouble for a slow result");
      };

    template<int __s>
      struct _Select_uint_least_t<__s, 4>
      { using type = unsigned int; };

    template<int __s>
      struct _Select_uint_least_t<__s, 3>
      { using type = unsigned long; };

    template<int __s>
      struct _Select_uint_least_t<__s, 2>
      { using type = unsigned long long; };

// Alternate specialization for  
#if __SIZEOF_INT128__ > __SIZEOF_LONG_LONG__
    template<int __s>
      struct _Select_uint_least_t<__s, 1>
      { __extension__ using type = unsigned __int128; };
#elif __has_builtin(__builtin_add_overflow)  \
    && __has_builtin(__builtin_sub_overflow)  \
    && defined __UINT64_TYPE__
    template<int __s>
      struct _Select_uint_least_t<__s, 1>; // Defined in bits/random.h
#endif
  }
  /// @endcond

  /**
   * @brief Uniform discrete distribution for random numbers.
   * A discrete random distribution on the range @f$[min, max]@f$ with equal
   * probability throughout the range.
   *
   * @ingroup random_distributions_uniform
   * @headerfile random
   * @since C++11
   */
  template<typename _IntType = int>
    class uniform_int_distribution
    {
      static_assert(std::is_integral<_IntType>::value,
		    "template argument must be an integral type");

    public:
      /** The type of the range of the distribution. */
      typedef _IntType result_type;
      /** Parameter type. */
      struct param_type
      {
	typedef uniform_int_distribution<_IntType> distribution_type;

	param_type() : param_type(0) { }

	explicit
	param_type(_IntType __a,
		   _IntType __b = __gnu_cxx::__int_traits<_IntType>::__max)
	: _M_a(__a), _M_b(__b)
	{
	  __glibcxx_assert(_M_a <= _M_b);
	}

	result_type
	a() const
	{ return _M_a; }

	result_type
	b() const
	{ return _M_b; }

	friend bool
	operator==(const param_type& __p1, const param_type& __p2)
	{ return __p1._M_a == __p2._M_a && __p1._M_b == __p2._M_b; }

	friend bool
	operator!=(const param_type& __p1, const param_type& __p2)
	{ return !(__p1 == __p2); }

      private:
	_IntType _M_a;
	_IntType _M_b;
      };

    public:
      /**
       * @brief Constructs a uniform distribution object.
       */
      uniform_int_distribution() : uniform_int_distribution(0) { }

      /**
       * @brief Constructs a uniform distribution object.
       */
      explicit
      uniform_int_distribution(_IntType __a,
			       _IntType __b
				 = __gnu_cxx::__int_traits<_IntType>::__max)
      : _M_param(__a, __b)
      { }

      explicit
      uniform_int_distribution(const param_type& __p)
      : _M_param(__p)
      { }

      /**
       * @brief Resets the distribution state.
       *
       * Does nothing for the uniform integer distribution.
       */
      void
      reset() { }

      result_type
      a() const
      { return _M_param.a(); }

      result_type
      b() const
      { return _M_param.b(); }

      /**
       * @brief Returns the parameter set of the distribution.
       */
      param_type
      param() const
      { return _M_param; }

      /**
       * @brief Sets the parameter set of the distribution.
       * @param __param The new parameter set of the distribution.
       */
      void
      param(const param_type& __param)
      { _M_param = __param; }

      /**
       * @brief Returns the inclusive lower bound of the distribution range.
       */
      result_type
      min() const
      { return this->a(); }

      /**
       * @brief Returns the inclusive upper bound of the distribution range.
       */
      result_type
      max() const
      { return this->b(); }

      /**
       * @brief Generating functions.
       */
      template<typename _UniformRandomBitGenerator>
	result_type
	operator()(_UniformRandomBitGenerator& __urng)
        { return this->operator()(__urng, _M_param); }

      template<typename _UniformRandomBitGenerator>
	result_type
	operator()(_UniformRandomBitGenerator& __urng,
		   const param_type& __p);

      template<typename _ForwardIterator,
	       typename _UniformRandomBitGenerator>
	void
	__generate(_ForwardIterator __f, _ForwardIterator __t,
		   _UniformRandomBitGenerator& __urng)
	{ this->__generate(__f, __t, __urng, _M_param); }

      template<typename _ForwardIterator,
	       typename _UniformRandomBitGenerator>
	void
	__generate(_ForwardIterator __f, _ForwardIterator __t,
		   _UniformRandomBitGenerator& __urng,
		   const param_type& __p)
	{ this->__generate_impl(__f, __t, __urng, __p); }

      template<typename _UniformRandomBitGenerator>
	void
	__generate(result_type* __f, result_type* __t,
		   _UniformRandomBitGenerator& __urng,
		   const param_type& __p)
	{ this->__generate_impl(__f, __t, __urng, __p); }

      /**
       * @brief Return true if two uniform integer distributions have
       *        the same parameters.
       */
      friend bool
      operator==(const uniform_int_distribution& __d1,
		 const uniform_int_distribution& __d2)
      { return __d1._M_param == __d2._M_param; }

    private:
      template<typename _ForwardIterator,
	       typename _UniformRandomBitGenerator>
	void
	__generate_impl(_ForwardIterator __f, _ForwardIterator __t,
			_UniformRandomBitGenerator& __urng,
			const param_type& __p);

      param_type _M_param;

      // Lemire's nearly divisionless algorithm.
      // Returns an unbiased random number from __g downscaled to [0,__range)
      // using an unsigned type _Wp twice as wide as unsigned type _Up.
      template<size_t _Bits, typename _Urbg>
	static typename __detail::_Select_uint_least_t<_Bits>::type
	_S_nd(_Urbg& __g, typename __detail::_Select_uint_least_t<_Bits>::type __range)
	{
	  using _Up = typename __detail::_Select_uint_least_t<_Bits>::type;
	  using _Wp = typename __detail::_Select_uint_least_t<2 * _Bits>::type;
	  using _Up_traits = __gnu_cxx::__int_traits<_Up>;

	  constexpr auto __min = _Urbg::min();
	  constexpr _Up __mask = (_Bits < _Up_traits::__digits)
				 ? (_Up(1) << _Bits) - 1 : ~_Up(0);

	  // reference: Fast Random Integer Generation in an Interval
	  // ACM Transactions on Modeling and Computer Simulation 29 (1), 2019
	  // https://arxiv.org/abs/1805.10941
	  _Wp __product = _Wp(__g() - __min) * _Wp(__range);
	  _Up __low = _Up(__product) & __mask;
	  if (__low < __range)
	    {
	      const _Up __threshold = -__range % __range;
	      while (__low < __threshold)
		{
		  __product = _Wp(__g() - __min) * _Wp(__range);
		  __low = _Up(__product) & __mask;

		  // The algorithm is modified to alternate between rejecting
		  // from the beginning and end of the range. This guarantees
		  // that we stop for non-uniform engines that always result
		  // in values below the __threshold.
		  const _Up __back_threshold = __mask - __threshold;
		  if (__low <= __back_threshold)
		    break;

		  __product = _Wp(__g() - __min) * _Wp(__range);
		  __low = _Up(__product) & __mask;
		}
	    }
	  return _Up(__product >> _Bits) & __mask;
	}
    };

  template<typename _IntType>
    template<typename _UniformRandomBitGenerator>
      typename uniform_int_distribution<_IntType>::result_type
      uniform_int_distribution<_IntType>::
      operator()(_UniformRandomBitGenerator& __urng,
		 const param_type& __param)
      {
	typedef decltype(__urng()) _Gresult_type;
	typedef typename make_unsigned<result_type>::type __utype;
	typedef typename common_type<_Gresult_type, __utype>::type __uctype;

	constexpr __uctype __urngmin = _UniformRandomBitGenerator::min();
	constexpr __uctype __urngmax = _UniformRandomBitGenerator::max();
	static_assert( __urngmin < __urngmax,
	    "Uniform random bit generator must define min() < max()");
	constexpr __uctype __urngrange = __urngmax - __urngmin;

	const __uctype __urange
	  = __uctype(__param.b()) - __uctype(__param.a());

	__uctype __ret;
	if (__urngrange > __urange)
	  {
	    // downscaling

	    const __uctype __uerange = __urange + 1; // __urange can be zero

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr
#if __has_builtin(__builtin_popcountg)
      	    constexpr auto __bits = __builtin_popcountg(__urngrange);
	    if constexpr (__detail::_Power_of_2(__urngrange + 1) && __bits <= 32)
	      {
		// __urng produces values that use no more than 32-bits,
		// so 64-bit integer is sufficient to downscale to desired range.
		__UINT32_TYPE__ __u32erange = __uerange;
		__ret = _S_nd<__bits>(__urng, __u32erange);
	      }
# if __SIZEOF_INT128__
	    else if constexpr (__detail::_Power_of_2(__urngrange + 1) && __bits <= 64)
	      {
		// __urng produces values that use no more than 64-bits,
		// so 128-bit integer is sufficient to downscale to desired range.
		__UINT64_TYPE__ __u64erange = __uerange;
		__ret = _S_nd<__bits>(__urng, __u64erange);
	      }
# endif
	    else
#endif
	      {
		// fallback case (2 divisions)
		const __uctype __scaling = __urngrange / __uerange;
		const __uctype __past = __uerange * __scaling;
		do
		  __ret = __uctype(__urng()) - __urngmin;
		while (__ret >= __past);
		__ret /= __scaling;
	      }
#pragma GCC diagnostic pop
	  }
	else if (__urngrange < __urange)
	  {
	    // upscaling
	    /*
	      Note that every value in [0, urange]
	      can be written uniquely as

	      (urngrange + 1) * high + low

	      where

	      high in [0, urange / (urngrange + 1)]

	      and

	      low in [0, urngrange].
	    */
	    __uctype __tmp; // wraparound control
	    do
	      {
		const __uctype __uerngrange = __urngrange + 1;
		__tmp = (__uerngrange * operator()
			 (__urng, param_type(0, __urange / __uerngrange)));
		__ret = __tmp + (__uctype(__urng()) - __urngmin);
	      }
	    while (__ret > __urange || __ret < __tmp);
	  }
	else
	  __ret = __uctype(__urng()) - __urngmin;

	return __ret + __param.a();
      }


  template<typename _IntType>
    template<typename _ForwardIterator,
	     typename _UniformRandomBitGenerator>
      void
      uniform_int_distribution<_IntType>::
      __generate_impl(_ForwardIterator __f, _ForwardIterator __t,
		      _UniformRandomBitGenerator& __urng,
		      const param_type& __param)
      {
	__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
	typedef decltype(__urng()) _Gresult_type;
	typedef typename make_unsigned<result_type>::type __utype;
	typedef typename common_type<_Gresult_type, __utype>::type __uctype;

	static_assert( __urng.min() < __urng.max(),
	    "Uniform random bit generator must define min() < max()");

	constexpr __uctype __urngmin = __urng.min();
	constexpr __uctype __urngmax = __urng.max();
	constexpr __uctype __urngrange = __urngmax - __urngmin;
	const __uctype __urange
	  = __uctype(__param.b()) - __uctype(__param.a());

	__uctype __ret;

	if (__urngrange > __urange)
	  {
	    if (__detail::_Power_of_2(__urngrange + 1)
		&& __detail::_Power_of_2(__urange + 1))
	      {
		while (__f != __t)
		  {
		    __ret = __uctype(__urng()) - __urngmin;
		    *__f++ = (__ret & __urange) + __param.a();
		  }
	      }
	    else
	      {
		// downscaling
		const __uctype __uerange = __urange + 1; // __urange can be zero
		const __uctype __scaling = __urngrange / __uerange;
		const __uctype __past = __uerange * __scaling;
		while (__f != __t)
		  {
		    do
		      __ret = __uctype(__urng()) - __urngmin;
		    while (__ret >= __past);
		    *__f++ = __ret / __scaling + __param.a();
		  }
	      }
	  }
	else if (__urngrange < __urange)
	  {
	    // upscaling
	    /*
	      Note that every value in [0, urange]
	      can be written uniquely as

	      (urngrange + 1) * high + low

	      where

	      high in [0, urange / (urngrange + 1)]

	      and

	      low in [0, urngrange].
	    */
	    __uctype __tmp; // wraparound control
	    while (__f != __t)
	      {
		do
		  {
		    constexpr __uctype __uerngrange = __urngrange + 1;
		    __tmp = (__uerngrange * operator()
			     (__urng, param_type(0, __urange / __uerngrange)));
		    __ret = __tmp + (__uctype(__urng()) - __urngmin);
		  }
		while (__ret > __urange || __ret < __tmp);
		*__f++ = __ret;
	      }
	  }
	else
	  while (__f != __t)
	    *__f++ = __uctype(__urng()) - __urngmin + __param.a();
      }

  // operator!= and operator<< and operator>> are defined in <bits/random.h>

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif
