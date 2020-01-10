// Core algorithmic facilities -*- C++ -*-

// Copyright (C) 2020 Free Software Foundation, Inc.
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

/** @file bits/ranges_algo.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{algorithm}
 */

#ifndef _RANGES_ALGO_H
#define _RANGES_ALGO_H 1

#if __cplusplus > 201703L

#include <compare>
#include <cmath>
#include <iterator>
// #include <bits/range_concepts.h>
#include <ranges>
#include <bits/invoke.h>
#include <bits/cpp_type_traits.h> // __is_byte
#include <bits/random.h> // concept uniform_random_bit_generator

#if __cpp_lib_concepts
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace ranges
{
  namespace __detail
  {
    template<typename _Tp>
      constexpr inline bool __is_normal_iterator = false;

    template<typename _Iterator, typename _Container>
      constexpr inline bool
	__is_normal_iterator<__gnu_cxx::__normal_iterator<_Iterator,
							  _Container>> = true;

    template<typename _Tp>
      constexpr inline bool __is_reverse_iterator = false;

    template<typename _Iterator>
      constexpr inline bool
	__is_reverse_iterator<reverse_iterator<_Iterator>> = true;

    template<typename _Tp>
      constexpr inline bool __is_move_iterator = false;

    template<typename _Iterator>
      constexpr inline bool
	__is_move_iterator<move_iterator<_Iterator>> = true;

    template<typename _Comp, typename _Proj>
      constexpr auto
      __make_comp_proj(_Comp& __comp, _Proj& __proj)
      {
	return [&] (auto&& __lhs, auto&& __rhs) -> bool {
	  using _TL = decltype(__lhs);
	  using _TR = decltype(__rhs);
	  return std::__invoke(__comp,
			       std::__invoke(__proj, std::forward<_TL>(__lhs)),
			       std::__invoke(__proj, std::forward<_TR>(__rhs)));
	};
      }

    template<typename _Pred, typename _Proj>
      constexpr auto
      __make_pred_proj(_Pred& __pred, _Proj& __proj)
      {
	return [&] <typename _Tp> (_Tp&& __arg) -> bool {
	  return std::__invoke(__pred,
			       std::__invoke(__proj, std::forward<_Tp>(__arg)));
	};
      }
  } // namespace __detail

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr bool
    all_of(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (!(bool)std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  return false;
      return true;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr bool
    all_of(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::all_of(ranges::begin(__r), ranges::end(__r),
			    std::move(__pred), std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr bool
    any_of(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  return true;
      return false;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr bool
    any_of(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::any_of(ranges::begin(__r), ranges::end(__r),
			    std::move(__pred), std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr bool
    none_of(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  return false;
      return true;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr bool
    none_of(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::none_of(ranges::begin(__r), ranges::end(__r),
			    std::move(__pred), std::move(__proj));
    }

  template<typename _Iter, typename _Fp>
    struct for_each_result
    {
      [[no_unique_address]] _Iter in;
      [[no_unique_address]] _Fp fun;

      template<typename _Iter2, typename _F2p>
	requires convertible_to<const _Iter&, _Iter2>
	  && convertible_to<const _Fp&, _F2p>
	operator for_each_result<_Iter2, _F2p>() const &
	{ return {in, fun}; }

      template<typename _Iter2, typename _F2p>
	requires convertible_to<_Iter, _Iter2> && convertible_to<_Fp, _F2p>
	operator for_each_result<_Iter2, _F2p>() &&
	{ return {std::move(in), std::move(fun)}; }
    };

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirectly_unary_invocable<projected<_Iter, _Proj>> _Fun>
    constexpr for_each_result<_Iter, _Fun>
    for_each(_Iter __first, _Sent __last, _Fun __f, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	std::__invoke(__f, std::__invoke(__proj, *__first));
      return { std::move(__first), std::move(__f) };
    }

  template<input_range _Range, typename _Proj = identity,
	   indirectly_unary_invocable<projected<iterator_t<_Range>, _Proj>>
	     _Fun>
    constexpr for_each_result<safe_iterator_t<_Range>, _Fun>
    for_each(_Range&& __r, _Fun __f, _Proj __proj = {})
    {
      return ranges::for_each(ranges::begin(__r), ranges::end(__r),
			      std::move(__f), std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent, typename _Tp,
	   typename _Proj = identity>
    requires indirect_binary_predicate<ranges::equal_to,
				       projected<_Iter, _Proj>, const _Tp*>
    constexpr _Iter
    find(_Iter __first, _Sent __last, const _Tp& __value, _Proj __proj = {})
    {
      while (__first != __last
	  && !(std::__invoke(__proj, *__first) == __value))
	++__first;
      return __first;
    }

  template<input_range _Range, typename _Tp, typename _Proj = identity>
    requires indirect_binary_predicate<ranges::equal_to,
				       projected<iterator_t<_Range>, _Proj>,
				       const _Tp*>
    constexpr safe_iterator_t<_Range>
    find(_Range&& __r, const _Tp& __value, _Proj __proj = {})
    {
      return ranges::find(ranges::begin(__r), ranges::end(__r), __value,
			  std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr _Iter
    find_if(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      while (__first != __last
	  && !(bool)std::__invoke(__pred, std::__invoke(__proj, *__first)))
	++__first;
      return __first;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>>
	     _Pred>
    constexpr safe_iterator_t<_Range>
    find_if(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::find_if(ranges::begin(__r), ranges::end(__r),
			     std::move(__pred), std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr _Iter
    find_if_not(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      while (__first != __last
	  && (bool)std::__invoke(__pred, std::__invoke(__proj, *__first)))
	++__first;
      return __first;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>>
	     _Pred>
    constexpr safe_iterator_t<_Range>
    find_if_not(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::find_if_not(ranges::begin(__r), ranges::end(__r),
				 std::move(__pred), std::move(__proj));
    }

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr _Iter1
    find_first_of(_Iter1 __first1, _Sent1 __last1,
		  _Iter2 __first2, _Sent2 __last2,
		  _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      for (; __first1 != __last1; ++__first1)
	for (auto __iter = __first2; __iter != __last2; ++__iter)
	  if (std::__invoke(__pred,
			    std::__invoke(__proj1, *__first1),
			    std::__invoke(__proj2, *__iter)))
	    return __first1;
      return __first1;
    }

  template<input_range _Range1, forward_range _Range2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				   _Pred, _Proj1, _Proj2>
    constexpr safe_iterator_t<_Range1>
    find_first_of(_Range1&& __r1, _Range2&& __r2,
		  _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::find_first_of(ranges::begin(__r1), ranges::end(__r1),
				   ranges::begin(__r2), ranges::end(__r2),
				   std::move(__pred),
				   std::move(__proj1), std::move(__proj2));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp, typename _Proj = identity>
    requires indirect_binary_predicate<ranges::equal_to,
				       projected<_Iter, _Proj>,
				       const _Tp*>
    constexpr iter_difference_t<_Iter>
    count(_Iter __first, _Sent __last, const _Tp& __value, _Proj __proj = {})
    {
      iter_difference_t<_Iter> __n = 0;
      for (; __first != __last; ++__first)
	if (std::__invoke(__proj, *__first) == __value)
	  ++__n;
      return __n;
    }

  template<input_range _Range, typename _Tp, typename _Proj = identity>
    requires indirect_binary_predicate<ranges::equal_to,
				       projected<iterator_t<_Range>, _Proj>,
				       const _Tp*>
    constexpr range_difference_t<_Range>
    count(_Range&& __r, const _Tp& __value, _Proj __proj = {})
    {
      return ranges::count(ranges::begin(__r), ranges::end(__r),
			   __value, std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr iter_difference_t<_Iter>
    count_if(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      iter_difference_t<_Iter> __n = 0;
      for (; __first != __last; ++__first)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  ++__n;
      return __n;
    }

  template<input_range _Range,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr range_difference_t<_Range>
    count_if(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::count_if(ranges::begin(__r), ranges::end(__r),
			      std::move(__pred), std::move(__proj));
    }

  template<typename _Iter1, typename _Iter2>
    struct mismatch_result
    {
      [[no_unique_address]] _Iter1 in1;
      [[no_unique_address]] _Iter2 in2;

      template<typename _IIter1, typename _IIter2>
	requires convertible_to<const _Iter1&, _IIter1>
	  && convertible_to<const _Iter2&, _IIter2>
	operator mismatch_result<_IIter1, _IIter2>() const &
	{ return {in1, in2}; }

      template<typename _IIter1, typename _IIter2>
	requires convertible_to<_Iter1, _IIter1>
	  && convertible_to<_Iter2, _IIter2>
	operator mismatch_result<_IIter1, _IIter2>() &&
	{ return {std::move(in1), std::move(in2)}; }
    };

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr mismatch_result<_Iter1, _Iter2>
    mismatch(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	     _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      while (__first1 != __last1 && __first2 != __last2
	     && (bool)std::__invoke(__pred,
				    std::__invoke(__proj1, *__first1),
				    std::__invoke(__proj2, *__first2)))
      {
	++__first1;
	++__first2;
      }
      return { std::move(__first1), std::move(__first2) };
    }

  template<input_range _Range1, input_range _Range2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				   _Pred, _Proj1, _Proj2>
    constexpr mismatch_result<iterator_t<_Range1>, iterator_t<_Range2>>
    mismatch(_Range1&& __r1, _Range2&& __r2,
	     _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::mismatch(ranges::begin(__r1), ranges::end(__r1),
			      ranges::begin(__r2), ranges::end(__r2),
			      std::move(__pred),
			      std::move(__proj1), std::move(__proj2));
    }

  template<forward_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr subrange<_Iter1>
    search(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	   _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      if (__first1 == __last1 || __first2 == __last2)
	return {__first1, __first1};

      for (;;)
	{
	  for (;;)
	    {
	      if (__first1 == __last1)
		return {__first1, __first1};
	      if (std::__invoke(__pred,
				std::__invoke(__proj1, *__first1),
				std::__invoke(__proj2, *__first2)))
		break;
	      ++__first1;
	    }
	  auto __cur1 = __first1;
	  auto __cur2 = __first2;
	  for (;;)
	    {
	      if (++__cur2 == __last2)
		return {__first1, ++__cur1};
	      if (++__cur1 == __last1)
		return {__cur1, __cur1};
	      if (!(bool)std::__invoke(__pred,
				       std::__invoke(__proj1, *__cur1),
				       std::__invoke(__proj2, *__cur2)))
		{
		  ++__first1;
		  break;
		}
	    }
	}
    }

  template<forward_range _Range1, forward_range _Range2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				   _Pred, _Proj1, _Proj2>
    constexpr safe_subrange_t<_Range1>
    search(_Range1&& __r1, _Range2&& __r2,
	   _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::search(ranges::begin(__r1), ranges::end(__r1),
			    ranges::begin(__r2), ranges::end(__r2),
			    std::move(__pred),
			    std::move(__proj1), std::move(__proj2));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent, typename _Tp,
	   typename _Pred = ranges::equal_to, typename _Proj = identity>
    requires indirectly_comparable<_Iter, const _Tp*, _Pred, _Proj>
    constexpr subrange<_Iter>
    search_n(_Iter __first, _Sent __last, iter_difference_t<_Iter> __count,
	     const _Tp& __value, _Pred __pred = {}, _Proj __proj = {})
    {
      if (__count <= 0)
	return {__first, __first};

      auto __value_comp = [&] <typename _Rp> (_Rp&& __arg) {
	  return std::__invoke(__pred, std::forward<_Rp>(__arg), __value);
      };
      if (__count == 1)
	{
	  __first = ranges::find_if(std::move(__first), __last,
				    std::move(__value_comp), std::move(__proj));
	  if (__first == __last)
	    return {__first, __first};
	  else
	    {
	      auto __end = __first;
	      return {__first, ++__end};
	    }
	}

      if constexpr (sized_sentinel_for<_Sent, _Iter>)
	{
	  auto __tail_size = __last - __first;
	  auto __remainder = __count;

	  while (__remainder <= __tail_size)
	    {
	      __first += __remainder;
	      __tail_size -= __remainder;
	      auto __backtrack = __first;
	      while (__value_comp(std::__invoke(__proj, *--__backtrack)))
		{
		  if (--__remainder == 0)
		    return {__first - __count, __first};
		}
	    }
	  auto __i = __first + __tail_size;
	  return {__i, __i};
	}
      else
	{
	  __first = ranges::find_if(__first, __last, __value_comp, __proj);
	  while (__first != __last)
	    {
	      auto __n = __count;
	      auto __i = __first;
	      ++__i;
	      while (__i != __last && __n != 1
		     && __value_comp(std::__invoke(__proj, *__i)))
		{
		  ++__i;
		  --__n;
		}
	      if (__n == 1)
		return {__first, __i};
	      if (__i == __last)
		return {__i, __i};
	      __first = ranges::find_if(++__i, __last, __value_comp, __proj);
	    }
	  return {__first, __first};
	}
    }

  template<forward_range _Range, typename _Tp,
	   typename _Pred = ranges::equal_to, typename _Proj = identity>
    requires indirectly_comparable<iterator_t<_Range>, const _Tp*, _Pred, _Proj>
    constexpr safe_subrange_t<_Range>
    search_n(_Range&& __r, range_difference_t<_Range> __count,
	     const _Tp& __value, _Pred __pred = {}, _Proj __proj = {})
    {
      return ranges::search_n(ranges::begin(__r), ranges::end(__r),
			      std::move(__count), __value,
			      std::move(__pred), std::move(__proj));
    }

  template<forward_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr subrange<_Iter1>
    __find_end(_Iter1 __first1, _Sent1 __last1,
	       _Iter2 __first2, _Sent2 __last2,
	       _Pred __pred, _Proj1 __proj1, _Proj2 __proj2)
    {
      auto __i = ranges::next(__first1, __last1);
      if (__first2 == __last2)
	return {__i, __i};

      auto __result_begin = __i;
      auto __result_end = __i;
      for (;;)
	{
	  auto __new_range = ranges::search(__first1, __last1,
					    __first2, __last2,
					    __pred, __proj1, __proj2);
	  auto __new_result_begin = ranges::begin(__new_range);
	  auto __new_result_end = ranges::end(__new_range);
	  if (__new_result_begin == __last1)
	    return {__result_begin, __result_end};
	  else
	    {
	      __result_begin = __new_result_begin;
	      __result_end = __new_result_end;
	      __first1 = __result_begin;
	      ++__first1;
	    }
	}
    }

  template<forward_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr subrange<_Iter1>
    find_end(_Iter1 __first1, _Sent1 __last1,
	     _Iter2 __first2, _Sent2 __last2,
	     _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      if constexpr (bidirectional_iterator<_Iter1>
		    && bidirectional_iterator<_Iter2>)
	{
	  auto __i1 = ranges::next(__first1, __last1);
	  auto __i2 = ranges::next(__first2, __last2);
	  auto __rresult
	    = ranges::search(reverse_iterator<_Iter1>{__i1},
			     reverse_iterator<_Iter1>{__first1},
			     reverse_iterator<_Iter2>{__i2},
			     reverse_iterator<_Iter2>{__first2},
			     std::move(__pred),
			     std::move(__proj1), std::move(__proj2));
	  auto __result_first = ranges::end(__rresult).base();
	  auto __result_last = ranges::begin(__rresult).base();
	  if (__result_last == __first1)
	    return {__i1, __i1};
	  else
	    return {__result_first, __result_last};
	}
      else
	return ranges::__find_end(__first1, __last1, __first2, __last2,
				  std::move(__pred),
				  std::move(__proj1), std::move(__proj2));
    }

  template<forward_range _Range1, forward_range _Range2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				   _Pred, _Proj1, _Proj2>
    constexpr safe_subrange_t<_Range1>
    find_end(_Range1&& __r1, _Range2&& __r2,
	     _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::find_end(ranges::begin(__r1), ranges::end(__r1),
			      ranges::begin(__r2), ranges::end(__r2),
			      std::move(__pred),
			      std::move(__proj1), std::move(__proj2));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_binary_predicate<projected<_Iter, _Proj>,
				     projected<_Iter, _Proj>> _Pred
	     = ranges::equal_to>
    constexpr _Iter
    adjacent_find(_Iter __first, _Sent __last,
		  _Pred __pred = {}, _Proj __proj = {})
    {
      if (__first == __last)
	return __first;
      auto __next = __first;
      for (; ++__next != __last; __first = __next)
	{
	  if (std::__invoke(__pred,
			    std::__invoke(__proj, *__first),
			    std::__invoke(__proj, *__next)))
	    return __first;
	}
      return __next;
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_binary_predicate<
	     projected<iterator_t<_Range>, _Proj>,
	     projected<iterator_t<_Range>, _Proj>> _Pred = ranges::equal_to>
    constexpr safe_iterator_t<_Range>
    adjacent_find(_Range&& __r, _Pred __pred = {}, _Proj __proj = {})
    {
      return ranges::adjacent_find(ranges::begin(__r), ranges::end(__r),
				   std::move(__pred), std::move(__proj));
    }

  template<forward_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Proj1 = identity, typename _Proj2 = identity,
	   indirect_equivalence_relation<projected<_Iter1, _Proj1>,
					 projected<_Iter2, _Proj2>> _Pred
	     = ranges::equal_to>
    constexpr bool
    is_permutation(_Iter1 __first1, _Sent1 __last1,
		   _Iter2 __first2, _Sent2 __last2, _Pred __pred = {},
		   _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      constexpr bool __sized_iters
	= (sized_sentinel_for<_Sent1, _Iter1>
	   && sized_sentinel_for<_Sent2, _Iter2>);
      if constexpr (__sized_iters)
	{
	  auto __d1 = ranges::distance(__first1, __last1);
	  auto __d2 = ranges::distance(__first2, __last2);
	  if (__d1 != __d2)
	    return false;
	}

      // Efficiently compare identical prefixes:  O(N) if sequences
      // have the same elements in the same order.
      for (; __first1 != __last1 && __first2 != __last2;
	   ++__first1, (void)++__first2)
	if (!(bool)std::__invoke(__pred,
				 std::__invoke(__proj1, *__first1),
				 std::__invoke(__proj2, *__first2)))
	    break;

      if constexpr (__sized_iters)
	{
	  if (__first1 == __last1)
	    return true;
	}
      else
	{
	  auto __d1 = ranges::distance(__first1, __last1);
	  auto __d2 = ranges::distance(__first2, __last2);
	  if (__d1 == 0 && __d2 == 0)
	    return true;
	  if (__d1 != __d2)
	    return false;
	}

      for (auto __scan = __first1; __scan != __last1; ++__scan)
	{
	  auto __proj_scan = std::__invoke(__proj1, *__scan);
	  auto __comp_scan = [&] <typename _Tp> (_Tp&& __arg) {
	    return std::__invoke(__pred, __proj_scan,
				 std::forward<_Tp>(__arg));
	  };
	  if (__scan != ranges::find_if(__first1, __scan,
					__comp_scan, __proj1))
	    continue; // We've seen this one before.

	  auto __matches = ranges::count_if(__first2, __last2,
					    __comp_scan, __proj2);
	  if (__matches == 0
	      || ranges::count_if(__scan, __last1,
				  __comp_scan, __proj1) != __matches)
	    return false;
	}
      return true;
    }

  template<forward_range _Range1, forward_range _Range2,
	   typename _Proj1 = identity, typename _Proj2 = identity,
	   indirect_equivalence_relation<
	     projected<iterator_t<_Range1>, _Proj1>,
	     projected<iterator_t<_Range2>, _Proj2>> _Pred = ranges::equal_to>
    constexpr bool
    is_permutation(_Range1&& __r1, _Range2&& __r2, _Pred __pred = {},
		   _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::is_permutation(ranges::begin(__r1), ranges::end(__r1),
				    ranges::begin(__r2), ranges::end(__r2),
				    std::move(__pred),
				    std::move(__proj1), std::move(__proj2));
    }

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred, typename _Proj1, typename _Proj2>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr bool
    __equal(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	    _Pred __pred, _Proj1 __proj1, _Proj2 __proj2)
    {
      // TODO: implement more specializations to at least have parity with
      // std::equal.
      constexpr bool __sized_iters
	= (sized_sentinel_for<_Sent1, _Iter1>
	   && sized_sentinel_for<_Sent2, _Iter2>);
      if constexpr (__sized_iters)
	{
	  auto __d1 = ranges::distance(__first1, __last1);
	  auto __d2 = ranges::distance(__first2, __last2);
	  if (__d1 != __d2)
	    return false;

	  using _ValueType1 = iter_value_t<_Iter1>;
	  using _ValueType2 = iter_value_t<_Iter2>;
	  constexpr bool __use_memcmp
	    = ((is_integral_v<_ValueType1> || is_pointer_v<_ValueType1>)
	       && is_same_v<_ValueType1, _ValueType2>
	       && is_pointer_v<_Iter1>
	       && is_pointer_v<_Iter2>
	       && is_same_v<_Pred, ranges::equal_to>
	       && is_same_v<_Proj1, identity>
	       && is_same_v<_Proj2, identity>);
	  if constexpr (__use_memcmp)
	    {
	      if (const size_t __len = (__last1 - __first1))
		return !std::__memcmp(__first1, __first2, __len);
	      return true;
	    }
	  else
	    {
	      for (; __first1 != __last1; ++__first1, (void)++__first2)
		if (!(bool)std::__invoke(__pred,
					 std::__invoke(__proj1, *__first1),
					 std::__invoke(__proj2, *__first2)))
		  return false;
	      return true;
	    }
	}
      else
	{
	  for (; __first1 != __last1 && __first2 != __last2;
	       ++__first1, (void)++__first2)
	    if (!(bool)std::__invoke(__pred,
				     std::__invoke(__proj1, *__first1),
				     std::__invoke(__proj2, *__first2)))
	      return false;
	  return __first1 == __last1 && __first2 == __last2;
	}
    }

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr bool
    equal(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	  _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::__equal(std::__niter_base(std::move(__first1)),
			     std::__niter_base(std::move(__last1)),
			     std::__niter_base(std::move(__first2)),
			     std::__niter_base(std::move(__last2)),
			     std::move(__pred),
			     std::move(__proj1), std::move(__proj2));
    }

  template<input_range _Range1, input_range _Range2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				   _Pred, _Proj1, _Proj2>
    constexpr bool
    equal(_Range1&& __r1, _Range2&& __r2,
	  _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::equal(ranges::begin(__r1), ranges::end(__r1),
			   ranges::begin(__r2), ranges::end(__r2),
			   std::move(__pred),
			   std::move(__proj1), std::move(__proj2));
    }

  template<typename _Iter, typename _Out>
    struct copy_result
    {
      [[no_unique_address]] _Iter in;
      [[no_unique_address]] _Out out;

      template<typename _Iter2, typename _Out2>
	requires convertible_to<const _Iter&, _Iter2>
	  && convertible_to<const _Out&, _Out2>
	operator copy_result<_Iter2, _Out2>() const &
	{ return {in, out}; }

      template<typename _Iter2, typename _Out2>
	requires convertible_to<_Iter, _Iter2>
	  && convertible_to<_Out, _Out2>
	operator copy_result<_Iter2, _Out2>() &&
	{ return {std::move(in), std::move(out)}; }
    };

  template<typename _Iter, typename _Out>
    using move_result = copy_result<_Iter, _Out>;

  template<typename _Iter1, typename _Iter2>
    using move_backward_result = copy_result<_Iter1, _Iter2>;

  template<typename _Iter1, typename _Iter2>
    using copy_backward_result = copy_result<_Iter1, _Iter2>;

  template<bool _IsMove,
	   bidirectional_iterator _Iter, sentinel_for<_Iter> _Sent,
	   bidirectional_iterator _Out>
    requires (_IsMove
	      ? indirectly_movable<_Iter, _Out>
	      : indirectly_copyable<_Iter, _Out>)
    constexpr conditional_t<_IsMove,
			    move_backward_result<_Iter, _Out>,
			    copy_backward_result<_Iter, _Out>>
    __copy_or_move_backward(_Iter __first, _Sent __last, _Out __result);

  template<bool _IsMove,
	   input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out>
    requires (_IsMove
	      ? indirectly_movable<_Iter, _Out>
	      : indirectly_copyable<_Iter, _Out>)
    constexpr conditional_t<_IsMove,
			    move_result<_Iter, _Out>,
			    copy_result<_Iter, _Out>>
    __copy_or_move(_Iter __first, _Sent __last, _Out __result)
    {
      // TODO: implement more specializations to be at least on par with
      // std::copy/std::move.
      constexpr bool __normal_iterator_p
	= (__detail::__is_normal_iterator<_Iter>
	   || __detail::__is_normal_iterator<_Out>);
      constexpr bool __reverse_p
	= (__detail::__is_reverse_iterator<_Iter>
	   && __detail::__is_reverse_iterator<_Out>);
      constexpr bool __move_iterator_p = __detail::__is_move_iterator<_Iter>;
      if constexpr (__move_iterator_p)
	{
	  auto [__in, __out]
	    = ranges::__copy_or_move<true>(std::move(__first).base(),
					   std::move(__last).base(),
					   std::move(__result));
	  return {move_iterator{std::move(__in)}, std::move(__out)};
	}
      else if constexpr (__reverse_p)
	{
	  auto [__in,__out]
	    = ranges::__copy_or_move_backward<_IsMove>(__last.base(),
						       __first.base(),
						       __result.base());
	  return {reverse_iterator{std::move(__in)},
		  reverse_iterator{std::move(__out)}};
	}
      else if constexpr (__normal_iterator_p)
	{
	  auto [__in,__out]
	    = ranges::__copy_or_move<_IsMove>(std::__niter_base(__first),
					      std::__niter_base(__last),
					      std::__niter_base(__result));
	  return {std::__niter_wrap(__first, std::move(__in)),
		  std::__niter_wrap(__result, std::move(__out))};
	}
      else if constexpr (sized_sentinel_for<_Sent, _Iter>)
	{
	  using _ValueTypeI = iter_value_t<_Iter>;
	  using _ValueTypeO = iter_value_t<_Out>;
	  constexpr bool __use_memmove
	    = (is_trivially_copyable_v<_ValueTypeI>
	       && is_same_v<_ValueTypeI, _ValueTypeO>
	       && is_pointer_v<_Iter>
	       && is_pointer_v<_Out>);

	  if constexpr (__use_memmove)
	    {
	      static_assert(_IsMove
			    ? is_move_assignable_v<_ValueTypeI>
			    : is_copy_assignable_v<_ValueTypeI>);
	      auto __num = __last - __first;
	      if (__num)
		std::__memmove<_IsMove>(__result, __first, __num);
	      return {__first + __num, __result + __num};
	    }
	  else
	    {
	      for (auto __n = __last - __first; __n > 0; --__n)
		{
		  if constexpr (_IsMove)
		    *__result = std::move(*__first);
		  else
		    *__result = *__first;
		  ++__first;
		  ++__result;
		}
	      return {std::move(__first), std::move(__result)};
	    }
	}
      else
	{
	  while (__first != __last)
	    {
	      if constexpr (_IsMove)
		*__result = std::move(*__first);
	      else
		*__result = *__first;
	      ++__first;
	      ++__result;
	    }
	  return {std::move(__first), std::move(__result)};
	}
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out>
    requires indirectly_copyable<_Iter, _Out>
    constexpr copy_result<_Iter, _Out>
    copy(_Iter __first, _Sent __last, _Out __result)
    {
      return ranges::__copy_or_move<false>(std::move(__first),
					   std::move(__last),
					   std::move(__result));
    }

  template<input_range _Range, weakly_incrementable _Out>
    requires indirectly_copyable<iterator_t<_Range>, _Out>
    constexpr copy_result<safe_iterator_t<_Range>, _Out>
    copy(_Range&& __r, _Out __result)
    {
      return ranges::copy(ranges::begin(__r), ranges::end(__r),
			  std::move(__result));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out>
    requires indirectly_movable<_Iter, _Out>
    constexpr move_result<_Iter, _Out>
    move(_Iter __first, _Sent __last, _Out __result)
    {
      return ranges::__copy_or_move<true>(std::move(__first),
					  std::move(__last),
					  std::move(__result));
    }

  template<input_range _Range, weakly_incrementable _Out>
    requires indirectly_movable<iterator_t<_Range>, _Out>
    constexpr move_result<safe_iterator_t<_Range>, _Out>
    move(_Range&& __r, _Out __result)
    {
      return ranges::move(ranges::begin(__r), ranges::end(__r),
			  std::move(__result));
    }

  template<bool _IsMove,
	   bidirectional_iterator _Iter, sentinel_for<_Iter> _Sent,
	   bidirectional_iterator _Out>
    requires (_IsMove
	      ? indirectly_movable<_Iter, _Out>
	      : indirectly_copyable<_Iter, _Out>)
    constexpr conditional_t<_IsMove,
			    move_backward_result<_Iter, _Out>,
			    copy_backward_result<_Iter, _Out>>
    __copy_or_move_backward(_Iter __first, _Sent __last, _Out __result)
    {
      // TODO: implement more specializations to be at least on par with
      // std::copy_backward/std::move_backward.
      constexpr bool __normal_iterator_p
	= (__detail::__is_normal_iterator<_Iter>
	   || __detail::__is_normal_iterator<_Out>);
      constexpr bool __reverse_p
	= (__detail::__is_reverse_iterator<_Iter>
	   && __detail::__is_reverse_iterator<_Out>);
      if constexpr (__reverse_p)
	{
	  auto [__in,__out]
	    = ranges::__copy_or_move<_IsMove>(__last.base(),
					      __first.base(),
					      __result.base());
	  return {reverse_iterator{std::move(__in)},
		  reverse_iterator{std::move(__out)}};
	}
      else if constexpr (__normal_iterator_p)
	{
	  auto [__in,__out]
	    = ranges::__copy_or_move_backward<_IsMove>
	      (std::__niter_base(__first),
	       std::__niter_base(__last),
	       std::__niter_base(__result));
	  return {std::__niter_wrap(__first, std::move(__in)),
		  std::__niter_wrap(__result, std::move(__out))};
	}
      else if constexpr (sized_sentinel_for<_Sent, _Iter>)
	{
	  using _ValueTypeI = iter_value_t<_Iter>;
	  using _ValueTypeO = iter_value_t<_Out>;
	  constexpr bool __use_memmove
	    = (is_trivially_copyable_v<_ValueTypeI>
	       && is_same_v<_ValueTypeI, _ValueTypeO>
	       && is_pointer_v<_Iter>
	       && is_pointer_v<_Out>);
	  if constexpr (__use_memmove)
	    {
	      static_assert(_IsMove
			    ? is_move_assignable_v<_ValueTypeI>
			    : is_copy_assignable_v<_ValueTypeI>);
	      auto __num = __last - __first;
	      if (__num)
		std::__memmove<_IsMove>(__result - __num, __first, __num);
	      return {__first + __num, __result - __num};
	    }
	  else
	    {
	      auto __lasti = ranges::next(__first, __last);
	      auto __tail = __lasti;

	      for (auto __n = __last - __first; __n > 0; --__n)
		{
		  --__tail;
		  --__result;
		  if constexpr (_IsMove)
		    *__result = std::move(*__tail);
		  else
		    *__result = *__tail;
		}
	      return {std::move(__lasti), std::move(__result)};
	    }
	}
      else
	{
	  auto __lasti = ranges::next(__first, __last);
	  auto __tail = __lasti;

	  while (__first != __tail)
	    {
	      --__tail;
	      --__result;
	      if constexpr (_IsMove)
		*__result = std::move(*__tail);
	      else
		*__result = *__tail;
	    }
	  return {std::move(__lasti), std::move(__result)};
	}
    }

  template<bidirectional_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   bidirectional_iterator _Iter2>
    requires indirectly_copyable<_Iter1, _Iter2>
    constexpr copy_backward_result<_Iter1, _Iter2>
    copy_backward(_Iter1 __first, _Sent1 __last, _Iter2 __result)
    {
      return ranges::__copy_or_move_backward<false>(std::move(__first),
						    std::move(__last),
						    std::move(__result));
    }

  template<bidirectional_range _Range, bidirectional_iterator _Iter>
    requires indirectly_copyable<iterator_t<_Range>, _Iter>
    constexpr copy_backward_result<safe_iterator_t<_Range>, _Iter>
    copy_backward(_Range&& __r, _Iter __result)
    {
      return ranges::copy_backward(ranges::begin(__r), ranges::end(__r),
				   std::move(__result));
    }

  template<bidirectional_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   bidirectional_iterator _Iter2>
    requires indirectly_movable<_Iter1, _Iter2>
    constexpr move_backward_result<_Iter1, _Iter2>
    move_backward(_Iter1 __first, _Sent1 __last, _Iter2 __result)
    {
      return ranges::__copy_or_move_backward<true>(std::move(__first),
						   std::move(__last),
						   std::move(__result));
    }

  template<bidirectional_range _Range, bidirectional_iterator _Iter>
    requires indirectly_movable<iterator_t<_Range>, _Iter>
    constexpr move_backward_result<safe_iterator_t<_Range>, _Iter>
    move_backward(_Range&& __r, _Iter __result)
    {
      return ranges::move_backward(ranges::begin(__r), ranges::end(__r),
				   std::move(__result));
    }

  template<typename _Iter, typename _Out>
    using copy_n_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter, weakly_incrementable _Out>
    requires indirectly_copyable<_Iter, _Out>
    constexpr copy_n_result<_Iter, _Out>
    copy_n(_Iter __first, iter_difference_t<_Iter> __n, _Out __result)
    {
      if constexpr (random_access_iterator<_Iter>)
	return ranges::copy(__first, __first + __n, std::move(__result));
      else
	{
	  for (; __n > 0; --__n, (void)++__result, (void)++__first)
	    *__result = *__first;
	  return {std::move(__first), std::move(__result)};
	}
    }

  template<typename _Iter, typename _Out>
    using copy_if_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out, typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    requires indirectly_copyable<_Iter, _Out>
    constexpr copy_if_result<_Iter, _Out>
    copy_if(_Iter __first, _Sent __last, _Out __result,
	    _Pred __pred, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  {
	    *__result = *__first;
	    ++__result;
	  }
      return {std::move(__first), std::move(__result)};
    }

  template<input_range _Range, weakly_incrementable _Out,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    requires indirectly_copyable<iterator_t<_Range>, _Out>
    constexpr copy_if_result<safe_iterator_t<_Range>, _Out>
    copy_if(_Range&& __r, _Out __result, _Pred __pred, _Proj __proj = {})
    {
      return ranges::copy_if(ranges::begin(__r), ranges::end(__r),
			     std::move(__result),
			     std::move(__pred), std::move(__proj));
    }

  template<typename _Iter1, typename _Iter2>
    using swap_ranges_result = mismatch_result<_Iter1, _Iter2>;

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2>
    requires indirectly_swappable<_Iter1, _Iter2>
    constexpr swap_ranges_result<_Iter1, _Iter2>
    swap_ranges(_Iter1 __first1, _Sent1 __last1,
		_Iter2 __first2, _Sent2 __last2)
    {
      for (; __first1 != __last1 && __first2 != __last2;
	   ++__first1, (void)++__first2)
	ranges::iter_swap(__first1, __first2);
      return {std::move(__first1), std::move(__first2)};
    }

  template<input_range _Range1, input_range _Range2>
    requires indirectly_swappable<iterator_t<_Range1>, iterator_t<_Range2>>
    constexpr swap_ranges_result<safe_iterator_t<_Range1>,
				 safe_iterator_t<_Range2>>
    swap_ranges(_Range1&& __r1, _Range2&& __r2)
    {
      return ranges::swap_ranges(ranges::begin(__r1), ranges::end(__r1),
				 ranges::begin(__r2), ranges::end(__r2));
    }

  template<typename _Iter, typename _Out>
    using unary_transform_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out,
	   copy_constructible _Fp, typename _Proj = identity>
    requires writable<_Out, indirect_result_t<_Fp&, projected<_Iter, _Proj>>>
    constexpr unary_transform_result<_Iter, _Out>
    transform(_Iter __first1, _Sent __last1, _Out __result,
	      _Fp __op, _Proj __proj = {})
    {
      for (; __first1 != __last1; ++__first1, (void)++__result)
	*__result = std::__invoke(__op, std::__invoke(__proj, *__first1));
      return {std::move(__first1), std::move(__result)};
    }

  template<input_range _Range, weakly_incrementable _Out,
	   copy_constructible _Fp, typename _Proj = identity>
    requires writable<_Out,
		      indirect_result_t<_Fp&, projected<iterator_t<_Range>,
						      _Proj>>>
    constexpr unary_transform_result<safe_iterator_t<_Range>, _Out>
    transform(_Range&& __r, _Out __result, _Fp __op, _Proj __proj = {})
    {
      return ranges::transform(ranges::begin(__r), ranges::end(__r),
			       std::move(__result),
			       std::move(__op), std::move(__proj));
    }

  template<typename _Iter1, typename _Iter2, typename _Out>
    struct binary_transform_result
    {
      [[no_unique_address]] _Iter1 in1;
      [[no_unique_address]] _Iter2 in2;
      [[no_unique_address]] _Out  out;

      template<typename _IIter1, typename _IIter2, typename _OOut>
	requires convertible_to<const _Iter1&, _IIter1> &&
	  && convertible_to<const _Iter2&, _IIter2>
	  && convertible_to<const _Out&, _OOut>
	operator binary_transform_result<_IIter1, _IIter2, _OOut>() const &
	{ return {in1, in2, out}; }

      template<typename _IIter1, typename _IIter2, typename _OOut>
	requires convertible_to<_Iter1, _IIter1>
	  && convertible_to<_Iter2, _IIter2>
	  && convertible_to<_Out, _OOut>
	operator binary_transform_result<_IIter1, _IIter2, _OOut>() &&
	{ return {std::move(in1), std::move(in2), std::move(out)}; }
    };

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   weakly_incrementable _Out, copy_constructible _Fp,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires writable<_Out, indirect_result_t<_Fp&, projected<_Iter1, _Proj1>,
					   projected<_Iter2, _Proj2>>>
    constexpr binary_transform_result<_Iter1, _Iter2, _Out>
    transform(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	      _Out __result, _Fp __binary_op,
	      _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      for (; __first1 != __last1 && __first2 != __last2;
	   ++__first1, (void)++__first2, ++__result)
	*__result = std::__invoke(__binary_op,
				  std::__invoke(__proj1, *__first1),
				  std::__invoke(__proj2, *__first2));
      return {std::move(__first1), std::move(__first2), std::move(__result)};
    }

  template<input_range _Range1, input_range _Range2,
	   weakly_incrementable _Out, copy_constructible _Fp,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires writable<_Out, indirect_result_t<_Fp&,
					      projected<iterator_t<_Range1>,
							_Proj1>,
					      projected<iterator_t<_Range2>,
							_Proj2>>>
    constexpr binary_transform_result<safe_iterator_t<_Range1>,
				      safe_iterator_t<_Range2>, _Out>
    transform(_Range1&& __r1, _Range2&& __r2, _Out __result,
	      _Fp __binary_op, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::transform(ranges::begin(__r1), ranges::end(__r1),
			       ranges::begin(__r2), ranges::end(__r2),
			       std::move(__result), std::move(__binary_op),
			       std::move(__proj1), std::move(__proj2));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp1, typename _Tp2, typename _Proj = identity>
    requires writable<_Iter, const _Tp2&> &&
	     indirect_binary_predicate<ranges::equal_to,
				       projected<_Iter, _Proj>, const _Tp1*>
    constexpr _Iter
    replace(_Iter __first, _Sent __last,
	    const _Tp1& __old_value, const _Tp2& __new_value,
	    _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (std::__invoke(__proj, *__first) == __old_value)
	  *__first = __new_value;
      return __first;
    }

  template<input_range _Range,
	   typename _Tp1, typename _Tp2, typename _Proj = identity>
    requires writable<iterator_t<_Range>, const _Tp2&> &&
	     indirect_binary_predicate<ranges::equal_to,
				       projected<iterator_t<_Range>, _Proj>,
						 const _Tp1*>
    constexpr safe_iterator_t<_Range>
    replace(_Range&& __r,
	    const _Tp1& __old_value, const _Tp2& __new_value,
	    _Proj __proj = {})
    {
      return ranges::replace(ranges::begin(__r), ranges::end(__r),
			     __old_value, __new_value, std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp, typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    requires writable<_Iter, const _Tp&>
    constexpr _Iter
    replace_if(_Iter __first, _Sent __last,
	       _Pred __pred, const _Tp& __new_value, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  *__first = __new_value;
      return std::move(__first);
    }

  template<input_range _Range, typename _Tp, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    requires writable<iterator_t<_Range>, const _Tp&>
    constexpr safe_iterator_t<_Range>
    replace_if(_Range&& __r,
	       _Pred __pred, const _Tp& __new_value, _Proj __proj = {})
    {
      return ranges::replace_if(ranges::begin(__r), ranges::end(__r),
				std::move(__pred), __new_value,
				std::move(__proj));
    }

  template<typename _Iter, typename _Out>
    using replace_copy_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp1, typename _Tp2, output_iterator<const _Tp2&> _Out,
	   typename _Proj = identity>
    requires indirectly_copyable<_Iter, _Out>
      && indirect_binary_predicate<ranges::equal_to,
				   projected<_Iter, _Proj>, const _Tp1*>
    constexpr replace_copy_result<_Iter, _Out>
    replace_copy(_Iter __first, _Sent __last, _Out __result,
		 const _Tp1& __old_value, const _Tp2& __new_value,
		 _Proj __proj = {})
    {
      for (; __first != __last; ++__first, (void)++__result)
	if (std::__invoke(__proj, *__first) == __old_value)
	  *__result = __new_value;
	else
	  *__result = *__first;
      return {std::move(__first), std::move(__result)};
    }

  template<input_range _Range, typename _Tp1, typename _Tp2,
	   output_iterator<const _Tp2&> _Out, typename _Proj = identity>
    requires indirectly_copyable<iterator_t<_Range>, _Out>
      && indirect_binary_predicate<ranges::equal_to,
				   projected<iterator_t<_Range>, _Proj>,
				   const _Tp1*>
    constexpr replace_copy_result<safe_iterator_t<_Range>, _Out>
    replace_copy(_Range&& __r, _Out __result,
		 const _Tp1& __old_value, const _Tp2& __new_value,
		 _Proj __proj = {})
    {
      return ranges::replace_copy(ranges::begin(__r), ranges::end(__r),
				  std::move(__result), __old_value,
				  __new_value, std::move(__proj));
    }

  template<typename _Iter, typename _Out>
    using replace_copy_if_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp, output_iterator<const _Tp&> _Out,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    requires indirectly_copyable<_Iter, _Out>
    constexpr replace_copy_if_result<_Iter, _Out>
    replace_copy_if(_Iter __first, _Sent __last, _Out __result,
		    _Pred __pred, const _Tp& __new_value, _Proj __proj = {})
    {
      for (; __first != __last; ++__first, (void)++__result)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  *__result = __new_value;
	else
	  *__result = *__first;
      return {std::move(__first), std::move(__result)};
    }

  template<input_range _Range,
	   typename _Tp, output_iterator<const _Tp&> _Out,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    requires indirectly_copyable<iterator_t<_Range>, _Out>
    constexpr replace_copy_if_result<safe_iterator_t<_Range>, _Out>
    replace_copy_if(_Range&& __r, _Out __result,
		    _Pred __pred, const _Tp& __new_value, _Proj __proj = {})
    {
      return ranges::replace_copy_if(ranges::begin(__r), ranges::end(__r),
				     std::move(__result), std::move(__pred),
				     __new_value, std::move(__proj));
    }

  template<typename _Tp, output_iterator<const _Tp&> _Out>
    constexpr _Out
    fill_n(_Out __first, iter_difference_t<_Out> __n, const _Tp& __value)
    {
      // TODO: implement more specializations to be at least on par with
      // std::fill_n
      if (__n <= 0)
	return __first;

      // TODO: is __is_byte the best condition?
      if constexpr (is_pointer_v<_Out> && __is_byte<_Tp>::__value)
	{
	  __builtin_memset(__first, static_cast<unsigned char>(__value), __n);
	  return __first + __n;
	}
      else if constexpr (is_scalar_v<_Tp>)
	{
	  const auto __tmp = __value;
	  for (; __n > 0; --__n, (void)++__first)
	    *__first = __tmp;
	  return __first;
	}
      else
	{
	  for (; __n > 0; --__n, (void)++__first)
	    *__first = __value;
	  return __first;
	}
    }

  template<typename _Tp,
	   output_iterator<const _Tp&> _Out, sentinel_for<_Out> _Sent>
    constexpr _Out
    fill(_Out __first, _Sent __last, const _Tp& __value)
    {
      // TODO: implement more specializations to be at least on par with
      // std::fill
      if constexpr (sized_sentinel_for<_Sent, _Out>)
	{
	  const auto __len = __last - __first;
	  return ranges::fill_n(__first, __len, __value);
	}
      else if constexpr (is_scalar_v<_Tp>)
	{
	  const auto __tmp = __value;
	  for (; __first != __last; ++__first)
	    *__first = __tmp;
	  return __first;
	}
      else
	{
	  for (; __first != __last; ++__first)
	    *__first = __value;
	  return __first;
	}
    }

  template<typename _Tp, output_range<const _Tp&> _Range>
    constexpr safe_iterator_t<_Range>
    fill(_Range&& __r, const _Tp& __value)
    {
      return ranges::fill(ranges::begin(__r), ranges::end(__r), __value);
    }

  template<input_or_output_iterator _Out, copy_constructible _Fp>
    requires invocable<_Fp&> && writable<_Out, invoke_result_t<_Fp&>>
    constexpr _Out
    generate_n(_Out __first, iter_difference_t<_Out> __n, _Fp __gen)
    {
      for (; __n > 0; --__n, (void)++__first)
	*__first = std::__invoke(__gen);
      return __first;
    }

  template<input_or_output_iterator _Out, sentinel_for<_Out> _Sent,
	   copy_constructible _Fp>
    requires invocable<_Fp&> && writable<_Out, invoke_result_t<_Fp&>>
    constexpr _Out
    generate(_Out __first, _Sent __last, _Fp __gen)
    {
      for (; __first != __last; ++__first)
	*__first = std::__invoke(__gen);
      return __first;
    }

  template<typename _Range, copy_constructible _Fp>
    requires invocable<_Fp&> && output_range<_Range, invoke_result_t<_Fp&>>
    constexpr safe_iterator_t<_Range>
    generate(_Range&& __r, _Fp __gen)
    {
      return ranges::generate(ranges::begin(__r), ranges::end(__r),
			      std::move(__gen));
    }

  template<permutable _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr subrange<_Iter>
    remove_if(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      __first = ranges::find_if(__first, __last, __pred, __proj);
      if (__first == __last)
	return {__first, __first};

      auto __result = __first;
      ++__first;
      for (; __first != __last; ++__first)
	if (!(bool)std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  {
	    *__result = std::move(*__first);
	    ++__result;
	  }

      return {__result, __first};
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    requires permutable<iterator_t<_Range>>
    constexpr safe_subrange_t<_Range>
    remove_if(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::remove_if(ranges::begin(__r), ranges::end(__r),
			       std::move(__pred), std::move(__proj));
    }

  template<permutable _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp, typename _Proj = identity>
    requires indirect_binary_predicate<ranges::equal_to,
				       projected<_Iter, _Proj>,
				       const _Tp*>
    constexpr subrange<_Iter>
    remove(_Iter __first, _Sent __last, const _Tp& __value, _Proj __proj = {})
    {
      auto __pred = [&] (auto&& __arg) {
	return std::forward<decltype(__arg)>(__arg) == __value;
      };
      return ranges::remove_if(__first, __last,
			       std::move(__pred), std::move(__proj));
    }

  template<forward_range _Range, typename _Tp, typename _Proj = identity>
    requires permutable<iterator_t<_Range>> &&
	     indirect_binary_predicate<ranges::equal_to,
				       projected<iterator_t<_Range>, _Proj>,
				       const _Tp*>
    constexpr safe_subrange_t<_Range>
    remove(_Range&& __r, const _Tp& __value, _Proj __proj = {})
    {
      return ranges::remove(ranges::begin(__r), ranges::end(__r),
			    __value, std::move(__proj));
    }

  template<typename _Iter, typename _Out>
    using remove_copy_if_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out, typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    requires indirectly_copyable<_Iter, _Out>
    constexpr remove_copy_if_result<_Iter, _Out>
    remove_copy_if(_Iter __first, _Sent __last, _Out __result,
		   _Pred __pred, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (!(bool)std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  {
	    *__result = *__first;
	    ++__result;
	  }
      return {std::move(__first), std::move(__result)};
    }

  template<input_range _Range, weakly_incrementable _Out,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    requires indirectly_copyable<iterator_t<_Range>, _Out>
    constexpr remove_copy_if_result<safe_iterator_t<_Range>, _Out>
    remove_copy_if(_Range&& __r, _Out __result,
		   _Pred __pred, _Proj __proj = {})
    {
      return ranges::remove_copy_if(ranges::begin(__r), ranges::end(__r),
				    std::move(__result),
				    std::move(__pred), std::move(__proj));
    }

  template<typename _Iter, typename _Out>
    using remove_copy_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out, typename _Tp, typename _Proj = identity>
    requires indirectly_copyable<_Iter, _Out>
      && indirect_binary_predicate<ranges::equal_to,
				   projected<_Iter, _Proj>,
				   const _Tp*>
    constexpr remove_copy_result<_Iter, _Out>
    remove_copy(_Iter __first, _Sent __last, _Out __result,
		const _Tp& __value, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (!(std::__invoke(__proj, *__first) == __value))
	  {
	    *__result = *__first;
	    ++__result;
	  }
      return {std::move(__first), std::move(__result)};
    }

  template<input_range _Range, weakly_incrementable _Out,
	   typename _Tp, typename _Proj = identity>
    requires indirectly_copyable<iterator_t<_Range>, _Out>
      && indirect_binary_predicate<ranges::equal_to,
				   projected<iterator_t<_Range>, _Proj>,
				   const _Tp*>
    constexpr remove_copy_result<safe_iterator_t<_Range>, _Out>
    remove_copy(_Range&& __r, _Out __result,
		const _Tp& __value, _Proj __proj = {})
    {
      return ranges::remove_copy(ranges::begin(__r), ranges::end(__r),
				 std::move(__result), __value,
				 std::move(__proj));

    }

  template<permutable _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_equivalence_relation<
	     projected<_Iter, _Proj>> _Comp = ranges::equal_to>
    constexpr subrange<_Iter>
    unique(_Iter __first, _Sent __last, _Comp __comp = {}, _Proj __proj = {})
    {
      __first = ranges::adjacent_find(__first, __last, __comp, __proj);
      if (__first == __last)
	return {__first, __first};

      auto __dest = __first;
      ++__first;
      while (++__first != __last)
	if (!(bool)std::__invoke(__comp,
				 std::__invoke(__proj, *__dest),
				 std::__invoke(__proj, *__first)))
	  *++__dest = std::move(*__first);
      return {++__dest, __first};
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_equivalence_relation<
	     projected<iterator_t<_Range>, _Proj>> _Comp = ranges::equal_to>
    requires permutable<iterator_t<_Range>>
    constexpr safe_subrange_t<_Range>
    unique(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::unique(ranges::begin(__r), ranges::end(__r),
			    std::move(__comp), std::move(__proj));
    }

  template<typename _Iter, typename _Out>
    using unique_copy_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out, typename _Proj = identity,
	   indirect_equivalence_relation<
	     projected<_Iter, _Proj>> _Comp = ranges::equal_to>
    requires indirectly_copyable<_Iter, _Out>
      && (forward_iterator<_Iter>
	  || (input_iterator<_Out>
	      && same_as<iter_value_t<_Iter>, iter_value_t<_Out>>)
	  || indirectly_copyable_storable<_Iter, _Out>)
    constexpr unique_copy_result<_Iter, _Out>
    unique_copy(_Iter __first, _Sent __last, _Out __result,
		_Comp __comp = {}, _Proj __proj = {})
    {
      if (__first == __last)
	return {std::move(__first), std::move(__result)};

      // TODO: perform a closer comparison with reference implementations
      if constexpr (forward_iterator<_Iter>)
	{
	  auto __next = __first;
	  *__result = *__next;
	  while (++__next != __last)
	    if (!(bool)std::__invoke(__comp,
				     std::__invoke(__proj, *__first),
				     std::__invoke(__proj, *__next)))
	      {
		__first = __next;
		*++__result = *__first;
	      }
	  return {__next, std::move(++__result)};
	}
      else if constexpr (input_iterator<_Out>
			 && same_as<iter_value_t<_Iter>, iter_value_t<_Out>>)
	{
	  *__result = *__first;
	  while (++__first != __last)
	    if (!(bool)std::__invoke(__comp,
				     std::__invoke(__proj, *__result),
				     std::__invoke(__proj, *__first)))
		*++__result = *__first;
	  return {std::move(__first), std::move(++__result)};
	}
      else // indirectly_copyable_storable<_Iter, _Out>
	{
	  auto __value = *__first;
	  *__result = __value;
	  while (++__first != __last)
	    {
	      if (!(bool)std::__invoke(__comp,
				       std::__invoke(__proj, *__first),
				       std::__invoke(__proj, __value)))
		{
		  __value = *__first;
		  *++__result = __value;
		}
	    }
	  return {std::move(__first), std::move(++__result)};
	}
    }

  template<input_range _Range,
	   weakly_incrementable _Out, typename _Proj = identity,
	   indirect_equivalence_relation<
	     projected<iterator_t<_Range>, _Proj>> _Comp = ranges::equal_to>
    requires indirectly_copyable<iterator_t<_Range>, _Out>
      && (forward_iterator<iterator_t<_Range>>
	  || (input_iterator<_Out>
	      && same_as<range_value_t<_Range>, iter_value_t<_Out>>)
	  || indirectly_copyable_storable<iterator_t<_Range>, _Out>)
    constexpr unique_copy_result<safe_iterator_t<_Range>, _Out>
    unique_copy(_Range&& __r, _Out __result,
		_Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::unique_copy(ranges::begin(__r), ranges::end(__r),
				 std::move(__result),
				 std::move(__comp), std::move(__proj));
    }

  template<bidirectional_iterator _Iter, sentinel_for<_Iter> _Sent>
    requires permutable<_Iter>
    constexpr _Iter
    reverse(_Iter __first, _Sent __last)
    {
      auto __i = ranges::next(__first, __last);
      auto __tail = __i;

      if constexpr (random_access_iterator<_Iter>)
	{
	  if (__first != __last)
	    {
	      --__tail;
	      while (__first < __tail)
		{
		  ranges::iter_swap(__first, __tail);
		  ++__first;
		  --__tail;
		}
	    }
	  return __i;
	}
      else
	{
	  for (;;)
	    if (__first == __tail || __first == --__tail)
	      break;
	    else
	      {
		ranges::iter_swap(__first, __tail);
		++__first;
	      }
	  return __i;
	}
    }

  template<bidirectional_range _Range>
    requires permutable<iterator_t<_Range>>
    constexpr safe_iterator_t<_Range>
    reverse(_Range&& __r)
    {
      return ranges::reverse(ranges::begin(__r), ranges::end(__r));
    }

  template<typename _Iter, typename _Out>
    using reverse_copy_result = copy_result<_Iter, _Out>;

  template<bidirectional_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out>
    requires indirectly_copyable<_Iter, _Out>
    constexpr reverse_copy_result<_Iter, _Out>
    reverse_copy(_Iter __first, _Sent __last, _Out __result)
    {
      auto __i = ranges::next(__first, __last);
      auto __tail = __i;
      while (__first != __tail)
	{
	  --__tail;
	  *__result = *__tail;
	  ++__result;
	}
      return {__i, __result};
    }

  template<bidirectional_range _Range, weakly_incrementable _Out>
    requires indirectly_copyable<iterator_t<_Range>, _Out>
    constexpr reverse_copy_result<safe_iterator_t<_Range>, _Out>
    reverse_copy(_Range&& __r, _Out __result)
    {
      return ranges::reverse_copy(ranges::begin(__r), ranges::end(__r),
				  std::move(__result));
    }

  template<permutable _Iter, sentinel_for<_Iter> _Sent>
    constexpr subrange<_Iter>
    rotate(_Iter __first, _Iter __middle, _Sent __last)
    {
      auto __lasti = ranges::next(__first, __last);
      if (__first == __middle)
	return {__lasti, __lasti};
      if (__last == __middle)
	return {std::move(__first), std::move(__lasti)};

      if constexpr (random_access_iterator<_Iter>)
	{
	  auto __n = __lasti - __first;
	  auto __k = __middle - __first;

	  if (__k == __n - __k)
	    {
	      ranges::swap_ranges(__first, __middle, __middle, __middle + __k);
	      return {std::move(__middle), std::move(__lasti)};
	    }

	  auto __p = __first;
	  auto __ret = __first + (__lasti - __middle);

	  for (;;)
	    {
	      if (__k < __n - __k)
		{
		  // TODO: is_pod is deprecated, but this condition is
		  // consistent with the STL implementation.
		  if constexpr (__is_pod(iter_value_t<_Iter>))
		    if (__k == 1)
		      {
			auto __t = std::move(*__p);
			ranges::move(__p + 1, __p + __n, __p);
			*(__p + __n - 1) = std::move(__t);
			return {std::move(__ret), std::move(__lasti)};
		      }
		  auto __q = __p + __k;
		  for (decltype(__n) __i = 0; __i < __n - __k; ++ __i)
		    {
		      ranges::iter_swap(__p, __q);
		      ++__p;
		      ++__q;
		    }
		  __n %= __k;
		  if (__n == 0)
		    return {std::move(__ret), std::move(__lasti)};
		  ranges::swap(__n, __k);
		  __k = __n - __k;
		}
	      else
		{
		  __k = __n - __k;
		  // TODO: is_pod is deprecated, but this condition is
		  // consistent with the STL implementation.
		  if constexpr (__is_pod(iter_value_t<_Iter>))
		    if (__k == 1)
		      {
			auto __t = std::move(*(__p + __n - 1));
			ranges::move_backward(__p, __p + __n - 1, __p + __n);
			*__p = std::move(__t);
			return {std::move(__ret), std::move(__lasti)};
		      }
		  auto __q = __p + __n;
		  __p = __q - __k;
		  for (decltype(__n) __i = 0; __i < __n - __k; ++ __i)
		    {
		      --__p;
		      --__q;
		      ranges::iter_swap(__p, __q);
		    }
		  __n %= __k;
		  if (__n == 0)
		    return {std::move(__ret), std::move(__lasti)};
		  std::swap(__n, __k);
		}
	    }
	}
      else if constexpr (bidirectional_iterator<_Iter>)
	{
	  auto __tail = __lasti;

	  ranges::reverse(__first, __middle);
	  ranges::reverse(__middle, __tail);

	  while (__first != __middle && __middle != __tail)
	    {
	      ranges::iter_swap(__first, --__tail);
	      ++__first;
	    }

	  if (__first == __middle)
	    {
	      ranges::reverse(__middle, __tail);
	      return {std::move(__tail), std::move(__lasti)};
	    }
	  else
	    {
	      ranges::reverse(__first, __middle);
	      return {std::move(__first), std::move(__lasti)};
	    }
	}
      else
	{
	  auto __first2 = __middle;
	  do
	    {
	      ranges::iter_swap(__first, __first2);
	      ++__first;
	      ++__first2;
	      if (__first == __middle)
		__middle = __first2;
	    } while (__first2 != __last);

	  auto __ret = __first;

	  __first2 = __middle;

	  while (__first2 != __last)
	    {
	      ranges::iter_swap(__first, __first2);
	      ++__first;
	      ++__first2;
	      if (__first == __middle)
		__middle = __first2;
	      else if (__first2 == __last)
		__first2 = __middle;
	    }
	  return {std::move(__ret), std::move(__lasti)};
	}
    }

  template<forward_range _Range>
    requires permutable<iterator_t<_Range>>
    constexpr safe_subrange_t<_Range>
    rotate(_Range&& __r, iterator_t<_Range> __middle)
    {
      return ranges::rotate(ranges::begin(__r),
			    std::move(__middle),
			    ranges::end(__r));
    }

  template<typename _Iter, typename _Out>
    using rotate_copy_result = copy_result<_Iter, _Out>;

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out>
    requires indirectly_copyable<_Iter, _Out>
    constexpr rotate_copy_result<_Iter, _Out>
    rotate_copy(_Iter __first, _Iter __middle, _Sent __last, _Out __result)
    {
      auto __copy1 = ranges::copy(__middle,
				  std::move(__last),
				  std::move(__result));
      auto __copy2 = ranges::copy(std::move(__first),
				  std::move(__middle),
				  std::move(__copy1.out));
      return { std::move(__copy1.in), std::move(__copy2.out) };
    }

  template<forward_range _Range, weakly_incrementable _Out>
    requires indirectly_copyable<iterator_t<_Range>, _Out>
    constexpr rotate_copy_result<safe_iterator_t<_Range>, _Out>
    rotate_copy(_Range&& __r, iterator_t<_Range> __middle, _Out __result)
    {
      return ranges::rotate_copy(ranges::begin(__r),
				 std::move(__middle),
				 ranges::end(__r),
				 std::move(__result));
    }

#ifdef _GLIBCXX_USE_C99_STDINT_TR1
  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Gen>
    requires permutable<_Iter>
      && uniform_random_bit_generator<remove_reference_t<_Gen>>
    _Iter
    shuffle(_Iter __first, _Sent __last, _Gen&& __g)
    {
      auto __lasti = ranges::next(__first, __last);
      std::shuffle(std::move(__first), __lasti, std::forward<_Gen>(__g));
      return __lasti;
    }

  template<random_access_range _Range, typename _Gen>
    requires permutable<iterator_t<_Range>>
      && uniform_random_bit_generator<remove_reference_t<_Gen>>
    safe_iterator_t<_Range>
    shuffle(_Range&& __r, _Gen&& __g)
    {
      return ranges::shuffle(ranges::begin(__r), ranges::end(__r),
			     std::forward<_Gen>(__g));
    }
#endif

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    constexpr _Iter
    push_heap(_Iter __first, _Sent __last, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __lasti = ranges::next(__first, __last);
      std::push_heap(__first, __lasti,
		     __detail::__make_comp_proj(__comp, __proj));
      return __lasti;
    }

  template<random_access_range _Range,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    constexpr safe_iterator_t<_Range>
    push_heap(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::push_heap(ranges::begin(__r), ranges::end(__r),
			       std::move(__comp), std::move(__proj));
    }

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    constexpr _Iter
    pop_heap(_Iter __first, _Sent __last, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __lasti = ranges::next(__first, __last);
      std::pop_heap(__first, __lasti,
		    __detail::__make_comp_proj(__comp, __proj));
      return __lasti;
    }

  template<random_access_range _Range,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    constexpr safe_iterator_t<_Range>
    pop_heap(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::pop_heap(ranges::begin(__r), ranges::end(__r),
			       std::move(__comp), std::move(__proj));
    }

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    constexpr _Iter
    make_heap(_Iter __first, _Sent __last, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __lasti = ranges::next(__first, __last);
      std::make_heap(__first, __lasti,
		     __detail::__make_comp_proj(__comp, __proj));
      return __lasti;
    }

  template<random_access_range _Range,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    constexpr safe_iterator_t<_Range>
    make_heap(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::make_heap(ranges::begin(__r), ranges::end(__r),
			       std::move(__comp), std::move(__proj));
    }

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    constexpr _Iter
    sort_heap(_Iter __first, _Sent __last, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __lasti = ranges::next(__first, __last);
      std::sort_heap(__first, __lasti,
		     __detail::__make_comp_proj(__comp, __proj));
      return __lasti;
    }

  template<random_access_range _Range,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    constexpr safe_iterator_t<_Range>
    sort_heap(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::sort_heap(ranges::begin(__r), ranges::end(__r),
			       std::move(__comp), std::move(__proj));
    }

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_strict_weak_order<projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr _Iter
    is_heap_until(_Iter __first, _Sent __last,
		  _Comp __comp = {}, _Proj __proj = {})
    {
      iter_difference_t<_Iter> __n = ranges::distance(__first, __last);
      iter_difference_t<_Iter> __parent = 0, __child = 1;
      for (; __child < __n; ++__child)
	if (std::__invoke(__comp,
			  std::__invoke(__proj, *(__first + __parent)),
			  std::__invoke(__proj, *(__first + __child))))
	  return __first + __child;
	else if ((__child & 1) == 0)
	  ++__parent;

      return __first + __n;
    }

  template<random_access_range _Range,
	   typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr safe_iterator_t<_Range>
    is_heap_until(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::is_heap_until(ranges::begin(__r), ranges::end(__r),
				   std::move(__comp), std::move(__proj));
    }

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_strict_weak_order<projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr bool
    is_heap(_Iter __first, _Sent __last, _Comp __comp = {}, _Proj __proj = {})
    {
      return (__last
	      == ranges::is_heap_until(__first, __last,
				       std::move(__comp),
				       std::move(__proj)));
    }

  template<random_access_range _Range,
	   typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr bool
    is_heap(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::is_heap(ranges::begin(__r), ranges::end(__r),
			     std::move(__comp), std::move(__proj));
    }

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    constexpr _Iter
    sort(_Iter __first, _Sent __last, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __lasti = ranges::next(__first, __last);
      std::sort(std::move(__first), __lasti,
		__detail::__make_comp_proj(__comp, __proj));
      return __lasti;
    }

  template<random_access_range _Range,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    constexpr safe_iterator_t<_Range>
    sort(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::sort(ranges::begin(__r), ranges::end(__r),
			  std::move(__comp), std::move(__proj));
    }

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    _Iter
    stable_sort(_Iter __first, _Sent __last,
		_Comp __comp = {}, _Proj __proj = {})
    {
      auto __lasti = ranges::next(__first, __last);
      std::stable_sort(std::move(__first), __lasti,
		       __detail::__make_comp_proj(__comp, __proj));
      return __lasti;
    }

  template<random_access_range _Range,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    safe_iterator_t<_Range>
    stable_sort(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::stable_sort(ranges::begin(__r), ranges::end(__r),
				 std::move(__comp), std::move(__proj));
    }

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    constexpr _Iter
    partial_sort(_Iter __first, _Iter __middle, _Sent __last,
		 _Comp __comp = {}, _Proj __proj = {})
    {
      if (__first == __middle)
	return ranges::next(__first, __last);

      ranges::make_heap(__first, __middle, __comp, __proj);
      auto __i = __middle;
      for (; __i != __last; ++__i)
	if (std::__invoke(__comp,
			  std::__invoke(__proj, *__i),
			  std::__invoke(__proj, *__first)))
	  {
	    ranges::pop_heap(__first, __middle, __comp, __proj);
	    ranges::iter_swap(__middle-1, __i);
	    ranges::push_heap(__first, __middle, __comp, __proj);
	  }
      ranges::sort_heap(__first, __middle, __comp, __proj);

      return __i;
    }

  template<random_access_range _Range,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    constexpr safe_iterator_t<_Range>
    partial_sort(_Range&& __r, iterator_t<_Range> __middle,
		 _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::partial_sort(ranges::begin(__r),
				  std::move(__middle),
				  ranges::end(__r),
				  std::move(__comp), std::move(__proj));
    }

  template<typename _Iter, typename _Out>
    using partial_sort_copy_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   random_access_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_copyable<_Iter1, _Iter2>
      && sortable<_Iter2, _Comp, _Proj2>
      && indirect_strict_weak_order<_Comp,
				    projected<_Iter1, _Proj1>,
				    projected<_Iter2, _Proj2>>
    constexpr partial_sort_copy_result<_Iter1, _Iter2>
    partial_sort_copy(_Iter1 __first, _Sent1 __last,
		      _Iter2 __result_first, _Sent2 __result_last,
		      _Comp __comp = {},
		      _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      if (__result_first == __result_last)
	{
	  // TODO: Eliminating the variable __lasti triggers an ICE.
	  auto __lasti = ranges::next(std::move(__first),
				      std::move(__last));
	  return {std::move(__lasti), std::move(__result_first)};
	}

      auto __result_real_last = __result_first;
      while (__first != __last && __result_real_last != __result_last)
	{
	  *__result_real_last = *__first;
	  ++__result_real_last;
	  ++__first;
	}

      ranges::make_heap(__result_first, __result_real_last, __comp, __proj2);
      for (; __first != __last; ++__first)
	if (std::__invoke(__comp,
			  std::__invoke(__proj1, *__first),
			  std::__invoke(__proj2, *__result_first)))
	  {
	    ranges::pop_heap(__result_first, __result_real_last,
			     __comp, __proj2);
	    *(__result_real_last-1) = *__first;
	    ranges::push_heap(__result_first, __result_real_last,
			      __comp, __proj2);
	  }
      ranges::sort_heap(__result_first, __result_real_last, __comp, __proj2);

      return {std::move(__first), std::move(__result_real_last)};
    }

  template<input_range _Range1, random_access_range _Range2,
	   typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_copyable<iterator_t<_Range1>, iterator_t<_Range2>>
      && sortable<iterator_t<_Range2>, _Comp, _Proj2>
      && indirect_strict_weak_order<_Comp,
				    projected<iterator_t<_Range1>, _Proj1>,
				    projected<iterator_t<_Range2>, _Proj2>>
    constexpr partial_sort_copy_result<safe_iterator_t<_Range1>,
				       safe_iterator_t<_Range2>>
    partial_sort_copy(_Range1&& __r, _Range2&& __out, _Comp __comp = {},
		      _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::partial_sort_copy(ranges::begin(__r), ranges::end(__r),
				       ranges::begin(__out), ranges::end(__out),
				       std::move(__comp),
				       std::move(__proj1), std::move(__proj2));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_strict_weak_order<projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr _Iter
    is_sorted_until(_Iter __first, _Sent __last,
		    _Comp __comp = {}, _Proj __proj = {})
    {
      if (__first == __last)
	return __first;

      auto __next = __first;
      for (++__next; __next != __last; __first = __next, (void)++__next)
	if (std::__invoke(__comp,
			  std::__invoke(__proj, *__next),
			  std::__invoke(__proj, *__first)))
	  return __next;
      return __next;
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr safe_iterator_t<_Range>
    is_sorted_until(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::is_sorted_until(ranges::begin(__r), ranges::end(__r),
				     std::move(__comp), std::move(__proj));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_strict_weak_order<projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr bool
    is_sorted(_Iter __first, _Sent __last, _Comp __comp = {}, _Proj __proj = {})
    {
      if (__first == __last)
	return true;

      auto __next = __first;
      for (++__next; __next != __last; __first = __next, (void)++__next)
	if (std::__invoke(__comp,
			  std::__invoke(__proj, *__next),
			  std::__invoke(__proj, *__first)))
	  return false;
      return true;
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr bool
    is_sorted(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::is_sorted(ranges::begin(__r), ranges::end(__r),
			       std::move(__comp), std::move(__proj));
    }

  template<random_access_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    constexpr _Iter
    nth_element(_Iter __first, _Iter __nth, _Sent __last,
		_Comp __comp = {}, _Proj __proj = {})
    {
      auto __lasti = ranges::next(__first, __last);
      std::nth_element(std::move(__first), std::move(__nth), __lasti,
		       __detail::__make_comp_proj(__comp, __proj));
      return __lasti;
    }

  template<random_access_range _Range,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    constexpr safe_iterator_t<_Range>
    nth_element(_Range&& __r, iterator_t<_Range> __nth,
		_Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::nth_element(ranges::begin(__r), std::move(__nth),
				 ranges::end(__r),
				 std::move(__comp), std::move(__proj));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<const _Tp*, projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr _Iter
    lower_bound(_Iter __first, _Sent __last,
		const _Tp& __value, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __len = ranges::distance(__first, __last);

      while (__len > 0)
	{
	  auto __half = __len / 2;
	  auto __middle = __first;
	  ranges::advance(__middle, __half);
	  if (std::__invoke(__comp, std::__invoke(__proj, *__middle), __value))
	    {
	      __first = __middle;
	      ++__first;
	      __len = __len - __half - 1;
	    }
	  else
	    __len = __half;
	}
      return __first;
    }

  template<forward_range _Range, typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<const _Tp*,
				      projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr safe_iterator_t<_Range>
    lower_bound(_Range&& __r,
		const _Tp& __value, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::lower_bound(ranges::begin(__r), ranges::end(__r),
				 __value,
				 std::move(__comp), std::move(__proj));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<const _Tp*, projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr _Iter
    upper_bound(_Iter __first, _Sent __last,
		const _Tp& __value, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __len = ranges::distance(__first, __last);

      while (__len > 0)
	{
	  auto __half = __len / 2;
	  auto __middle = __first;
	  ranges::advance(__middle, __half);
	  if (std::__invoke(__comp, __value, std::__invoke(__proj, *__middle)))
	    __len = __half;
	  else
	    {
	      __first = __middle;
	      ++__first;
	      __len = __len - __half - 1;
	    }
	}
      return __first;
    }

  template<forward_range _Range, typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<const _Tp*,
				      projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr safe_iterator_t<_Range>
    upper_bound(_Range&& __r,
		const _Tp& __value, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::upper_bound(ranges::begin(__r), ranges::end(__r),
				 __value,
				 std::move(__comp), std::move(__proj));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<const _Tp*, projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr subrange<_Iter>
    equal_range(_Iter __first, _Sent __last,
		const _Tp& __value, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __len = ranges::distance(__first, __last);

      while (__len > 0)
	{
	  auto __half = __len / 2;
	  auto __middle = __first;
	  ranges::advance(__middle, __half);
	  if (std::__invoke(__comp,
			    std::__invoke(__proj, *__middle),
			    __value))
	    {
	      __first = __middle;
	      ++__first;
	      __len = __len - __half - 1;
	    }
	  else if (std::__invoke(__comp,
				 __value,
				 std::__invoke(__proj, *__middle)))
	    __len = __half;
	  else
	    {
	      auto __left
		= ranges::lower_bound(__first, __middle,
				      __value, __comp, __proj);
	      ranges::advance(__first, __len);
	      auto __right
		= ranges::upper_bound(++__middle, __first,
				      __value, __comp, __proj);
	      return {__left, __right};
	    }
	}
      return {__first, __first};
    }

  template<forward_range _Range,
	   typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<const _Tp*,
				      projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr safe_subrange_t<_Range>
    equal_range(_Range&& __r, const _Tp& __value,
		_Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::equal_range(ranges::begin(__r), ranges::end(__r),
				 __value,
				 std::move(__comp), std::move(__proj));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<const _Tp*, projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr bool
    binary_search(_Iter __first, _Sent __last,
		  const _Tp& __value, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __i = ranges::lower_bound(__first, __last, __value, __comp, __proj);
      if (__i == __last)
	return false;
      return !(bool)std::__invoke(__comp, __value, std::__invoke(__proj, *__i));
    }

  template<forward_range _Range,
	   typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<const _Tp*,
				      projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr bool
    binary_search(_Range&& __r, const _Tp& __value, _Comp __comp = {},
		  _Proj __proj = {})
    {
      return ranges::binary_search(ranges::begin(__r), ranges::end(__r),
				   __value,
				   std::move(__comp), std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr bool
    is_partitioned(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      __first = ranges::find_if_not(std::move(__first), __last, __pred, __proj);
      if (__first == __last)
	return true;
      ++__first;
      return ranges::none_of(std::move(__first), std::move(__last),
			     std::move(__pred), std::move(__proj));
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr bool
    is_partitioned(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::is_partitioned(ranges::begin(__r), ranges::end(__r),
				    std::move(__pred), std::move(__proj));
    }

  template<permutable _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr subrange<_Iter>
    partition(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      if constexpr (bidirectional_iterator<_Iter>)
	{
	  auto __lasti = ranges::next(__first, __last);
	  auto __tail = __lasti;
	  for (;;)
	    {
	      for (;;)
		if (__first == __tail)
		  return {std::move(__first), std::move(__lasti)};
		else if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
		  ++__first;
		else
		  break;
	      --__tail;
	      for (;;)
		if (__first == __tail)
		  return {std::move(__first), std::move(__lasti)};
		else if (!(bool)std::__invoke(__pred,
					      std::__invoke(__proj, *__tail)))
		  --__tail;
		else
		  break;
	      ranges::iter_swap(__first, __tail);
	      ++__first;
	    }
	}
      else
	{
	  if (__first == __last)
	    return {std::move(__first), std::move(__first)};

	  while (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	    if (++__first == __last)
	      return {std::move(__first), std::move(__first)};

	  auto __next = __first;
	  while (++__next != __last)
	    if (std::__invoke(__pred, std::__invoke(__proj, *__next)))
	      {
		ranges::iter_swap(__first, __next);
		++__first;
	      }

	  return {std::move(__first), std::move(__next)};
	}
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    requires permutable<iterator_t<_Range>>
    constexpr safe_subrange_t<_Range>
    partition(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::partition(ranges::begin(__r), ranges::end(__r),
			       std::move(__pred), std::move(__proj));
    }

  template<bidirectional_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    requires permutable<_Iter>
    subrange<_Iter>
    stable_partition(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      auto __lasti = ranges::next(__first, __last);
      auto __middle
	= std::stable_partition(std::move(__first), __lasti,
				__detail::__make_pred_proj(__pred, __proj));
      return {std::move(__middle), std::move(__lasti)};
    }

  template<bidirectional_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    requires permutable<iterator_t<_Range>>
    safe_subrange_t<_Range>
    stable_partition(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::stable_partition(ranges::begin(__r), ranges::end(__r),
				      std::move(__pred), std::move(__proj));
    }

  template<typename _Iter, typename _Out1, typename _O2>
    struct partition_copy_result
    {
      [[no_unique_address]] _Iter  in;
      [[no_unique_address]] _Out1 out1;
      [[no_unique_address]] _O2 out2;

      template<typename _IIter, typename _OOut1, typename _OOut2>
	requires convertible_to<const _Iter&, _IIter>
	  && convertible_to<const _Out1&, _OOut1>
	  && convertible_to<const _O2&, _OOut2>
	operator partition_copy_result<_IIter, _OOut1, _OOut2>() const &
	{ return {in, out1, out2}; }

      template<typename _IIter, typename _OOut1, typename _OOut2>
	requires convertible_to<_Iter, _IIter>
	  && convertible_to<_Out1, _OOut1>
	  && convertible_to<_O2, _OOut2>
	operator partition_copy_result<_IIter, _OOut1, _OOut2>() &&
	{ return {std::move(in), std::move(out1), std::move(out2)}; }
    };

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   weakly_incrementable _Out1, weakly_incrementable _O2,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    requires indirectly_copyable<_Iter, _Out1>
      && indirectly_copyable<_Iter, _O2>
    constexpr partition_copy_result<_Iter, _Out1, _O2>
    partition_copy(_Iter __first, _Sent __last,
		   _Out1 __out_true, _O2 __out_false,
		   _Pred __pred, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  {
	    *__out_true = *__first;
	    ++__out_true;
	  }
	else
	  {
	    *__out_false = *__first;
	    ++__out_false;
	  }

      return {std::move(__first), std::move(__out_true), std::move(__out_false)};
    }

  template<input_range _Range, weakly_incrementable _Out1,
	   weakly_incrementable _O2,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    requires indirectly_copyable<iterator_t<_Range>, _Out1>
      && indirectly_copyable<iterator_t<_Range>, _O2>
    constexpr partition_copy_result<safe_iterator_t<_Range>, _Out1, _O2>
    partition_copy(_Range&& __r, _Out1 out_true, _O2 out_false,
		   _Pred __pred, _Proj __proj = {})
    {
      return ranges::partition_copy(ranges::begin(__r), ranges::end(__r),
				    std::move(out_true), std::move(out_false),
				    std::move(__pred), std::move(__proj));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr _Iter
    partition_point(_Iter __first, _Sent __last,
		    _Pred __pred, _Proj __proj = {})
    {
      auto __len = ranges::distance(__first, __last);

      while (__len > 0)
	{
	  auto __half = __len / 2;
	  auto __middle = __first;
	  ranges::advance(__middle, __half);
	  if (std::__invoke(__pred, std::__invoke(__proj, *__middle)))
	    {
	      __first = __middle;
	      ++__first;
	      __len = __len - __half - 1;
	    }
	  else
	    __len = __half;
	}
      return __first;
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr safe_iterator_t<_Range>
    partition_point(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::partition_point(ranges::begin(__r), ranges::end(__r),
				     std::move(__pred), std::move(__proj));
    }

  template<typename _Iter1, typename _Iter2, typename _Out>
    using merge_result = binary_transform_result<_Iter1, _Iter2, _Out>;

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   weakly_incrementable _Out, typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<_Iter1, _Iter2, _Out, _Comp, _Proj1, _Proj2>
    constexpr merge_result<_Iter1, _Iter2, _Out>
    merge(_Iter1 __first1, _Sent1 __last1,
	  _Iter2 __first2, _Sent2 __last2, _Out __result,
	  _Comp __comp = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      while (__first1 != __last1 && __first2 != __last2)
	{
	  if (std::__invoke(__comp,
			    std::__invoke(__proj2, *__first2),
			    std::__invoke(__proj1, *__first1)))
	    {
	      *__result = *__first2;
	      ++__first2;
	    }
	  else
	    {
	      *__result = *__first1;
	      ++__first1;
	    }
	  ++__result;
	}
      auto __copy1 = ranges::copy(std::move(__first1), std::move(__last1),
				  std::move(__result));
      auto __copy2 = ranges::copy(std::move(__first2), std::move(__last2),
				  std::move(__copy1.out));
      return { std::move(__copy1.in), std::move(__copy2.in),
	       std::move(__copy2.out) };
    }

  template<input_range _Range1, input_range _Range2, weakly_incrementable _Out,
	   typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<iterator_t<_Range1>, iterator_t<_Range2>, _Out,
		       _Comp, _Proj1, _Proj2>
    constexpr merge_result<safe_iterator_t<_Range1>,
			   safe_iterator_t<_Range2>,
			   _Out>
    merge(_Range1&& __r1, _Range2&& __r2, _Out __result,
	  _Comp __comp = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::merge(ranges::begin(__r1), ranges::end(__r1),
			   ranges::begin(__r2), ranges::end(__r2),
			   std::move(__result), std::move(__comp),
			   std::move(__proj1), std::move(__proj2));
    }

  template<bidirectional_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less,
	   typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    _Iter
    inplace_merge(_Iter __first, _Iter __middle, _Sent __last,
		  _Comp __comp = {}, _Proj __proj = {})
    {
      auto __lasti = ranges::next(__first, __last);
      std::inplace_merge(std::move(__first), std::move(__middle), __lasti,
			 __detail::__make_comp_proj(__comp, __proj));
      return __lasti;
    }

  template<bidirectional_range _Range,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    safe_iterator_t<_Range>
    inplace_merge(_Range&& __r, iterator_t<_Range> __middle,
		  _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::inplace_merge(ranges::begin(__r), std::move(__middle),
				   ranges::end(__r),
				   std::move(__comp), std::move(__proj));
    }

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Proj1 = identity, typename _Proj2 = identity,
	   indirect_strict_weak_order<projected<_Iter1, _Proj1>,
				      projected<_Iter2, _Proj2>>
	     _Comp = ranges::less>
    constexpr bool
    includes(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	     _Comp __comp = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      while (__first1 != __last1 && __first2 != __last2)
	if (std::__invoke(__comp,
			  std::__invoke(__proj2, *__first2),
			  std::__invoke(__proj1, *__first1)))
	  return false;
	else if (std::__invoke(__comp,
			       std::__invoke(__proj1, *__first1),
			       std::__invoke(__proj2, *__first2)))
	  ++__first1;
	else
	  {
	    ++__first1;
	    ++__first2;
	  }

      return __first2 == __last2;
    }

  template<input_range _Range1, input_range _Range2, typename _Proj1 = identity,
	   typename _Proj2 = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range1>, _Proj1>,
				      projected<iterator_t<_Range2>, _Proj2>>
	     _Comp = ranges::less>
    constexpr bool
    includes(_Range1&& __r1, _Range2&& __r2, _Comp __comp = {},
	     _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::includes(ranges::begin(__r1), ranges::end(__r1),
			      ranges::begin(__r2), ranges::end(__r2),
			      std::move(__comp),
			      std::move(__proj1), std::move(__proj2));
    }

  template<typename _Iter1, typename _Iter2, typename _Out>
    using set_union_result = binary_transform_result<_Iter1, _Iter2, _Out>;

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   weakly_incrementable _Out, typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<_Iter1, _Iter2, _Out, _Comp, _Proj1, _Proj2>
    constexpr set_union_result<_Iter1, _Iter2, _Out>
    set_union(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	      _Out __result, _Comp __comp = {},
	      _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      while (__first1 != __last1 && __first2 != __last2)
	{
	  if (std::__invoke(__comp,
			    std::__invoke(__proj1, *__first1),
			    std::__invoke(__proj2, *__first2)))
	    {
	      *__result = *__first1;
	      ++__first1;
	    }
	  else if (std::__invoke(__comp,
				 std::__invoke(__proj2, *__first2),
				 std::__invoke(__proj1, *__first1)))
	    {
	      *__result = *__first2;
	      ++__first2;
	    }
	  else
	    {
	      *__result = *__first1;
	      ++__first1;
	      ++__first2;
	    }
	  ++__result;
	}
      auto __copy1 = ranges::copy(std::move(__first1), std::move(__last1),
				  std::move(__result));
      auto __copy2 = ranges::copy(std::move(__first2), std::move(__last2),
				  std::move(__copy1.out));
      return {std::move(__copy1.in), std::move(__copy2.in),
	      std::move(__copy2.out)};
    }

  template<input_range _Range1, input_range _Range2, weakly_incrementable _Out,
	   typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<iterator_t<_Range1>, iterator_t<_Range2>, _Out,
		       _Comp, _Proj1, _Proj2>
    constexpr set_union_result<safe_iterator_t<_Range1>,
			       safe_iterator_t<_Range2>, _Out>
    set_union(_Range1&& __r1, _Range2&& __r2, _Out __result, _Comp __comp = {},
	      _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::set_union(ranges::begin(__r1), ranges::end(__r1),
			       ranges::begin(__r2), ranges::end(__r2),
			       std::move(__result), std::move(__comp),
			       std::move(__proj1), std::move(__proj2));
    }

  template<typename _Iter1, typename _Iter2, typename _Out>
    using set_intersection_result = binary_transform_result<_Iter1, _Iter2, _Out>;

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   weakly_incrementable _Out, typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<_Iter1, _Iter2, _Out, _Comp, _Proj1, _Proj2>
    constexpr set_intersection_result<_Iter1, _Iter2, _Out>
    set_intersection(_Iter1 __first1, _Sent1 __last1,
		     _Iter2 __first2, _Sent2 __last2, _Out __result,
		     _Comp __comp = {},
		     _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      while (__first1 != __last1 && __first2 != __last2)
	if (std::__invoke(__comp,
			  std::__invoke(__proj1, *__first1),
			  std::__invoke(__proj2, *__first2)))
	  ++__first1;
	else if (std::__invoke(__comp,
			       std::__invoke(__proj2, *__first2),
			       std::__invoke(__proj1, *__first1)))
	  ++__first2;
	else
	  {
	    *__result = *__first1;
	    ++__first1;
	    ++__first2;
	    ++__result;
	  }
      // TODO: Eliminating these variables triggers an ICE.
      auto __last1i = ranges::next(std::move(__first1), std::move(__last1));
      auto __last2i = ranges::next(std::move(__first2), std::move(__last2));
      return {std::move(__last1i), std::move(__last2i), std::move(__result)};
    }

  template<input_range _Range1, input_range _Range2, weakly_incrementable _Out,
	   typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<iterator_t<_Range1>, iterator_t<_Range2>, _Out,
		       _Comp, _Proj1, _Proj2>
    constexpr set_intersection_result<safe_iterator_t<_Range1>,
				      safe_iterator_t<_Range2>, _Out>
    set_intersection(_Range1&& __r1, _Range2&& __r2, _Out __result,
		     _Comp __comp = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::set_intersection(ranges::begin(__r1), ranges::end(__r1),
				      ranges::begin(__r2), ranges::end(__r2),
				      std::move(__result), std::move(__comp),
				      std::move(__proj1), std::move(__proj2));
    }

  template<typename _Iter, typename _Out>
    using set_difference_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   weakly_incrementable _Out, typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<_Iter1, _Iter2, _Out, _Comp, _Proj1, _Proj2>
    constexpr set_difference_result<_Iter1, _Out>
    set_difference(_Iter1 __first1, _Sent1 __last1,
		   _Iter2 __first2, _Sent2 __last2, _Out __result,
		   _Comp __comp = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      while (__first1 != __last1 && __first2 != __last2)
	if (std::__invoke(__comp,
			  std::__invoke(__proj1, *__first1),
			  std::__invoke(__proj2, *__first2)))
	  {
	    *__result = *__first1;
	    ++__first1;
	    ++__result;
	  }
	else if (std::__invoke(__comp,
			       std::__invoke(__proj2, *__first2),
			       std::__invoke(__proj1, *__first1)))
	  ++__first2;
	else
	  {
	    ++__first1;
	    ++__first2;
	  }
      return ranges::copy(std::move(__first1), std::move(__last1),
			  std::move(__result));
    }

  template<input_range _Range1, input_range _Range2, weakly_incrementable _Out,
	   typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<iterator_t<_Range1>, iterator_t<_Range2>, _Out,
		       _Comp, _Proj1, _Proj2>
    constexpr set_difference_result<safe_iterator_t<_Range1>, _Out>
    set_difference(_Range1&& __r1, _Range2&& __r2, _Out __result,
		   _Comp __comp = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::set_difference(ranges::begin(__r1), ranges::end(__r1),
				    ranges::begin(__r2), ranges::end(__r2),
				    std::move(__result), std::move(__comp),
				    std::move(__proj1), std::move(__proj2));
    }

  template<typename _Iter1, typename _Iter2, typename _Out>
    using set_symmetric_difference_result
      = binary_transform_result<_Iter1, _Iter2, _Out>;

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   weakly_incrementable _Out, typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<_Iter1, _Iter2, _Out, _Comp, _Proj1, _Proj2>
    constexpr set_symmetric_difference_result<_Iter1, _Iter2, _Out>
    set_symmetric_difference(_Iter1 __first1, _Sent1 __last1,
			     _Iter2 __first2, _Sent2 __last2,
			     _Out __result, _Comp __comp = {},
			     _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      while (__first1 != __last1 && __first2 != __last2)
	if (std::__invoke(__comp,
			  std::__invoke(__proj1, *__first1),
			  std::__invoke(__proj2, *__first2)))
	  {
	    *__result = *__first1;
	    ++__first1;
	    ++__result;
	  }
	else if (std::__invoke(__comp,
			       std::__invoke(__proj2, *__first2),
			       std::__invoke(__proj1, *__first1)))
	  {
	    *__result = *__first2;
	    ++__first2;
	    ++__result;
	  }
	else
	  {
	    ++__first1;
	    ++__first2;
	  }
      auto __copy1 = ranges::copy(std::move(__first1), std::move(__last1),
				  std::move(__result));
      auto __copy2 = ranges::copy(std::move(__first2), std::move(__last2),
				  std::move(__copy1.out));
      return {std::move(__copy1.in), std::move(__copy2.in),
	      std::move(__copy2.out)};
    }

  template<input_range _Range1, input_range _Range2, weakly_incrementable _Out,
	   typename _Comp = ranges::less,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires mergeable<iterator_t<_Range1>, iterator_t<_Range2>, _Out,
		       _Comp, _Proj1, _Proj2>
    constexpr set_symmetric_difference_result<safe_iterator_t<_Range1>,
					      safe_iterator_t<_Range2>,
					      _Out>
    set_symmetric_difference(_Range1&& __r1, _Range2&& __r2, _Out __result,
			     _Comp __comp = {},
			     _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return (ranges::set_symmetric_difference
	      (ranges::begin(__r1), ranges::end(__r1),
	       ranges::begin(__r2), ranges::end(__r2),
	       std::move(__result), std::move(__comp),
	       std::move(__proj1), std::move(__proj2)));
    }

  template<typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<projected<const _Tp*, _Proj>>
	     _Comp = ranges::less>
    constexpr const _Tp&
    min(const _Tp& __a, const _Tp& __b, _Comp __comp = {}, _Proj __proj = {})
    {
      if (std::__invoke(std::move(__comp),
			std::__invoke(__proj, __b),
			std::__invoke(__proj, __a)))
	return __b;
      else
	return __a;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    requires indirectly_copyable_storable<iterator_t<_Range>,
					  range_value_t<_Range>*>
    constexpr range_value_t<_Range>
    min(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __first = ranges::begin(__r);
      auto __last = ranges::end(__r);
      __glibcxx_assert(__first != __last);
      auto __result = *__first;
      while (++__first != __last)
	{
	  auto __tmp = *__first;
	  if (std::__invoke(__comp,
			    std::__invoke(__proj, __tmp),
			    std::__invoke(__proj, __result)))
	    __result = std::move(__tmp);
	}
      return __result;
    }

  template<copyable _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<projected<const _Tp*, _Proj>>
	     _Comp = ranges::less>
    constexpr _Tp
    min(initializer_list<_Tp> __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::min(ranges::subrange(__r),
			 std::move(__comp), std::move(__proj));
    }

  template<typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<projected<const _Tp*, _Proj>>
	     _Comp = ranges::less>
    constexpr const _Tp&
    max(const _Tp& __a, const _Tp& __b, _Comp __comp = {}, _Proj __proj = {})
    {
      if (std::__invoke(std::move(__comp),
			std::__invoke(__proj, __a),
			std::__invoke(__proj, __b)))
	return __b;
      else
	return __a;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    requires indirectly_copyable_storable<iterator_t<_Range>,
					  range_value_t<_Range>*>
    constexpr range_value_t<_Range>
    max(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __first = ranges::begin(__r);
      auto __last = ranges::end(__r);
      __glibcxx_assert(__first != __last);
      auto __result = *__first;
      while (++__first != __last)
	{
	  auto __tmp = *__first;
	  if (std::__invoke(__comp,
			    std::__invoke(__proj, __result),
			    std::__invoke(__proj, __tmp)))
	    __result = std::move(__tmp);
	}
      return __result;
    }

  template<copyable _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<projected<const _Tp*, _Proj>>
	     _Comp = ranges::less>
    constexpr _Tp
    max(initializer_list<_Tp> __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::max(ranges::subrange(__r),
			 std::move(__comp), std::move(__proj));
    }

  template<typename _Tp>
    struct minmax_result
    {
      [[no_unique_address]] _Tp min;
      [[no_unique_address]] _Tp max;

      template<typename _Tp2>
	requires convertible_to<const _Tp&, _Tp2>
	operator minmax_result<_Tp2>() const &
	{ return {min, max}; }

      template<typename _Tp2>
	requires convertible_to<_Tp, _Tp2>
	operator minmax_result<_Tp2>() &&
	{ return {std::move(min), std::move(max)}; }
    };

  template<typename _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<projected<const _Tp*, _Proj>>
	     _Comp = ranges::less>
    constexpr minmax_result<const _Tp&>
    minmax(const _Tp& __a, const _Tp& __b, _Comp __comp = {}, _Proj __proj = {})
    {
      if (std::__invoke(std::move(__comp),
			std::__invoke(__proj, __b),
			std::__invoke(__proj, __a)))
	return {__b, __a};
      else
	return {__a, __b};
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    requires indirectly_copyable_storable<iterator_t<_Range>,
    range_value_t<_Range>*>
    constexpr minmax_result<range_value_t<_Range>>
    minmax(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      auto __first = ranges::begin(__r);
      auto __last = ranges::end(__r);
      __glibcxx_assert(__first != __last);
      minmax_result<range_value_t<_Range>> __result = {*__first, *__first};
      while (++__first != __last)
	{
	  auto __tmp = *__first;
	  if (std::__invoke(__comp,
			    std::__invoke(__proj, __tmp),
			    std::__invoke(__proj, __result.min)))
	    __result.min = std::move(__tmp);
	  if (!(bool)std::__invoke(__comp,
				   std::__invoke(__proj, __tmp),
				   std::__invoke(__proj, __result.max)))
	    __result.max = std::move(__tmp);
	}
      return __result;
    }

  template<copyable _Tp, typename _Proj = identity,
	   indirect_strict_weak_order<projected<const _Tp*, _Proj>>
	     _Comp = ranges::less>
    constexpr minmax_result<_Tp>
    minmax(initializer_list<_Tp> __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::minmax(ranges::subrange(__r),
			    std::move(__comp), std::move(__proj));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_strict_weak_order<projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr _Iter
    min_element(_Iter __first, _Sent __last,
		_Comp __comp = {}, _Proj __proj = {})
    {
      if (__first == __last)
	return __first;

      auto __i = __first;
      while (++__i != __last)
	{
	  if (std::__invoke(__comp,
			    std::__invoke(__proj, *__i),
			    std::__invoke(__proj, *__first)))
	    __first = __i;
	}
      return __first;
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr safe_iterator_t<_Range>
    min_element(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::min_element(ranges::begin(__r), ranges::end(__r),
				 std::move(__comp), std::move(__proj));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_strict_weak_order<projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr _Iter
    max_element(_Iter __first, _Sent __last,
		_Comp __comp = {}, _Proj __proj = {})
    {
      if (__first == __last)
	return __first;

      auto __i = __first;
      while (++__i != __last)
	{
	  if (std::__invoke(__comp,
			    std::__invoke(__proj, *__first),
			    std::__invoke(__proj, *__i)))
	    __first = __i;
	}
      return __first;
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr safe_iterator_t<_Range>
    max_element(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::max_element(ranges::begin(__r), ranges::end(__r),
				 std::move(__comp), std::move(__proj));
    }

  template<typename _Iter>
    using minmax_element_result = minmax_result<_Iter>;

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_strict_weak_order<projected<_Iter, _Proj>>
	     _Comp = ranges::less>
    constexpr minmax_element_result<_Iter>
    minmax_element(_Iter __first, _Sent __last,
		   _Comp __comp = {}, _Proj __proj = {})
    {
      if (__first == __last)
	return {__first, __first};

      minmax_element_result<_Iter> __result = {__first, __first};
      auto __i = __first;
      while (++__i != __last)
	{
	  if (std::__invoke(__comp,
			    std::__invoke(__proj, *__i),
			    std::__invoke(__proj, *__result.min)))
	    __result.min = __i;
	  if (!(bool)std::__invoke(__comp,
				   std::__invoke(__proj, *__i),
				   std::__invoke(__proj, *__result.max)))
	    __result.max = __i;
	}
      return __result;
    }

  template<forward_range _Range, typename _Proj = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range>, _Proj>>
	     _Comp = ranges::less>
    constexpr minmax_element_result<safe_iterator_t<_Range>>
    minmax_element(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::minmax_element(ranges::begin(__r), ranges::end(__r),
				    std::move(__comp), std::move(__proj));
    }

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Proj1, typename _Proj2,
	   indirect_strict_weak_order<projected<_Iter1, _Proj1>,
				      projected<_Iter2, _Proj2>> _Comp>
    constexpr bool
    __lexicographical_compare(_Iter1 __first1, _Sent1 __last1,
			      _Iter2 __first2, _Sent2 __last2,
			      _Comp __comp, _Proj1 __proj1, _Proj2 __proj2)
    {
      constexpr bool __sized_iters
	= (sized_sentinel_for<_Sent1, _Iter1>
	   && sized_sentinel_for<_Sent2, _Iter2>);
      if constexpr (__sized_iters)
	{
	  auto __d1 = ranges::distance(__first1, __last1);
	  auto __d2 = ranges::distance(__first2, __last2);

	  using _ValueType1 = iter_value_t<_Iter1>;
	  using _ValueType2 = iter_value_t<_Iter2>;
	  constexpr bool __use_memcmp
	    = ((is_integral_v<_ValueType1> || is_pointer_v<_ValueType1>)
	       && is_same_v<_ValueType1, _ValueType2>
	       && is_pointer_v<_Iter1>
	       && is_pointer_v<_Iter2>
	       && (is_same_v<_Comp, ranges::less>
		   || is_same_v<_Comp, ranges::greater>)
	       && is_same_v<_Proj1, identity>
	       && is_same_v<_Proj2, identity>);
	  if constexpr (__use_memcmp)
	    {
	      if (const auto __len = std::min(__d1, __d2))
		{
		  const auto __c = std::__memcmp(__first1, __first2, __len);
		  if constexpr (is_same_v<_Comp, ranges::less>)
		    {
		      if (__c < 0)
			return true;
		      if (__c > 0)
			return false;
		    }
		  else if constexpr (is_same_v<_Comp, ranges::greater>)
		    {
		      if (__c > 0)
			return true;
		      if (__c < 0)
			return false;
		    }
		  else
		    __builtin_unreachable();
		}
	      return (__last1 - __first1 < __last2 - __first2);
	    }
	}

      for (; __first1 != __last1 && __first2 != __last2;
	   ++__first1, (void) ++__first2)
	{
	  if (std::__invoke(__comp,
			    std::__invoke(__proj1, *__first1),
			    std::__invoke(__proj2, *__first2)))
	    return true;
	  if (std::__invoke(__comp,
			    std::__invoke(__proj2, *__first2),
			    std::__invoke(__proj1, *__first1)))
	    return false;
	}
      return __first1 == __last1 && __first2 != __last2;
    }

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Proj1 = identity, typename _Proj2 = identity,
	   indirect_strict_weak_order<projected<_Iter1, _Proj1>,
				      projected<_Iter2, _Proj2>>
	     _Comp = ranges::less>
    constexpr bool
    lexicographical_compare(_Iter1 __first1, _Sent1 __last1,
			    _Iter2 __first2, _Sent2 __last2,
			    _Comp __comp = {},
			    _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return (ranges::__lexicographical_compare
	      (std::__niter_base(std::move(__first1)),
	       std::__niter_base(std::move(__last1)),
	       std::__niter_base(std::move(__first2)),
	       std::__niter_base(std::move(__last2)),
	       std::move(__comp),
	       std::move(__proj1), std::move(__proj2)));
    }

  template<input_range _Range1, input_range _Range2, typename _Proj1 = identity,
	   typename _Proj2 = identity,
	   indirect_strict_weak_order<projected<iterator_t<_Range1>, _Proj1>,
				      projected<iterator_t<_Range2>, _Proj2>>
	     _Comp = ranges::less>
    constexpr bool
    lexicographical_compare(_Range1&& __r1, _Range2&& __r2, _Comp __comp = {},
			    _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return (ranges::lexicographical_compare
	      (ranges::begin(__r1), ranges::end(__r1),
	       ranges::begin(__r2), ranges::end(__r2),
	       std::move(__comp),
	       std::move(__proj1), std::move(__proj2)));
    }

  template<typename _Iter>
    struct next_permutation_result
    {
      bool found;
      _Iter in;
    };

  template<bidirectional_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    constexpr next_permutation_result<_Iter>
    next_permutation(_Iter __first, _Sent __last,
		     _Comp __comp = {}, _Proj __proj = {})
    {
      if (__first == __last)
	return {false, std::move(__first)};

      auto __i = __first;
      ++__i;
      if (__i == __last)
	return {false, std::move(__i)};

      auto __lasti = ranges::next(__first, __last);
      __i = __lasti;
      --__i;

      for (;;)
	{
	  auto __ii = __i;
	  --__i;
	  if (std::__invoke(__comp,
			    std::__invoke(__proj, *__i),
			    std::__invoke(__proj, *__ii)))
	    {
	      auto __j = __lasti;
	      while (!(bool)std::__invoke(__comp,
					  std::__invoke(__proj, *__i),
					  std::__invoke(__proj, *--__j)))
		;
	      ranges::iter_swap(__i, __j);
	      ranges::reverse(__ii, __last);
	      return {true, std::move(__lasti)};
	    }
	  if (__i == __first)
	    {
	      ranges::reverse(__first, __last);
	      return {false, std::move(__lasti)};
	    }
	}
    }

  template<bidirectional_range _Range, typename _Comp = ranges::less,
	   typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    constexpr next_permutation_result<safe_iterator_t<_Range>>
    next_permutation(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::next_permutation(ranges::begin(__r), ranges::end(__r),
				      std::move(__comp), std::move(__proj));
    }

  template<typename _Iter>
    using prev_permutation_result = next_permutation_result<_Iter>;

  template<bidirectional_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Comp = ranges::less, typename _Proj = identity>
    requires sortable<_Iter, _Comp, _Proj>
    constexpr prev_permutation_result<_Iter>
    prev_permutation(_Iter __first, _Sent __last,
		     _Comp __comp = {}, _Proj __proj = {})
    {
      if (__first == __last)
	return {false, std::move(__first)};

      auto __i = __first;
      ++__i;
      if (__i == __last)
	return {false, std::move(__i)};

      auto __lasti = ranges::next(__first, __last);
      __i = __lasti;
      --__i;

      for (;;)
	{
	  auto __ii = __i;
	  --__i;
	  if (std::__invoke(__comp,
			    std::__invoke(__proj, *__ii),
			    std::__invoke(__proj, *__i)))
	    {
	      auto __j = __lasti;
	      while (!(bool)std::__invoke(__comp,
					  std::__invoke(__proj, *--__j),
					  std::__invoke(__proj, *__i)))
		;
	      ranges::iter_swap(__i, __j);
	      ranges::reverse(__ii, __last);
	      return {true, std::move(__lasti)};
	    }
	  if (__i == __first)
	    {
	      ranges::reverse(__first, __last);
	      return {false, std::move(__lasti)};
	    }
	}
    }

  template<bidirectional_range _Range, typename _Comp = ranges::less,
	   typename _Proj = identity>
    requires sortable<iterator_t<_Range>, _Comp, _Proj>
    constexpr prev_permutation_result<safe_iterator_t<_Range>>
    prev_permutation(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
    {
      return ranges::prev_permutation(ranges::begin(__r), ranges::end(__r),
				      std::move(__comp), std::move(__proj));
    }

} // namespace ranges
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // concepts
#endif // C++20
#endif // _RANGES_ALGO_H
