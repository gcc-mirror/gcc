// Raw memory manipulators -*- C++ -*-

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

/** @file bits/ranges_uninitialized.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _RANGES_UNINITIALIZED_H
#define _RANGES_UNINITIALIZED_H 1

#if __cplusplus > 201703L
#if __cpp_lib_concepts

#include <bits/ranges_algobase.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace ranges
{
  namespace __detail
  {
    template<typename _Tp>
      constexpr void*
      __voidify(_Tp& __obj) noexcept
      {
	return const_cast<void*>
		 (static_cast<const volatile void*>(std::__addressof(__obj)));
      }

    template<typename _Iter>
      concept __nothrow_input_iterator
	= (input_iterator<_Iter>
	   && is_lvalue_reference_v<iter_reference_t<_Iter>>
	   && same_as<remove_cvref_t<iter_reference_t<_Iter>>,
		      iter_value_t<_Iter>>);

    template<typename _Sent, typename _Iter>
      concept __nothrow_sentinel = sentinel_for<_Sent, _Iter>;

    template<typename _Range>
      concept __nothrow_input_range
	= (range<_Range>
	   && __nothrow_input_iterator<iterator_t<_Range>>
	   && __nothrow_sentinel<sentinel_t<_Range>, iterator_t<_Range>>);

    template<typename _Iter>
      concept __nothrow_forward_iterator
	= (__nothrow_input_iterator<_Iter>
	   && forward_iterator<_Iter>
	   && __nothrow_sentinel<_Iter, _Iter>);

    template<typename _Range>
      concept __nothrow_forward_range
	= (__nothrow_input_range<_Range>
	   && __nothrow_forward_iterator<iterator_t<_Range>>);
  } // namespace __detail

  template<__detail::__nothrow_input_iterator _Iter,
	   __detail::__nothrow_sentinel<_Iter> _Sent>
    requires destructible<iter_value_t<_Iter>>
    constexpr _Iter
    destroy(_Iter __first, _Sent __last) noexcept;

  namespace __detail
  {
    template<typename _Iter>
      requires destructible<iter_value_t<_Iter>>
      struct _DestroyGuard
      {
      private:
	_Iter _M_first;
	const _Iter* _M_cur;

      public:
	explicit
	_DestroyGuard(const _Iter* __iter)
	  : _M_first(*__iter), _M_cur(__iter)
	{ }

	void
	release() noexcept
	{ _M_cur = nullptr; }

	~_DestroyGuard()
	{
	  if (_M_cur != nullptr)
	    ranges::destroy(std::move(_M_first), *_M_cur);
	}
      };

    template<typename _Iter>
      requires destructible<iter_value_t<_Iter>>
	&& is_trivially_destructible_v<iter_value_t<_Iter>>
      struct _DestroyGuard<_Iter>
      {
	explicit
	_DestroyGuard(const _Iter*)
	{ }

	void
	release() noexcept
	{ }
      };
  } // namespace __detail

  template<__detail::__nothrow_forward_iterator _Iter,
	   __detail::__nothrow_sentinel<_Iter> _Sent>
    requires default_initializable<iter_value_t<_Iter>>
    _Iter
    uninitialized_default_construct(_Iter __first, _Sent __last)
    {
      using _ValueType = remove_reference_t<iter_reference_t<_Iter>>;
      if constexpr (is_trivially_default_constructible_v<_ValueType>)
	return ranges::next(__first, __last);
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__first);
	  for (; __first != __last; ++__first)
	    ::new (__detail::__voidify(*__first)) _ValueType;
	  __guard.release();
	  return __first;
	}
    }

  template<__detail::__nothrow_forward_range _Range>
    requires default_initializable<range_value_t<_Range>>
    safe_iterator_t<_Range>
    uninitialized_default_construct(_Range&& __r)
    {
      return ranges::uninitialized_default_construct(ranges::begin(__r),
						     ranges::end(__r));
    }

  template<__detail::__nothrow_forward_iterator _Iter>
    requires default_initializable<iter_value_t<_Iter>>
    _Iter
    uninitialized_default_construct_n(_Iter __first,
				      iter_difference_t<_Iter> __n)
    {
      using _ValueType = remove_reference_t<iter_reference_t<_Iter>>;
      if constexpr (is_trivially_default_constructible_v<_ValueType>)
	return ranges::next(__first, __n);
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__first);
	  for (; __n > 0; ++__first, (void) --__n)
	    ::new (__detail::__voidify(*__first)) _ValueType;
	  __guard.release();
	  return __first;
	}
    }

  template<__detail::__nothrow_forward_iterator _Iter,
	   __detail::__nothrow_sentinel<_Iter> _Sent>
    requires default_initializable<iter_value_t<_Iter>>
    _Iter
    uninitialized_value_construct(_Iter __first, _Sent __last)
    {
      using _ValueType = remove_reference_t<iter_reference_t<_Iter>>;
      if constexpr (is_trivial_v<_ValueType>
		    && is_copy_assignable_v<_ValueType>)
	return ranges::fill(__first, __last, _ValueType());
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__first);
	  for (; __first != __last; ++__first)
	    ::new (__detail::__voidify(*__first)) _ValueType();
	  __guard.release();
	  return __first;
	}
    }

  template<__detail::__nothrow_forward_range _Range>
    requires default_initializable<range_value_t<_Range>>
    safe_iterator_t<_Range>
    uninitialized_value_construct(_Range&& __r)
    {
      return ranges::uninitialized_value_construct(ranges::begin(__r),
						   ranges::end(__r));
    }

  template<__detail::__nothrow_forward_iterator _Iter>
    requires default_initializable<iter_value_t<_Iter>>
    _Iter
    uninitialized_value_construct_n(_Iter __first, iter_difference_t<_Iter> __n)
    {
      using _ValueType = remove_reference_t<iter_reference_t<_Iter>>;
      if constexpr (is_trivial_v<_ValueType>
		    && is_copy_assignable_v<_ValueType>)
	return ranges::fill_n(__first, __n, _ValueType());
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__first);
	  for (; __n > 0; ++__first, (void) --__n)
	    ::new (__detail::__voidify(*__first)) _ValueType();
	  __guard.release();
	  return __first;
	}
    }

  template<typename _Iter, typename _Out>
    using uninitialized_copy_result = copy_result<_Iter, _Out>;

  template<input_iterator _Iter, sentinel_for<_Iter> _ISent,
	   __detail::__nothrow_forward_iterator _Out,
	   __detail::__nothrow_sentinel<_Out> _OSent>
    requires constructible_from<iter_value_t<_Out>, iter_reference_t<_Iter>>
    uninitialized_copy_result<_Iter, _Out>
    uninitialized_copy(_Iter __ifirst, _ISent __ilast,
		       _Out __ofirst, _OSent __olast)
    {
      using _OutType = remove_reference_t<iter_reference_t<_Out>>;
      if constexpr (sized_sentinel_for<_ISent, _Iter>
		    && sized_sentinel_for<_OSent, _Out>
		    && is_trivial_v<_OutType>
		    && is_nothrow_assignable_v<_OutType,
					       iter_reference_t<_Iter>>)
	{
	  auto __d1 = ranges::distance(__ifirst, __ilast);
	  auto __d2 = ranges::distance(__ofirst, __olast);
	  return ranges::copy_n(__ifirst, std::min(__d1, __d2), __ofirst);
	}
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__ofirst);
	  for (; __ifirst != __ilast && __ofirst != __olast;
	       ++__ofirst, (void)++__ifirst)
	    ::new (__detail::__voidify(*__ofirst)) _OutType(*__ifirst);
	  __guard.release();
	  return {__ifirst, __ofirst};
	}
    }

  template<input_range _IRange, __detail::__nothrow_forward_range _ORange>
    requires constructible_from<range_value_t<_ORange>,
				range_reference_t<_IRange>>
    uninitialized_copy_result<safe_iterator_t<_IRange>,
			      safe_iterator_t<_ORange>>
    uninitialized_copy(_IRange&& __inr, _ORange&& __outr)
    {
      return ranges::uninitialized_copy(ranges::begin(__inr),
					ranges::end(__inr),
					ranges::begin(__outr),
					ranges::end(__outr));
    }

  template<typename _Iter, typename _Out>
    using uninitialized_copy_n_result = uninitialized_copy_result<_Iter, _Out>;

    template<input_iterator _Iter, __detail::__nothrow_forward_iterator _Out,
      __detail::__nothrow_sentinel<_Out> _Sent>
    requires constructible_from<iter_value_t<_Out>, iter_reference_t<_Iter>>
    uninitialized_copy_n_result<_Iter, _Out>
    uninitialized_copy_n(_Iter __ifirst, iter_difference_t<_Iter> __n,
			 _Out __ofirst, _Sent __olast)
    {
      using _OutType = remove_reference_t<iter_reference_t<_Out>>;
      if constexpr (sized_sentinel_for<_Sent, _Out>
		    && is_trivial_v<_OutType>
		    && is_nothrow_assignable_v<_OutType,
					       iter_reference_t<_Iter>>)
	{
	  auto __d = ranges::distance(__ofirst, __olast);
	  return ranges::copy_n(__ifirst, std::min(__n, __d), __ofirst);
	}
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__ofirst);
	  for (; __n > 0 && __ofirst != __olast;
	       ++__ofirst, (void)++__ifirst, (void)--__n)
	    ::new (__detail::__voidify(*__ofirst)) _OutType(*__ifirst);
	  __guard.release();
	  return {__ifirst, __ofirst};
	}
    }

  template<typename _Iter, typename _Out>
    using uninitialized_move_result = uninitialized_copy_result<_Iter, _Out>;

  template<input_iterator _Iter, sentinel_for<_Iter> _ISent,
	   __detail::__nothrow_forward_iterator _Out,
	   __detail::__nothrow_sentinel<_Out> _OSent>
    requires constructible_from<iter_value_t<_Out>,
				iter_rvalue_reference_t<_Iter>>
    uninitialized_move_result<_Iter, _Out>
    uninitialized_move(_Iter __ifirst, _ISent __ilast,
		       _Out __ofirst, _OSent __olast)
    {
      using _OutType = remove_reference_t<iter_reference_t<_Out>>;
      if constexpr (sized_sentinel_for<_ISent, _Iter>
		    && sized_sentinel_for<_OSent, _Out>
		    && is_trivial_v<_OutType>
		    && is_nothrow_assignable_v<_OutType,
					       iter_rvalue_reference_t<_Iter>>)
	{
	  auto __d1 = ranges::distance(__ifirst, __ilast);
	  auto __d2 = ranges::distance(__ofirst, __olast);
	  return ranges::copy_n(std::make_move_iterator(__ifirst),
				std::min(__d1, __d2), __ofirst);
	}
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__ofirst);
	  for (; __ifirst != __ilast && __ofirst != __olast;
	       ++__ofirst, (void)++__ifirst)
	    ::new (__detail::__voidify(*__ofirst))
		  _OutType(ranges::iter_move(__ifirst));
	  __guard.release();
	  return {__ifirst, __ofirst};
	}
    }

  template<input_range _IRange, __detail::__nothrow_forward_range _ORange>
    requires constructible_from<range_value_t<_ORange>,
	     range_rvalue_reference_t<_IRange>>
    uninitialized_move_result<safe_iterator_t<_IRange>,
			      safe_iterator_t<_ORange>>
    uninitialized_move(_IRange&& __inr, _ORange&& __outr)
    {
      return ranges::uninitialized_move(ranges::begin(__inr),
					ranges::end(__inr),
					ranges::begin(__outr),
					ranges::end(__outr));
    }

  template<typename _Iter, typename _Out>
    using uninitialized_move_n_result = uninitialized_copy_result<_Iter, _Out>;

  template<input_iterator _Iter, __detail::__nothrow_forward_iterator _Out,
    __detail::__nothrow_sentinel<_Out> _Sent>
      requires constructible_from<iter_value_t<_Out>,
				  iter_rvalue_reference_t<_Iter>>
    uninitialized_move_n_result<_Iter, _Out>
    uninitialized_move_n(_Iter __ifirst, iter_difference_t<_Iter> __n,
			 _Out __ofirst, _Sent __olast)
    {
      using _OutType = remove_reference_t<iter_reference_t<_Out>>;
      if constexpr (sized_sentinel_for<_Sent, _Out>
		    && is_trivial_v<_OutType>
		    && is_nothrow_assignable_v<_OutType,
					       iter_rvalue_reference_t<_Iter>>)
	{
	  auto __d = ranges::distance(__ofirst, __olast);
	  return ranges::copy_n(std::make_move_iterator(__ifirst),
				std::min(__n, __d), __ofirst);
	}
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__ofirst);
	  for (; __n > 0 && __ofirst != __olast;
	       ++__ofirst, (void)++__ifirst, (void)--__n)
	    ::new (__detail::__voidify(*__ofirst))
		  _OutType(ranges::iter_move(__ifirst));
	  __guard.release();
	  return {__ifirst, __ofirst};
	}
    }

  template<__detail::__nothrow_forward_iterator _Iter,
	   __detail::__nothrow_sentinel<_Iter> _Sent, typename _Tp>
    requires constructible_from<iter_value_t<_Iter>, const _Tp&>
    _Iter
    uninitialized_fill(_Iter __first, _Sent __last, const _Tp& __x)
    {
      using _ValueType = remove_reference_t<iter_reference_t<_Iter>>;
      if constexpr (is_trivial_v<_ValueType>
		    && is_nothrow_assignable_v<_ValueType, const _Tp&>)
	return ranges::fill(__first, __last, __x);
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__first);
	  for (; __first != __last; ++__first)
	    ::new (__detail::__voidify(*__first)) _ValueType(__x);
	  __guard.release();
	  return __first;
	}
    }

  template<__detail::__nothrow_forward_range _Range, typename _Tp>
    requires constructible_from<range_value_t<_Range>, const _Tp&>
    safe_iterator_t<_Range>
    uninitialized_fill(_Range&& __r, const _Tp& __x)
    {
      return ranges::uninitialized_fill(ranges::begin(__r), ranges::end(__r),
					__x);
    }

  template<__detail::__nothrow_forward_iterator _Iter, typename _Tp>
    requires constructible_from<iter_value_t<_Iter>, const _Tp&>
    _Iter
    uninitialized_fill_n(_Iter __first, iter_difference_t<_Iter> __n,
			 const _Tp& __x)
    {
      using _ValueType = remove_reference_t<iter_reference_t<_Iter>>;
      if constexpr (is_trivial_v<_ValueType>
		    && is_nothrow_assignable_v<_ValueType, const _Tp&>)
	return ranges::fill_n(__first, __n, __x);
      else
	{
	  auto __guard = __detail::_DestroyGuard(&__first);
	  for (; __n > 0; ++__first, (void)--__n)
	    ::new (__detail::__voidify(*__first)) _ValueType(__x);
	  __guard.release();
	  return __first;
	}
    }

  template<typename _Tp, typename... _Args>
    requires requires { ::new (declval<void*>()) _Tp(declval<_Args>()...); }
    constexpr _Tp*
    construct_at(_Tp* __location, _Args&&... __args)
    {
      return ::new (__detail::__voidify(*__location))
		   _Tp(std::forward<_Args>(__args)...);
    }

  template<destructible _Tp>
    constexpr void
    destroy_at(_Tp* __location) noexcept
    {
      if constexpr (is_array_v<_Tp>)
	ranges::destroy(ranges::begin(*__location), ranges::end(*__location));
      else
	__location->~_Tp();
    }

  template<__detail::__nothrow_input_iterator _Iter,
	   __detail::__nothrow_sentinel<_Iter> _Sent>
    requires destructible<iter_value_t<_Iter>>
    constexpr _Iter
    destroy(_Iter __first, _Sent __last) noexcept
    {
      if constexpr (is_trivially_destructible_v<iter_value_t<_Iter>>)
	return ranges::next(__first, __last);
      else
	{
	  for (; __first != __last; ++__first)
	    ranges::destroy_at(std::__addressof(*__first));
	  return __first;
	}
    }

  template<__detail::__nothrow_input_range _Range>
    requires destructible<range_value_t<_Range>>
    constexpr safe_iterator_t<_Range>
    destroy(_Range&& __r) noexcept
    { return ranges::destroy(ranges::begin(__r), ranges::end(__r)); }

  template<__detail::__nothrow_input_iterator _Iter>
    requires destructible<iter_value_t<_Iter>>
    constexpr _Iter
    destroy_n(_Iter __first, iter_difference_t<_Iter> __n) noexcept
    {
      if constexpr (is_trivially_destructible_v<iter_value_t<_Iter>>)
	return ranges::next(__first, __n);
      else
	{
	  for (; __n > 0; ++__first, (void)--__n)
	    ranges::destroy_at(std::__addressof(*__first));
	  return __first;
	}
    }
}
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // concepts
#endif // C++20
#endif // _RANGES_UNINITIALIZED_H
