// Utilities for representing and manipulating ranges -*- C++ -*-

// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

/** @file bits/ranges_util.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{ranges}
 */

#ifndef _RANGES_UTIL_H
#define _RANGES_UTIL_H 1

#if __cplusplus > 201703L
# include <bits/ranges_base.h>

#ifdef __cpp_lib_ranges
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace ranges
{
  // C++20 24.5 [range.utility] Range utilities

  namespace __detail
  {
    template<typename _Range>
      concept __simple_view = view<_Range> && range<const _Range>
	&& same_as<iterator_t<_Range>, iterator_t<const _Range>>
	&& same_as<sentinel_t<_Range>, sentinel_t<const _Range>>;

    template<typename _It>
      concept __has_arrow = input_iterator<_It>
	&& (is_pointer_v<_It> || requires(_It __it) { __it.operator->(); });

    template<typename _Tp, typename _Up>
      concept __not_same_as
	= !same_as<remove_cvref_t<_Tp>, remove_cvref_t<_Up>>;
  } // namespace __detail

  /// The ranges::view_interface class template
  template<typename _Derived>
    requires is_class_v<_Derived> && same_as<_Derived, remove_cv_t<_Derived>>
    class view_interface : public view_base
    {
    private:
      constexpr _Derived& _M_derived() noexcept
      {
	static_assert(derived_from<_Derived, view_interface<_Derived>>);
	static_assert(view<_Derived>);
	return static_cast<_Derived&>(*this);
      }

      constexpr const _Derived& _M_derived() const noexcept
      {
	static_assert(derived_from<_Derived, view_interface<_Derived>>);
	static_assert(view<_Derived>);
	return static_cast<const _Derived&>(*this);
      }

    public:
      constexpr bool
      empty() requires forward_range<_Derived>
      { return ranges::begin(_M_derived()) == ranges::end(_M_derived()); }

      constexpr bool
      empty() const requires forward_range<const _Derived>
      { return ranges::begin(_M_derived()) == ranges::end(_M_derived()); }

      constexpr explicit
      operator bool() requires requires { ranges::empty(_M_derived()); }
      { return !ranges::empty(_M_derived()); }

      constexpr explicit
      operator bool() const requires requires { ranges::empty(_M_derived()); }
      { return !ranges::empty(_M_derived()); }

      constexpr auto
      data() requires contiguous_iterator<iterator_t<_Derived>>
      { return to_address(ranges::begin(_M_derived())); }

      constexpr auto
      data() const
      requires range<const _Derived>
	&& contiguous_iterator<iterator_t<const _Derived>>
      { return to_address(ranges::begin(_M_derived())); }

      constexpr auto
      size()
      requires forward_range<_Derived>
	&& sized_sentinel_for<sentinel_t<_Derived>, iterator_t<_Derived>>
      { return ranges::end(_M_derived()) - ranges::begin(_M_derived()); }

      constexpr auto
      size() const
      requires forward_range<const _Derived>
	&& sized_sentinel_for<sentinel_t<const _Derived>,
			      iterator_t<const _Derived>>
      { return ranges::end(_M_derived()) - ranges::begin(_M_derived()); }

      constexpr decltype(auto)
      front() requires forward_range<_Derived>
      {
	__glibcxx_assert(!empty());
	return *ranges::begin(_M_derived());
      }

      constexpr decltype(auto)
      front() const requires forward_range<const _Derived>
      {
	__glibcxx_assert(!empty());
	return *ranges::begin(_M_derived());
      }

      constexpr decltype(auto)
      back()
      requires bidirectional_range<_Derived> && common_range<_Derived>
      {
	__glibcxx_assert(!empty());
	return *ranges::prev(ranges::end(_M_derived()));
      }

      constexpr decltype(auto)
      back() const
      requires bidirectional_range<const _Derived>
	&& common_range<const _Derived>
      {
	__glibcxx_assert(!empty());
	return *ranges::prev(ranges::end(_M_derived()));
      }

      template<random_access_range _Range = _Derived>
	constexpr decltype(auto)
	operator[](range_difference_t<_Range> __n)
	{ return ranges::begin(_M_derived())[__n]; }

      template<random_access_range _Range = const _Derived>
	constexpr decltype(auto)
	operator[](range_difference_t<_Range> __n) const
	{ return ranges::begin(_M_derived())[__n]; }
    };

  namespace __detail
  {
    template<class _From, class _To>
      concept __convertible_to_non_slicing = convertible_to<_From, _To>
	&& !(is_pointer_v<decay_t<_From>> && is_pointer_v<decay_t<_To>>
	    && __not_same_as<remove_pointer_t<decay_t<_From>>,
			     remove_pointer_t<decay_t<_To>>>);

    template<typename _Tp>
      concept __pair_like
	= !is_reference_v<_Tp> && requires(_Tp __t)
	{
	  typename tuple_size<_Tp>::type;
	  requires derived_from<tuple_size<_Tp>, integral_constant<size_t, 2>>;
	  typename tuple_element_t<0, remove_const_t<_Tp>>;
	  typename tuple_element_t<1, remove_const_t<_Tp>>;
	  { get<0>(__t) } -> convertible_to<const tuple_element_t<0, _Tp>&>;
	  { get<1>(__t) } -> convertible_to<const tuple_element_t<1, _Tp>&>;
	};

    template<typename _Tp, typename _Up, typename _Vp>
      concept __pair_like_convertible_from
	= !range<_Tp> && __pair_like<_Tp>
	&& constructible_from<_Tp, _Up, _Vp>
	&& __convertible_to_non_slicing<_Up, tuple_element_t<0, _Tp>>
	&& convertible_to<_Vp, tuple_element_t<1, _Tp>>;

  } // namespace __detail

  enum class subrange_kind : bool { unsized, sized };

  /// The ranges::subrange class template
  template<input_or_output_iterator _It, sentinel_for<_It> _Sent = _It,
	   subrange_kind _Kind = sized_sentinel_for<_Sent, _It>
	     ? subrange_kind::sized : subrange_kind::unsized>
    requires (_Kind == subrange_kind::sized || !sized_sentinel_for<_Sent, _It>)
    class subrange : public view_interface<subrange<_It, _Sent, _Kind>>
    {
    private:
      // XXX: gcc complains when using constexpr here
      static const bool _S_store_size
	= _Kind == subrange_kind::sized && !sized_sentinel_for<_Sent, _It>;

      _It _M_begin = _It();
      [[no_unique_address]] _Sent _M_end = _Sent();

      using __size_type
	= __detail::__make_unsigned_like_t<iter_difference_t<_It>>;

      template<typename, bool = _S_store_size>
	struct _Size
	{ };

      template<typename _Tp>
	struct _Size<_Tp, true>
	{ _Tp _M_size; };

      [[no_unique_address]] _Size<__size_type> _M_size = {};

    public:
      subrange() = default;

      constexpr
      subrange(__detail::__convertible_to_non_slicing<_It> auto __i, _Sent __s)
	requires (!_S_store_size)
      : _M_begin(std::move(__i)), _M_end(__s)
      { }

      constexpr
      subrange(__detail::__convertible_to_non_slicing<_It> auto __i, _Sent __s,
	       __size_type __n)
	requires (_Kind == subrange_kind::sized)
      : _M_begin(std::move(__i)), _M_end(__s)
      {
	if constexpr (_S_store_size)
	  _M_size._M_size = __n;
      }

      template<__detail::__not_same_as<subrange> _Rng>
	requires borrowed_range<_Rng>
	  && __detail::__convertible_to_non_slicing<iterator_t<_Rng>, _It>
	  && convertible_to<sentinel_t<_Rng>, _Sent>
	constexpr
	subrange(_Rng&& __r) requires _S_store_size && sized_range<_Rng>
	: subrange(__r, ranges::size(__r))
	{ }

      template<__detail::__not_same_as<subrange> _Rng>
	requires borrowed_range<_Rng>
	  && __detail::__convertible_to_non_slicing<iterator_t<_Rng>, _It>
	  && convertible_to<sentinel_t<_Rng>, _Sent>
	constexpr
	subrange(_Rng&& __r) requires (!_S_store_size)
	: subrange(ranges::begin(__r), ranges::end(__r))
	{ }

      template<borrowed_range _Rng>
	requires __detail::__convertible_to_non_slicing<iterator_t<_Rng>, _It>
	  && convertible_to<sentinel_t<_Rng>, _Sent>
	constexpr
	subrange(_Rng&& __r, __size_type __n)
	requires (_Kind == subrange_kind::sized)
	: subrange{ranges::begin(__r), ranges::end(__r), __n}
	{ }

      template<__detail::__not_same_as<subrange> _PairLike>
	requires __detail::__pair_like_convertible_from<_PairLike, const _It&,
							const _Sent&>
	constexpr
	operator _PairLike() const
	{ return _PairLike(_M_begin, _M_end); }

      constexpr _It
      begin() const requires copyable<_It>
      { return _M_begin; }

      [[nodiscard]] constexpr _It
      begin() requires (!copyable<_It>)
      { return std::move(_M_begin); }

      constexpr _Sent end() const { return _M_end; }

      constexpr bool empty() const { return _M_begin == _M_end; }

      constexpr __size_type
      size() const requires (_Kind == subrange_kind::sized)
      {
	if constexpr (_S_store_size)
	  return _M_size._M_size;
	else
	  return __detail::__to_unsigned_like(_M_end - _M_begin);
      }

      [[nodiscard]] constexpr subrange
      next(iter_difference_t<_It> __n = 1) const &
	requires forward_iterator<_It>
      {
	auto __tmp = *this;
	__tmp.advance(__n);
	return __tmp;
      }

      [[nodiscard]] constexpr subrange
      next(iter_difference_t<_It> __n = 1) &&
      {
	advance(__n);
	return std::move(*this);
      }

      [[nodiscard]] constexpr subrange
      prev(iter_difference_t<_It> __n = 1) const
	requires bidirectional_iterator<_It>
      {
	auto __tmp = *this;
	__tmp.advance(-__n);
	return __tmp;
      }

      constexpr subrange&
      advance(iter_difference_t<_It> __n)
      {
	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 3433. subrange::advance(n) has UB when n < 0
	if constexpr (bidirectional_iterator<_It>)
	  if (__n < 0)
	    {
	      ranges::advance(_M_begin, __n);
	      if constexpr (_S_store_size)
		_M_size._M_size += __detail::__to_unsigned_like(-__n);
	      return *this;
	    }

	__glibcxx_assert(__n >= 0);
	auto __d = __n - ranges::advance(_M_begin, __n, _M_end);
	if constexpr (_S_store_size)
	  _M_size._M_size -= __detail::__to_unsigned_like(__d);
	return *this;
      }
    };

  template<input_or_output_iterator _It, sentinel_for<_It> _Sent>
    subrange(_It, _Sent) -> subrange<_It, _Sent>;

  template<input_or_output_iterator _It, sentinel_for<_It> _Sent>
    subrange(_It, _Sent,
	     __detail::__make_unsigned_like_t<iter_difference_t<_It>>)
      -> subrange<_It, _Sent, subrange_kind::sized>;

  template<borrowed_range _Rng>
    subrange(_Rng&&)
      -> subrange<iterator_t<_Rng>, sentinel_t<_Rng>,
		 (sized_range<_Rng>
		  || sized_sentinel_for<sentinel_t<_Rng>, iterator_t<_Rng>>)
		 ? subrange_kind::sized : subrange_kind::unsized>;

  template<borrowed_range _Rng>
    subrange(_Rng&&,
	     __detail::__make_unsigned_like_t<range_difference_t<_Rng>>)
      -> subrange<iterator_t<_Rng>, sentinel_t<_Rng>, subrange_kind::sized>;

  template<size_t _Num, class _It, class _Sent, subrange_kind _Kind>
    requires (_Num < 2)
    constexpr auto
    get(const subrange<_It, _Sent, _Kind>& __r)
    {
      if constexpr (_Num == 0)
	return __r.begin();
      else
	return __r.end();
    }

  template<size_t _Num, class _It, class _Sent, subrange_kind _Kind>
    requires (_Num < 2)
    constexpr auto
    get(subrange<_It, _Sent, _Kind>&& __r)
    {
      if constexpr (_Num == 0)
	return __r.begin();
      else
	return __r.end();
    }

  template<input_or_output_iterator _It, sentinel_for<_It> _Sent,
	   subrange_kind _Kind>
    inline constexpr bool
      enable_borrowed_range<subrange<_It, _Sent, _Kind>> = true;

  template<range _Range>
    using borrowed_subrange_t = conditional_t<borrowed_range<_Range>,
					      subrange<iterator_t<_Range>>,
					      dangling>;

} // namespace ranges

  using ranges::get;

  template<typename _Iter, typename _Sent, ranges::subrange_kind _Kind>
    struct tuple_size<ranges::subrange<_Iter, _Sent, _Kind>>
    : integral_constant<size_t, 2>
    { };

  template<typename _Iter, typename _Sent, ranges::subrange_kind _Kind>
    struct tuple_element<0, ranges::subrange<_Iter, _Sent, _Kind>>
    { using type = _Iter; };

  template<typename _Iter, typename _Sent, ranges::subrange_kind _Kind>
    struct tuple_element<1, ranges::subrange<_Iter, _Sent, _Kind>>
    { using type = _Sent; };

  template<typename _Iter, typename _Sent, ranges::subrange_kind _Kind>
    struct tuple_element<0, const ranges::subrange<_Iter, _Sent, _Kind>>
    { using type = _Iter; };

  template<typename _Iter, typename _Sent, ranges::subrange_kind _Kind>
    struct tuple_element<1, const ranges::subrange<_Iter, _Sent, _Kind>>
    { using type = _Sent; };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // library concepts
#endif // C++20
#endif // _RANGES_UTIL_H
