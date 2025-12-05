// Implementation of std::move_only_function, std::copyable_function
// and std::function_ref  -*- C++ -*-

// Copyright The GNU Toolchain Authors.
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

/** @file include/bits/binder.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{functional}
 */

#ifndef _GLIBCXX_BINDERS_H
#define _GLIBCXX_BINDERS_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#if __cplusplus >= 202002L

#include <bits/invoke.h>
#include <bits/move.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<size_t, typename _Tp>
    struct _Indexed_bound_arg
    {
      [[no_unique_address]] _Tp _M_val;
    };

  template<typename... _IndexedArgs>
    struct _Bound_arg_storage : _IndexedArgs...
    {
      template<bool _Back, typename _Fd, typename _Self, typename... _CallArgs>
	static constexpr
	decltype(auto)
	_S_apply(_Fd&& __fd, _Self&& __self, _CallArgs&&... __call_args)
	{
	  if constexpr (_Back)
	    return std::__invoke(std::forward<_Fd>(__fd),
				 std::forward<_CallArgs>(__call_args)...,
				 __like_t<_Self, _IndexedArgs>(__self)._M_val...);
	  else
	    return std::__invoke(std::forward<_Fd>(__fd),
				 __like_t<_Self, _IndexedArgs>(__self)._M_val...,
				 std::forward<_CallArgs>(__call_args)...);
	}
    };

  template<typename... _BoundArgs, typename... _Args>
    constexpr auto
    __make_bound_args(_Args&&... __args)
    {
      if constexpr (sizeof...(_BoundArgs) == 1)
	// pack has one element, so return copy of arg
	return (_BoundArgs(std::forward<_Args>(__args)), ...);
      else
	{
	  auto __impl = [&]<size_t... _Inds>(index_sequence<_Inds...>)
	  {
	    return _Bound_arg_storage<_Indexed_bound_arg<_Inds, _BoundArgs>...>
		   { {_BoundArgs(std::forward<_Args>(__args))}... };
	  };
	  return __impl(index_sequence_for<_BoundArgs...>());
	}
    }

  template<bool _Back, typename _Fd, typename... _BoundArgs>
    class _Binder
    {
      template<typename _Self, typename... _CallArgs>
	using _Result_t = __conditional_t<
	  _Back,
	  invoke_result<__like_t<_Self, _Fd>,
			_CallArgs..., __like_t<_Self, _BoundArgs>...>,
	  invoke_result<__like_t<_Self, _Fd>,
			__like_t<_Self, _BoundArgs>..., _CallArgs...>>::type;

      template<typename _Self, typename... _CallArgs>
	static consteval bool
	_S_noexcept_invocable()
	{
	  if constexpr (_Back)
	    return is_nothrow_invocable_v< __like_t<_Self, _Fd>,
		     _CallArgs..., __like_t<_Self, _BoundArgs>...>;
	  else
	    return is_nothrow_invocable_v<__like_t<_Self, _Fd>,
		     __like_t<_Self, _BoundArgs>..., _CallArgs...>;
	}

    public:
      static_assert(is_move_constructible_v<_Fd>);
      static_assert((is_move_constructible_v<_BoundArgs> && ...));

      // First parameter is to ensure this constructor is never used
      // instead of the copy/move constructor.
      template<typename _Fn, typename... _Args>
	explicit constexpr
	_Binder(int, _Fn&& __fn, _Args&&... __args)
	noexcept(__and_<is_nothrow_constructible<_Fd, _Fn>,
			is_nothrow_constructible<_BoundArgs, _Args>...>::value)
	: _M_fd(std::forward<_Fn>(__fn)),
	  _M_bound_args(__make_bound_args<_BoundArgs...>(std::forward<_Args>(__args)...))
	{ static_assert(sizeof...(_Args) == sizeof...(_BoundArgs)); }

#if _GLIBCXX_EXPLICIT_THIS_PARAMETER
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wc++23-extensions" // deducing this
      template<typename _Self, typename... _CallArgs>
	constexpr _Result_t<_Self, _CallArgs...>
	operator()(this _Self&& __self, _CallArgs&&... __call_args)
	noexcept(_S_noexcept_invocable<_Self, _CallArgs...>())
	{
	  return _S_call(__like_t<_Self, _Binder>(__self),
			 std::forward<_CallArgs>(__call_args)...);
	}
# pragma GCC diagnostic pop
#else
      template<typename... _CallArgs>
	requires true
	constexpr _Result_t<_Binder&, _CallArgs...>
	operator()(_CallArgs&&... __call_args) &
	noexcept(_S_noexcept_invocable<_Binder&, _CallArgs...>())
	{
	  return _S_call(*this, std::forward<_CallArgs>(__call_args)...);
	}

      template<typename... _CallArgs>
	requires true
	constexpr _Result_t<const _Binder&, _CallArgs...>
	operator()(_CallArgs&&... __call_args) const &
	noexcept(_S_noexcept_invocable<const _Binder&, _CallArgs...>())
	{
	  return _S_call(*this, std::forward<_CallArgs>(__call_args)...);
	}

      template<typename... _CallArgs>
	requires true
	constexpr _Result_t<_Binder&&, _CallArgs...>
	operator()(_CallArgs&&... __call_args) &&
	noexcept(_S_noexcept_invocable<_Binder&&, _CallArgs...>())
	{
	  return _S_call(std::move(*this),
			 std::forward<_CallArgs>(__call_args)...);
	}

      template<typename... _CallArgs>
	requires true
	constexpr _Result_t<const _Binder&&, _CallArgs...>
	operator()(_CallArgs&&... __call_args) const &&
	noexcept(_S_noexcept_invocable<const _Binder&&, _CallArgs...>())
	{
	  return _S_call(std::move(*this),
			 std::forward<_CallArgs>(__call_args)...);
	}

      template<typename... _CallArgs>
	void operator()(_CallArgs&&...) & = delete;

      template<typename... _CallArgs>
	void operator()(_CallArgs&&...) const & = delete;

      template<typename... _CallArgs>
	void operator()(_CallArgs&&...) && = delete;

      template<typename... _CallArgs>
	void operator()(_CallArgs&&...) const && = delete;
#endif

      template<typename _Tp, typename... _CallArgs>
	static constexpr
	decltype(auto)
	_S_call(_Tp&& __g, _CallArgs&&... __call_args)
	{
	  if constexpr (sizeof...(_BoundArgs) > 1)
	    return _BoundArgsStorage::template _S_apply<_Back>(
		      std::forward<_Tp>(__g)._M_fd,
		      std::forward<_Tp>(__g)._M_bound_args,
		      std::forward<_CallArgs>(__call_args)...);
	  else if constexpr (sizeof...(_BoundArgs) == 0)
	    return std::__invoke(std::forward<_Tp>(__g)._M_fd,
				 std::forward<_CallArgs>(__call_args)...);
	  else if constexpr (_Back) // sizeof...(_BoundArgs) == 1
	    return std::__invoke(std::forward<_Tp>(__g)._M_fd,
				 std::forward<_CallArgs>(__call_args)...,
				 std::forward<_Tp>(__g)._M_bound_args);
	  else // !_Back && sizeof...(_BoundArgs) == 1
	    return std::__invoke(std::forward<_Tp>(__g)._M_fd,
				 std::forward<_Tp>(__g)._M_bound_args,
				 std::forward<_CallArgs>(__call_args)...);
	}

    private:
      using _BoundArgsStorage
	// _BoundArgs are required to be move-constructible, so this is valid.
	= decltype(__make_bound_args<_BoundArgs...>(std::declval<_BoundArgs>()...));

      [[no_unique_address]] _Fd _M_fd;
      [[no_unique_address]] _BoundArgsStorage _M_bound_args;
    };

  template<typename _Fn, typename... _Args>
    using _Bind_front_t = _Binder<false, decay_t<_Fn>, decay_t<_Args>...>;

  // for zero bounds args behavior of bind_front and bind_back is the same,
  // so reuse _Bind_front_t, i.e. _Binder<false, ...>
  template<typename _Fn, typename... _Args>
    using _Bind_back_t
      = _Binder<(sizeof...(_Args) > 0), decay_t<_Fn>, decay_t<_Args>...>;

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // __cplusplus >= 202002L
#endif // _GLIBCXX_BINDERS_H
