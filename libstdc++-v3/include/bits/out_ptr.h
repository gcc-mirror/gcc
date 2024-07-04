// Smart pointer adaptors -*- C++ -*-

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

/** @file include/bits/out_ptr.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _GLIBCXX_OUT_PTR_H
#define _GLIBCXX_OUT_PTR_H 1

#pragma GCC system_header

#include <bits/version.h>

#ifdef __glibcxx_out_ptr // C++ >= 23

#include <tuple>
#include <bits/ptr_traits.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /// Smart pointer adaptor for functions taking an output pointer parameter.
  /**
   * @tparam _Smart The type of pointer to adapt.
   * @tparam _Pointer The type of pointer to convert to.
   * @tparam _Args... Argument types used when resetting the smart pointer.
   * @since C++23
   * @headerfile <memory>
   */
  template<typename _Smart, typename _Pointer, typename... _Args>
    class out_ptr_t
    {
#if _GLIBCXX_HOSTED
      static_assert(!__is_shared_ptr<_Smart> || sizeof...(_Args) != 0,
		    "a deleter must be used when adapting std::shared_ptr "
		    "with std::out_ptr");
#endif

    public:
      explicit
      out_ptr_t(_Smart& __smart, _Args... __args)
      : _M_impl{__smart, std::forward<_Args>(__args)...}
      {
	if constexpr (requires { _M_impl._M_out_init(); })
	  _M_impl._M_out_init();
      }

      out_ptr_t(const out_ptr_t&) = delete;

      ~out_ptr_t() = default;

      operator _Pointer*() const noexcept
      { return _M_impl._M_get(); }

      operator void**() const noexcept requires (!same_as<_Pointer, void*>)
      {
	static_assert(is_pointer_v<_Pointer>);
	_Pointer* __p = *this;
	return static_cast<void**>(static_cast<void*>(__p));
      }

    private:
      // TODO: Move this to namespace scope? e.g. __detail::_Ptr_adapt_impl
      template<typename, typename, typename...>
	struct _Impl
	{
	  // This constructor must not modify __s because out_ptr_t and
	  // inout_ptr_t want to do different things. After construction
	  // they call _M_out_init() or _M_inout_init() respectively.
	  _Impl(_Smart& __s, _Args&&... __args)
	  : _M_smart(__s), _M_args(std::forward<_Args>(__args)...)
	  { }

	  // Called by out_ptr_t to clear the smart pointer before using it.
	  void
	  _M_out_init()
	  {
	    // _GLIBCXX_RESOLVE_LIB_DEFECTS
	    // 3734. Inconsistency in inout_ptr and out_ptr for empty case
	    if constexpr (requires { _M_smart.reset(); })
	      _M_smart.reset();
	    else
	      _M_smart = _Smart();
	  }

	  // Called by inout_ptr_t to copy the smart pointer's value
	  // to the pointer that is returned from _M_get().
	  void
	  _M_inout_init()
	  { _M_ptr = _M_smart.release(); }

	  // The pointer value returned by operator Pointer*().
	  _Pointer*
	  _M_get() const
	  { return __builtin_addressof(const_cast<_Pointer&>(_M_ptr)); }

	  // Finalize the effects on the smart pointer.
	  ~_Impl() noexcept(false);

	  _Smart& _M_smart;
	  [[no_unique_address]] _Pointer _M_ptr{};
	  [[no_unique_address]] tuple<_Args...> _M_args;
	};

      // Partial specialization for raw pointers.
      template<typename _Tp>
	struct _Impl<_Tp*, _Tp*>
	{
	  void
	  _M_out_init()
	  { _M_p = nullptr; }

	  void
	  _M_inout_init()
	  { }

	  _Tp**
	  _M_get() const
	  { return __builtin_addressof(const_cast<_Tp*&>(_M_p)); }

	  _Tp*& _M_p;
	};

      // Partial specialization for raw pointers, with conversion.
      template<typename _Tp, typename _Ptr> requires (!is_same_v<_Ptr, _Tp*>)
	struct _Impl<_Tp*, _Ptr>
	{
	  explicit
	  _Impl(_Tp*& __p)
	  : _M_p(__p)
	  { }

	  void
	  _M_out_init()
	  { _M_p = nullptr; }

	  void
	  _M_inout_init()
	  { _M_ptr = _M_p; }

	  _Pointer*
	  _M_get() const
	  { return __builtin_addressof(const_cast<_Pointer&>(_M_ptr)); }

	  ~_Impl() { _M_p = static_cast<_Tp*>(_M_ptr); }

	  _Tp*& _M_p;
	  _Pointer _M_ptr{};
	};

      // Partial specialization for std::unique_ptr.
      // This specialization gives direct access to the private member
      // of the unique_ptr, avoiding the overhead of storing a separate
      // pointer and then resetting the unique_ptr in the destructor.
      // FIXME: constrain to only match the primary template,
      // not program-defined specializations of unique_ptr.
      template<typename _Tp, typename _Del>
	struct _Impl<unique_ptr<_Tp, _Del>,
		     typename unique_ptr<_Tp, _Del>::pointer>
	{
	  void
	  _M_out_init()
	  { _M_smart.reset(); }

	  _Pointer*
	  _M_get() const noexcept
	  { return __builtin_addressof(_M_smart._M_t._M_ptr()); }

	  _Smart& _M_smart;
	};

      // Partial specialization for std::unique_ptr with replacement deleter.
      // FIXME: constrain to only match the primary template,
      // not program-defined specializations of unique_ptr.
      template<typename _Tp, typename _Del, typename _Del2>
	struct _Impl<unique_ptr<_Tp, _Del>,
		     typename unique_ptr<_Tp, _Del>::pointer, _Del2>
	{
	  void
	  _M_out_init()
	  { _M_smart.reset(); }

	  _Pointer*
	  _M_get() const noexcept
	  { return __builtin_addressof(_M_smart._M_t._M_ptr()); }

	  ~_Impl()
	  {
	    if (_M_smart.get())
	      _M_smart._M_t._M_deleter() = std::forward<_Del2>(_M_del);
	  }

	  _Smart& _M_smart;
	  [[no_unique_address]] _Del2 _M_del;
	};

#if _GLIBCXX_HOSTED
      // Partial specialization for std::shared_ptr.
      // This specialization gives direct access to the private member
      // of the shared_ptr, avoiding the overhead of storing a separate
      // pointer and then resetting the shared_ptr in the destructor.
      // A new control block is allocated in the constructor, so that if
      // allocation fails it doesn't throw an exception from the destructor.
      template<typename _Tp, typename _Del, typename _Alloc>
	requires (is_base_of_v<__shared_ptr<_Tp>, shared_ptr<_Tp>>)
	struct _Impl<shared_ptr<_Tp>,
		     typename shared_ptr<_Tp>::element_type*, _Del, _Alloc>
	{
	  _Impl(_Smart& __s, _Del __d, _Alloc __a = _Alloc())
	  : _M_smart(__s)
	  {
	    // We know shared_ptr cannot be used with inout_ptr_t
	    // so we can do all set up here, instead of in _M_out_init().
	    _M_smart.reset();

	    // Similar to the shared_ptr(Y*, D, A) constructor, except that if
	    // the allocation throws we do not need (or want) to call deleter.
	    typename _Scd::__allocator_type __a2(__a);
	    auto __mem = __a2.allocate(1);
	    ::new (__mem) _Scd(nullptr, std::forward<_Del>(__d),
			       std::forward<_Alloc>(__a));
	    _M_smart._M_refcount._M_pi = __mem;
	  }

	  _Pointer*
	  _M_get() const noexcept
	  { return __builtin_addressof(_M_smart._M_ptr); }

	  ~_Impl()
	  {
	    auto& __pi = _M_smart._M_refcount._M_pi;

	    if (_Sp __ptr = _M_smart.get())
	      static_cast<_Scd*>(__pi)->_M_impl._M_ptr = __ptr;
	    else // Destroy the control block manually without invoking deleter.
	      std::__exchange(__pi, nullptr)->_M_destroy();
	  }

	  _Smart& _M_smart;

	  using _Sp = typename _Smart::element_type*;
	  using _Scd = _Sp_counted_deleter<_Sp, decay_t<_Del>,
					   remove_cvref_t<_Alloc>,
					   __default_lock_policy>;
	};

      // Partial specialization for std::shared_ptr, without custom allocator.
      template<typename _Tp, typename _Del>
	requires (is_base_of_v<__shared_ptr<_Tp>, shared_ptr<_Tp>>)
	struct _Impl<shared_ptr<_Tp>,
		     typename shared_ptr<_Tp>::element_type*, _Del>
	: _Impl<_Smart, _Pointer, _Del, allocator<void>>
	{
	  using _Impl<_Smart, _Pointer, _Del, allocator<void>>::_Impl;
	};
#endif

      using _Impl_t = _Impl<_Smart, _Pointer, _Args...>;

      _Impl_t _M_impl;

      template<typename, typename, typename...> friend class inout_ptr_t;
    };

  /// Smart pointer adaptor for functions taking an inout pointer parameter.
  /**
   * @tparam _Smart The type of pointer to adapt.
   * @tparam _Pointer The type of pointer to convert to.
   * @tparam _Args... Argument types used when resetting the smart pointer.
   * @since C++23
   * @headerfile <memory>
   */
  template<typename _Smart, typename _Pointer, typename... _Args>
    class inout_ptr_t
    {
#if _GLIBCXX_HOSTED
      static_assert(!__is_shared_ptr<_Smart>,
		    "std::inout_ptr can not be used to wrap std::shared_ptr");
#endif

    public:
      explicit
      inout_ptr_t(_Smart& __smart, _Args... __args)
      : _M_impl{__smart, std::forward<_Args>(__args)...}
      {
	if constexpr (requires { _M_impl._M_inout_init(); })
	  _M_impl._M_inout_init();
      }

      inout_ptr_t(const inout_ptr_t&) = delete;

      ~inout_ptr_t() = default;

      operator _Pointer*() const noexcept
      { return _M_impl._M_get(); }

      operator void**() const noexcept requires (!same_as<_Pointer, void*>)
      {
	static_assert(is_pointer_v<_Pointer>);
	_Pointer* __p = *this;
	return static_cast<void**>(static_cast<void*>(__p));
      }

    private:
#if _GLIBCXX_HOSTED
      // Avoid an invalid instantiation of out_ptr_t<shared_ptr<T>, ...>
      using _Out_ptr_t
	= __conditional_t<__is_shared_ptr<_Smart>,
			  out_ptr_t<void*, void*>,
			  out_ptr_t<_Smart, _Pointer, _Args...>>;
#else
      using _Out_ptr_t = out_ptr_t<_Smart, _Pointer, _Args...>;
#endif
      using _Impl_t = typename _Out_ptr_t::_Impl_t;
      _Impl_t _M_impl;
    };

/// @cond undocumented
namespace __detail
{
  // POINTER_OF metafunction
  template<typename _Tp>
    consteval auto
    __pointer_of()
    {
      if constexpr (requires { typename _Tp::pointer; })
	return type_identity<typename _Tp::pointer>{};
      else if constexpr (requires { typename _Tp::element_type; })
	return type_identity<typename _Tp::element_type*>{};
      else
	{
	  using _Traits = pointer_traits<_Tp>;
	  if constexpr (requires { typename _Traits::element_type; })
	    return type_identity<typename _Traits::element_type*>{};
	}
      // else POINTER_OF(S) is not a valid type, return void.
    }

  // POINTER_OF_OR metafunction
  template<typename _Smart, typename _Ptr>
    consteval auto
    __pointer_of_or()
    {
      using _TypeId = decltype(__detail::__pointer_of<_Smart>());
      if constexpr (is_void_v<_TypeId>)
	return type_identity<_Ptr>{};
      else
	return _TypeId{};
    }

  // Returns Pointer if !is_void_v<Pointer>, otherwise POINTER_OF(Smart).
  template<typename _Ptr, typename _Smart>
    consteval auto
    __choose_ptr()
    {
      if constexpr (!is_void_v<_Ptr>)
	return type_identity<_Ptr>{};
      else
	return __detail::__pointer_of<_Smart>();
    }

  template<typename _Smart, typename _Sp, typename... _Args>
    concept __resettable = requires (_Smart& __s) {
      __s.reset(std::declval<_Sp>(), std::declval<_Args>()...);
    };
}
/// @endcond

  /// Adapt a smart pointer for functions taking an output pointer parameter.
  /**
   * @tparam _Pointer The type of pointer to convert to.
   * @param __s The pointer that should take ownership of the result.
   * @param __args... Arguments to use when resetting the smart pointer.
   * @return A std::inout_ptr_t referring to `__s`.
   * @since C++23
   * @headerfile <memory>
   */
  template<typename _Pointer = void, typename _Smart, typename... _Args>
    inline auto
    out_ptr(_Smart& __s, _Args&&... __args)
    {
      using _TypeId = decltype(__detail::__choose_ptr<_Pointer, _Smart>());
      static_assert(!is_void_v<_TypeId>, "first argument to std::out_ptr "
		    "must be a pointer-like type");

      using _Ret = out_ptr_t<_Smart, typename _TypeId::type, _Args&&...>;
      return _Ret(__s, std::forward<_Args>(__args)...);
    }

  /// Adapt a smart pointer for functions taking an inout pointer parameter.
  /**
   * @tparam _Pointer The type of pointer to convert to.
   * @param __s The pointer that should take ownership of the result.
   * @param __args... Arguments to use when resetting the smart pointer.
   * @return A std::inout_ptr_t referring to `__s`.
   * @since C++23
   * @headerfile <memory>
   */
  template<typename _Pointer = void, typename _Smart, typename... _Args>
    inline auto
    inout_ptr(_Smart& __s, _Args&&... __args)
    {
      using _TypeId = decltype(__detail::__choose_ptr<_Pointer, _Smart>());
      static_assert(!is_void_v<_TypeId>, "first argument to std::inout_ptr "
		    "must be a pointer-like type");

      using _Ret = inout_ptr_t<_Smart, typename _TypeId::type, _Args&&...>;
      return _Ret(__s, std::forward<_Args>(__args)...);
    }

  /// @cond undocumented
  template<typename _Smart, typename _Pointer, typename... _Args>
  template<typename _Smart2, typename _Pointer2, typename... _Args2>
    inline
    out_ptr_t<_Smart, _Pointer, _Args...>::
    _Impl<_Smart2, _Pointer2, _Args2...>::~_Impl()
    {
      using _TypeId = decltype(__detail::__pointer_of_or<_Smart, _Pointer>());
      using _Sp = typename _TypeId::type;

      if (!_M_ptr)
	return;

      _Smart& __s = _M_smart;
      _Pointer& __p = _M_ptr;

      auto __reset = [&](auto&&... __args) {
	if constexpr (__detail::__resettable<_Smart, _Sp, _Args...>)
	  __s.reset(static_cast<_Sp>(__p), std::forward<_Args>(__args)...);
	else if constexpr (is_constructible_v<_Smart, _Sp, _Args...>)
	  __s = _Smart(static_cast<_Sp>(__p), std::forward<_Args>(__args)...);
	else
	  static_assert(is_constructible_v<_Smart, _Sp, _Args...>);
      };

      if constexpr (sizeof...(_Args) >= 2)
	std::apply(__reset, std::move(_M_args));
      else if constexpr (sizeof...(_Args) == 1)
	__reset(std::get<0>(std::move(_M_args)));
      else
	__reset();
    }
  /// @endcond

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif // __glibcxx_out_ptr
#endif /* _GLIBCXX_OUT_PTR_H */
