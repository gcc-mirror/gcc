// Vocabulary Types for Composite Class Design -*- C++ -*-

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

/** @file include/bits/indirect.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _GLIBCXX_INDIRECT_H
#define _GLIBCXX_INDIRECT_H 1

#pragma GCC system_header

#include <bits/version.h>

#if __glibcxx_indirect || __glibcxx_polymorphic // >= C++26
#include <compare>
#include <initializer_list>
#include <bits/allocator.h>
#include <bits/alloc_traits.h>
#include <bits/allocated_ptr.h>   // __allocate_guarded
#include <bits/uses_allocator.h>  // allocator_arg_t
#include <bits/utility.h>         // __is_in_place_type_v
#include <bits/functional_hash.h> // hash
#include <bits/memory_resource.h> // polymorphic_allocator

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#if  __glibcxx_indirect
  template<typename _Tp, typename _Alloc = allocator<_Tp>>
    class indirect;

  template<typename _Tp>
    constexpr bool __is_indirect = false;
  template<typename _Tp, typename _Alloc>
    constexpr bool __is_indirect<indirect<_Tp, _Alloc>> = true;

#if _GLIBCXX_HOSTED
  namespace pmr
  {
    template<typename _Tp>
      using indirect = indirect<_Tp, polymorphic_allocator<_Tp>>;
  }
#endif

  // [indirect], class template indirect
  template<typename _Tp, typename _Alloc>
    class indirect
    {
      static_assert(is_object_v<_Tp>);
      static_assert(!is_array_v<_Tp>);
      static_assert(!is_same_v<_Tp, in_place_t>);
      static_assert(!__is_in_place_type_v<_Tp>);
      static_assert(!is_const_v<_Tp> && !is_volatile_v<_Tp>);

      using _ATraits = allocator_traits<_Alloc>;
      static_assert(is_same_v<_Tp, typename _ATraits::value_type>);

    public:
      using value_type = _Tp;
      using allocator_type = _Alloc;
      using pointer = typename allocator_traits<_Alloc>::pointer;
      using const_pointer = typename allocator_traits<_Alloc>::const_pointer;

      constexpr explicit
      indirect() requires is_default_constructible_v<_Alloc>
      : _M_objp(_M_make_obj_chk())
      { }

      constexpr explicit
      indirect(allocator_arg_t, const _Alloc& __a)
      : _M_alloc(__a), _M_objp(_M_make_obj_chk())
      { }

      constexpr
      indirect(const indirect& __o)
      : indirect(allocator_arg,
		 _ATraits::select_on_container_copy_construction(__o._M_alloc),
		 __o)
      { }

      constexpr
      indirect(allocator_arg_t, const _Alloc& __a, const indirect& __other)
      : _M_alloc(__a)
      {
	if (__other._M_objp)
	  _M_objp = _M_make_obj_chk(__other.__get());
	else
	  _M_objp = nullptr;
      }

      constexpr
      indirect(indirect&& __other) noexcept
      : _M_alloc(std::move(__other._M_alloc)),
	_M_objp(std::__exchange(__other._M_objp, nullptr))
      { }

      constexpr
      indirect(allocator_arg_t, const _Alloc& __a,
	       indirect&& __other) noexcept(_ATraits::is_always_equal::value)
      : _M_alloc(__a),
	_M_objp(std::__exchange(__other._M_objp, nullptr))
      {
	if constexpr (!_ATraits::is_always_equal::value)
	  if (_M_objp && _M_alloc != __other._M_alloc)
	    {
	      static_assert(sizeof(_Tp) != 0, "must be a complete type");

	      // _M_alloc cannot free _M_objp, give it back to __other.
	      __other._M_objp = std::__exchange(_M_objp, nullptr);
	      // And create a new object that can be freed by _M_alloc.
	      _M_objp = _M_make_obj(std::move(*__other._M_objp));
	    }
      }

      template<typename _Up = _Tp>
	requires (!is_same_v<remove_cvref_t<_Up>, in_place_t>)
	  && (!is_same_v<remove_cvref_t<_Up>, indirect>)
	  && is_constructible_v<_Tp, _Up>
	  && is_default_constructible_v<_Alloc>
	constexpr explicit
	indirect(_Up&& __u)
	: _M_objp(_M_make_obj(std::forward<_Up>(__u)))
	{ }

      template<typename _Up = _Tp>
	requires (!is_same_v<remove_cvref_t<_Up>, in_place_t>)
	  && (!is_same_v<remove_cvref_t<_Up>, indirect>)
	  && is_constructible_v<_Tp, _Up>
	constexpr explicit
	indirect(allocator_arg_t, const _Alloc& __a, _Up&& __u)
	: _M_alloc(__a), _M_objp(_M_make_obj(std::forward<_Up>(__u)))
	{ }

      template<typename... _Us>
	requires is_constructible_v<_Tp, _Us...>
	  && is_default_constructible_v<_Alloc>
	constexpr explicit
	indirect(in_place_t, _Us&&... __us)
	: _M_objp(_M_make_obj(std::forward<_Us>(__us)...))
	{ }

      template<typename... _Us>
	requires is_constructible_v<_Tp, _Us...>
	constexpr explicit
	indirect(allocator_arg_t, const _Alloc& __a, in_place_t, _Us&&... __us)
	: _M_alloc(__a),
	  _M_objp(_M_make_obj(std::forward<_Us>(__us)...))
	{ }

      template<typename _Ip, typename... _Us>
	requires is_constructible_v<_Tp, initializer_list<_Ip>&, _Us...>
	  && is_default_constructible_v<_Alloc>
	constexpr explicit
	indirect(in_place_t, initializer_list<_Ip> __il, _Us&&... __us)
	: _M_objp(_M_make_obj(__il, std::forward<_Us>(__us)...))
	{ }

      template<typename _Ip, typename... _Us>
	requires is_constructible_v<_Tp, initializer_list<_Ip>&, _Us...>
	constexpr explicit
	indirect(allocator_arg_t, const _Alloc& __a,
		 in_place_t, initializer_list<_Ip> __il, _Us&&... __us)
	: _M_alloc(__a),
	  _M_objp(_M_make_obj(__il, std::forward<_Us>(__us)...))
	{ }

      constexpr ~indirect()
      {
	static_assert(sizeof(_Tp) != 0, "must be a complete type");
	_M_reset(nullptr);
      }

      constexpr indirect&
      operator=(const indirect& __other)
      {
	static_assert(is_copy_assignable_v<_Tp>);
	static_assert(is_copy_constructible_v<_Tp>);

	if (__builtin_addressof(__other) == this) [[unlikely]]
	  return *this;

	constexpr bool __pocca
	  = _ATraits::propagate_on_container_copy_assignment::value;

	pointer __ptr = nullptr;
	if (__other._M_objp)
	  {
	    if (_ATraits::is_always_equal::value
		  || _M_alloc == __other._M_alloc)
	      {
		if (_M_objp)
		  {
		    *_M_objp = __other.__get();
		    if constexpr (__pocca)
		      _M_alloc = __other._M_alloc;
		    return *this;
		  }
	      }
	    const indirect& __x = __pocca ? __other : *this;
	    __ptr = __x._M_make_obj(__other.__get());
	  }

	_M_reset(__ptr);

	if constexpr (__pocca)
	  _M_alloc = __other._M_alloc;

	return *this;
      }

      constexpr indirect&
      operator=(indirect&& __other)
      noexcept(_ATraits::propagate_on_container_move_assignment::value
		 || _ATraits::is_always_equal::value)
      {
	// N5008 says is_copy_constructible_v<T> here, but that seems wrong.
	// We only require move-constructible, and only for unequal allocators.

	if (__builtin_addressof(__other) == this) [[unlikely]]
	  return *this;

	constexpr bool __pocma
	  = _ATraits::propagate_on_container_move_assignment::value;

	pointer __ptr = nullptr;

	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 4251. Move assignment for indirect unnecessarily requires copy construction
	if constexpr (_ATraits::is_always_equal::value || __pocma)
	  __ptr = std::__exchange(__other._M_objp, nullptr);
	else if (_M_alloc == __other._M_alloc)
	  __ptr = std::__exchange(__other._M_objp, nullptr);
	else if (__other._M_objp)
	  {
	    static_assert(is_move_constructible_v<_Tp>);
	    __ptr = _M_make_obj(std::move(*__other._M_objp));
	  }

	_M_reset(__ptr);

	if constexpr (__pocma)
	  _M_alloc = std::move(__other._M_alloc);

	return *this;
      }

      template<typename _Up = _Tp>
	requires (!is_same_v<remove_cvref_t<_Up>, indirect>)
	  && is_constructible_v<_Tp, _Up> && is_assignable_v<_Tp&, _Up>
	constexpr indirect&
	operator=(_Up&& __u)
	{
	  if (_M_objp == nullptr)
	    _M_objp = _M_make_obj(std::forward<_Up>(__u));
	  else
	    *_M_objp = std::forward<_Up>(__u);

	  return *this;
	}

      template<typename _Self>
      constexpr auto&&
      operator*(this _Self&& __self) noexcept
      {
	// n.b. [allocator.requirements.general] p22 implies
	// dereferencing const pointer is same as pointer
	const indirect& __iself = (const indirect&)__self;
	__glibcxx_assert(__iself._M_objp != nullptr);
	return std::forward_like<_Self>(*__iself._M_objp);
      }

      constexpr const_pointer
      operator->() const noexcept
      {
	// Do we want to enforce this? __glibcxx_assert(_M_objp != nullptr);
	return _M_objp;
      }

      constexpr pointer
      operator->() noexcept
      {
	// Do we want to enforce this? __glibcxx_assert(_M_objp != nullptr);
	return _M_objp;
      }

      constexpr bool
      valueless_after_move() const noexcept { return _M_objp == nullptr; }

      constexpr allocator_type
      get_allocator() const noexcept { return _M_alloc; }

      constexpr void
      swap(indirect& __other)
      noexcept(_ATraits::propagate_on_container_swap::value
		 || _ATraits::is_always_equal::value)
      {
	using std::swap;
	swap(_M_objp, __other._M_objp);
	if constexpr (_ATraits::propagate_on_container_swap::value)
	  swap(_M_alloc, __other._M_alloc);
	else if constexpr (!_ATraits::is_always_equal::value)
	  __glibcxx_assert(_M_alloc == __other._M_alloc);
      }

      friend constexpr void
      swap(indirect& __lhs, indirect& __rhs)
      noexcept(_ATraits::propagate_on_container_swap::value
		 || _ATraits::is_always_equal::value)
      { __lhs.swap(__rhs); }

      template<typename _Up, typename _Alloc2>
	requires requires (const _Tp& __t, const _Up& __u) { __t == __u; }
	friend constexpr bool
	operator==(const indirect& __lhs, const indirect<_Up, _Alloc2>& __rhs)
	noexcept(noexcept(*__lhs == *__rhs))
	{
	  if (!__lhs._M_objp || !__rhs._M_objp)
	    return bool(__lhs._M_objp) == bool(__rhs._M_objp);
	  else
	    return __lhs.__get() == __rhs.__get();
	}

      template<typename _Up>
	requires (!__is_indirect<_Up>) // See PR c++/99599
	  && requires (const _Tp& __t, const _Up& __u) { __t == __u; }
	friend constexpr bool
	operator==(const indirect& __lhs, const _Up& __rhs)
	noexcept(noexcept(*__lhs == __rhs))
	{
	  if (!__lhs._M_objp)
	    return false;
	  else
	    return __lhs.__get() == __rhs;
	}

      template<typename _Up, typename _Alloc2>
	friend constexpr __detail::__synth3way_t<_Tp, _Up>
	operator<=>(const indirect& __lhs, const indirect<_Up, _Alloc2>& __rhs)
	noexcept(noexcept(__detail::__synth3way(*__lhs, *__rhs)))
	{
	  if (!__lhs._M_objp || !__rhs._M_objp)
	    return bool(__lhs._M_objp) <=> bool(__rhs._M_objp);
	  else
	    return __detail::__synth3way(__lhs.__get(), __rhs.__get());
	}

      template<typename _Up>
	requires (!__is_indirect<_Up>) // See PR c++/99599
	friend constexpr __detail::__synth3way_t<_Tp, _Up>
	operator<=>(const indirect& __lhs, const _Up& __rhs)
	noexcept(noexcept(__detail::__synth3way(*__lhs, __rhs)))
	{
	  if (!__lhs._M_objp)
	    return strong_ordering::less;
	  else
	    return __detail::__synth3way(__lhs.__get(), __rhs);
	}

    private:
      template<typename, typename> friend class indirect;

      constexpr void
      _M_reset(pointer __ptr) noexcept
      {
	if (_M_objp)
	  {
	    _ATraits::destroy(_M_alloc, std::to_address(_M_objp));
	    _ATraits::deallocate(_M_alloc, _M_objp, 1);
	  }
	_M_objp = __ptr;
      }

      template<typename... _Args>
	constexpr pointer
	_M_make_obj(_Args&&... __args) const
	{
	  _Scoped_allocation __sa(_M_alloc, in_place,
				  std::forward<_Args>(__args)...);
	  return __sa.release();
	}

      // Enforces is_constructible check and then calls _M_make_obj.
      template<typename... _Args>
	[[__gnu__::__always_inline__]]
	constexpr pointer
	_M_make_obj_chk(_Args&&... __args) const
	{
	  static_assert(is_constructible_v<_Tp, _Args...>);
	  return _M_make_obj(std::forward<_Args>(__args)...);
	}

      // Always-const accessor that avoids ADL for operator*.
      // This can be preferable to using *_M_objp because that might give _Tp&.
      // This can be preferable to using **this because that does ADL.
      [[__gnu__::__always_inline__]]
      constexpr const _Tp&
      __get() const noexcept
      { return *_M_objp; }

      [[no_unique_address]] _Alloc _M_alloc = _Alloc();
      pointer _M_objp; // Pointer to the owned object.
    };

  template<typename _Value>
    indirect(_Value) -> indirect<_Value>;

  template<typename _Alloc, typename _Value>
    indirect(allocator_arg_t, _Alloc, _Value)
      -> indirect<_Value, __alloc_rebind<_Alloc, _Value>>;

  // [indirect.hash], hash support
  template<typename _Tp, typename _Alloc>
    requires is_default_constructible_v<hash<_Tp>>
    struct hash<indirect<_Tp, _Alloc>>
    {
      constexpr size_t
      operator()(const indirect<_Tp, _Alloc>& __t) const
      noexcept(noexcept(hash<_Tp>{}(*__t)))
      {
	// We pick an arbitrary hash for valueless indirect objects
	// which hopefully usual values of _Tp won't typically hash to.
	if (__t.valueless_after_move())
	  return -4444zu;
	return hash<_Tp>{}(*__t);
      }
    };

  template<typename _Tp, typename _Alloc>
    struct __is_fast_hash<hash<indirect<_Tp, _Alloc>>>
    : __is_fast_hash<hash<_Tp>>
    { };
#endif // __glibcxx_indirect

#if __glibcxx_polymorphic // C++26 && HOSTED
  template<typename _Tp, typename _Alloc = allocator<_Tp>>
    class polymorphic;

  namespace pmr
  {
    template<typename _Tp>
      using polymorphic = polymorphic<_Tp, polymorphic_allocator<_Tp>>;
  }

  // [polymorphic], class template polymorphic
  template<typename _Tp, typename _Alloc>
    class polymorphic
    {
      static_assert(is_object_v<_Tp>);
      static_assert(!is_array_v<_Tp>);
      static_assert(!is_same_v<_Tp, in_place_t>);
      static_assert(!__is_in_place_type_v<_Tp>);
      static_assert(!is_const_v<_Tp> && !is_volatile_v<_Tp>);

      using _ATraits = allocator_traits<_Alloc>;
      static_assert(is_same_v<_Tp, typename _ATraits::value_type>);

      // The owned object is embedded within a control block which knows the
      // dynamic type and manages cloning and destroying the owned object.
      struct _Obj
      {
	typename _ATraits::pointer _M_objp{}; // pointer to the owned object.

	// A pointer to this type, e.g. _Obj*
	using pointer
	  = typename _ATraits::template rebind_traits<_Obj>::pointer;

	enum class _Op { _Dispose = 1, _Copy = 2, _Move = 3 };

	constexpr virtual pointer
	_M_manage(const _Alloc&, _Op, void* = nullptr) = 0;
      };

      template<typename _Up>
	struct _Obj_impl : _Obj
	{
	  using _MyTraits
	    = typename _ATraits::template rebind_traits<_Obj_impl>;

	  using _Op = _Obj::_Op;

	  union _Uninitialized {
	    constexpr _Uninitialized() { }
	    constexpr ~_Uninitialized() { }
	    _Up _M_objp;
	  };
	  _Uninitialized _M_u;

	  template<typename... _Args>
	    constexpr
	    _Obj_impl(typename _MyTraits::allocator_type& __a,
		      _Args&&... __args)
	    {
	      using _PtrTr = pointer_traits<typename _ATraits::pointer>;
	      _MyTraits::construct(__a, __builtin_addressof(_M_u._M_objp),
				   std::forward<_Args>(__args)...);
	      this->_M_objp = _PtrTr::pointer_to(_M_u._M_objp);
	    }

	  constexpr virtual typename _Obj::pointer
	  _M_manage(const _Alloc& __a, _Op __op, void*) override
	  {

	    switch (__op)
	      {
	      case _Op::_Move:
	        return _S_make_obj<_Up>(__a, std::move(_M_u._M_objp));
	      case _Op::_Copy:
	        return _S_make_obj<_Up>(__a,
				        const_cast<const _Up&>(_M_u._M_objp));
	      case _Op::_Dispose:
		{
		  using _PtrTr = pointer_traits<typename _MyTraits::pointer>;
	          typename _MyTraits::allocator_type __a2(__a);
		  _MyTraits::destroy(__a2, std::__addressof(_M_u._M_objp));
		  _MyTraits::deallocate(__a2, _PtrTr::pointer_to(*this), 1);
		  return nullptr;
		}
	      }
	    __builtin_unreachable();
	  }
	};

      // TODO: the standard permits a small-object optimization where the
      // owned object is nested within the std::polymorphic not on the heap.

    public:

      using value_type = _Tp;
      using allocator_type = _Alloc;
      using pointer = typename allocator_traits<_Alloc>::pointer;
      using const_pointer  = typename allocator_traits<_Alloc>::const_pointer;

      constexpr explicit
      polymorphic() requires is_default_constructible_v<_Alloc>
      : polymorphic(in_place_type<_Tp>)
      { }

      constexpr explicit
      polymorphic(allocator_arg_t, const _Alloc& __a)
      : polymorphic(allocator_arg, __a, in_place_type<_Tp>)
      { }

      constexpr
      polymorphic(const polymorphic& __other)
      : polymorphic(allocator_arg,
		    _ATraits::select_on_container_copy_construction(
		      __other._M_alloc),
		    __other)
      { }

      constexpr
      polymorphic(allocator_arg_t, const _Alloc& __a,
		  const polymorphic& __other)
      : _M_alloc(__a)
      {
	if (__other._M_objp)
	  _M_objp = __other._M_objp->_M_manage(__a, _Obj::_Op::_Copy);
	else
	  _M_objp = nullptr;
      }

      constexpr
      polymorphic(polymorphic&& __other) noexcept
      : _M_alloc(std::move(__other._M_alloc)),
	_M_objp(std::__exchange(__other._M_objp, nullptr))
      { }

      constexpr
      polymorphic(allocator_arg_t, const _Alloc& __a, polymorphic&& __other)
      noexcept(_ATraits::is_always_equal::value)
      : _M_alloc(__a),
	_M_objp(std::__exchange(__other._M_objp, nullptr))
      {
	if constexpr (!_ATraits::is_always_equal::value)
	  if (_M_objp && _M_alloc != __other._M_alloc)
	    {
	      // _M_alloc cannot free _M_objp, give it back to __other.
	      __other._M_objp = std::__exchange(_M_objp, nullptr);
	      // And create a new object that can be freed by _M_alloc.
	      _M_objp = __other._M_objp->_M_manage(__a, _Obj::_Op::_Move);
	    }
      }

      template<typename _Up = _Tp, typename _UUp = remove_cvref_t<_Up>>
	requires (!is_same_v<_UUp, polymorphic>)
	  && (!__is_in_place_type_v<_UUp>)
	  && derived_from<_UUp, _Tp>
	  && is_constructible_v<_UUp, _Up>
	  && is_copy_constructible_v<_UUp>
	  && is_default_constructible_v<_Alloc>
	constexpr explicit
	polymorphic(_Up&& __u)
	: _M_objp(_M_make_obj<_UUp>(std::forward<_Up>(__u)))
	{ }

      template<typename _Up = _Tp, typename _UUp = remove_cvref_t<_Up>>
	requires (!is_same_v<_UUp, polymorphic>)
	  && (!__is_in_place_type_v<_UUp>)
	  && derived_from<_UUp, _Tp>
	  && is_constructible_v<_UUp, _Up>
	  && is_copy_constructible_v<_UUp>
	constexpr explicit
	polymorphic(allocator_arg_t, const _Alloc& __a, _Up&& __u)
	: _M_alloc(__a), _M_objp(_M_make_obj<_UUp>(std::forward<_Up>(__u)))
	{ }

      template<typename _Up, typename... _Ts>
	requires is_same_v<remove_cvref_t<_Up>, _Up>
	  && derived_from<_Up, _Tp>
	  && is_constructible_v<_Up, _Ts...>
	  && is_copy_constructible_v<_Up>
	  && is_default_constructible_v<_Alloc>
	constexpr explicit
	polymorphic(in_place_type_t<_Up> __t, _Ts&&... __ts)
	: _M_objp(_M_make_obj<_Up>(std::forward<_Ts>(__ts)...))
	{ }

      template<typename _Up, typename... _Ts>
	requires is_same_v<remove_cvref_t<_Up>, _Up>
	  && derived_from<_Up, _Tp>
	  && is_constructible_v<_Up, _Ts...>
	  && is_copy_constructible_v<_Up>
	constexpr explicit
	polymorphic(allocator_arg_t, const _Alloc& __a,
		    in_place_type_t<_Up>, _Ts&&... __ts)
	: _M_alloc(__a),
	  _M_objp(_M_make_obj<_Up>(std::forward<_Ts>(__ts)...))
	{ }

      template<typename _Up, typename _Ip, typename... _Us>
	requires is_same_v<remove_cvref_t<_Up>, _Up>
	  && derived_from<_Up, _Tp>
	  && is_constructible_v<_Up, initializer_list<_Ip>&, _Us...>
	  && is_copy_constructible_v<_Up>
	  && is_default_constructible_v<_Alloc>
	constexpr explicit
	polymorphic(in_place_type_t<_Up>, initializer_list<_Ip> __il,
		    _Us&&... __us)
	: _M_objp(_M_make_obj<_Up>(__il, std::forward<_Us>(__us)...))
	{ }

      template<typename _Up, typename _Ip, typename... _Us>
	requires is_same_v<remove_cvref_t<_Up>, _Up>
	  && derived_from<_Up, _Tp>
	  && is_constructible_v<_Up, initializer_list<_Ip>&, _Us...>
	  && is_copy_constructible_v<_Up>
	constexpr explicit
	polymorphic(allocator_arg_t, const _Alloc& __a,
		    in_place_type_t<_Up>, initializer_list<_Ip> __il,
		    _Us&&... __us)
	: _M_alloc(__a),
	  _M_objp(_M_make_obj<_Up>(__il, std::forward<_Us>(__us)...))
	{ }

      constexpr ~polymorphic()
      {
	static_assert(sizeof(_Tp) != 0, "must be a complete type");
	_M_reset(nullptr);
      }

      constexpr polymorphic&
      operator=(const polymorphic& __other)
      {
	static_assert(sizeof(_Tp) != 0, "must be a complete type");

	if (__builtin_addressof(__other) == this) [[unlikely]]
	  return *this;

	constexpr bool __pocca
	  = _ATraits::propagate_on_container_copy_assignment::value;

	typename _Obj::pointer __ptr = nullptr;
	if (__other._M_objp)
	  {
	    auto& __a = __pocca ? __other._M_alloc : _M_alloc;
	    __ptr = __other._M_objp->_M_manage(__a, _Obj::_Op::_Copy);
	  }

	_M_reset(__ptr);

	if constexpr (__pocca)
	  _M_alloc = __other._M_alloc;

	return *this;
      }

      constexpr polymorphic&
      operator=(polymorphic&& __other)
      noexcept(_ATraits::propagate_on_container_move_assignment::value
		 || _ATraits::is_always_equal::value)
      {
	if (__builtin_addressof(__other) == this) [[unlikely]]
	  return *this;

	constexpr bool __pocma
	  = _ATraits::propagate_on_container_move_assignment::value;

	typename _Obj::pointer __ptr = nullptr;

	// _GLIBCXX_RESOLVE_LIB_DEFECTS
	// 4251. Move assignment for indirect unnecessarily requires copy construction
	if constexpr (_ATraits::is_always_equal::value || __pocma)
	  __ptr = std::__exchange(__other._M_objp, nullptr);
	else if (_M_alloc == __other._M_alloc)
	  __ptr = std::__exchange(__other._M_objp, nullptr);
	else if (__other._M_objp)
	  {
	    static_assert(sizeof(_Tp) != 0, "must be a complete type");
	    __ptr = __other._M_objp->_M_manage(_M_alloc, _Obj::_Op::_Move);
	  }

	_M_reset(__ptr);

	if constexpr (__pocma)
	  _M_alloc = std::move(__other._M_alloc);

	return *this;
      }

      constexpr const _Tp&
      operator*() const noexcept
      {
	__glibcxx_assert(_M_objp != nullptr);
	return *_M_objp->_M_objp;
      }

      constexpr _Tp&
      operator*() noexcept
      {
	__glibcxx_assert(_M_objp != nullptr);
	return *_M_objp->_M_objp;
      }

      constexpr const_pointer
      operator->() const noexcept
      {
	__glibcxx_assert(_M_objp != nullptr);
	return _M_objp ? _M_objp->_M_objp : const_pointer{};
      }

      constexpr pointer
      operator->() noexcept
      {
	__glibcxx_assert(_M_objp != nullptr);
	return _M_objp ? _M_objp->_M_objp : pointer{};
      }

      constexpr bool
      valueless_after_move() const noexcept { return _M_objp == nullptr; }

      constexpr allocator_type
      get_allocator() const noexcept { return _M_alloc; }

      constexpr void
      swap(polymorphic& __other)
      noexcept(_ATraits::propagate_on_container_swap::value
		 || _ATraits::is_always_equal::value)
      {
	using std::swap;
	swap(_M_objp, __other._M_objp);
	if constexpr (_ATraits::propagate_on_container_swap::value)
	  swap(_M_alloc, __other._M_alloc);
	else if constexpr (!_ATraits::is_always_equal::value)
	  __glibcxx_assert(_M_alloc == __other._M_alloc);
      }

      friend constexpr void
      swap(polymorphic& __lhs, polymorphic& __rhs)
      noexcept(_ATraits::propagate_on_container_swap::value
	       || _ATraits::is_always_equal::value)
      { __lhs.swap(__rhs); }

    private:
      template<typename _Up, typename... _Args>
	static constexpr typename _Obj::pointer
	_S_make_obj(const _Alloc& __a, _Args&&... __args)
	{
	  __alloc_rebind<_Alloc, _Obj_impl<_Up>> __objalloc(__a);
	  _Scoped_allocation __sa(__objalloc, in_place, __objalloc,
				  std::forward<_Args>(__args)...);
	  auto __obj = __sa.release();
	  // FIXME: We need to downcast from _Obj_impl<U>* to _Obj* but the
	  // the pointer_traits usage breaks in constexpr. PR c++/110714
	  if constexpr (is_pointer_v<typename _Obj::pointer>)
	    return __obj;
	  else
	    return pointer_traits<typename _Obj::pointer>::pointer_to(*__obj);
	}

      template<typename _Up, typename... _Args>
	constexpr typename _Obj::pointer
	_M_make_obj(_Args&&... __args) const
	{ return _S_make_obj<_Up>(_M_alloc, std::forward<_Args>(__args)...); }

      constexpr void
      _M_reset(typename _Obj::pointer __ptr) noexcept
      {
	if (_M_objp)
	  _M_objp->_M_manage(_M_alloc, _Obj::_Op::_Dispose);
	_M_objp = __ptr;
      }

      [[no_unique_address]] _Alloc _M_alloc = _Alloc();
      typename _Obj::pointer _M_objp;
    };
#endif // __glibcxx_polymorphic

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
#endif // C++26 __glibcxx_indirect || __glibcxx_polymorphic

#endif // _GLIBCXX_INDIRECT_H
