// unique_ptr implementation -*- C++ -*-

// Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

/** @file bits/unique_ptr.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _UNIQUE_PTR_H
#define _UNIQUE_PTR_H 1

#include <bits/c++config.h>
#include <debug/debug.h>
#include <type_traits>
#include <utility>
#include <tuple>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   * @addtogroup pointer_abstractions
   * @{
   */

  /// Primary template, default_delete.
  template<typename _Tp>
    struct default_delete
    {
      constexpr default_delete() = default;

      template<typename _Up, typename = typename
	       std::enable_if<std::is_convertible<_Up*, _Tp*>::value>::type>
        default_delete(const default_delete<_Up>&) { }

      void
      operator()(_Tp* __ptr) const
      {
	static_assert(sizeof(_Tp)>0,
		      "can't delete pointer to incomplete type");
	delete __ptr;
      }
    };

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 740 - omit specialization for array objects with a compile time length
  /// Specialization, default_delete.
  template<typename _Tp>
    struct default_delete<_Tp[]>
    {
      constexpr default_delete() = default;

      void
      operator()(_Tp* __ptr) const
      {
	static_assert(sizeof(_Tp)>0,
		      "can't delete pointer to incomplete type");
	delete [] __ptr;
      }

      template<typename _Up> void operator()(_Up*) const = delete;
    };

  /// 20.7.12.2 unique_ptr for single objects.
  template <typename _Tp, typename _Dp = default_delete<_Tp> >
    class unique_ptr
    {
      // use SFINAE to determine whether _Del::pointer exists
      class _Pointer
      {
	template<typename _Up>
	  static typename _Up::pointer __test(typename _Up::pointer*);

	template<typename _Up>
	  static _Tp* __test(...);

	typedef typename remove_reference<_Dp>::type _Del;

      public:
	typedef decltype( __test<_Del>(0)) type;
      };

      typedef std::tuple<typename _Pointer::type, _Dp>  __tuple_type;
      __tuple_type                                      _M_t;

    public:
      typedef typename _Pointer::type   pointer;
      typedef _Tp                       element_type;
      typedef _Dp                       deleter_type;

      // Constructors.
      constexpr unique_ptr()
      : _M_t()
      { static_assert(!std::is_pointer<deleter_type>::value,
		     "constructed with null function pointer deleter"); }

      explicit
      unique_ptr(pointer __p)
      : _M_t(__p, deleter_type())
      { static_assert(!std::is_pointer<deleter_type>::value,
		     "constructed with null function pointer deleter"); }

      unique_ptr(pointer __p,
	  typename std::conditional<std::is_reference<deleter_type>::value,
	    deleter_type, const deleter_type&>::type __d)
      : _M_t(__p, __d) { }

      unique_ptr(pointer __p,
	  typename std::remove_reference<deleter_type>::type&& __d)
      : _M_t(std::move(__p), std::move(__d))
      { static_assert(!std::is_reference<deleter_type>::value,
		      "rvalue deleter bound to reference"); }

      constexpr unique_ptr(nullptr_t)
      : _M_t()
      { static_assert(!std::is_pointer<deleter_type>::value,
		     "constructed with null function pointer deleter"); }

      // Move constructors.
      unique_ptr(unique_ptr&& __u)
      : _M_t(__u.release(), std::forward<deleter_type>(__u.get_deleter())) { }

      template<typename _Up, typename _Ep, typename = typename
	std::enable_if
	  <std::is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
			       pointer>::value
	   && !std::is_array<_Up>::value
	   && ((std::is_reference<_Dp>::value
		&& std::is_same<_Ep, _Dp>::value)
	       || (!std::is_reference<_Dp>::value
		   && std::is_convertible<_Ep, _Dp>::value))>
	     ::type>
	unique_ptr(unique_ptr<_Up, _Ep>&& __u)
	: _M_t(__u.release(), std::forward<deleter_type>(__u.get_deleter()))
	{ }

#if _GLIBCXX_USE_DEPRECATED
      template<typename _Up, typename = typename
	std::enable_if<std::is_convertible<_Up*, _Tp*>::value
		       && std::is_same<_Dp,
				       default_delete<_Tp>>::value>::type>
	unique_ptr(auto_ptr<_Up>&& __u)
	: _M_t(__u.release(), deleter_type()) { }
#endif

      // Destructor.
      ~unique_ptr() { reset(); }

      // Assignment.
      unique_ptr&
      operator=(unique_ptr&& __u)
      {
	reset(__u.release());
	get_deleter() = std::forward<deleter_type>(__u.get_deleter());
	return *this;
      }

      template<typename _Up, typename _Ep, typename = typename
	std::enable_if
	  <std::is_convertible<typename unique_ptr<_Up, _Ep>::pointer,
			       pointer>::value
	   && !std::is_array<_Up>::value>::type>
	unique_ptr&
	operator=(unique_ptr<_Up, _Ep>&& __u)
	{
	  reset(__u.release());
	  get_deleter() = std::forward<deleter_type>(__u.get_deleter());
	  return *this;
	}

      unique_ptr&
      operator=(nullptr_t)
      {
	reset();
	return *this;
      }

      // Observers.
      typename std::add_lvalue_reference<element_type>::type
      operator*() const
      {
	_GLIBCXX_DEBUG_ASSERT(get() != pointer());
	return *get();
      }

      pointer
      operator->() const
      {
	_GLIBCXX_DEBUG_ASSERT(get() != pointer());
	return get();
      }

      pointer
      get() const
      { return std::get<0>(_M_t); }

      deleter_type&
      get_deleter()
      { return std::get<1>(_M_t); }

      const deleter_type&
      get_deleter() const
      { return std::get<1>(_M_t); }

      explicit operator bool() const
      { return get() == pointer() ? false : true; }

      // Modifiers.
      pointer
      release()
      {
	pointer __p = get();
	std::get<0>(_M_t) = pointer();
	return __p;
      }

      void
      reset(pointer __p = pointer())
      {
	using std::swap;
	swap(std::get<0>(_M_t), __p);
	if (__p != pointer())
	  get_deleter()(__p);
      }

      void
      swap(unique_ptr& __u)
      {
	using std::swap;
	swap(_M_t, __u._M_t);
      }

      // Disable copy from lvalue.
      unique_ptr(const unique_ptr&) = delete;
      unique_ptr& operator=(const unique_ptr&) = delete;
  };

  /// 20.7.12.3 unique_ptr for array objects with a runtime length
  // [unique.ptr.runtime]
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 740 - omit specialization for array objects with a compile time length
  template<typename _Tp, typename _Dp>
    class unique_ptr<_Tp[], _Dp>
    {
      typedef std::tuple<_Tp*, _Dp>  	__tuple_type;
      __tuple_type 			_M_t;

    public:
      typedef _Tp*		 	pointer;
      typedef _Tp		 	element_type;
      typedef _Dp                       deleter_type;

      // Constructors.
      constexpr unique_ptr()
      : _M_t()
      { static_assert(!std::is_pointer<deleter_type>::value,
		     "constructed with null function pointer deleter"); }

      explicit
      unique_ptr(pointer __p)
      : _M_t(__p, deleter_type())
      { static_assert(!std::is_pointer<deleter_type>::value,
		     "constructed with null function pointer deleter"); }

      unique_ptr(pointer __p,
	  typename std::conditional<std::is_reference<deleter_type>::value,
	      deleter_type, const deleter_type&>::type __d)
      : _M_t(__p, __d) { }

      unique_ptr(pointer __p,
		 typename std::remove_reference<deleter_type>::type && __d)
      : _M_t(std::move(__p), std::move(__d))
      { static_assert(!std::is_reference<deleter_type>::value,
		      "rvalue deleter bound to reference"); }

      constexpr unique_ptr(nullptr_t)
      : _M_t()
      { static_assert(!std::is_pointer<deleter_type>::value,
		     "constructed with null function pointer deleter"); }

      // Move constructors.
      unique_ptr(unique_ptr&& __u)
      : _M_t(__u.release(), std::forward<deleter_type>(__u.get_deleter())) { }

      template<typename _Up, typename _Ep>
	unique_ptr(unique_ptr<_Up, _Ep>&& __u)
	: _M_t(__u.release(), std::forward<deleter_type>(__u.get_deleter()))
	{ }

      // Destructor.
      ~unique_ptr() { reset(); }

      // Assignment.
      unique_ptr&
      operator=(unique_ptr&& __u)
      {
	reset(__u.release());
	get_deleter() = std::forward<deleter_type>(__u.get_deleter());
	return *this;
      }

      template<typename _Up, typename _Ep>
	unique_ptr&
	operator=(unique_ptr<_Up, _Ep>&& __u)
	{
	  reset(__u.release());
	  get_deleter() = std::forward<deleter_type>(__u.get_deleter());
	  return *this;
	}

      unique_ptr&
      operator=(nullptr_t)
      {
	reset();
	return *this;
      }

      // Observers.
      typename std::add_lvalue_reference<element_type>::type
      operator[](size_t __i) const
      {
	_GLIBCXX_DEBUG_ASSERT(get() != pointer());
	return get()[__i];
      }

      pointer
      get() const
      { return std::get<0>(_M_t); }

      deleter_type&
      get_deleter()
      { return std::get<1>(_M_t); }

      const deleter_type&
      get_deleter() const
      { return std::get<1>(_M_t); }

      explicit operator bool() const
      { return get() == pointer() ? false : true; }

      // Modifiers.
      pointer
      release()
      {
	pointer __p = get();
	std::get<0>(_M_t) = pointer();
	return __p;
      }

      void
      reset(pointer __p = pointer())
      {
	using std::swap;
	swap(std::get<0>(_M_t), __p);
	if (__p != nullptr)
	  get_deleter()(__p);
      }

      void
      reset(nullptr_t)
      {
	pointer __p = get();
	std::get<0>(_M_t) = pointer();
	if (__p != nullptr)
	  get_deleter()(__p);
      }

      // DR 821.
      template<typename _Up>
	void reset(_Up) = delete;

      void
      swap(unique_ptr& __u)
      {
	using std::swap;
	swap(_M_t, __u._M_t);
      }

      // Disable copy from lvalue.
      unique_ptr(const unique_ptr&) = delete;
      unique_ptr& operator=(const unique_ptr&) = delete;

      // Disable construction from convertible pointer types.
      // (N2315 - 20.6.5.3.1)
      template<typename _Up>
	unique_ptr(_Up*, typename
		   std::conditional<std::is_reference<deleter_type>::value,
		   deleter_type, const deleter_type&>::type,
		   typename std::enable_if<std::is_convertible<_Up*,
		   pointer>::value>::type* = 0) = delete;

      template<typename _Up>
	unique_ptr(_Up*, typename std::remove_reference<deleter_type>::type&&,
		   typename std::enable_if<std::is_convertible<_Up*,
		   pointer>::value>::type* = 0) = delete;

      template<typename _Up>
	explicit
	unique_ptr(_Up*, typename std::enable_if<std::is_convertible<_Up*,
		   pointer>::value>::type* = 0) = delete;
  };

  template<typename _Tp, typename _Dp>
    inline void
    swap(unique_ptr<_Tp, _Dp>& __x,
	 unique_ptr<_Tp, _Dp>& __y)
    { __x.swap(__y); }

  template<typename _Tp, typename _Dp,
	   typename _Up, typename _Ep>
    inline bool
    operator==(const unique_ptr<_Tp, _Dp>& __x,
	       const unique_ptr<_Up, _Ep>& __y)
    { return __x.get() == __y.get(); }

  template<typename _Tp, typename _Dp>
    inline bool
    operator==(const unique_ptr<_Tp, _Dp>& __x, nullptr_t)
    { return __x.get() == nullptr; }

  template<typename _Tp, typename _Dp>
    inline bool
    operator==(nullptr_t, const unique_ptr<_Tp, _Dp>& __y)
    { return nullptr == __y.get(); }

  template<typename _Tp, typename _Dp,
	   typename _Up, typename _Ep>
    inline bool
    operator!=(const unique_ptr<_Tp, _Dp>& __x,
	       const unique_ptr<_Up, _Ep>& __y)
    { return !(__x.get() == __y.get()); }

  template<typename _Tp, typename _Dp>
    inline bool
    operator!=(const unique_ptr<_Tp, _Dp>& __x, nullptr_t)
    { return __x.get() != nullptr; }

  template<typename _Tp, typename _Dp>
    inline bool
    operator!=(nullptr_t, const unique_ptr<_Tp, _Dp>& __y)
    { return nullptr != __y.get(); }

  template<typename _Tp, typename _Dp,
	   typename _Up, typename _Ep>
    inline bool
    operator<(const unique_ptr<_Tp, _Dp>& __x,
	      const unique_ptr<_Up, _Ep>& __y)
    { return __x.get() < __y.get(); }

  template<typename _Tp, typename _Dp,
	   typename _Up, typename _Ep>
    inline bool
    operator<=(const unique_ptr<_Tp, _Dp>& __x,
	       const unique_ptr<_Up, _Ep>& __y)
    { return !(__y.get() < __x.get()); }

  template<typename _Tp, typename _Dp,
	   typename _Up, typename _Ep>
    inline bool
    operator>(const unique_ptr<_Tp, _Dp>& __x,
	      const unique_ptr<_Up, _Ep>& __y)
    { return __y.get() < __x.get(); }

  template<typename _Tp, typename _Dp,
	   typename _Up, typename _Ep>
    inline bool
    operator>=(const unique_ptr<_Tp, _Dp>& __x,
	       const unique_ptr<_Up, _Ep>& __y)
    { return !(__x.get() < __y.get()); }

  /// std::hash specialization for unique_ptr.
  template<typename _Tp, typename _Dp>
    struct hash<unique_ptr<_Tp, _Dp>>
    : public std::unary_function<unique_ptr<_Tp, _Dp>, size_t>
    {
      size_t
      operator()(const unique_ptr<_Tp, _Dp>& __u) const
      {
	typedef unique_ptr<_Tp, _Dp> _UP;
	return std::hash<typename _UP::pointer>()(__u.get());
      }
    };

  // @} group pointer_abstractions

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif /* _UNIQUE_PTR_H */
