// unique_ptr implementation -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/** @file unique_ptr.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _UNIQUE_PTR_H
#define _UNIQUE_PTR_H 1

#ifndef __GXX_EXPERIMENTAL_CXX0X__
# include <c++0x_warning.h>
#endif

#include <bits/c++config.h>
#include <debug/debug.h>
#include <type_traits>
#include <utility>
#include <tuple>

_GLIBCXX_BEGIN_NAMESPACE(std)

  /// Primary template, default_delete.
  template<typename _Tp> 
    struct default_delete
      {
	default_delete() { }

	template<typename _Up>
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
      void
      operator()(_Tp* __ptr) const
      {
	static_assert(sizeof(_Tp)>0,
		      "can't delete pointer to incomplete type");
	delete [] __ptr;
      }
    };

  /// 20.6.11.2 unique_ptr for single objects.
  template <typename _Tp, typename _Tp_Deleter = default_delete<_Tp> > 
    class unique_ptr
    {
      typedef _Tp* pointer;
      typedef unique_ptr<_Tp, _Tp_Deleter> __this_type;
      typedef std::tuple<pointer, _Tp_Deleter> __tuple_type;
      typedef __tuple_type __this_type::* __unspecified_bool_type;
      typedef pointer __this_type::* __unspecified_pointer_type;

    public:
      typedef _Tp         element_type;      
      typedef _Tp_Deleter deleter_type;

      // constructors
      unique_ptr()
      : _M_t(pointer(), deleter_type())
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

      // move constructors
      unique_ptr(unique_ptr && __u) 
      : _M_t(__u.release(), std::forward<deleter_type>(__u.get_deleter())) { }

      template<typename _Up, typename _Up_Deleter> 
        unique_ptr(unique_ptr<_Up, _Up_Deleter>&& __u) 
        : _M_t(__u.release(), std::forward<deleter_type>(__u.get_deleter()))
	{ }

      // destructor
      ~unique_ptr() { reset(); }
    
      // assignment
      unique_ptr&
      operator=(unique_ptr&& __u)
      { 
        reset(__u.release()); 
        get_deleter() = std::move(__u.get_deleter()); 
        return *this;
      }

      template<typename _Up, typename _Up_Deleter> 
        unique_ptr&
        operator=(unique_ptr<_Up, _Up_Deleter>&& __u)
	{
          reset(__u.release()); 
          get_deleter() = std::move(__u.get_deleter()); 
          return *this;
        }

      unique_ptr&
      operator=(__unspecified_pointer_type) 
      {
	reset();
	return *this;
      }

      // observers
      typename std::add_lvalue_reference<element_type>::type operator*() const
      {
	_GLIBCXX_DEBUG_ASSERT(get() != 0);
	return *get();
      }

      pointer
      operator->() const
      {
	_GLIBCXX_DEBUG_ASSERT(get() != 0);
	return get();
      }

      pointer
      get() const
      { return std::get<0>(_M_t); }

      typename std::add_lvalue_reference<deleter_type>::type
      get_deleter()
      { return std::get<1>(_M_t); }

      typename std::add_lvalue_reference<
          typename std::add_const<deleter_type>::type
              >::type
      get_deleter() const
      { return std::get<1>(_M_t); }

      operator __unspecified_bool_type () const
      { return get() == 0 ? 0 : &__this_type::_M_t; }

      // modifiers
      pointer
      release() 
      {
	pointer __p = get();
	std::get<0>(_M_t) = 0;
	return __p;
      }

      void
      reset(pointer __p = 0) 
      {
	if (__p != get())
	  {
	    get_deleter()(get());
	    std::get<0>(_M_t) = __p;
	  }
      }

      void
      swap(unique_ptr&& __u)
      { using std::swap;
	swap(_M_t, __u._M_t);
      }

    private: 
      // disable copy from lvalue
      unique_ptr(const unique_ptr&);

      template<typename _Up, typename _Up_Deleter> 
        unique_ptr(const unique_ptr<_Up, _Up_Deleter>&);
      
      // disable assignment from lvalue
      unique_ptr& operator=(const unique_ptr&);

      template<typename _Up, typename _Up_Deleter> 
        unique_ptr& operator=(const unique_ptr<_Up, _Up_Deleter>&);
      
    private:
      __tuple_type _M_t;
  };
 
  /// 20.6.11.3 unique_ptr for array objects with a runtime length
  // [unique.ptr.runtime]
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 740 - omit specialization for array objects with a compile time length
  template<typename _Tp, typename _Tp_Deleter> 
    class unique_ptr<_Tp[], _Tp_Deleter>
    {
      typedef _Tp* pointer;
      typedef unique_ptr<_Tp[], _Tp_Deleter> __this_type;
      typedef std::tuple<pointer, _Tp_Deleter> __tuple_type;
      typedef __tuple_type __this_type::* __unspecified_bool_type;
      typedef pointer __this_type::* __unspecified_pointer_type;
    public:
      typedef _Tp         element_type;      
      typedef _Tp_Deleter deleter_type;
    
      // constructors
      unique_ptr()
      : _M_t(pointer(), deleter_type())
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

      // move constructors
      unique_ptr(unique_ptr&& __u) 
      : _M_t(__u.release(), std::forward<deleter_type>(__u.get_deleter())) { }

      template<typename _Up, typename _Up_Deleter> 
        unique_ptr(unique_ptr<_Up, _Up_Deleter>&& __u) 
	: _M_t(__u.release(), std::forward<deleter_type>(__u.get_deleter()))
	{ }

      // destructor
      ~unique_ptr() { reset(); }

      // assignment
      unique_ptr&
      operator=(unique_ptr&& __u)
      {
	reset(__u.release());
	get_deleter() = std::move(__u.get_deleter()); 
	return *this; 
      }

      template<typename _Up, typename _Up_Deleter> 
        unique_ptr&
        operator=(unique_ptr<_Up, _Up_Deleter>&& __u)
	{
          reset(__u.release());
          get_deleter() = std::move(__u.get_deleter()); 
          return *this;
        }

      unique_ptr&
      operator=(__unspecified_pointer_type)
      {
	reset();
	return *this;
      }

      // observers
      typename std::add_lvalue_reference<element_type>::type 
      operator[](size_t __i) const 
      {
	_GLIBCXX_DEBUG_ASSERT(get() != 0);
	return get()[__i];
      }

      pointer
      get() const
      { return std::get<0>(_M_t); }

      typename std::add_lvalue_reference<deleter_type>::type 
      get_deleter()
      { return std::get<1>(_M_t); }

      typename std::add_lvalue_reference<
          typename std::add_const<deleter_type>::type
              >::type 
      get_deleter() const
      { return std::get<1>(_M_t); }    

      operator __unspecified_bool_type () const 
      { return get() == 0 ? 0 : &__this_type::_M_t; }
    
      // modifiers
      pointer
      release() 
      {
	pointer __p = get();
	std::get<0>(_M_t) = 0;
	return __p;
      }

      void
      reset(pointer __p = 0) 
      {
	if (__p != get())
	{
	  get_deleter()(get());
	  std::get<0>(_M_t) = __p;
	}
      }

      void
      swap(unique_ptr&& __u)
      {
	using std::swap;
	swap(_M_t, __u._M_t);
      }

    private:
      // disable copy from lvalue
      unique_ptr(const unique_ptr&);
      unique_ptr& operator=(const unique_ptr&);

      // disable construction from convertible pointer types
      // (N2315 - 20.6.5.3.1)
      template<typename _Up> unique_ptr(_Up*,
        typename std::conditional<std::is_reference<deleter_type>::value, 
          deleter_type, const deleter_type&>::type,
            typename std::enable_if<std::is_convertible<_Up*, 
                pointer>::value>::type* = 0);

      template<typename _Up> unique_ptr(_Up*,
        typename std::remove_reference<deleter_type>::type&&,
          typename std::enable_if<std::is_convertible<_Up*, 
              pointer>::value>::type* = 0);

      template<typename _Up> explicit unique_ptr(_Up*,
        typename std::enable_if<std::is_convertible<_Up*, 
            pointer>::value>::type* = 0);

      // disable reset with convertible pointer types (N2315 - 20.6.5.3.3) 
      template<typename _Up>
        typename std::enable_if<std::is_convertible<_Up*,
          pointer>::value>::type reset(_Up*);
          
    private:
      __tuple_type _M_t;
  };
  
  template<typename _Tp, typename _Tp_Deleter> 
    inline void
    swap(unique_ptr<_Tp, _Tp_Deleter>& __x, 
	 unique_ptr<_Tp, _Tp_Deleter>& __y) 
    { __x.swap(__y); }

  template<typename _Tp, typename _Tp_Deleter> 
    inline void
    swap(unique_ptr<_Tp, _Tp_Deleter>&& __x, 
	 unique_ptr<_Tp, _Tp_Deleter>& __y)
    { __x.swap(__y); }

  template<typename _Tp, typename _Tp_Deleter> 
    inline void
    swap(unique_ptr<_Tp, _Tp_Deleter>& __x, 
	 unique_ptr<_Tp, _Tp_Deleter>&& __y)
    { __x.swap(__y); }
  
  template<typename _Tp, typename _Tp_Deleter,
	   typename _Up, typename _Up_Deleter>
    inline bool
    operator==(const unique_ptr<_Tp, _Tp_Deleter>& __x, 
	       const unique_ptr<_Up, _Up_Deleter>& __y)
    { return __x.get() == __y.get(); }

  template<typename _Tp, typename _Tp_Deleter,
	   typename _Up, typename _Up_Deleter>
    inline bool
    operator!=(const unique_ptr<_Tp, _Tp_Deleter>& __x, 
	       const unique_ptr<_Up, _Up_Deleter>& __y)
    { return !(__x.get() == __y.get()); }

  template<typename _Tp, typename _Tp_Deleter,
	   typename _Up, typename _Up_Deleter>
    inline bool
    operator<(const unique_ptr<_Tp, _Tp_Deleter>& __x,
	      const unique_ptr<_Up, _Up_Deleter>& __y)
    { return __x.get() < __y.get(); }

  template<typename _Tp, typename _Tp_Deleter,
	   typename _Up, typename _Up_Deleter>
    inline bool
    operator<=(const unique_ptr<_Tp, _Tp_Deleter>& __x,
	       const unique_ptr<_Up, _Up_Deleter>& __y)
    { return !(__y.get() < __x.get()); }

  template<typename _Tp, typename _Tp_Deleter,
	   typename _Up, typename _Up_Deleter>
    inline bool
    operator>(const unique_ptr<_Tp, _Tp_Deleter>& __x,
	      const unique_ptr<_Up, _Up_Deleter>& __y)
    { return __y.get() < __x.get(); }

  template<typename _Tp, typename _Tp_Deleter,
	   typename _Up, typename _Up_Deleter>
    inline bool
    operator>=(const unique_ptr<_Tp, _Tp_Deleter>& __x,
	       const unique_ptr<_Up, _Up_Deleter>& __y)
    { return !(__x.get() < __y.get()); }

_GLIBCXX_END_NAMESPACE

#endif /* _UNIQUE_PTR_H */
