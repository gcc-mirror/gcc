// shared_ptr and weak_ptr implementation details -*- C++ -*-

// Copyright (C) 2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

// GCC Note: Based on files from version 1.32.0 of the Boost library.

//  shared_count.hpp
//  Copyright (c) 2001, 2002, 2003 Peter Dimov and Multi Media Ltd.

//  shared_ptr.hpp
//  Copyright (C) 1998, 1999 Greg Colvin and Beman Dawes.
//  Copyright (C) 2001, 2002, 2003 Peter Dimov

//  weak_ptr.hpp
//  Copyright (C) 2001, 2002, 2003 Peter Dimov

//  enable_shared_from_this.hpp
//  Copyright (C) 2002 Peter Dimov

// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

/** @file bits/shared_ptr_base.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _SHARED_PTR_BASE_H
#define _SHARED_PTR_BASE_H 1

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

 /**
   *  @brief  Exception possibly thrown by @c shared_ptr.
   *  @ingroup exceptions
   */
  class bad_weak_ptr : public std::exception
  {
  public:
    virtual char const*
    what() const throw();

    virtual ~bad_weak_ptr() throw();    
  };

  // Substitute for bad_weak_ptr object in the case of -fno-exceptions.
  inline void
  __throw_bad_weak_ptr()
  {
#if __EXCEPTIONS
    throw bad_weak_ptr();
#else
    __builtin_abort();
#endif
  }

  using __gnu_cxx::_Lock_policy;
  using __gnu_cxx::__default_lock_policy;
  using __gnu_cxx::_S_single;
  using __gnu_cxx::_S_mutex;
  using __gnu_cxx::_S_atomic;

  // Empty helper class except when the template argument is _S_mutex.
  template<_Lock_policy _Lp>
    class _Mutex_base
    {
    protected:
      // The atomic policy uses fully-fenced builtins, single doesn't care.
      enum { _S_need_barriers = 0 };
    };

  template<>
    class _Mutex_base<_S_mutex>
    : public __gnu_cxx::__mutex
    {
    protected:
      // This policy is used when atomic builtins are not available.
      // The replacement atomic operations might not have the necessary
      // memory barriers.
      enum { _S_need_barriers = 1 };
    };

  template<_Lock_policy _Lp = __default_lock_policy>
    class _Sp_counted_base
    : public _Mutex_base<_Lp>
    {
    public:  
      _Sp_counted_base()
      : _M_use_count(1), _M_weak_count(1) { }
      
      virtual
      ~_Sp_counted_base() // nothrow 
      { }
  
      // Called when _M_use_count drops to zero, to release the resources
      // managed by *this.
      virtual void
      _M_dispose() = 0; // nothrow
      
      // Called when _M_weak_count drops to zero.
      virtual void
      _M_destroy() // nothrow
      { delete this; }
      
      virtual void*
      _M_get_deleter(const std::type_info&) = 0;

      void
      _M_add_ref_copy()
      { __gnu_cxx::__atomic_add_dispatch(&_M_use_count, 1); }
  
      void
      _M_add_ref_lock();
      
      void
      _M_release() // nothrow
      {
        // Be race-detector-friendly.  For more info see bits/c++config.
        _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(&_M_use_count);
	if (__gnu_cxx::__exchange_and_add_dispatch(&_M_use_count, -1) == 1)
	  {
            _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(&_M_use_count);
	    _M_dispose();
	    // There must be a memory barrier between dispose() and destroy()
	    // to ensure that the effects of dispose() are observed in the
	    // thread that runs destroy().
	    // See http://gcc.gnu.org/ml/libstdc++/2005-11/msg00136.html
	    if (_Mutex_base<_Lp>::_S_need_barriers)
	      {
	        _GLIBCXX_READ_MEM_BARRIER;
	        _GLIBCXX_WRITE_MEM_BARRIER;
	      }

            // Be race-detector-friendly.  For more info see bits/c++config.
            _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(&_M_weak_count);
	    if (__gnu_cxx::__exchange_and_add_dispatch(&_M_weak_count,
						       -1) == 1)
              {
                _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(&_M_weak_count);
	        _M_destroy();
              }
	  }
      }
  
      void
      _M_weak_add_ref() // nothrow
      { __gnu_cxx::__atomic_add_dispatch(&_M_weak_count, 1); }

      void
      _M_weak_release() // nothrow
      {
        // Be race-detector-friendly. For more info see bits/c++config.
        _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(&_M_weak_count);
	if (__gnu_cxx::__exchange_and_add_dispatch(&_M_weak_count, -1) == 1)
	  {
            _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(&_M_weak_count);
	    if (_Mutex_base<_Lp>::_S_need_barriers)
	      {
	        // See _M_release(),
	        // destroy() must observe results of dispose()
	        _GLIBCXX_READ_MEM_BARRIER;
	        _GLIBCXX_WRITE_MEM_BARRIER;
	      }
	    _M_destroy();
	  }
      }
  
      long
      _M_get_use_count() const // nothrow
      {
        // No memory barrier is used here so there is no synchronization
        // with other threads.
        return const_cast<const volatile _Atomic_word&>(_M_use_count);
      }

    private:  
      _Sp_counted_base(_Sp_counted_base const&);
      _Sp_counted_base& operator=(_Sp_counted_base const&);

      _Atomic_word  _M_use_count;     // #shared
      _Atomic_word  _M_weak_count;    // #weak + (#shared != 0)
    };

  template<>
    inline void
    _Sp_counted_base<_S_single>::
    _M_add_ref_lock()
    {
      if (__gnu_cxx::__exchange_and_add_dispatch(&_M_use_count, 1) == 0)
	{
	  _M_use_count = 0;
	  __throw_bad_weak_ptr();
	}
    }

  template<>
    inline void
    _Sp_counted_base<_S_mutex>::
    _M_add_ref_lock()
    {
      __gnu_cxx::__scoped_lock sentry(*this);
      if (__gnu_cxx::__exchange_and_add_dispatch(&_M_use_count, 1) == 0)
	{
	  _M_use_count = 0;
	  __throw_bad_weak_ptr();
	}
    }

  template<> 
    inline void
    _Sp_counted_base<_S_atomic>::
    _M_add_ref_lock()
    {
      // Perform lock-free add-if-not-zero operation.
      _Atomic_word __count;
      do
	{
	  __count = _M_use_count;
	  if (__count == 0)
	    __throw_bad_weak_ptr();
	  
	  // Replace the current counter value with the old value + 1, as
	  // long as it's not changed meanwhile. 
	}
      while (!__sync_bool_compare_and_swap(&_M_use_count, __count,
					   __count + 1));
    }


  // Forward declarations.
  template<typename _Tp, _Lock_policy _Lp = __default_lock_policy>
    class __shared_ptr;

  template<typename _Tp, _Lock_policy _Lp = __default_lock_policy>
    class __weak_ptr;

  template<typename _Tp, _Lock_policy _Lp = __default_lock_policy>
    class __enable_shared_from_this;

  template<typename _Tp>
    class shared_ptr;

  template<typename _Tp>
    class weak_ptr;

  template<typename _Tp>
    struct owner_less;

  template<typename _Tp>
    class enable_shared_from_this;

  template<_Lock_policy _Lp = __default_lock_policy>
    class __weak_count;

  template<_Lock_policy _Lp = __default_lock_policy>
    class __shared_count;


  // Counted ptr with no deleter or allocator support
  template<typename _Ptr, _Lock_policy _Lp>
    class _Sp_counted_ptr : public _Sp_counted_base<_Lp>
    {
    public:
      explicit
      _Sp_counted_ptr(_Ptr __p)
      : _M_ptr(__p) { }

      virtual void
      _M_dispose() // nothrow
      { delete _M_ptr; }

      virtual void
      _M_destroy() // nothrow
      { delete this; }

      virtual void*
      _M_get_deleter(const std::type_info&)
      { return 0; }

      _Sp_counted_ptr(const _Sp_counted_ptr&) = delete;
      _Sp_counted_ptr& operator=(const _Sp_counted_ptr&) = delete;

    protected:
      _Ptr             _M_ptr;  // copy constructor must not throw
    };

  template<>
    inline void
    _Sp_counted_ptr<nullptr_t, _S_single>::_M_dispose() { }

  template<>
    inline void
    _Sp_counted_ptr<nullptr_t, _S_mutex>::_M_dispose() { }

  template<>
    inline void
    _Sp_counted_ptr<nullptr_t, _S_atomic>::_M_dispose() { }

  // Support for custom deleter and/or allocator
  template<typename _Ptr, typename _Deleter, typename _Alloc, _Lock_policy _Lp>
    class _Sp_counted_deleter : public _Sp_counted_base<_Lp>
    {
      typedef typename _Alloc::template
	  rebind<_Sp_counted_deleter>::other _My_alloc_type;

      // Helper class that stores the Deleter and also acts as an allocator.
      // Used to dispose of the owned pointer and the internal refcount
      // Requires that copies of _Alloc can free each other's memory.
      struct _My_Deleter
      : public _My_alloc_type    // copy constructor must not throw
      {
	_Deleter _M_del;         // copy constructor must not throw
	_My_Deleter(_Deleter __d, const _Alloc& __a)
	: _My_alloc_type(__a), _M_del(__d) { }
      };

    public:
      // __d(__p) must not throw.
      _Sp_counted_deleter(_Ptr __p, _Deleter __d)
      : _M_ptr(__p), _M_del(__d, _Alloc()) { }

      // __d(__p) must not throw.
      _Sp_counted_deleter(_Ptr __p, _Deleter __d, const _Alloc& __a)
      : _M_ptr(__p), _M_del(__d, __a) { }

      virtual void
      _M_dispose() // nothrow
      { _M_del._M_del(_M_ptr); }

      virtual void
      _M_destroy() // nothrow
      {
	_My_alloc_type __a(_M_del);
	this->~_Sp_counted_deleter();
	__a.deallocate(this, 1);
      }

      virtual void*
      _M_get_deleter(const std::type_info& __ti)
      {
#ifdef __GXX_RTTI
        return __ti == typeid(_Deleter) ? &_M_del._M_del : 0;
#else
        return 0;
#endif
      }

    protected:
      _Ptr             _M_ptr;  // copy constructor must not throw
      _My_Deleter      _M_del;  // copy constructor must not throw
    };

  // helpers for make_shared / allocate_shared

  template<typename _Tp>
    struct _Sp_destroy_inplace
    {
      void operator()(_Tp* __p) const { if (__p) __p->~_Tp(); }
    };

  struct _Sp_make_shared_tag { };

  template<typename _Tp, typename _Alloc, _Lock_policy _Lp>
    class _Sp_counted_ptr_inplace
    : public _Sp_counted_deleter<_Tp*, _Sp_destroy_inplace<_Tp>, _Alloc, _Lp>
    {
      typedef _Sp_counted_deleter<_Tp*, _Sp_destroy_inplace<_Tp>, _Alloc, _Lp>
	_Base_type;

    public:
      explicit
      _Sp_counted_ptr_inplace(_Alloc __a)
      : _Base_type(static_cast<_Tp*>(0), _Sp_destroy_inplace<_Tp>(), __a)
      , _M_storage()
      {
	void* __p = &_M_storage;
	::new (__p) _Tp();  // might throw
	_Base_type::_M_ptr = static_cast<_Tp*>(__p);
      }

      template<typename... _Args>
	_Sp_counted_ptr_inplace(_Alloc __a, _Args&&... __args)
	: _Base_type(static_cast<_Tp*>(0), _Sp_destroy_inplace<_Tp>(), __a)
	, _M_storage()
	{
	  void* __p = &_M_storage;
	  ::new (__p) _Tp(std::forward<_Args>(__args)...);  // might throw
	  _Base_type::_M_ptr = static_cast<_Tp*>(__p);
	}

      // Override because the allocator needs to know the dynamic type
      virtual void
      _M_destroy() // nothrow
      {
	typedef typename _Alloc::template
	    rebind<_Sp_counted_ptr_inplace>::other _My_alloc_type;
	_My_alloc_type __a(_Base_type::_M_del);
	this->~_Sp_counted_ptr_inplace();
	__a.deallocate(this, 1);
      }

      // Sneaky trick so __shared_ptr can get the managed pointer
      virtual void*
      _M_get_deleter(const std::type_info& __ti)
      {
#ifdef __GXX_RTTI
	return __ti == typeid(_Sp_make_shared_tag)
	       ? static_cast<void*>(&_M_storage)
	       : _Base_type::_M_get_deleter(__ti);
#else
        return 0;
#endif
      }

    private:
      typename aligned_storage<sizeof(_Tp), alignment_of<_Tp>::value>::type
	_M_storage;
    };

  template<_Lock_policy _Lp>
    class __shared_count
    {
    public:
      constexpr __shared_count() : _M_pi(0) // nothrow
      { }

      template<typename _Ptr>
        explicit
	__shared_count(_Ptr __p) : _M_pi(0)
	{
	  __try
	    {
	      _M_pi = new _Sp_counted_ptr<_Ptr, _Lp>(__p);
	    }
	  __catch(...)
	    {
	      delete __p;
	      __throw_exception_again;
	    }
	}

      template<typename _Ptr, typename _Deleter>
	__shared_count(_Ptr __p, _Deleter __d) : _M_pi(0)
	{
	  // The allocator's value_type doesn't matter, will rebind it anyway.
	  typedef std::allocator<int> _Alloc;
	  typedef _Sp_counted_deleter<_Ptr, _Deleter, _Alloc, _Lp> _Sp_cd_type;
	  typedef std::allocator<_Sp_cd_type> _Alloc2;
	  _Alloc2 __a2;
	  __try
	    {
	      _M_pi = __a2.allocate(1);
	      ::new(static_cast<void*>(_M_pi)) _Sp_cd_type(__p, __d);
	    }
	  __catch(...)
	    {
	      __d(__p); // Call _Deleter on __p.
	      if (_M_pi)
		__a2.deallocate(static_cast<_Sp_cd_type*>(_M_pi), 1);
	      __throw_exception_again;
	    }
	}

      template<typename _Ptr, typename _Deleter, typename _Alloc>
	__shared_count(_Ptr __p, _Deleter __d, _Alloc __a) : _M_pi(0)
	{
	  typedef _Sp_counted_deleter<_Ptr, _Deleter, _Alloc, _Lp> _Sp_cd_type;
	  typedef typename _Alloc::template rebind<_Sp_cd_type>::other _Alloc2;
	  _Alloc2 __a2(__a);
	  __try
	    {
	      _M_pi = __a2.allocate(1);
	      ::new(static_cast<void*>(_M_pi)) _Sp_cd_type(__p, __d, __a);
	    }
	  __catch(...)
	    {
	      __d(__p); // Call _Deleter on __p.
	      if (_M_pi)
		__a2.deallocate(static_cast<_Sp_cd_type*>(_M_pi), 1);
	      __throw_exception_again;
	    }
	}

      template<typename _Tp, typename _Alloc, typename... _Args>
	__shared_count(_Sp_make_shared_tag, _Tp*, const _Alloc& __a,
		       _Args&&... __args)
	: _M_pi(0)
	{
	  typedef _Sp_counted_ptr_inplace<_Tp, _Alloc, _Lp> _Sp_cp_type;
	  typedef typename _Alloc::template rebind<_Sp_cp_type>::other _Alloc2;
	  _Alloc2 __a2(__a);
	  __try
	    {
	      _M_pi = __a2.allocate(1);
	      ::new(static_cast<void*>(_M_pi)) _Sp_cp_type(__a,
		    std::forward<_Args>(__args)...);
	    }
	  __catch(...)
	    {
	      if (_M_pi)
		__a2.deallocate(static_cast<_Sp_cp_type*>(_M_pi), 1);
	      __throw_exception_again;
	    }
	}

#if _GLIBCXX_USE_DEPRECATED
      // Special case for auto_ptr<_Tp> to provide the strong guarantee.
      template<typename _Tp>
        explicit
	__shared_count(std::auto_ptr<_Tp>&& __r)
	: _M_pi(new _Sp_counted_ptr<_Tp*, _Lp>(__r.get()))
	{ __r.release(); }
#endif

      // Special case for unique_ptr<_Tp,_Del> to provide the strong guarantee.
      template<typename _Tp, typename _Del>
        explicit
	__shared_count(std::unique_ptr<_Tp, _Del>&& __r)
	: _M_pi(_S_create_from_up(std::move(__r)))
	{ __r.release(); }

      // Throw bad_weak_ptr when __r._M_get_use_count() == 0.
      explicit __shared_count(const __weak_count<_Lp>& __r);

      ~__shared_count() // nothrow
      {
	if (_M_pi != 0)
	  _M_pi->_M_release();
      }

      __shared_count(const __shared_count& __r)
      : _M_pi(__r._M_pi) // nothrow
      {
	if (_M_pi != 0)
	  _M_pi->_M_add_ref_copy();
      }

      __shared_count&
      operator=(const __shared_count& __r) // nothrow
      {
	_Sp_counted_base<_Lp>* __tmp = __r._M_pi;
	if (__tmp != _M_pi)
	  {
	    if (__tmp != 0)
	      __tmp->_M_add_ref_copy();
	    if (_M_pi != 0)
	      _M_pi->_M_release();
	    _M_pi = __tmp;
	  }
	return *this;
      }

      void
      _M_swap(__shared_count& __r) // nothrow
      {
	_Sp_counted_base<_Lp>* __tmp = __r._M_pi;
	__r._M_pi = _M_pi;
	_M_pi = __tmp;
      }

      long
      _M_get_use_count() const // nothrow
      { return _M_pi != 0 ? _M_pi->_M_get_use_count() : 0; }

      bool
      _M_unique() const // nothrow
      { return this->_M_get_use_count() == 1; }

      void*
      _M_get_deleter(const std::type_info& __ti) const
      { return _M_pi ? _M_pi->_M_get_deleter(__ti) : 0; }

      bool
      _M_less(const __shared_count& __rhs) const
      { return std::less<_Sp_counted_base<_Lp>*>()(this->_M_pi, __rhs._M_pi); }

      bool
      _M_less(const __weak_count<_Lp>& __rhs) const
      { return std::less<_Sp_counted_base<_Lp>*>()(this->_M_pi, __rhs._M_pi); }

      // Friend function injected into enclosing namespace and found by ADL
      friend inline bool
      operator==(const __shared_count& __a, const __shared_count& __b)
      { return __a._M_pi == __b._M_pi; }

    private:
      friend class __weak_count<_Lp>;

      template<typename _Tp, typename _Del>
	static _Sp_counted_base<_Lp>*
	_S_create_from_up(std::unique_ptr<_Tp, _Del>&& __r,
	  typename std::enable_if<!std::is_reference<_Del>::value>::type* = 0)
	{
	  return new _Sp_counted_deleter<_Tp*, _Del, std::allocator<_Tp>,
	    _Lp>(__r.get(), __r.get_deleter());
	}

      template<typename _Tp, typename _Del>
	static _Sp_counted_base<_Lp>*
	_S_create_from_up(std::unique_ptr<_Tp, _Del>&& __r,
	  typename std::enable_if<std::is_reference<_Del>::value>::type* = 0)
	{
	  typedef typename std::remove_reference<_Del>::type _Del1;
	  typedef std::reference_wrapper<_Del1> _Del2;
	  return new _Sp_counted_deleter<_Tp*, _Del2, std::allocator<_Tp>,
	    _Lp>(__r.get(), std::ref(__r.get_deleter()));
	}

      _Sp_counted_base<_Lp>*  _M_pi;
    };


  template<_Lock_policy _Lp>
    class __weak_count
    {
    public:
      constexpr __weak_count() : _M_pi(0) // nothrow
      { }

      __weak_count(const __shared_count<_Lp>& __r) : _M_pi(__r._M_pi) // nothrow
      {
	if (_M_pi != 0)
	  _M_pi->_M_weak_add_ref();
      }

      __weak_count(const __weak_count<_Lp>& __r) : _M_pi(__r._M_pi) // nothrow
      {
	if (_M_pi != 0)
	  _M_pi->_M_weak_add_ref();
      }

      ~__weak_count() // nothrow
      {
	if (_M_pi != 0)
	  _M_pi->_M_weak_release();
      }

      __weak_count<_Lp>&
      operator=(const __shared_count<_Lp>& __r) // nothrow
      {
	_Sp_counted_base<_Lp>* __tmp = __r._M_pi;
	if (__tmp != 0)
	  __tmp->_M_weak_add_ref();
	if (_M_pi != 0)
	  _M_pi->_M_weak_release();
	_M_pi = __tmp;
	return *this;
      }

      __weak_count<_Lp>&
      operator=(const __weak_count<_Lp>& __r) // nothrow
      {
	_Sp_counted_base<_Lp>* __tmp = __r._M_pi;
	if (__tmp != 0)
	  __tmp->_M_weak_add_ref();
	if (_M_pi != 0)
	  _M_pi->_M_weak_release();
	_M_pi = __tmp;
	return *this;
      }

      void
      _M_swap(__weak_count<_Lp>& __r) // nothrow
      {
	_Sp_counted_base<_Lp>* __tmp = __r._M_pi;
	__r._M_pi = _M_pi;
	_M_pi = __tmp;
      }

      long
      _M_get_use_count() const // nothrow
      { return _M_pi != 0 ? _M_pi->_M_get_use_count() : 0; }

      bool
      _M_less(const __weak_count& __rhs) const
      { return std::less<_Sp_counted_base<_Lp>*>()(this->_M_pi, __rhs._M_pi); }

      bool
      _M_less(const __shared_count<_Lp>& __rhs) const
      { return std::less<_Sp_counted_base<_Lp>*>()(this->_M_pi, __rhs._M_pi); }

      // Friend function injected into enclosing namespace and found by ADL
      friend inline bool
      operator==(const __weak_count& __a, const __weak_count& __b)
      { return __a._M_pi == __b._M_pi; }

    private:
      friend class __shared_count<_Lp>;

      _Sp_counted_base<_Lp>*  _M_pi;
    };

  // Now that __weak_count is defined we can define this constructor:
  template<_Lock_policy _Lp>
    inline __shared_count<_Lp>:: __shared_count(const __weak_count<_Lp>& __r)
    : _M_pi(__r._M_pi)
    {
      if (_M_pi != 0)
	_M_pi->_M_add_ref_lock();
      else
	__throw_bad_weak_ptr();
    }


  // Support for enable_shared_from_this.

  // Friend of __enable_shared_from_this.
  template<_Lock_policy _Lp, typename _Tp1, typename _Tp2>
    void
    __enable_shared_from_this_helper(const __shared_count<_Lp>&,
				     const __enable_shared_from_this<_Tp1,
				     _Lp>*, const _Tp2*);

  // Friend of enable_shared_from_this.
  template<typename _Tp1, typename _Tp2>
    void
    __enable_shared_from_this_helper(const __shared_count<>&,
				     const enable_shared_from_this<_Tp1>*,
				     const _Tp2*);

  template<_Lock_policy _Lp>
    inline void
    __enable_shared_from_this_helper(const __shared_count<_Lp>&, ...)
    { }


  template<typename _Tp, _Lock_policy _Lp>
    class __shared_ptr
    {
    public:
      typedef _Tp   element_type;

      constexpr __shared_ptr()
      : _M_ptr(0), _M_refcount() // never throws
      { }

      template<typename _Tp1>
	explicit __shared_ptr(_Tp1* __p)
        : _M_ptr(__p), _M_refcount(__p)
	{
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  static_assert( sizeof(_Tp1) > 0, "incomplete type" );
	  __enable_shared_from_this_helper(_M_refcount, __p, __p);
	}

      template<typename _Tp1, typename _Deleter>
	__shared_ptr(_Tp1* __p, _Deleter __d)
	: _M_ptr(__p), _M_refcount(__p, __d)
	{
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  // TODO requires _Deleter CopyConstructible and __d(__p) well-formed
	  __enable_shared_from_this_helper(_M_refcount, __p, __p);
	}

      template<typename _Tp1, typename _Deleter, typename _Alloc>
	__shared_ptr(_Tp1* __p, _Deleter __d, _Alloc __a)
	: _M_ptr(__p), _M_refcount(__p, __d, std::move(__a))
	{
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  // TODO requires _Deleter CopyConstructible and __d(__p) well-formed
	  __enable_shared_from_this_helper(_M_refcount, __p, __p);
	}

      template<typename _Deleter>
	__shared_ptr(nullptr_t __p, _Deleter __d)
	: _M_ptr(0), _M_refcount(__p, __d)
	{ }

      template<typename _Deleter, typename _Alloc>
        __shared_ptr(nullptr_t __p, _Deleter __d, _Alloc __a)
	: _M_ptr(0), _M_refcount(__p, __d, std::move(__a))
	{ }

      template<typename _Tp1>
	__shared_ptr(const __shared_ptr<_Tp1, _Lp>& __r, _Tp* __p)
	: _M_ptr(__p), _M_refcount(__r._M_refcount) // never throws
	{ }

      //  generated copy constructor, assignment, destructor are fine.

      template<typename _Tp1, typename = typename
	       std::enable_if<std::is_convertible<_Tp1*, _Tp*>::value>::type>
	__shared_ptr(const __shared_ptr<_Tp1, _Lp>& __r)
	: _M_ptr(__r._M_ptr), _M_refcount(__r._M_refcount) // never throws
	{ }

      __shared_ptr(__shared_ptr&& __r)
      : _M_ptr(__r._M_ptr), _M_refcount() // never throws
      {
	_M_refcount._M_swap(__r._M_refcount);
	__r._M_ptr = 0;
      }

      template<typename _Tp1, typename = typename
	       std::enable_if<std::is_convertible<_Tp1*, _Tp*>::value>::type>
	__shared_ptr(__shared_ptr<_Tp1, _Lp>&& __r)
	: _M_ptr(__r._M_ptr), _M_refcount() // never throws
	{
	  _M_refcount._M_swap(__r._M_refcount);
	  __r._M_ptr = 0;
	}

      template<typename _Tp1>
	explicit __shared_ptr(const __weak_ptr<_Tp1, _Lp>& __r)
	: _M_refcount(__r._M_refcount) // may throw
	{
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)

	  // It is now safe to copy __r._M_ptr, as
	  // _M_refcount(__r._M_refcount) did not throw.
	  _M_ptr = __r._M_ptr;
	}

      // If an exception is thrown this constructor has no effect.
      template<typename _Tp1, typename _Del>
	__shared_ptr(std::unique_ptr<_Tp1, _Del>&& __r)
	: _M_ptr(__r.get()), _M_refcount()
	{
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  _Tp1* __tmp = __r.get();
	  _M_refcount = __shared_count<_Lp>(std::move(__r));
	  __enable_shared_from_this_helper(_M_refcount, __tmp, __tmp);
	}

#if _GLIBCXX_USE_DEPRECATED
      // Postcondition: use_count() == 1 and __r.get() == 0
      template<typename _Tp1>
	__shared_ptr(std::auto_ptr<_Tp1>&& __r)
	: _M_ptr(__r.get()), _M_refcount()
	{
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  static_assert( sizeof(_Tp1) > 0, "incomplete type" );
	  _Tp1* __tmp = __r.get();
	  _M_refcount = __shared_count<_Lp>(std::move(__r));
	  __enable_shared_from_this_helper(_M_refcount, __tmp, __tmp);
	}
#endif

      /* TODO: use delegating constructor */
      constexpr __shared_ptr(nullptr_t)
      : _M_ptr(0), _M_refcount() // never throws
      { }

      template<typename _Tp1>
	__shared_ptr&
	operator=(const __shared_ptr<_Tp1, _Lp>& __r) // never throws
	{
	  _M_ptr = __r._M_ptr;
	  _M_refcount = __r._M_refcount; // __shared_count::op= doesn't throw
	  return *this;
	}

#if _GLIBCXX_USE_DEPRECATED
      template<typename _Tp1>
	__shared_ptr&
	operator=(std::auto_ptr<_Tp1>&& __r)
	{
	  __shared_ptr(std::move(__r)).swap(*this);
	  return *this;
	}
#endif

      __shared_ptr&
      operator=(__shared_ptr&& __r)
      {
	__shared_ptr(std::move(__r)).swap(*this);
	return *this;
      }

      template<class _Tp1>
	__shared_ptr&
	operator=(__shared_ptr<_Tp1, _Lp>&& __r)
	{
	  __shared_ptr(std::move(__r)).swap(*this);
	  return *this;
	}

      template<typename _Tp1, typename _Del>
	__shared_ptr&
	operator=(std::unique_ptr<_Tp1, _Del>&& __r)
	{
	  __shared_ptr(std::move(__r)).swap(*this);
	  return *this;
	}

      void
      reset() // never throws
      { __shared_ptr().swap(*this); }

      template<typename _Tp1>
	void
	reset(_Tp1* __p) // _Tp1 must be complete.
	{
	  // Catch self-reset errors.
	  _GLIBCXX_DEBUG_ASSERT(__p == 0 || __p != _M_ptr);
	  __shared_ptr(__p).swap(*this);
	}

      template<typename _Tp1, typename _Deleter>
	void
	reset(_Tp1* __p, _Deleter __d)
	{ __shared_ptr(__p, __d).swap(*this); }

      template<typename _Tp1, typename _Deleter, typename _Alloc>
	void
        reset(_Tp1* __p, _Deleter __d, _Alloc __a)
        { __shared_ptr(__p, __d, std::move(__a)).swap(*this); }

      // Allow class instantiation when _Tp is [cv-qual] void.
      typename std::add_lvalue_reference<_Tp>::type
      operator*() const // never throws
      {
	_GLIBCXX_DEBUG_ASSERT(_M_ptr != 0);
	return *_M_ptr;
      }

      _Tp*
      operator->() const // never throws
      {
	_GLIBCXX_DEBUG_ASSERT(_M_ptr != 0);
	return _M_ptr;
      }

      _Tp*
      get() const // never throws
      { return _M_ptr; }

      explicit operator bool() const // never throws
      { return _M_ptr == 0 ? false : true; }

      bool
      unique() const // never throws
      { return _M_refcount._M_unique(); }

      long
      use_count() const // never throws
      { return _M_refcount._M_get_use_count(); }

      void
      swap(__shared_ptr<_Tp, _Lp>& __other) // never throws
      {
	std::swap(_M_ptr, __other._M_ptr);
	_M_refcount._M_swap(__other._M_refcount);
      }

      template<typename _Tp1>
	bool
	owner_before(__shared_ptr<_Tp1, _Lp> const& __rhs) const
	{ return _M_refcount._M_less(__rhs._M_refcount); }

      template<typename _Tp1>
	bool
	owner_before(__weak_ptr<_Tp1, _Lp> const& __rhs) const
	{ return _M_refcount._M_less(__rhs._M_refcount); }

#ifdef __GXX_RTTI
    protected:
      // This constructor is non-standard, it is used by allocate_shared.
      template<typename _Alloc, typename... _Args>
	__shared_ptr(_Sp_make_shared_tag __tag, const _Alloc& __a,
		     _Args&&... __args)
	: _M_ptr(), _M_refcount(__tag, (_Tp*)0, __a,
				std::forward<_Args>(__args)...)
	{
	  // _M_ptr needs to point to the newly constructed object.
	  // This relies on _Sp_counted_ptr_inplace::_M_get_deleter.
	  void* __p = _M_refcount._M_get_deleter(typeid(__tag));
	  _M_ptr = static_cast<_Tp*>(__p);
	  __enable_shared_from_this_helper(_M_refcount, _M_ptr, _M_ptr);
	}
#else
      template<typename _Alloc>
        struct _Deleter
        {
          void operator()(_Tp* __ptr)
          {
            _M_alloc.destroy(__ptr);
            _M_alloc.deallocate(__ptr, 1);
          }
          _Alloc _M_alloc;
        };

      template<typename _Alloc, typename... _Args>
	__shared_ptr(_Sp_make_shared_tag __tag, const _Alloc& __a,
		     _Args&&... __args)
	: _M_ptr(), _M_refcount()
        {
	  typedef typename _Alloc::template rebind<_Tp>::other _Alloc2;
          _Deleter<_Alloc2> __del = { _Alloc2(__a) };
          _M_ptr = __del._M_alloc.allocate(1);
	  __try
	    {
              __del._M_alloc.construct(_M_ptr, std::forward<_Args>(__args)...);
	    }
	  __catch(...)
	    {
              __del._M_alloc.deallocate(_M_ptr, 1);
	      __throw_exception_again;
	    }
          __shared_count<_Lp> __count(_M_ptr, __del, __del._M_alloc);
          _M_refcount._M_swap(__count);
	  __enable_shared_from_this_helper(_M_refcount, _M_ptr, _M_ptr);
        }
#endif

      template<typename _Tp1, _Lock_policy _Lp1, typename _Alloc,
	       typename... _Args>
	friend __shared_ptr<_Tp1, _Lp1>
	__allocate_shared(const _Alloc& __a, _Args&&... __args);

    private:
      void*
      _M_get_deleter(const std::type_info& __ti) const
      { return _M_refcount._M_get_deleter(__ti); }

      template<typename _Tp1, _Lock_policy _Lp1> friend class __shared_ptr;
      template<typename _Tp1, _Lock_policy _Lp1> friend class __weak_ptr;

      template<typename _Del, typename _Tp1, _Lock_policy _Lp1>
	friend _Del* get_deleter(const __shared_ptr<_Tp1, _Lp1>&);

      _Tp*	   	   _M_ptr;         // Contained pointer.
      __shared_count<_Lp>  _M_refcount;    // Reference counter.
    };


  // 20.8.13.2.7 shared_ptr comparisons
  template<typename _Tp1, typename _Tp2, _Lock_policy _Lp>
    inline bool
    operator==(const __shared_ptr<_Tp1, _Lp>& __a,
	       const __shared_ptr<_Tp2, _Lp>& __b)
    { return __a.get() == __b.get(); }

  template<typename _Tp, _Lock_policy _Lp>
    inline bool
    operator==(const __shared_ptr<_Tp, _Lp>& __a, nullptr_t)
    { return __a.get() == nullptr; }

  template<typename _Tp, _Lock_policy _Lp>
    inline bool
    operator==(nullptr_t, const __shared_ptr<_Tp, _Lp>& __b)
    { return nullptr == __b.get(); }

  template<typename _Tp1, typename _Tp2, _Lock_policy _Lp>
    inline bool
    operator!=(const __shared_ptr<_Tp1, _Lp>& __a,
	       const __shared_ptr<_Tp2, _Lp>& __b)
    { return __a.get() != __b.get(); }

  template<typename _Tp, _Lock_policy _Lp>
    inline bool
    operator!=(const __shared_ptr<_Tp, _Lp>& __a, nullptr_t)
    { return __a.get() != nullptr; }

  template<typename _Tp, _Lock_policy _Lp>
    inline bool
    operator!=(nullptr_t, const __shared_ptr<_Tp, _Lp>& __b)
    { return nullptr != __b.get(); }

  template<typename _Tp1, typename _Tp2, _Lock_policy _Lp>
    inline bool
    operator<(const __shared_ptr<_Tp1, _Lp>& __a,
	      const __shared_ptr<_Tp2, _Lp>& __b)
    { return __a.get() < __b.get(); }

  template<typename _Sp>
    struct _Sp_less : public binary_function<_Sp, _Sp, bool>
    {
      bool
      operator()(const _Sp& __lhs, const _Sp& __rhs) const
      {
	typedef typename _Sp::element_type element_type;
	return std::less<element_type*>()(__lhs.get(), __rhs.get());
      }
    };

  template<typename _Tp, _Lock_policy _Lp>
    struct less<__shared_ptr<_Tp, _Lp>>
    : public _Sp_less<__shared_ptr<_Tp, _Lp>>
    { };

  // 2.2.3.8 shared_ptr specialized algorithms.
  template<typename _Tp, _Lock_policy _Lp>
    inline void
    swap(__shared_ptr<_Tp, _Lp>& __a, __shared_ptr<_Tp, _Lp>& __b)
    { __a.swap(__b); }

  // 2.2.3.9 shared_ptr casts

  // The seemingly equivalent code:
  // shared_ptr<_Tp, _Lp>(static_cast<_Tp*>(__r.get()))
  // will eventually result in undefined behaviour, attempting to
  // delete the same object twice.
  /// static_pointer_cast
  template<typename _Tp, typename _Tp1, _Lock_policy _Lp>
    inline __shared_ptr<_Tp, _Lp>
    static_pointer_cast(const __shared_ptr<_Tp1, _Lp>& __r)
    { return __shared_ptr<_Tp, _Lp>(__r, static_cast<_Tp*>(__r.get())); }

  // The seemingly equivalent code:
  // shared_ptr<_Tp, _Lp>(const_cast<_Tp*>(__r.get()))
  // will eventually result in undefined behaviour, attempting to
  // delete the same object twice.
  /// const_pointer_cast
  template<typename _Tp, typename _Tp1, _Lock_policy _Lp>
    inline __shared_ptr<_Tp, _Lp>
    const_pointer_cast(const __shared_ptr<_Tp1, _Lp>& __r)
    { return __shared_ptr<_Tp, _Lp>(__r, const_cast<_Tp*>(__r.get())); }

  // The seemingly equivalent code:
  // shared_ptr<_Tp, _Lp>(dynamic_cast<_Tp*>(__r.get()))
  // will eventually result in undefined behaviour, attempting to
  // delete the same object twice.
  /// dynamic_pointer_cast
  template<typename _Tp, typename _Tp1, _Lock_policy _Lp>
    inline __shared_ptr<_Tp, _Lp>
    dynamic_pointer_cast(const __shared_ptr<_Tp1, _Lp>& __r)
    {
      if (_Tp* __p = dynamic_cast<_Tp*>(__r.get()))
	return __shared_ptr<_Tp, _Lp>(__r, __p);
      return __shared_ptr<_Tp, _Lp>();
    }


  template<typename _Tp, _Lock_policy _Lp>
    class __weak_ptr
    {
    public:
      typedef _Tp element_type;

      constexpr __weak_ptr()
      : _M_ptr(0), _M_refcount() // never throws
      { }

      // Generated copy constructor, assignment, destructor are fine.

      // The "obvious" converting constructor implementation:
      //
      //  template<typename _Tp1>
      //    __weak_ptr(const __weak_ptr<_Tp1, _Lp>& __r)
      //    : _M_ptr(__r._M_ptr), _M_refcount(__r._M_refcount) // never throws
      //    { }
      //
      // has a serious problem.
      //
      //  __r._M_ptr may already have been invalidated. The _M_ptr(__r._M_ptr)
      //  conversion may require access to *__r._M_ptr (virtual inheritance).
      //
      // It is not possible to avoid spurious access violations since
      // in multithreaded programs __r._M_ptr may be invalidated at any point.
      template<typename _Tp1, typename = typename
	       std::enable_if<std::is_convertible<_Tp1*, _Tp*>::value>::type>
	__weak_ptr(const __weak_ptr<_Tp1, _Lp>& __r)
	: _M_refcount(__r._M_refcount) // never throws
        { _M_ptr = __r.lock().get(); }

      template<typename _Tp1, typename = typename
	       std::enable_if<std::is_convertible<_Tp1*, _Tp*>::value>::type>
	__weak_ptr(const __shared_ptr<_Tp1, _Lp>& __r)
	: _M_ptr(__r._M_ptr), _M_refcount(__r._M_refcount) // never throws
	{ }

      template<typename _Tp1>
	__weak_ptr&
	operator=(const __weak_ptr<_Tp1, _Lp>& __r) // never throws
	{
	  _M_ptr = __r.lock().get();
	  _M_refcount = __r._M_refcount;
	  return *this;
	}

      template<typename _Tp1>
	__weak_ptr&
	operator=(const __shared_ptr<_Tp1, _Lp>& __r) // never throws
	{
	  _M_ptr = __r._M_ptr;
	  _M_refcount = __r._M_refcount;
	  return *this;
	}

      __shared_ptr<_Tp, _Lp>
      lock() const // never throws
      {
#ifdef __GTHREADS
	// Optimization: avoid throw overhead.
	if (expired())
	  return __shared_ptr<element_type, _Lp>();

	__try
	  {
	    return __shared_ptr<element_type, _Lp>(*this);
	  }
	__catch(const bad_weak_ptr&)
	  {
	    // Q: How can we get here?
	    // A: Another thread may have invalidated r after the
	    //    use_count test above.
	    return __shared_ptr<element_type, _Lp>();
	  }

#else
	// Optimization: avoid try/catch overhead when single threaded.
	return expired() ? __shared_ptr<element_type, _Lp>()
			 : __shared_ptr<element_type, _Lp>(*this);

#endif
      } // XXX MT

      long
      use_count() const // never throws
      { return _M_refcount._M_get_use_count(); }

      bool
      expired() const // never throws
      { return _M_refcount._M_get_use_count() == 0; }

      template<typename _Tp1>
	bool
	owner_before(const __shared_ptr<_Tp1, _Lp>& __rhs) const
	{ return _M_refcount._M_less(__rhs._M_refcount); }

      template<typename _Tp1>
	bool
	owner_before(const __weak_ptr<_Tp1, _Lp>& __rhs) const
	{ return _M_refcount._M_less(__rhs._M_refcount); }

      void
      reset() // never throws
      { __weak_ptr().swap(*this); }

      void
      swap(__weak_ptr& __s) // never throws
      {
	std::swap(_M_ptr, __s._M_ptr);
	_M_refcount._M_swap(__s._M_refcount);
      }

    private:
      // Used by __enable_shared_from_this.
      void
      _M_assign(_Tp* __ptr, const __shared_count<_Lp>& __refcount)
      {
	_M_ptr = __ptr;
	_M_refcount = __refcount;
      }

      template<typename _Tp1, _Lock_policy _Lp1> friend class __shared_ptr;
      template<typename _Tp1, _Lock_policy _Lp1> friend class __weak_ptr;
      friend class __enable_shared_from_this<_Tp, _Lp>;
      friend class enable_shared_from_this<_Tp>;

      _Tp*	 	 _M_ptr;         // Contained pointer.
      __weak_count<_Lp>  _M_refcount;    // Reference counter.
    };

  // 20.8.13.3.7 weak_ptr specialized algorithms.
  template<typename _Tp, _Lock_policy _Lp>
    inline void
    swap(__weak_ptr<_Tp, _Lp>& __a, __weak_ptr<_Tp, _Lp>& __b)
    { __a.swap(__b); }

  template<typename _Tp, typename _Tp1>
    struct _Sp_owner_less : public binary_function<_Tp, _Tp, bool>
    {
      bool
      operator()(const _Tp& __lhs, const _Tp& __rhs) const
      { return __lhs.owner_before(__rhs); }

      bool
      operator()(const _Tp& __lhs, const _Tp1& __rhs) const
      { return __lhs.owner_before(__rhs); }

      bool
      operator()(const _Tp1& __lhs, const _Tp& __rhs) const
      { return __lhs.owner_before(__rhs); }
    };

  template<typename _Tp, _Lock_policy _Lp>
    struct owner_less<__shared_ptr<_Tp, _Lp>>
    : public _Sp_owner_less<__shared_ptr<_Tp, _Lp>, __weak_ptr<_Tp, _Lp>>
    { };

  template<typename _Tp, _Lock_policy _Lp>
    struct owner_less<__weak_ptr<_Tp, _Lp>>
    : public _Sp_owner_less<__weak_ptr<_Tp, _Lp>, __shared_ptr<_Tp, _Lp>>
    { };


  template<typename _Tp, _Lock_policy _Lp>
    class __enable_shared_from_this
    {
    protected:
      constexpr __enable_shared_from_this() { }

      __enable_shared_from_this(const __enable_shared_from_this&) { }

      __enable_shared_from_this&
      operator=(const __enable_shared_from_this&)
      { return *this; }

      ~__enable_shared_from_this() { }

    public:
      __shared_ptr<_Tp, _Lp>
      shared_from_this()
      { return __shared_ptr<_Tp, _Lp>(this->_M_weak_this); }

      __shared_ptr<const _Tp, _Lp>
      shared_from_this() const
      { return __shared_ptr<const _Tp, _Lp>(this->_M_weak_this); }

    private:
      template<typename _Tp1>
	void
	_M_weak_assign(_Tp1* __p, const __shared_count<_Lp>& __n) const
	{ _M_weak_this._M_assign(__p, __n); }

      template<typename _Tp1>
	friend void
	__enable_shared_from_this_helper(const __shared_count<_Lp>& __pn,
					 const __enable_shared_from_this* __pe,
					 const _Tp1* __px)
	{
	  if (__pe != 0)
	    __pe->_M_weak_assign(const_cast<_Tp1*>(__px), __pn);
	}

      mutable __weak_ptr<_Tp, _Lp>  _M_weak_this;
    };


  template<typename _Tp, _Lock_policy _Lp, typename _Alloc, typename... _Args>
    inline __shared_ptr<_Tp, _Lp>
    __allocate_shared(const _Alloc& __a, _Args&&... __args)
    {
      return __shared_ptr<_Tp, _Lp>(_Sp_make_shared_tag(), __a,
				    std::forward<_Args>(__args)...);
    }

  template<typename _Tp, _Lock_policy _Lp, typename... _Args>
    inline __shared_ptr<_Tp, _Lp>
    __make_shared(_Args&&... __args)
    {
      typedef typename std::remove_const<_Tp>::type _Tp_nc;
      return std::__allocate_shared<_Tp, _Lp>(std::allocator<_Tp_nc>(),
					      std::forward<_Args>(__args)...);
    }

  /// std::hash specialization for __shared_ptr.
  template<typename _Tp, _Lock_policy _Lp>
    struct hash<__shared_ptr<_Tp, _Lp>>
    : public std::unary_function<__shared_ptr<_Tp, _Lp>, size_t>
    {
      size_t
      operator()(const __shared_ptr<_Tp, _Lp>& __s) const
      { return std::hash<_Tp*>()(__s.get()); }
    };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif // _SHARED_PTR_BASE_H
