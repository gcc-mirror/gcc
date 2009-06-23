// shared_ptr and weak_ptr implementation -*- C++ -*-

// Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.
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

// GCC Note:  based on version 1.32.0 of the Boost library.

/** @file bits/shared_ptr.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _SHARED_PTR_H
#define _SHARED_PTR_H 1

#ifndef __GXX_EXPERIMENTAL_CXX0X__
# include <c++0x_warning.h>
#endif

#if defined(_GLIBCXX_INCLUDE_AS_TR1)
#  error C++0x header cannot be included from TR1 header
#endif

_GLIBCXX_BEGIN_NAMESPACE(std)

  /**
   * @addtogroup pointer_abstractions
   * @{
   */

  // counted ptr with no deleter or allocator support
  template<typename _Ptr, _Lock_policy _Lp>
    class _Sp_counted_ptr
    : public _Sp_counted_base<_Lp>
    {
    public:
      _Sp_counted_ptr(_Ptr __p)
      : _M_ptr(__p) { }
    
      virtual void
      _M_dispose() // nothrow
      { delete _M_ptr; }
      
      virtual void
      _M_destroy() // nothrow
      { delete this; }
      
      virtual void*
      _M_get_deleter(const std::type_info& __ti)
      { return 0; }
      
      _Sp_counted_ptr(const _Sp_counted_ptr&) = delete;
      _Sp_counted_ptr& operator=(const _Sp_counted_ptr&) = delete;
      
    protected:
      _Ptr             _M_ptr;  // copy constructor must not throw
    };

  // support for custom deleter and/or allocator
  template<typename _Ptr, typename _Deleter, typename _Alloc, _Lock_policy _Lp>
    class _Sp_counted_deleter
    : public _Sp_counted_ptr<_Ptr, _Lp>
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

    protected:
      typedef _Sp_counted_ptr<_Ptr, _Lp> _Base_type;

    public:
      /**
       *  @brief   
       *  @pre     __d(__p) must not throw.
       */
      _Sp_counted_deleter(_Ptr __p, _Deleter __d)
      : _Base_type(__p), _M_del(__d, _Alloc()) { }
    
      /**
       *  @brief   
       *  @pre     __d(__p) must not throw.
       */
      _Sp_counted_deleter(_Ptr __p, _Deleter __d, const _Alloc& __a)
      : _Base_type(__p), _M_del(__d, __a) { }
    
      virtual void
      _M_dispose() // nothrow
      { _M_del._M_del(_Base_type::_M_ptr); }
      
      virtual void
      _M_destroy() // nothrow
      {
        _My_alloc_type __a(_M_del);
        this->~_Sp_counted_deleter();
        __a.deallocate(this, 1);
      }
      
      virtual void*
      _M_get_deleter(const std::type_info& __ti)
      { return __ti == typeid(_Deleter) ? &_M_del._M_del : 0; }
      
    protected:
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
      _Sp_counted_ptr_inplace(_Alloc __a)
      : _Base_type(static_cast<_Tp*>(0), _Sp_destroy_inplace<_Tp>(), __a)
      , _M_storage()
      {
        void* __p = &_M_storage;
        ::new (__p) _Tp();  // might throw
        _Base_type::_Base_type::_M_ptr = static_cast<_Tp*>(__p);
      }

      template<typename... _Args>
        _Sp_counted_ptr_inplace(_Alloc __a, _Args&&... __args)
        : _Base_type(static_cast<_Tp*>(0), _Sp_destroy_inplace<_Tp>(), __a)
        , _M_storage()
        {
          void* __p = &_M_storage;
          ::new (__p) _Tp(std::forward<_Args>(__args)...);  // might throw
          _Base_type::_Base_type::_M_ptr = static_cast<_Tp*>(__p);
        }

      // override because the allocator needs to know the dynamic type
      virtual void
      _M_destroy() // nothrow
      {
        typedef typename _Alloc::template
            rebind<_Sp_counted_ptr_inplace>::other _My_alloc_type;
        _My_alloc_type __a(_Base_type::_M_del);
        this->~_Sp_counted_ptr_inplace();
        __a.deallocate(this, 1);
      }

      // sneaky trick so __shared_ptr can get the managed pointer
      virtual void*
      _M_get_deleter(const std::type_info& __ti)
      {
        return __ti == typeid(_Sp_make_shared_tag)
               ? static_cast<void*>(&_M_storage)
               : _Base_type::_M_get_deleter(__ti);
      }
      
    private:
      typename aligned_storage<sizeof(_Tp), alignment_of<_Tp>::value>::type
        _M_storage;
    };

  template<_Lock_policy _Lp = __default_lock_policy>
    class __weak_count;

  template<_Lock_policy _Lp = __default_lock_policy>
    class __shared_count
    {
    public: 
      __shared_count()
      : _M_pi(0) // nothrow
      { }
  
      template<typename _Ptr>
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
          // allocator's value_type doesn't matter, will rebind it anyway
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
        __shared_count(_Sp_make_shared_tag, _Tp*, _Alloc __a, _Args&&... __args)
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

#if _GLIBCXX_DEPRECATED
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
      explicit
      __shared_count(const __weak_count<_Lp>& __r);
  
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

      // friend function injected into enclosing namespace and found by ADL
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
      __weak_count()
      : _M_pi(0) // nothrow
      { }
  
      __weak_count(const __shared_count<_Lp>& __r)
      : _M_pi(__r._M_pi) // nothrow
      {
	if (_M_pi != 0)
	  _M_pi->_M_weak_add_ref();
      }
      
      __weak_count(const __weak_count<_Lp>& __r)
      : _M_pi(__r._M_pi) // nothrow
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

      // friend function injected into enclosing namespace and found by ADL
      friend inline bool
      operator==(const __weak_count& __a, const __weak_count& __b)
      { return __a._M_pi == __b._M_pi; }

    private:
      friend class __shared_count<_Lp>;

      _Sp_counted_base<_Lp>*  _M_pi;
    };

  // now that __weak_count is defined we can define this constructor:
  template<_Lock_policy _Lp>
    inline
    __shared_count<_Lp>::
    __shared_count(const __weak_count<_Lp>& __r)
    : _M_pi(__r._M_pi)
    {
      if (_M_pi != 0)
	_M_pi->_M_add_ref_lock();
      else
	__throw_bad_weak_ptr();
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
    class enable_shared_from_this;

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
      
      /** @brief  Construct an empty %__shared_ptr.
       *  @post   use_count()==0 && get()==0
       */
      __shared_ptr()
      : _M_ptr(0), _M_refcount() // never throws
      { }

      /** @brief  Construct a %__shared_ptr that owns the pointer @a __p.
       *  @param  __p  A pointer that is convertible to element_type*.
       *  @post   use_count() == 1 && get() == __p
       *  @throw  std::bad_alloc, in which case @c delete @a __p is called.
       */
      template<typename _Tp1>
        explicit
        __shared_ptr(_Tp1* __p)
	: _M_ptr(__p), _M_refcount(__p)
        {
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  // __glibcxx_function_requires(_CompleteConcept<_Tp1*>)
	  __enable_shared_from_this_helper(_M_refcount, __p, __p);
	}

      //
      // Requirements: _Deleter's copy constructor and destructor must
      // not throw
      //
      // __shared_ptr will release __p by calling __d(__p)
      //
      /** @brief  Construct a %__shared_ptr that owns the pointer @a __p
       *          and the deleter @a __d.
       *  @param  __p  A pointer.
       *  @param  __d  A deleter.
       *  @post   use_count() == 1 && get() == __p
       *  @throw  std::bad_alloc, in which case @a __d(__p) is called.
       */
      template<typename _Tp1, typename _Deleter>
        __shared_ptr(_Tp1* __p, _Deleter __d)
        : _M_ptr(__p), _M_refcount(__p, __d)
        {
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  // TODO requires _Deleter CopyConstructible and __d(__p) well-formed
	  __enable_shared_from_this_helper(_M_refcount, __p, __p);
	}
      
      //
      // Requirements: _Deleter's copy constructor and destructor must
      // not throw _Alloc's copy constructor and destructor must not
      // throw.
      //
      // __shared_ptr will release __p by calling __d(__p)
      //
      /** @brief  Construct a %__shared_ptr that owns the pointer @a __p
       *          and the deleter @a __d.
       *  @param  __p  A pointer.
       *  @param  __d  A deleter.
       *  @param  __a  An allocator.
       *  @post   use_count() == 1 && get() == __p
       *  @throw  std::bad_alloc, in which case @a __d(__p) is called.
       */
      template<typename _Tp1, typename _Deleter, typename _Alloc>
        __shared_ptr(_Tp1* __p, _Deleter __d, const _Alloc& __a)
	: _M_ptr(__p), _M_refcount(__p, __d, __a)
        {
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  // TODO requires _Deleter CopyConstructible and __d(__p) well-formed
	  __enable_shared_from_this_helper(_M_refcount, __p, __p);
	}

      /** @brief  Constructs a %__shared_ptr instance that stores @a __p
       *          and shares ownership with @a __r.
       *  @param  __r  A %__shared_ptr.
       *  @param  __p  A pointer that will remain valid while @a *__r is valid.
       *  @post   get() == __p && use_count() == __r.use_count()
       *
       *  This can be used to construct a @c shared_ptr to a sub-object
       *  of an object managed by an existing @c shared_ptr.
       *
       * @code
       * shared_ptr< pair<int,int> > pii(new pair<int,int>());
       * shared_ptr<int> pi(pii, &pii->first);
       * assert(pii.use_count() == 2);
       * @endcode
       */
      template<typename _Tp1>
        __shared_ptr(const __shared_ptr<_Tp1, _Lp>& __r, _Tp* __p)
	: _M_ptr(__p), _M_refcount(__r._M_refcount) // never throws
        { }

      //  generated copy constructor, assignment, destructor are fine.
      
      /** @brief  If @a __r is empty, constructs an empty %__shared_ptr;
       *          otherwise construct a %__shared_ptr that shares ownership
       *          with @a __r.
       *  @param  __r  A %__shared_ptr.
       *  @post   get() == __r.get() && use_count() == __r.use_count()
       */
      template<typename _Tp1>
        __shared_ptr(const __shared_ptr<_Tp1, _Lp>& __r)
	: _M_ptr(__r._M_ptr), _M_refcount(__r._M_refcount) // never throws
        { __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>) }

      /** @brief  Move-constructs a %__shared_ptr instance from @a __r.
       *  @param  __r  A %__shared_ptr rvalue.
       *  @post   *this contains the old value of @a __r, @a __r is empty.
       */
      __shared_ptr(__shared_ptr&& __r)
      : _M_ptr(__r._M_ptr), _M_refcount() // never throws
      {
        _M_refcount._M_swap(__r._M_refcount);
        __r._M_ptr = 0;
      }

      /** @brief  Move-constructs a %__shared_ptr instance from @a __r.
       *  @param  __r  A %__shared_ptr rvalue.
       *  @post   *this contains the old value of @a __r, @a __r is empty.
       */
      template<typename _Tp1>
        __shared_ptr(__shared_ptr<_Tp1, _Lp>&& __r)
	: _M_ptr(__r._M_ptr), _M_refcount() // never throws
        {
          __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
          _M_refcount._M_swap(__r._M_refcount);
          __r._M_ptr = 0;
        }

      /** @brief  Constructs a %__shared_ptr that shares ownership with @a __r
       *          and stores a copy of the pointer stored in @a __r.
       *  @param  __r  A weak_ptr.
       *  @post   use_count() == __r.use_count()
       *  @throw  bad_weak_ptr when __r.expired(),
       *          in which case the constructor has no effect.
       */
      template<typename _Tp1>
        explicit
        __shared_ptr(const __weak_ptr<_Tp1, _Lp>& __r)
	: _M_refcount(__r._M_refcount) // may throw
        {
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  // It is now safe to copy __r._M_ptr, as _M_refcount(__r._M_refcount)
	  // did not throw.
	  _M_ptr = __r._M_ptr;
	}

      template<typename _Tp1, typename _Del>
        explicit
        __shared_ptr(const std::unique_ptr<_Tp1, _Del>&) = delete;

      /**
       * If an exception is thrown this constructor has no effect.
       */
      template<typename _Tp1, typename _Del>
        explicit
        __shared_ptr(std::unique_ptr<_Tp1, _Del>&& __r)
	: _M_ptr(__r.get()), _M_refcount()
        {
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  _Tp1* __tmp = __r.get();
	  _M_refcount = __shared_count<_Lp>(std::move(__r));
	  __enable_shared_from_this_helper(_M_refcount, __tmp, __tmp);
	}

#if _GLIBCXX_DEPRECATED
      /**
       * @post use_count() == 1 and __r.get() == 0
       */
      template<typename _Tp1>
        explicit
        __shared_ptr(std::auto_ptr<_Tp1>&& __r)
	: _M_ptr(__r.get()), _M_refcount()
        {
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  // TODO requires _Tp1 is complete, delete __r.release() well-formed
	  _Tp1* __tmp = __r.get();
	  _M_refcount = __shared_count<_Lp>(std::move(__r));
	  __enable_shared_from_this_helper(_M_refcount, __tmp, __tmp);
	}
#endif

      template<typename _Tp1>
        __shared_ptr&
        operator=(const __shared_ptr<_Tp1, _Lp>& __r) // never throws
        {
	  _M_ptr = __r._M_ptr;
	  _M_refcount = __r._M_refcount; // __shared_count::op= doesn't throw
	  return *this;
	}

#if _GLIBCXX_DEPRECATED
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
        operator=(const std::unique_ptr<_Tp1, _Del>& __r) = delete;

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
        reset(_Tp1* __p, _Deleter __d, const _Alloc& __a)
        { __shared_ptr(__p, __d, __a).swap(*this); }

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

    protected:
      // This constructor is non-standard, it is used by allocate_shared.
      template<typename _Alloc, typename... _Args>
        __shared_ptr(_Sp_make_shared_tag __tag, _Alloc __a, _Args&&... __args)
        : _M_ptr(), _M_refcount(__tag, (_Tp*)0, __a,
				std::forward<_Args>(__args)...)
        {
          // _M_ptr needs to point to the newly constructed object.
          // This relies on _Sp_counted_ptr_inplace::_M_get_deleter.
          void* __p = _M_refcount._M_get_deleter(typeid(__tag));
          _M_ptr = static_cast<_Tp*>(__p);
	  __enable_shared_from_this_helper(_M_refcount, _M_ptr, _M_ptr);
        }

      template<typename _Tp1, _Lock_policy _Lp1, typename _Alloc,
               typename... _Args>
        friend __shared_ptr<_Tp1, _Lp1>
        __allocate_shared(_Alloc __a, _Args&&... __args);

    private:
      void*
      _M_get_deleter(const std::type_info& __ti) const
      { return _M_refcount._M_get_deleter(__ti); }

      template<typename _Tp1, _Lock_policy _Lp1> friend class __shared_ptr;
      template<typename _Tp1, _Lock_policy _Lp1> friend class __weak_ptr;

      template<typename _Del, typename _Tp1, _Lock_policy _Lp1>
        friend _Del* get_deleter(const __shared_ptr<_Tp1, _Lp1>&);

      _Tp*         	   _M_ptr;         // Contained pointer.
      __shared_count<_Lp>  _M_refcount;    // Reference counter.
    };

  // 20.8.13.2.7 shared_ptr comparisons
  template<typename _Tp1, typename _Tp2, _Lock_policy _Lp>
    inline bool
    operator==(const __shared_ptr<_Tp1, _Lp>& __a,
        const __shared_ptr<_Tp2, _Lp>& __b)
    { return __a.get() == __b.get(); }

  template<typename _Tp1, typename _Tp2, _Lock_policy _Lp>
    inline bool
    operator!=(const __shared_ptr<_Tp1, _Lp>& __a,
        const __shared_ptr<_Tp2, _Lp>& __b)
    { return __a.get() != __b.get(); }

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
        return std::less<typename _Sp::element_type*>()(__lhs.get(),
            __rhs.get());
      }
    };

  template<typename _Tp, _Lock_policy _Lp>
    struct less<__shared_ptr<_Tp, _Lp>>
    : public _Sp_less<__shared_ptr<_Tp, _Lp>>
    { };

  // XXX LessThanComparable<_Tp> concept should provide >, >= and <=
  template<typename _Tp, _Lock_policy _Lp>
    inline bool
    operator>(const __shared_ptr<_Tp, _Lp>& __a,
        const __shared_ptr<_Tp, _Lp>& __b)
    { return __a.get() > __b.get(); }

  template<typename _Tp, _Lock_policy _Lp>
    inline bool
    operator>=(const __shared_ptr<_Tp, _Lp>& __a,
        const __shared_ptr<_Tp, _Lp>& __b)
    { return __a.get() >= __b.get(); }

  template<typename _Tp, _Lock_policy _Lp>
    inline bool
    operator<=(const __shared_ptr<_Tp, _Lp>& __a,
        const __shared_ptr<_Tp, _Lp>& __b)
    { return __a.get() <= __b.get(); }

  // 2.2.3.8 shared_ptr specialized algorithms.
  template<typename _Tp, _Lock_policy _Lp>
    inline void
    swap(__shared_ptr<_Tp, _Lp>& __a, __shared_ptr<_Tp, _Lp>& __b)
    { __a.swap(__b); }

  // 2.2.3.9 shared_ptr casts
  /** @warning The seemingly equivalent
   *           <code>shared_ptr<_Tp, _Lp>(static_cast<_Tp*>(__r.get()))</code>
   *           will eventually result in undefined behaviour,
   *           attempting to delete the same object twice.
   */
  template<typename _Tp, typename _Tp1, _Lock_policy _Lp>
    inline __shared_ptr<_Tp, _Lp>
    static_pointer_cast(const __shared_ptr<_Tp1, _Lp>& __r)
    { return __shared_ptr<_Tp, _Lp>(__r, static_cast<_Tp*>(__r.get())); }

  /** @warning The seemingly equivalent
   *           <code>shared_ptr<_Tp, _Lp>(const_cast<_Tp*>(__r.get()))</code>
   *           will eventually result in undefined behaviour,
   *           attempting to delete the same object twice.
   */
  template<typename _Tp, typename _Tp1, _Lock_policy _Lp>
    inline __shared_ptr<_Tp, _Lp>
    const_pointer_cast(const __shared_ptr<_Tp1, _Lp>& __r)
    { return __shared_ptr<_Tp, _Lp>(__r, const_cast<_Tp*>(__r.get())); }

  /** @warning The seemingly equivalent
   *           <code>shared_ptr<_Tp, _Lp>(dynamic_cast<_Tp*>(__r.get()))</code>
   *           will eventually result in undefined behaviour,
   *           attempting to delete the same object twice.
   */
  template<typename _Tp, typename _Tp1, _Lock_policy _Lp>
    inline __shared_ptr<_Tp, _Lp>
    dynamic_pointer_cast(const __shared_ptr<_Tp1, _Lp>& __r)
    {
      if (_Tp* __p = dynamic_cast<_Tp*>(__r.get()))
        return __shared_ptr<_Tp, _Lp>(__r, __p);
      return __shared_ptr<_Tp, _Lp>();
    }

  // 2.2.3.7 shared_ptr I/O
  template<typename _Ch, typename _Tr, typename _Tp, _Lock_policy _Lp>
    std::basic_ostream<_Ch, _Tr>&
    operator<<(std::basic_ostream<_Ch, _Tr>& __os, 
	       const __shared_ptr<_Tp, _Lp>& __p)
    {
      __os << __p.get();
      return __os;
    }

  // 2.2.3.10 shared_ptr get_deleter (experimental)
  template<typename _Del, typename _Tp, _Lock_policy _Lp>
    inline _Del*
    get_deleter(const __shared_ptr<_Tp, _Lp>& __p)
    { return static_cast<_Del*>(__p._M_get_deleter(typeid(_Del))); }


  template<typename _Tp, _Lock_policy _Lp>
    class __weak_ptr
    {
    public:
      typedef _Tp element_type;
      
      __weak_ptr()
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
      template<typename _Tp1>
        __weak_ptr(const __weak_ptr<_Tp1, _Lp>& __r)
	: _M_refcount(__r._M_refcount) // never throws
        {
	  __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
	  _M_ptr = __r.lock().get();
	}

      template<typename _Tp1>
        __weak_ptr(const __shared_ptr<_Tp1, _Lp>& __r)
	: _M_ptr(__r._M_ptr), _M_refcount(__r._M_refcount) // never throws
        { __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>) }

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

      // comparisons
      template<typename _Tp1>
        bool operator<(const __weak_ptr<_Tp1, _Lp>&) const = delete;
      template<typename _Tp1>
        bool operator<=(const __weak_ptr<_Tp1, _Lp>&) const = delete;
      template<typename _Tp1>
        bool operator>(const __weak_ptr<_Tp1, _Lp>&) const = delete;
      template<typename _Tp1>
        bool operator>=(const __weak_ptr<_Tp1, _Lp>&) const = delete;

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

      _Tp*       	 _M_ptr;         // Contained pointer.
      __weak_count<_Lp>  _M_refcount;    // Reference counter.
    };

  // 20.8.13.3.7 weak_ptr specialized algorithms.
  template<typename _Tp, _Lock_policy _Lp>
    inline void
    swap(__weak_ptr<_Tp, _Lp>& __a, __weak_ptr<_Tp, _Lp>& __b)
    { __a.swap(__b); }

  /// owner_less
  template<typename _Tp> struct owner_less;

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
    {
    };


  template<typename _Tp, _Lock_policy _Lp>
    class __enable_shared_from_this
    {
    protected:
      __enable_shared_from_this() { }
      
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

  /**
   *  @brief A smart pointer with reference-counted copy semantics. 
   *  
   *  The object pointed to is deleted when the last shared_ptr pointing to
   *  it is destroyed or reset.
   */
  template<typename _Tp>
    class shared_ptr
    : public __shared_ptr<_Tp>
    {
    public:
      shared_ptr()
      : __shared_ptr<_Tp>() { }

      template<typename _Tp1>
        explicit
        shared_ptr(_Tp1* __p)
	: __shared_ptr<_Tp>(__p) { }

      template<typename _Tp1, typename _Deleter>
        shared_ptr(_Tp1* __p, _Deleter __d)
	: __shared_ptr<_Tp>(__p, __d) { }

      template<typename _Tp1, typename _Deleter, typename _Alloc>
        shared_ptr(_Tp1* __p, _Deleter __d, const _Alloc& __a)
	: __shared_ptr<_Tp>(__p, __d, __a) { }

      // Aliasing constructor
      template<typename _Tp1>
        shared_ptr(const shared_ptr<_Tp1>& __r, _Tp* __p)
	: __shared_ptr<_Tp>(__r, __p) { }

      template<typename _Tp1>
        shared_ptr(const shared_ptr<_Tp1>& __r)
	: __shared_ptr<_Tp>(__r) { }

      shared_ptr(shared_ptr&& __r)
      : __shared_ptr<_Tp>(std::move(__r)) { }

      template<typename _Tp1>
        shared_ptr(shared_ptr<_Tp1>&& __r)
        : __shared_ptr<_Tp>(std::move(__r)) { }

      template<typename _Tp1>
        explicit
        shared_ptr(const weak_ptr<_Tp1>& __r)
	: __shared_ptr<_Tp>(__r) { }

#if _GLIBCXX_DEPRECATED
      template<typename _Tp1>
        explicit
        shared_ptr(std::auto_ptr<_Tp1>&& __r)
	: __shared_ptr<_Tp>(std::move(__r)) { }
#endif

      template<typename _Tp1, typename _Del>
        explicit
        shared_ptr(const std::unique_ptr<_Tp1, _Del>&) = delete;

      template<typename _Tp1, typename _Del>
        explicit
        shared_ptr(std::unique_ptr<_Tp1, _Del>&& __r)
	: __shared_ptr<_Tp>(std::move(__r)) { }

      template<typename _Tp1>
        shared_ptr&
        operator=(const shared_ptr<_Tp1>& __r) // never throws
        {
	  this->__shared_ptr<_Tp>::operator=(__r);
	  return *this;
	}

#if _GLIBCXX_DEPRECATED
      template<typename _Tp1>
        shared_ptr&
        operator=(std::auto_ptr<_Tp1>&& __r)
        {
	  this->__shared_ptr<_Tp>::operator=(std::move(__r));
	  return *this;
	}
#endif

      shared_ptr&
      operator=(shared_ptr&& __r)
      {
        this->__shared_ptr<_Tp>::operator=(std::move(__r));
        return *this;
      }
     
      template<class _Tp1>
        shared_ptr&
        operator=(shared_ptr<_Tp1>&& __r)
        {
          this->__shared_ptr<_Tp>::operator=(std::move(__r));
          return *this;
        }

      template<typename _Tp1, typename _Del>
        shared_ptr&
        operator=(const std::unique_ptr<_Tp1, _Del>& __r) = delete;

      template<typename _Tp1, typename _Del>
        shared_ptr&
        operator=(std::unique_ptr<_Tp1, _Del>&& __r)
        {
	  this->__shared_ptr<_Tp>::operator=(std::move(__r));
	  return *this;
	}

    private:
      // This constructor is non-standard, it is used by allocate_shared.
      template<typename _Alloc, typename... _Args>
        shared_ptr(_Sp_make_shared_tag __tag, _Alloc __a, _Args&&... __args)
        : __shared_ptr<_Tp>(__tag, __a, std::forward<_Args>(__args)...)
        { }

      template<typename _Tp1, typename _Alloc, typename... _Args>
        friend shared_ptr<_Tp1>
        allocate_shared(_Alloc __a, _Args&&... __args);
    };

  // 20.8.13.2.7 shared_ptr comparisons
  template<typename _Tp1, typename _Tp2>
    inline bool
    operator==(const shared_ptr<_Tp1>& __a, const shared_ptr<_Tp2>& __b)
    { return __a.get() == __b.get(); }

  template<typename _Tp1, typename _Tp2>
    inline bool
    operator!=(const shared_ptr<_Tp1>& __a, const shared_ptr<_Tp2>& __b)
    { return __a.get() != __b.get(); }

  template<typename _Tp1, typename _Tp2>
    inline bool
    operator<(const shared_ptr<_Tp1>& __a, const shared_ptr<_Tp2>& __b)
    { return __a.get() < __b.get(); }

  template<typename _Tp>
    struct less<shared_ptr<_Tp>>
    : public _Sp_less<shared_ptr<_Tp>>
    { };

  // 20.8.13.2.9 shared_ptr specialized algorithms.
  template<typename _Tp>
    inline void
    swap(shared_ptr<_Tp>& __a, shared_ptr<_Tp>& __b)
    { __a.swap(__b); }

  // 20.8.13.2.10 shared_ptr casts.
  template<typename _Tp, typename _Tp1>
    inline shared_ptr<_Tp>
    static_pointer_cast(const shared_ptr<_Tp1>& __r)
    { return shared_ptr<_Tp>(__r, static_cast<_Tp*>(__r.get())); }

  template<typename _Tp, typename _Tp1>
    inline shared_ptr<_Tp>
    const_pointer_cast(const shared_ptr<_Tp1>& __r)
    { return shared_ptr<_Tp>(__r, const_cast<_Tp*>(__r.get())); }

  template<typename _Tp, typename _Tp1>
    inline shared_ptr<_Tp>
    dynamic_pointer_cast(const shared_ptr<_Tp1>& __r)
    {
      if (_Tp* __p = dynamic_cast<_Tp*>(__r.get()))
        return shared_ptr<_Tp>(__r, __p);
      return shared_ptr<_Tp>();
    }


  /** 
   *  @brief A smart pointer with weak semantics.
   *  
   *  With forwarding constructors and assignment operators.
   */
  template<typename _Tp>
    class weak_ptr
    : public __weak_ptr<_Tp>
    {
    public:
      weak_ptr()
      : __weak_ptr<_Tp>() { }
      
      template<typename _Tp1>
        weak_ptr(const weak_ptr<_Tp1>& __r)
	: __weak_ptr<_Tp>(__r) { }

      template<typename _Tp1>
        weak_ptr(const shared_ptr<_Tp1>& __r)
	: __weak_ptr<_Tp>(__r) { }

      template<typename _Tp1>
        weak_ptr&
        operator=(const weak_ptr<_Tp1>& __r) // never throws
        {
	  this->__weak_ptr<_Tp>::operator=(__r);
	  return *this;
	}

      template<typename _Tp1>
        weak_ptr&
        operator=(const shared_ptr<_Tp1>& __r) // never throws
        {
	  this->__weak_ptr<_Tp>::operator=(__r);
	  return *this;
	}

      shared_ptr<_Tp>
      lock() const // never throws
      {
#ifdef __GTHREADS
	if (this->expired())
	  return shared_ptr<_Tp>();

	__try
	  {
	    return shared_ptr<_Tp>(*this);
	  }
	__catch(const bad_weak_ptr&)
	  {
	    return shared_ptr<_Tp>();
	  }
#else
	return this->expired() ? shared_ptr<_Tp>()
	                       : shared_ptr<_Tp>(*this);
#endif
      }

      // comparisons
      template<typename _Tp1>
        bool operator<(const weak_ptr<_Tp1>&) const = delete;
      template<typename _Tp1>
        bool operator<=(const weak_ptr<_Tp1>&) const = delete;
      template<typename _Tp1>
        bool operator>(const weak_ptr<_Tp1>&) const = delete;
      template<typename _Tp1>
        bool operator>=(const weak_ptr<_Tp1>&) const = delete;
    };

  // 20.8.13.3.7 weak_ptr specialized algorithms.
  template<typename _Tp>
    inline void
    swap(weak_ptr<_Tp>& __a, weak_ptr<_Tp>& __b)
    { __a.swap(__b); }

  /// owner_less
  template<typename _Tp>
    struct owner_less<shared_ptr<_Tp>>
    : public _Sp_owner_less<shared_ptr<_Tp>, weak_ptr<_Tp>>
    { };

  template<typename _Tp>
    struct owner_less<weak_ptr<_Tp>>
    : public _Sp_owner_less<weak_ptr<_Tp>, shared_ptr<_Tp>>
    { };

  /** 
   *  @brief Base class allowing use of member function shared_from_this.
   */
  template<typename _Tp>
    class enable_shared_from_this
    {
    protected:
      enable_shared_from_this() { }
      
      enable_shared_from_this(const enable_shared_from_this&) { }

      enable_shared_from_this&
      operator=(const enable_shared_from_this&)
      { return *this; }

      ~enable_shared_from_this() { }

    public:
      shared_ptr<_Tp>
      shared_from_this()
      { return shared_ptr<_Tp>(this->_M_weak_this); }

      shared_ptr<const _Tp>
      shared_from_this() const
      { return shared_ptr<const _Tp>(this->_M_weak_this); }

    private:
      template<typename _Tp1>
        void
        _M_weak_assign(_Tp1* __p, const __shared_count<>& __n) const
        { _M_weak_this._M_assign(__p, __n); }

      template<typename _Tp1>
        friend void
        __enable_shared_from_this_helper(const __shared_count<>& __pn,
					 const enable_shared_from_this* __pe,
					 const _Tp1* __px)
        {
	  if (__pe != 0)
	    __pe->_M_weak_assign(const_cast<_Tp1*>(__px), __pn);
	}

      mutable weak_ptr<_Tp>  _M_weak_this;
    };

  template<typename _Tp, _Lock_policy _Lp, typename _Alloc, typename... _Args>
    inline __shared_ptr<_Tp, _Lp>
    __allocate_shared(_Alloc __a, _Args&&... __args)
    {
      return __shared_ptr<_Tp, _Lp>(_Sp_make_shared_tag(),
          std::forward<_Alloc>(__a), std::forward<_Args>(__args)...);
    }

  template<typename _Tp, _Lock_policy _Lp, typename... _Args>
    inline __shared_ptr<_Tp, _Lp>
    __make_shared(_Args&&... __args)
    {
      typedef typename std::remove_const<_Tp>::type _Tp_nc;
      return __allocate_shared<_Tp, _Lp>(std::allocator<_Tp_nc>(),
              std::forward<_Args>(__args)...);
    }

  /** @brief  Create an object that is owned by a shared_ptr.
   *  @param  __a     An allocator.
   *  @param  __args  Arguments for the @a _Tp object's constructor.
   *  @return A shared_ptr that owns the newly created object.
   *  @throw  An exception thrown from @a _Alloc::allocate or from the
   *          constructor of @a _Tp.
   *
   *  A copy of @a __a will be used to allocate memory for the shared_ptr
   *  and the new object.
   */
  template<typename _Tp, typename _Alloc, typename... _Args>
    inline shared_ptr<_Tp>
    allocate_shared(_Alloc __a, _Args&&... __args)
    {
      return shared_ptr<_Tp>(_Sp_make_shared_tag(), std::forward<_Alloc>(__a),
              std::forward<_Args>(__args)...);
    }

  /** @brief  Create an object that is owned by a shared_ptr. 
   *  @param  __args  Arguments for the @a _Tp object's constructor.
   *  @return A shared_ptr that owns the newly created object.
   *  @throw  std::bad_alloc, or an exception thrown from the
   *          constructor of @a _Tp.
   */
  template<typename _Tp, typename... _Args>
    inline shared_ptr<_Tp>
    make_shared(_Args&&... __args)
    {
      typedef typename std::remove_const<_Tp>::type _Tp_nc;
      return allocate_shared<_Tp>(std::allocator<_Tp_nc>(),
              std::forward<_Args>(__args)...);
    }

  // @} group pointer_abstractions

_GLIBCXX_END_NAMESPACE

#endif // _SHARED_PTR_H
