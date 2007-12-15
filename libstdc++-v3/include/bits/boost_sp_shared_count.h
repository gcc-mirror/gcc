// <bits/boost_sp_shared_count.h> -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

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

/** @file bits/boost_sp_shared_count.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef __GXX_EXPERIMENTAL_CXX0X__
# include <c++0x_warning.h>
#endif

#if defined(_GLIBCXX_INCLUDE_AS_TR1)
#  error C++0x header cannot be included from TR1 header
#endif

namespace std
{
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
      
    private:
      _Sp_counted_ptr(const _Sp_counted_ptr&);
      _Sp_counted_ptr& operator=(const _Sp_counted_ptr&);
      
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
      
    private:
      _Sp_counted_deleter(const _Sp_counted_deleter&);
      _Sp_counted_deleter& operator=(const _Sp_counted_deleter&);
      
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
          try
            {
              _M_pi = new _Sp_counted_ptr<_Ptr, _Lp>(__p);
            }
          catch(...)
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
          try
            {
              _M_pi = __a2.allocate(1);
              new(static_cast<void*>(_M_pi)) _Sp_cd_type(__p, __d);
            }
          catch(...)
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
          try
            {
              _M_pi = __a2.allocate(1);
              new(static_cast<void*>(_M_pi)) _Sp_cd_type(__p, __d, __a);
            }
          catch(...)
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
          try
            {
              _M_pi = __a2.allocate(1);
              new(static_cast<void*>(_M_pi)) _Sp_cp_type(__a,
                  std::forward<_Args>(__args)...);
            }
          catch(...)
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
        __shared_count(std::auto_ptr<_Tp>& __r)
        : _M_pi(new _Sp_counted_ptr<_Tp*, _Lp>(__r.get()))
        { __r.release(); }
#endif
  
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

      friend inline bool
      operator==(const __shared_count& __a, const __shared_count& __b)
      { return __a._M_pi == __b._M_pi; }
  
      friend inline bool
      operator<(const __shared_count& __a, const __shared_count& __b)
      { return std::less<_Sp_counted_base<_Lp>*>()(__a._M_pi, __b._M_pi); }
  
      void*
      _M_get_deleter(const std::type_info& __ti) const
      { return _M_pi ? _M_pi->_M_get_deleter(__ti) : 0; }

    private:
      friend class __weak_count<_Lp>;

      _Sp_counted_base<_Lp>*  _M_pi;
    };
}
