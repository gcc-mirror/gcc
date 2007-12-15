// <tr1/boost_sp_shared_count.h> -*- C++ -*-

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

/** @file tr1/boost_sp_shared_count.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#if defined(_GLIBCXX_INCLUDE_AS_CXX0X)
#  error TR1 header cannot be included from C++0x header
#endif

namespace std
{
namespace tr1
{

  template<typename _Ptr, typename _Deleter, _Lock_policy _Lp>
    class _Sp_counted_base_impl
    : public _Sp_counted_base<_Lp>
    {
    public:
      /**
       *  @brief   
       *  @pre     __d(__p) must not throw.
       */
      _Sp_counted_base_impl(_Ptr __p, _Deleter __d)
      : _M_ptr(__p), _M_del(__d) { }
    
      virtual void
      _M_dispose() // nothrow
      { _M_del(_M_ptr); }
      
      virtual void*
      _M_get_deleter(const std::type_info& __ti)
      { return __ti == typeid(_Deleter) ? &_M_del : 0; }
      
    private:
      _Sp_counted_base_impl(const _Sp_counted_base_impl&);
      _Sp_counted_base_impl& operator=(const _Sp_counted_base_impl&);
      
      _Ptr      _M_ptr;  // copy constructor must not throw
      _Deleter  _M_del;  // copy constructor must not throw
    };

  template<_Lock_policy _Lp = __default_lock_policy>
    class __weak_count;

  template<typename _Tp>
    struct _Sp_deleter
    {
      typedef void result_type;
      typedef _Tp* argument_type;
      void operator()(_Tp* __p) const { delete __p; }
    };

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
	      typedef typename std::tr1::remove_pointer<_Ptr>::type _Tp;
	      _M_pi = new _Sp_counted_base_impl<_Ptr, _Sp_deleter<_Tp>, _Lp>(
	          __p, _Sp_deleter<_Tp>());
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
	  try
	    {
	      _M_pi = new _Sp_counted_base_impl<_Ptr, _Deleter, _Lp>(__p, __d);
	    }
	  catch(...)
	    {
	      __d(__p); // Call _Deleter on __p.
	      __throw_exception_again;
	    }
	}

      // Special case for auto_ptr<_Tp> to provide the strong guarantee.
      template<typename _Tp>
        explicit
        __shared_count(std::auto_ptr<_Tp>& __r)
	: _M_pi(new _Sp_counted_base_impl<_Tp*,
		_Sp_deleter<_Tp>, _Lp >(__r.get(), _Sp_deleter<_Tp>()))
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
}
