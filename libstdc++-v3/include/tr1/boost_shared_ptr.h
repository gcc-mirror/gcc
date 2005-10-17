// <tr1/boost_shared_ptr.h> -*- C++ -*-

// Copyright (C) 2005 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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

/** @file boost_memory.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _BOOST_SHARED_PTR_H
#define _BOOST_SHARED_PTR_H 1

// namespace std::tr1
namespace std
{
namespace tr1
{

class bad_weak_ptr : public std::exception
{
public:

  virtual char const* what() const throw()
  {
    return "tr1::bad_weak_ptr";
  }
};

// Helper for exception objects in <tr1/memory>
// TODO this should be defined in a different file.
inline void
__throw_bad_weak_ptr()
{
#if __EXCEPTIONS
  throw bad_weak_ptr();
#else
  std::abort();
#endif
}


template <typename _Tp>
  struct _Sp_deleter
  {
    typedef void result_type;
    typedef _Tp* argument_type;

    void
    operator()(_Tp* p) const
    { delete p; }
  };


class _Sp_counted_base
{
public:

  _Sp_counted_base()
  : _M_use_count(1), _M_weak_count(1)
  {
    // For the case of __GTHREAD_MUTEX_INIT we haven't initialised
    // the mutex yet, so do it now.
#if defined(__GTHREADS) && defined(__GTHREAD_MUTEX_INIT)
    __gthread_mutex_t __tmp = __GTHREAD_MUTEX_INIT;
    _M_mutex = __tmp;
#endif
  }

  virtual
  ~_Sp_counted_base() // nothrow
  { }

  // dispose() is called when _M_use_count drops to zero, to release
  // the resources managed by *this.
  virtual void
  dispose() = 0; // nothrow

  // destroy() is called when _M_weak_count drops to zero.
  virtual void
  destroy() // nothrow
  {
    delete this;
  }

  virtual void*
  get_deleter(const std::type_info&) = 0;

  void
  add_ref_copy()
  {
    __gnu_cxx::__atomic_add(&_M_use_count, 1);
  }

  void
  add_ref_lock()
  {
    __gnu_cxx::lock lock(_M_mutex);
    if (__gnu_cxx::__exchange_and_add(&_M_use_count, 1) == 0)
    {
      _M_use_count = 0;
      __throw_bad_weak_ptr();
    }
  }

  void
  release() // nothrow
  {
    if (__gnu_cxx::__exchange_and_add(&_M_use_count, -1) == 1)
    {
      dispose();
      __glibcxx_mutex_lock(_M_mutex);
      __glibcxx_mutex_unlock(_M_mutex);
      weak_release();
    }
  }

  void
  weak_add_ref() // nothrow
  {
    __gnu_cxx::__atomic_add(&_M_weak_count, 1);
  }

  void
  weak_release() // nothrow
  {
    if (__gnu_cxx::__exchange_and_add(&_M_weak_count, -1) == 1)
    {
      __glibcxx_mutex_lock(_M_mutex);
      __glibcxx_mutex_unlock(_M_mutex);
      destroy();
    }
  }

  long
  use_count() const // nothrow
  {
    return _M_use_count;  // XXX is this MT safe?
  }

private:

  _Sp_counted_base(_Sp_counted_base const&);
  _Sp_counted_base& operator= (_Sp_counted_base const&);

  _Atomic_word _M_use_count;        // #shared
  _Atomic_word _M_weak_count;       // #weak + (#shared != 0)
  __gnu_cxx::mutex_type _M_mutex;
};

template <typename _Ptr, typename _Deleter>
class _Sp_counted_base_impl : public _Sp_counted_base
{
public:

  /**
   *  @brief   
   *  @pre     d(p) must not throw.
   */
  _Sp_counted_base_impl(_Ptr __p, _Deleter __d)
  : _M_ptr(__p), _M_del(__d)
  { }

  virtual void
  dispose() // nothrow
  {
    _M_del(_M_ptr);
  }

  virtual void*
  get_deleter(const std::type_info& __ti)
  {
    return __ti == typeid(_Deleter) ? &_M_del : 0;
  }

private:
  _Sp_counted_base_impl(const _Sp_counted_base_impl&);
  _Sp_counted_base_impl& operator=(const _Sp_counted_base_impl&);

  _Ptr     _M_ptr; // copy constructor must not throw
  _Deleter _M_del; // copy constructor must not throw
};

class weak_count;

class shared_count
{
private:

  _Sp_counted_base* _M_pi;

  friend class weak_count;

public:

  shared_count()
  : _M_pi(0) // nothrow
  { }

  template <typename _Ptr, typename _Deleter>
    shared_count(_Ptr __p, _Deleter __d)
    : _M_pi(0)
    {
      try
      {
        _M_pi = new _Sp_counted_base_impl<_Ptr, _Deleter>(__p, __d);
      }
      catch(...)
      {
        __d(__p); // delete __p
        __throw_exception_again;
      }
    }

  // auto_ptr<_Tp> is special cased to provide the strong guarantee

  template <typename _Tp>
    explicit shared_count(std::auto_ptr<_Tp>& __r)
    : _M_pi(new _Sp_counted_base_impl<_Tp*,_Sp_deleter<_Tp> >(
            __r.get(), _Sp_deleter<_Tp>()
            ))
    { __r.release(); }

  // throws bad_weak_ptr when __r.use_count() == 0
  explicit shared_count(const weak_count& __r);

  ~shared_count() // nothrow
  {
    if (_M_pi != 0)
      _M_pi->release();
  }

  shared_count(const shared_count& __r)
  : _M_pi(__r._M_pi) // nothrow
  {
    if (_M_pi != 0)
      _M_pi->add_ref_copy();
  }

  shared_count&
  operator=(const shared_count& __r) // nothrow
  {
    _Sp_counted_base* __tmp = __r._M_pi;

    if(__tmp != _M_pi)
    {
      if(__tmp != 0)
        __tmp->add_ref_copy();
      if(_M_pi != 0)
        _M_pi->release();
      _M_pi = __tmp;
    }
    return *this;
  }

  void swap(shared_count& __r) // nothrow
  {
    _Sp_counted_base* __tmp = __r._M_pi;
    __r._M_pi = _M_pi;
    _M_pi = __tmp;
  }

  long
  use_count() const // nothrow
  { return _M_pi != 0 ? _M_pi->use_count() : 0; }

  bool
  unique() const // nothrow
  { return this->use_count() == 1; }

  friend inline bool
  operator==(const shared_count& __a, const shared_count& __b)
  { return __a._M_pi == __b._M_pi; }

  friend inline bool
  operator<(const shared_count& __a, const shared_count& __b)
  { return std::less<_Sp_counted_base*>()(__a._M_pi, __b._M_pi); }

  void*
  get_deleter(const std::type_info& __ti) const
  { return _M_pi ? _M_pi->get_deleter(__ti) : 0; }
};


class weak_count
{
private:

  _Sp_counted_base * _M_pi;

  friend class shared_count;

public:

  weak_count()
  : _M_pi(0) // nothrow
  { }

  weak_count(const shared_count& __r)
  : _M_pi(__r._M_pi) // nothrow
  {
    if (_M_pi != 0)
      _M_pi->weak_add_ref();
  }

  weak_count(const weak_count& __r)
  : _M_pi(__r._M_pi) // nothrow
  {
    if (_M_pi != 0)
      _M_pi->weak_add_ref();
  }

  ~weak_count() // nothrow
  {
    if (_M_pi != 0)
      _M_pi->weak_release();
  }

  weak_count&
  operator=(const shared_count& __r) // nothrow
  {
    _Sp_counted_base* __tmp = __r._M_pi;
    if (__tmp != 0)
      __tmp->weak_add_ref();
    if (_M_pi != 0)
      _M_pi->weak_release();
    _M_pi = __tmp;

    return *this;
  }

  weak_count&
  operator=(const weak_count& __r) // nothrow
  {
    _Sp_counted_base * __tmp = __r._M_pi;
    if (__tmp != 0)
      __tmp->weak_add_ref();
    if (_M_pi != 0)
      _M_pi->weak_release();
    _M_pi = __tmp;

    return *this;
  }

  void
  swap(weak_count& __r) // nothrow
  {
    _Sp_counted_base * __tmp = __r._M_pi;
    __r._M_pi = _M_pi;
    _M_pi = __tmp;
  }

  long
  use_count() const // nothrow
  { return _M_pi != 0 ? _M_pi->use_count() : 0; }

  friend inline bool
  operator==(const weak_count& __a, const weak_count& __b)
  { return __a._M_pi == __b._M_pi; }

  friend inline bool
  operator<(const weak_count& __a, const weak_count& __b)
  { return std::less<_Sp_counted_base*>()(__a._M_pi, __b._M_pi); }
};

inline
shared_count::shared_count(const weak_count& __r)
: _M_pi(__r._M_pi)
{
  if (_M_pi != 0)
  {
    _M_pi->add_ref_lock();
  }
  else
  {
    __throw_bad_weak_ptr();
  }
}

// fwd decls
template <typename _Tp> class weak_ptr;
template <typename _Tp> class enable_shared_from_this;

struct __static_cast_tag {};
struct __const_cast_tag {};
struct __dynamic_cast_tag {};
struct __polymorphic_cast_tag {};

template<class _Tp> struct shared_ptr_traits
{
    typedef _Tp & reference;
};

template<> struct shared_ptr_traits<void>
{
    typedef void reference;
};

template<> struct shared_ptr_traits<void const>
{
    typedef void reference;
};

template<> struct shared_ptr_traits<void volatile>
{
    typedef void reference;
};

template<> struct shared_ptr_traits<void const volatile>
{
    typedef void reference;
};


// enable_shared_from_this support

// friend of enable_shared_from_this
template <typename _Tp1, typename _Tp2>
  void
  __enable_shared_from_this( const shared_count& __pn,
                             const enable_shared_from_this<_Tp1>* __pe,
                             const _Tp2* __px );

inline void
__enable_shared_from_this(const shared_count&, ...)
{ }

/**
 *  @class shared_ptr <tr1/memory>
 *
 *  A smart pointer with reference-counted copy semantics.
 *  The object pointed to is deleted when the last shared_ptr pointing to it
 *  is destroyed or reset.
 */

template <typename _Tp>
  class shared_ptr
  {
    typedef typename shared_ptr_traits<_Tp>::reference _Reference;

  public:

    typedef _Tp   element_type;

    /** @brief  Construct an empty %shared_ptr.
     *  @post   use_count()==0 && get()==0
     */
    shared_ptr() : _M_ptr(0), _M_refcount() // never throws
    { }

    /** @brief  Construct a %shared_ptr that owns the pointer @a p.
     *  @param  p  A pointer that is convertible to element_type*.
     *  @post   use_count()==1 && get()==p
     *  @throw  std::bad_alloc, in which case @c delete @a p is called.
     */
    template <typename _Tp1>
      explicit shared_ptr(_Tp1* __p)
      : _M_ptr(__p), _M_refcount(__p, _Sp_deleter<_Tp1>())
      {
        __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
        // __glibcxx_function_requires(_CompleteConcept<_Tp1*>)

        __enable_shared_from_this( _M_refcount, __p, __p );
      }

    //
    // Requirements: D's copy constructor and destructor must not throw
    //
    // shared_ptr will release p by calling d(p)
    //
    /** @brief  Construct a %shared_ptr that owns the pointer @a p
     *          and the deleter @a d.
     *  @param  p  A pointer.
     *  @param  d  A deleter.
     *  @post   use_count()==1 && get()==p
     *  @throw  std::bad_alloc, in which case @a d(p) is called.
     */
    template <typename _Tp1, typename _Deleter>
      shared_ptr(_Tp1* __p, _Deleter __d)
      : _M_ptr(__p), _M_refcount(__p, __d)
      {
        __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
        // TODO requires D is CopyConstructible and d(p) well-formed

        __enable_shared_from_this( _M_refcount, __p, __p );
      }

    //  generated copy constructor, assignment, destructor are fine.

    /** @brief  If @a r is empty, constructs an empty %shared_ptr; otherwise
     *          construct a %shared_ptr that shares ownership with @a r.
     *  @param  r  A %shared_ptr.
     *  @post   get()==r.get() && use_count()==r.use_count()
     *  @throw  std::bad_alloc, in which case 
     */
    template <typename _Tp1>
      shared_ptr(const shared_ptr<_Tp1>& __r)
      : _M_ptr(__r._M_ptr), _M_refcount(__r._M_refcount) // never throws
      {
        __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
      }

    /** @brief  Constructs a %shared_ptr that shares ownership with @a r
     *          and stores a copy of the pointer stored in @a r.
     *  @param  r  A weak_ptr.
     *  @post   use_count()==r.use_count()
     *  @throw  bad_weak_ptr when r.expired(),
     *          in which case the constructor has no effect.
     */
    template <typename _Tp1>
      explicit shared_ptr(const weak_ptr<_Tp1>& __r)
      : _M_refcount(__r._M_refcount) // may throw
      {
        __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
        // it is now safe to copy r__._M_ptr, as _M_refcount(__r._M_refcount)
        // did not throw
        _M_ptr = __r._M_ptr;
      }

    /**
     * @post use_count()==1 and r.get()==0
     */
    template <typename _Tp1>
      explicit shared_ptr(std::auto_ptr<_Tp1>& __r)
      : _M_ptr(__r.get()), _M_refcount()
      {
        // TODO requires r.release() convertible to _Tp*, Tp1 is complete,
        // delete r.release() well-formed
        _Tp1 * __tmp = __r.get();
        _M_refcount = shared_count(__r);

        __enable_shared_from_this( _M_refcount, __tmp, __tmp );
      }

    template <typename _Tp1>
      shared_ptr(const shared_ptr<_Tp1>& __r, __static_cast_tag)
      : _M_ptr(static_cast<element_type*>(__r._M_ptr))
      , _M_refcount(__r._M_refcount)
      { }

    template <typename _Tp1>
      shared_ptr(const shared_ptr<_Tp1>& __r, __const_cast_tag)
      : _M_ptr(const_cast<element_type*>(__r._M_ptr))
      , _M_refcount(__r._M_refcount)
      { }

    template <typename _Tp1>
      shared_ptr(const shared_ptr<_Tp1>& __r, __dynamic_cast_tag)
      : _M_ptr(dynamic_cast<element_type*>(__r._M_ptr))
      , _M_refcount(__r._M_refcount)
      {
        if (_M_ptr == 0) // need to allocate new counter -- the cast failed
        {
          _M_refcount = shared_count();
        }
      }

    template <typename _Tp1>
      shared_ptr&
      operator=(const shared_ptr<_Tp1>& __r) // never throws
      {
        _M_ptr = __r._M_ptr;
        _M_refcount = __r._M_refcount; // shared_count::op= doesn't throw
        return *this;
      }

    template <typename _Tp1>
      shared_ptr&
      operator=(std::auto_ptr<_Tp1>& __r)
      {
        shared_ptr(__r).swap(*this);
        return *this;
      }

    void
    reset() // never throws
    { shared_ptr().swap(*this); }

    template <typename _Tp1>
      void
      reset(_Tp1* __p) // _Tp1 must be complete
      {
        _GLIBCXX_DEBUG_ASSERT(__p == 0 || __p != _M_ptr); // catch self-reset errors
        shared_ptr(__p).swap(*this);
      }

    template <typename _Tp1, typename _Deleter>
      void
      reset(_Tp1 * __p, _Deleter __d)
      { shared_ptr(__p, __d).swap(*this); }

    // error to instantiate if _Tp is [cv-qual] void
    _Reference
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

    // implicit conversion to "bool"
  private:
    typedef _Tp* shared_ptr::*__unspecified_bool_type;

  public:
    operator __unspecified_bool_type() const // never throws
    { return _M_ptr == 0 ? 0 : &shared_ptr::_M_ptr; }

    bool
    unique() const // never throws
    { return _M_refcount.unique(); }

    long
    use_count() const // never throws
    { return _M_refcount.use_count(); }

    void
    swap(shared_ptr<_Tp>& __other) // never throws
    {
      std::swap(_M_ptr, __other._M_ptr);
      _M_refcount.swap(__other._M_refcount);
    }

  private:
    template <typename _Tp1>
      bool
      _M_less(const shared_ptr<_Tp1>& __rhs) const
      { return _M_refcount < __rhs._M_refcount; }

    void*
    _M_get_deleter(const std::type_info& __ti) const
    { return _M_refcount.get_deleter(__ti); }

    template <typename _Tp1> friend class shared_ptr;
    template <typename _Tp1> friend class weak_ptr;

    // friends injected into enclosing namespace and found by ADL:

    // get_deleter (experimental)
    template <typename _Del>
      friend inline _Del*
      get_deleter(const shared_ptr& __p)
      { return static_cast<_Del*>(__p._M_get_deleter(typeid(_Del))); }

    template <typename _Tp1>
      friend inline bool
      operator==(const shared_ptr& __a, const shared_ptr<_Tp1>& __b)
      { return __a.get() == __b.get(); }

    template <typename _Tp1>
      friend inline bool
      operator!=(const shared_ptr& __a, const shared_ptr<_Tp1>& __b)
      { return __a.get() != __b.get(); }

    template <typename _Tp1>
      friend inline bool
      operator<(const shared_ptr& __a, const shared_ptr<_Tp1>& __b)
      { return __a._M_less(__b); }

    _Tp*         _M_ptr;         // contained pointer
    shared_count _M_refcount;    // reference counter
  };  // shared_ptr

// 2.2.3.9 shared_ptr casts

/** @warning The seemingly equivalent
 *           <code>shared_ptr<T>(static_cast<T*>(r.get()))</code>
 *           will eventually result in undefined behaviour,
 *           attempting to delete the same object twice.
 */
template <typename _Tp, typename _Tp1>
  shared_ptr<_Tp>
  static_pointer_cast(const shared_ptr<_Tp1>& __r)
  {
    return shared_ptr<_Tp>(__r, __static_cast_tag());
  }

/** @warning The seemingly equivalent
 *           <code>shared_ptr<T>(const_cast<T*>(r.get()))</code>
 *           will eventually result in undefined behaviour,
 *           attempting to delete the same object twice.
 */
template <typename _Tp, typename _Tp1>
  shared_ptr<_Tp>
  const_pointer_cast(const shared_ptr<_Tp1>& __r)
  {
    return shared_ptr<_Tp>(__r, __const_cast_tag());
  }

/** @warning The seemingly equivalent
 *           <code>shared_ptr<T>(dynamic_cast<T*>(r.get()))</code>
 *           will eventually result in undefined behaviour,
 *           attempting to delete the same object twice.
 */
template <typename _Tp, typename _Tp1>
  shared_ptr<_Tp>
  dynamic_pointer_cast(const shared_ptr<_Tp1>& __r)
  {
    return shared_ptr<_Tp>(__r, __dynamic_cast_tag());
  }

// operator<<
template <typename _Ch, typename _Tr, typename _Tp>
  std::basic_ostream<_Ch,_Tr>&
  operator<<(std::basic_ostream<_Ch,_Tr>& __os, const shared_ptr<_Tp>& __p)
  {
    __os << __p.get();
    return __os;
  }


template <typename _Tp>
  class weak_ptr
  {
  public:

    typedef _Tp element_type;

    weak_ptr()
    : _M_ptr(0), _M_refcount() // never throws
    { }

  //  generated copy constructor, assignment, destructor are fine

  //
  //  The "obvious" converting constructor implementation:
  //
  //  template<class Y>
  //  weak_ptr(weak_ptr<Y> const & r): _M_ptr(r._M_ptr), _M_refcount(r._M_refcount) // never throws
  //  {
  //  }
  //
  //  has a serious problem.
  //
  //  r._M_ptr may already have been invalidated. The _M_ptr(r._M_ptr)
  //  conversion may require access to *r._M_ptr (virtual inheritance).
  //
  //  It is not possible to avoid spurious access violations since
  //  in multithreaded programs r._M_ptr may be invalidated at any point.
  //

    template <typename _Tp1>
      weak_ptr(const weak_ptr<_Tp1>& r)
      : _M_refcount(r._M_refcount) // never throws
      {
        __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
        _M_ptr = r.lock().get();
      }

    template <typename _Tp1>
      weak_ptr(const shared_ptr<_Tp1>& r)
      : _M_ptr(r._M_ptr), _M_refcount(r._M_refcount) // never throws
      {
        __glibcxx_function_requires(_ConvertibleConcept<_Tp1*, _Tp*>)
      }

    template <typename _Tp1>
      weak_ptr&
      operator=(const weak_ptr<_Tp1>& r) // never throws
      {
        _M_ptr = r.lock().get();
        _M_refcount = r._M_refcount;
        return *this;
      }

    template <typename _Tp1>
      weak_ptr&
      operator=(const shared_ptr<_Tp1>& r) // never throws
      {
        _M_ptr = r._M_ptr;
        _M_refcount = r._M_refcount;
        return *this;
      }

    shared_ptr<_Tp>
    lock() const // never throws
    {
#ifdef __GTHREADS

      // optimization: avoid throw overhead
      if (expired())
      {
        return shared_ptr<element_type>();
      }

      try
      {
        return shared_ptr<element_type>(*this);
      }
      catch (const bad_weak_ptr&)
      {
        // Q: how can we get here?
        // A: another thread may have invalidated r after the use_count test above.
        return shared_ptr<element_type>();
      }

#else

      // optimization: avoid try/catch overhead when single threaded
      return expired() ? shared_ptr<element_type>() : shared_ptr<element_type>(*this);

#endif
    } // XXX MT


    long
    use_count() const // never throws
    { return _M_refcount.use_count(); }

    bool
    expired() const // never throws
    { return _M_refcount.use_count() == 0; }

    void
    reset() // never throws
    { weak_ptr().swap(*this); }

    void
    swap(weak_ptr& __s) // never throws
    {
      std::swap(_M_ptr, __s._M_ptr);
      _M_refcount.swap(__s._M_refcount);
    }

  private:

    template <typename _Tp1>
      bool
      _M_less(const weak_ptr<_Tp1>& __rhs) const
      { return _M_refcount < __rhs._M_refcount; }

    // used by __enable_shared_from_this
    void
    _M_assign(_Tp* __ptr, const shared_count& __refcount)
    {
      _M_ptr = __ptr;
      _M_refcount = __refcount;
    }

    // friend injected into namespace and found by ADL

    template <typename _Tp1>
      friend inline bool
      operator<(const weak_ptr& __lhs, const weak_ptr<_Tp1>& __rhs)
      { return __lhs._M_less(__rhs); }

    template <typename _Tp1> friend class weak_ptr;
    template <typename _Tp1> friend class shared_ptr;
    friend class enable_shared_from_this<_Tp>;

    _Tp*       _M_ptr;           // contained pointer
    weak_count _M_refcount;      // reference counter

  };  // weak_ptr



template <typename _Tp>
  class enable_shared_from_this
  {
  protected:

    enable_shared_from_this()
    { }

    enable_shared_from_this(const enable_shared_from_this&)
    { }

    enable_shared_from_this&
    operator=(const enable_shared_from_this&)
    { return *this; }

    ~enable_shared_from_this()
    { }

  public:

    shared_ptr<_Tp>
    shared_from_this()
    {
      shared_ptr<_Tp> p(this->_M_weak_this);
      return p;
    }

    shared_ptr<const _Tp>
    shared_from_this() const
    {
      shared_ptr<const _Tp> p(this->_M_weak_this);
      return p;
    }

  private:
    template <typename _Tp1>
      void
      _M_weak_assign(_Tp1* __p, const shared_count& __n) const
      { _M_weak_this._M_assign(__p, __n); }

    template <typename _Tp1>
      friend void
      __enable_shared_from_this( const shared_count& __pn, const enable_shared_from_this* __pe, const _Tp1* __px)
      {
        if(__pe != 0)
          __pe->_M_weak_assign(const_cast<_Tp1*>(__px), __pn);
      }

    mutable weak_ptr<_Tp> _M_weak_this;
  };

} // namespace tr1

/**
 *  @brief   std::swap() specialisation for shared_ptr.
 *  @relates shared_ptr.
 */
template <typename _Tp>
  inline void
  swap(tr1::shared_ptr<_Tp>& __a, tr1::shared_ptr<_Tp>& __b)
  {
    __a.swap(__b);
  }

/**
 *  @brief   std::swap() specialisation for weak_ptr.
 *  @relates weak_ptr.
 */
template <typename _Tp>
  void
  swap(tr1::weak_ptr<_Tp>& __a, tr1::weak_ptr<_Tp>& __b)
  {
    __a.swap(__b);
  }

} // namespace std

#endif
