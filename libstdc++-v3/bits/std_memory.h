/*
 * Copyright (c) 1997-1999
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */

#ifndef _CPP_MEMORY
#define _CPP_MEMORY 1

#include <bits/stl_algobase.h>
#include <bits/stl_alloc.h>
#include <bits/stl_construct.h>
#include <bits/stl_iterator_base.h> //for iterator_traits
#include <bits/stl_tempbuf.h>
#include <bits/stl_uninitialized.h>
#include <bits/stl_raw_storage_iter.h>

__STL_BEGIN_NAMESPACE

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
# if defined(__SGI_STL_USE_AUTO_PTR_CONVERSIONS) && \
       defined(__STL_MEMBER_TEMPLATES)

template<typename _Tp> class auto_ptr_ref {
  template<typename _Tp1> friend class auto_ptr;
  _Tp* _M_ptr;
  void*& _M_ptr_ref;
  explicit auto_ptr_ref(_Tp* __p, void*& __r) __STL_NOTHROW
    : _M_ptr(__p), _M_ptr_ref(__r) {}
  _Tp* _M_release() const __STL_NOTHROW {
    _M_ptr_ref = 0;
    return _M_ptr;
  }
};

# endif /* auto ptr conversions && member templates */
#endif /* _GLIBCPP_RESOLVE_LIB_DEFECTS */

template <class _Tp> class auto_ptr {
private:
  void* _M_ptr;

public:
  typedef _Tp element_type;

  explicit auto_ptr(_Tp* __p = 0) __STL_NOTHROW : _M_ptr(__p) {}
  auto_ptr(auto_ptr& __a) __STL_NOTHROW : _M_ptr(__a.release()) {}

#ifdef __STL_MEMBER_TEMPLATES
  template <class _Tp1> auto_ptr(auto_ptr<_Tp1>& __a) __STL_NOTHROW {
    _Tp* const __tmp = __a.release(); // Must have implicit conversion
    _M_ptr = __tmp;
  }
#endif /* __STL_MEMBER_TEMPLATES */

  auto_ptr& operator=(auto_ptr& __a) __STL_NOTHROW {
    reset(__a.release());
    return *this;
  }

#ifdef __STL_MEMBER_TEMPLATES
  template <class _Tp1>
  auto_ptr& operator=(auto_ptr<_Tp1>& __a) __STL_NOTHROW {
    reset(__a.release());
    return *this;
  }
#endif /* __STL_MEMBER_TEMPLATES */
  
  // Note: The C++ standard says there is supposed to be an empty throw
  // specification here, but omitting it is standard conforming.  Its 
  // presence can be detected only if _Tp::~_Tp() throws, but (17.4.3.6/2)
  // this is prohibited.
  ~auto_ptr() { delete this->get(); }
 
  _Tp& operator*() const __STL_NOTHROW {
    return  this->get();
  }
  _Tp* operator->() const __STL_NOTHROW {
    return static_cast<_Tp*>(_M_ptr);
  }
  _Tp* get() const __STL_NOTHROW {
    return static_cast<_Tp*>(_M_ptr);
  }
  _Tp* release() __STL_NOTHROW {
    _Tp* const __tmp = this->get();
    _M_ptr = 0;
    return __tmp;
  }
  void reset(_Tp* __p = 0) __STL_NOTHROW {
    _Tp* const __tmp = this->get();
    if (__p != _M_ptr) {
      delete _M_ptr;
      _M_ptr = __p;
    }    
  }

  // According to the C++ standard, these conversions are required.  Most
  // present-day compilers, however, do not enforce that requirement---and, 
  // in fact, most present-day compilers do not support the language 
  // features that these conversions rely on.
  
#if defined(__SGI_STL_USE_AUTO_PTR_CONVERSIONS) && \
    defined(__STL_MEMBER_TEMPLATES)

# ifndef _GLIBCPP_RESOLVE_LIB_DEFECTS
private:
  template<typename _Tp1> struct auto_ptr_ref {
    _Tp1* _M_ptr;
    void*& _M_ptr_ref;
    explicit auto_ptr_ref(_Tp1* __p, void*& __r) __STL_NOTHROW
    : _M_ptr(__p), _M_ptr_ref(__r) {}
    _Tp1* _M_release() const __STL_NOTHROW {
      _M_ptr_ref = 0;
      return _M_ptr;
    }
  };
# endif /* !_GLIBCPP_RESOLVE_LIB_DEFECTS */

public:
  auto_ptr(auto_ptr_ref<_Tp> __ref) __STL_NOTHROW
  : _M_ptr(__ref._M_release()) {}
  
# ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
  auto_ptr& operator=(auto_ptr_ref<_Tp> __ref) __STL_NOTHROW {
    reset(__ref._M_release());
    return *this; 
  }
# endif /* _GLIBCPP_RESOLVE_LIB_DEFECTS */
  
  template <class _Tp1> operator auto_ptr_ref<_Tp1>() __STL_NOTHROW
  { return auto_ptr_ref<_Tp1>(this->get(), _M_ptr); }
  template <class _Tp1> operator auto_ptr<_Tp1>() __STL_NOTHROW
  { return auto_ptr<_Tp1>(this->release()); }
#endif /* auto ptr conversions && member templates */
};

__STL_END_NAMESPACE

#endif /* _CPP_MEMORY */


// Local Variables:
// mode:C++
// End:
