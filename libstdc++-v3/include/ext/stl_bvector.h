/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996-1999
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/* NOTE: This is an internal header file, included by other STL headers.
 *   You should not attempt to use it directly.
 */

#ifndef __SGI_STL_INTERNAL_BVECTOR_H
#define __SGI_STL_INTERNAL_BVECTOR_H

__STL_BEGIN_NAMESPACE 

static const int __WORD_BIT = int(CHAR_BIT*sizeof(unsigned int));

#if defined(__sgi) && !defined(__GNUC__) && (_MIPS_SIM != _MIPS_SIM_ABI32)
#pragma set woff 1174
#pragma set woff 1375
#endif

struct _Bit_reference {
  unsigned int* _M_p;
  unsigned int _M_mask;
  _Bit_reference(unsigned int* __x, unsigned int __y) 
    : _M_p(__x), _M_mask(__y) {}

public:
  _Bit_reference() : _M_p(0), _M_mask(0) {}
  operator bool() const { return !(!(*_M_p & _M_mask)); }
  _Bit_reference& operator=(bool __x)
  {
    if (__x)  *_M_p |= _M_mask;
    else      *_M_p &= ~_M_mask;
    return *this;
  }
  _Bit_reference& operator=(const _Bit_reference& __x) 
    { return *this = bool(__x); }
  bool operator==(const _Bit_reference& __x) const
    { return bool(*this) == bool(__x); }
  bool operator<(const _Bit_reference& __x) const {
    return !bool(*this) && bool(__x);
  }
  void flip() { *_M_p ^= _M_mask; }
};

inline void swap(_Bit_reference __x, _Bit_reference __y)
{
  bool __tmp = __x;
  __x = __y;
  __y = __tmp;
}

struct _Bit_iterator_base : public random_access_iterator<bool, ptrdiff_t> 
{
  unsigned int* _M_p;
  unsigned int _M_offset;

  _Bit_iterator_base(unsigned int* __x, unsigned int __y)
    : _M_p(__x), _M_offset(__y) {}

  void _M_bump_up() {
    if (_M_offset++ == __WORD_BIT - 1) {
      _M_offset = 0;
      ++_M_p;
    }
  }
  void _M_bump_down() {
    if (_M_offset-- == 0) {
      _M_offset = __WORD_BIT - 1;
      --_M_p;
    }
  }

  void _M_incr(ptrdiff_t __i) {
    difference_type __n = __i + _M_offset;
    _M_p += __n / __WORD_BIT;
    __n = __n % __WORD_BIT;
    if (__n < 0) {
      _M_offset = (unsigned int) __n + __WORD_BIT;
      --_M_p;
    } else
      _M_offset = (unsigned int) __n;
  }

  bool operator==(const _Bit_iterator_base& __i) const {
    return _M_p == __i._M_p && _M_offset == __i._M_offset;
  }
  bool operator<(const _Bit_iterator_base& __i) const {
    return _M_p < __i._M_p || (_M_p == __i._M_p && _M_offset < __i._M_offset);
  }
  bool operator!=(const _Bit_iterator_base& __i) const {
    return !(*this == __i);
  }
  bool operator>(const _Bit_iterator_base& __i) const {
    return __i < *this;
  }
  bool operator<=(const _Bit_iterator_base& __i) const {
    return !(__i < *this); 
  }
  bool operator>=(const _Bit_iterator_base& __i) const {
    return !(*this < __i);
  }
};

inline ptrdiff_t
operator-(const _Bit_iterator_base& __x, const _Bit_iterator_base& __y) {
  return __WORD_BIT * (__x._M_p - __y._M_p) + __x._M_offset - __y._M_offset;
}


struct _Bit_iterator : public _Bit_iterator_base
{
  typedef _Bit_reference  reference;
  typedef _Bit_reference* pointer;
  typedef _Bit_iterator   iterator;

  _Bit_iterator() : _Bit_iterator_base(0, 0) {}
  _Bit_iterator(unsigned int* __x, unsigned int __y) 
    : _Bit_iterator_base(__x, __y) {}

  reference operator*() const { return reference(_M_p, 1U << _M_offset); }
  iterator& operator++() {
    _M_bump_up();
    return *this;
  }
  iterator operator++(int) {
    iterator __tmp = *this;
    _M_bump_up();
    return __tmp;
  }
  iterator& operator--() {
    _M_bump_down();
    return *this;
  }
  iterator operator--(int) {
    iterator __tmp = *this;
    _M_bump_down();
    return __tmp;
  }
  iterator& operator+=(difference_type __i) {
    _M_incr(__i);
    return *this;
  }
  iterator& operator-=(difference_type __i) {
    *this += -__i;
    return *this;
  }
  iterator operator+(difference_type __i) const {
    iterator __tmp = *this;
    return __tmp += __i;
  }
  iterator operator-(difference_type __i) const {
    iterator __tmp = *this;
    return __tmp -= __i;
  }

  reference operator[](difference_type __i) { return *(*this + __i); }
};

inline _Bit_iterator 
operator+(ptrdiff_t __n, const _Bit_iterator& __x) { return __x + __n; }


struct _Bit_const_iterator : public _Bit_iterator_base
{
  typedef bool                 reference;
  typedef bool                 const_reference;
  typedef const bool*          pointer;
  typedef _Bit_const_iterator  const_iterator;

  _Bit_const_iterator() : _Bit_iterator_base(0, 0) {}
  _Bit_const_iterator(unsigned int* __x, unsigned int __y) 
    : _Bit_iterator_base(__x, __y) {}
  _Bit_const_iterator(const _Bit_iterator& __x) 
    : _Bit_iterator_base(__x._M_p, __x._M_offset) {}

  const_reference operator*() const {
    return _Bit_reference(_M_p, 1U << _M_offset);
  }
  const_iterator& operator++() {
    _M_bump_up();
    return *this;
  }
  const_iterator operator++(int) {
    const_iterator __tmp = *this;
    _M_bump_up();
    return __tmp;
  }
  const_iterator& operator--() {
    _M_bump_down();
    return *this;
  }
  const_iterator operator--(int) {
    const_iterator __tmp = *this;
    _M_bump_down();
    return __tmp;
  }
  const_iterator& operator+=(difference_type __i) {
    _M_incr(__i);
    return *this;
  }
  const_iterator& operator-=(difference_type __i) {
    *this += -__i;
    return *this;
  }
  const_iterator operator+(difference_type __i) const {
    const_iterator __tmp = *this;
    return __tmp += __i;
  }
  const_iterator operator-(difference_type __i) const {
    const_iterator __tmp = *this;
    return __tmp -= __i;
  }
  const_reference operator[](difference_type __i) { 
    return *(*this + __i); 
  }
};

inline _Bit_const_iterator 
operator+(ptrdiff_t __n, const _Bit_const_iterator& __x) { return __x + __n; }


// Bit-vector base class, which encapsulates the difference between
// old SGI-style allocators and standard-conforming allocators.

#ifdef __STL_USE_STD_ALLOCATORS

// Base class for ordinary allocators.
template <class _Allocator, bool __is_static>
class _Bvector_alloc_base {
public:
  typedef typename _Alloc_traits<bool, _Allocator>::allocator_type
          allocator_type;
  allocator_type get_allocator() const { return _M_data_allocator; }

  _Bvector_alloc_base(const allocator_type& __a)
    : _M_data_allocator(__a), _M_start(), _M_finish(), _M_end_of_storage(0) {}

protected:
  unsigned int* _M_bit_alloc(size_t __n) 
    { return _M_data_allocator.allocate((__n + __WORD_BIT - 1)/__WORD_BIT); }
  void _M_deallocate() {
    if (_M_start._M_p)
      _M_data_allocator.deallocate(_M_start._M_p, 
                                   _M_end_of_storage - _M_start._M_p);
  }  

  typename _Alloc_traits<unsigned int, _Allocator>::allocator_type 
          _M_data_allocator;
  _Bit_iterator _M_start;
  _Bit_iterator _M_finish;
  unsigned int* _M_end_of_storage;
};

// Specialization for instanceless allocators.
template <class _Allocator>
class _Bvector_alloc_base<_Allocator, true> {
public:
  typedef typename _Alloc_traits<bool, _Allocator>::allocator_type
          allocator_type;
  allocator_type get_allocator() const { return allocator_type(); }

  _Bvector_alloc_base(const allocator_type&)
    : _M_start(), _M_finish(), _M_end_of_storage(0) {}

protected:
  typedef typename _Alloc_traits<unsigned int, _Allocator>::_Alloc_type
          _Alloc_type;
          
  unsigned int* _M_bit_alloc(size_t __n) 
    { return _Alloc_type::allocate((__n + __WORD_BIT - 1)/__WORD_BIT); }
  void _M_deallocate() {
    if (_M_start._M_p)
      _Alloc_type::deallocate(_M_start._M_p,
                              _M_end_of_storage - _M_start._M_p);
  }  

  _Bit_iterator _M_start;
  _Bit_iterator _M_finish;
  unsigned int* _M_end_of_storage;
};  

template <class _Alloc>
class _Bvector_base
  : public _Bvector_alloc_base<_Alloc,
                               _Alloc_traits<bool, _Alloc>::_S_instanceless>
{
  typedef _Bvector_alloc_base<_Alloc,
                              _Alloc_traits<bool, _Alloc>::_S_instanceless>
          _Base;
public:
  typedef typename _Base::allocator_type allocator_type;

  _Bvector_base(const allocator_type& __a) : _Base(__a) {}
  ~_Bvector_base() { _Base::_M_deallocate(); }
};

#else /* __STL_USE_STD_ALLOCATORS */

template <class _Alloc>
class _Bvector_base
{
public:
  typedef _Alloc allocator_type;
  allocator_type get_allocator() const { return allocator_type(); }

  _Bvector_base(const allocator_type&)
    : _M_start(), _M_finish(), _M_end_of_storage(0) {}
  ~_Bvector_base() { _M_deallocate(); }

protected:
  typedef simple_alloc<unsigned int, _Alloc> _Alloc_type;
  
  unsigned int* _M_bit_alloc(size_t __n) 
    { return _Alloc_type::allocate((__n + __WORD_BIT - 1)/__WORD_BIT); }
  void _M_deallocate() {
    if (_M_start._M_p)
      _Alloc_type::deallocate(_M_start._M_p,
                              _M_end_of_storage - _M_start._M_p);
  }

  _Bit_iterator _M_start;
  _Bit_iterator _M_finish;
  unsigned int* _M_end_of_storage;  
};

#endif /* __STL_USE_STD_ALLOCATORS */

// The next few lines are confusing.  What we're doing is declaring a
//  partial specialization of vector<T, Alloc> if we have the necessary
//  compiler support.  Otherwise, we define a class bit_vector which uses
//  the default allocator. 

#if defined(__STL_CLASS_PARTIAL_SPECIALIZATION) && !defined(__STL_NO_BOOL)
#  define __SGI_STL_VECBOOL_TEMPLATE
#  define __BVECTOR           vector<bool, _Alloc>
#  define __VECTOR            vector
#  define __BVECTOR_BASE      _Bvector_base<_Alloc>
#  define __BVECTOR_TMPL_LIST template <class _Alloc>
   __STL_END_NAMESPACE
#  include <bits/stl_vector.h>
   __STL_BEGIN_NAMESPACE
#else  /* __STL_CLASS_PARTIAL_SPECIALIZATION && !__STL_NO_BOOL */
#  undef  __SGI_STL_VECBOOL_TEMPLATE
#  define __BVECTOR           bit_vector
#  define __VECTOR            bit_vector
#  define __BVECTOR_BASE      _Bvector_base<__STL_DEFAULT_ALLOCATOR(bool) >
#  define __BVECTOR_TMPL_LIST
#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION && !__STL_NO_BOOL */


__BVECTOR_TMPL_LIST 
class __BVECTOR : public __BVECTOR_BASE 
{
public:
  typedef bool value_type;
  typedef size_t size_type;
  typedef ptrdiff_t difference_type; 
  typedef _Bit_reference reference;
  typedef bool const_reference;
  typedef _Bit_reference* pointer;
  typedef const bool* const_pointer;

  typedef _Bit_iterator                iterator;
  typedef _Bit_const_iterator          const_iterator;

#ifdef __STL_CLASS_PARTIAL_SPECIALIZATION
  typedef reverse_iterator<const_iterator> const_reverse_iterator;
  typedef reverse_iterator<iterator> reverse_iterator;
#else /* __STL_CLASS_PARTIAL_SPECIALIZATION */
  typedef reverse_iterator<const_iterator, value_type, const_reference, 
                           difference_type> const_reverse_iterator;
  typedef reverse_iterator<iterator, value_type, reference, difference_type>
          reverse_iterator;
#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

  typedef typename __BVECTOR_BASE::allocator_type allocator_type;
  allocator_type get_allocator() const {
    return __BVECTOR_BASE::get_allocator();
  }

protected:
#ifdef __STL_USE_NAMESPACES  
  using __BVECTOR_BASE::_M_bit_alloc;
  using __BVECTOR_BASE::_M_deallocate;
  using __BVECTOR_BASE::_M_start;
  using __BVECTOR_BASE::_M_finish;
  using __BVECTOR_BASE::_M_end_of_storage;
#endif /* __STL_USE_NAMESPACES */

protected:
  void _M_initialize(size_type __n) {
    unsigned int* __q = _M_bit_alloc(__n);
    _M_end_of_storage = __q + (__n + __WORD_BIT - 1)/__WORD_BIT;
    _M_start = iterator(__q, 0);
    _M_finish = _M_start + difference_type(__n);
  }
  void _M_insert_aux(iterator __position, bool __x) {
    if (_M_finish._M_p != _M_end_of_storage) {
      copy_backward(__position, _M_finish, _M_finish + 1);
      *__position = __x;
      ++_M_finish;
    }
    else {
      size_type __len = size() ? 2 * size() : __WORD_BIT;
      unsigned int* __q = _M_bit_alloc(__len);
      iterator __i = copy(begin(), __position, iterator(__q, 0));
      *__i++ = __x;
      _M_finish = copy(__position, end(), __i);
      _M_deallocate();
      _M_end_of_storage = __q + (__len + __WORD_BIT - 1)/__WORD_BIT;
      _M_start = iterator(__q, 0);
    }
  }

#ifdef __STL_MEMBER_TEMPLATES
  template <class _InputIterator>
  void _M_initialize_range(_InputIterator __first, _InputIterator __last,
                           input_iterator_tag) {
    _M_start = iterator();
    _M_finish = iterator();
    _M_end_of_storage = 0;
    for ( ; __first != __last; ++__first) 
      push_back(*__first);
  }

  template <class _ForwardIterator>
  void _M_initialize_range(_ForwardIterator __first, _ForwardIterator __last,
                           forward_iterator_tag) {
    size_type __n = 0;
    distance(__first, __last, __n);
    _M_initialize(__n);
    copy(__first, __last, _M_start);
  }

  template <class _InputIterator>
  void _M_insert_range(iterator __pos,
                       _InputIterator __first, _InputIterator __last,
                       input_iterator_tag) {
    for ( ; __first != __last; ++__first) {
      __pos = insert(__pos, *__first);
      ++__pos;
    }
  }

  template <class _ForwardIterator>
  void _M_insert_range(iterator __position,
                       _ForwardIterator __first, _ForwardIterator __last,
                       forward_iterator_tag) {
    if (__first != __last) {
      size_type __n = 0;
      distance(__first, __last, __n);
      if (capacity() - size() >= __n) {
        copy_backward(__position, end(), _M_finish + difference_type(__n));
        copy(__first, __last, __position);
        _M_finish += difference_type(__n);
      }
      else {
        size_type __len = size() + max(size(), __n);
        unsigned int* __q = _M_bit_alloc(__len);
        iterator __i = copy(begin(), __position, iterator(__q, 0));
        __i = copy(__first, __last, __i);
        _M_finish = copy(__position, end(), __i);
        _M_deallocate();
        _M_end_of_storage = __q + (__len + __WORD_BIT - 1)/__WORD_BIT;
        _M_start = iterator(__q, 0);
      }
    }
  }      

#endif /* __STL_MEMBER_TEMPLATES */

public:
  iterator begin() { return _M_start; }
  const_iterator begin() const { return _M_start; }
  iterator end() { return _M_finish; }
  const_iterator end() const { return _M_finish; }

  reverse_iterator rbegin() { return reverse_iterator(end()); }
  const_reverse_iterator rbegin() const { 
    return const_reverse_iterator(end()); 
  }
  reverse_iterator rend() { return reverse_iterator(begin()); }
  const_reverse_iterator rend() const { 
    return const_reverse_iterator(begin()); 
  }

  size_type size() const { return size_type(end() - begin()); }
  size_type max_size() const { return size_type(-1); }
  size_type capacity() const {
    return size_type(const_iterator(_M_end_of_storage, 0) - begin());
  }
  bool empty() const { return begin() == end(); }

  reference operator[](size_type __n)
    { return *(begin() + difference_type(__n)); }
  const_reference operator[](size_type __n) const
    { return *(begin() + difference_type(__n)); }

#ifdef __STL_THROW_RANGE_ERRORS
  void _M_range_check(size_type __n) const {
    if (__n >= this->size())
      __throw_range_error("vector<bool>");
  }

  reference at(size_type __n)
    { _M_range_check(__n); return (*this)[__n]; }
  const_reference at(size_type __n) const
    { _M_range_check(__n); return (*this)[__n]; }
#endif /* __STL_THROW_RANGE_ERRORS */

  explicit __VECTOR(const allocator_type& __a = allocator_type())
    : __BVECTOR_BASE(__a) {}

  __VECTOR(size_type __n, bool __value,
            const allocator_type& __a = allocator_type())
    : __BVECTOR_BASE(__a)
  {
    _M_initialize(__n);
    fill(_M_start._M_p, _M_end_of_storage, __value ? ~0 : 0);
  }

  explicit __VECTOR(size_type __n)
    : __BVECTOR_BASE(allocator_type())
  {
    _M_initialize(__n);
    fill(_M_start._M_p, _M_end_of_storage, 0);
  }

  __VECTOR(const __VECTOR& __x) : __BVECTOR_BASE(__x.get_allocator()) {
    _M_initialize(__x.size());
    copy(__x.begin(), __x.end(), _M_start);
  }

#ifdef __STL_MEMBER_TEMPLATES

  // Check whether it's an integral type.  If so, it's not an iterator.

  template <class _Integer>
  void _M_initialize_dispatch(_Integer __n, _Integer __x, __true_type) {
    _M_initialize(__n);
    fill(_M_start._M_p, _M_end_of_storage, __x ? ~0 : 0);
  }

  template <class _InputIterator>
  void _M_initialize_dispatch(_InputIterator __first, _InputIterator __last,
                              __false_type) {
    _M_initialize_range(__first, __last, __ITERATOR_CATEGORY(__first));
  }

  template <class _InputIterator>
  __VECTOR(_InputIterator __first, _InputIterator __last,
           const allocator_type& __a = allocator_type())
    : __BVECTOR_BASE(__a)
  {
    typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
    _M_initialize_dispatch(__first, __last, _Integral());
  }
    
#else /* __STL_MEMBER_TEMPLATES */

  __VECTOR(const_iterator __first, const_iterator __last,
           const allocator_type& __a = allocator_type())
    : __BVECTOR_BASE(__a)
  {
    size_type __n = 0;
    distance(__first, __last, __n);
    _M_initialize(__n);
    copy(__first, __last, _M_start);
  }
  __VECTOR(const bool* __first, const bool* __last,
           const allocator_type& __a = allocator_type())
    : __BVECTOR_BASE(__a)
  {
    size_type __n = 0;
    distance(__first, __last, __n);
    _M_initialize(__n);
    copy(__first, __last, _M_start);
  }

#endif /* __STL_MEMBER_TEMPLATES */

  ~__VECTOR() { }

  __VECTOR& operator=(const __VECTOR& __x) {
    if (&__x == this) return *this;
    if (__x.size() > capacity()) {
      _M_deallocate();
      _M_initialize(__x.size());
    }
    copy(__x.begin(), __x.end(), begin());
    _M_finish = begin() + difference_type(__x.size());
    return *this;
  }

  // assign(), a generalized assignment member function.  Two
  // versions: one that takes a count, and one that takes a range.
  // The range version is a member template, so we dispatch on whether
  // or not the type is an integer.

  void _M_fill_assign(size_t __n, bool __x) {
    if (__n > size()) {
      fill(_M_start._M_p, _M_end_of_storage, __x ? ~0 : 0);
      insert(end(), __n - size(), __x);
    }
    else {
      erase(begin() + __n, end());
      fill(_M_start._M_p, _M_end_of_storage, __x ? ~0 : 0);
    }
  }

  void assign(size_t __n, bool __x) { _M_fill_assign(__n, __x); }

#ifdef __STL_MEMBER_TEMPLATES

  template <class _InputIterator>
  void assign(_InputIterator __first, _InputIterator __last) {
    typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
    _M_assign_dispatch(__first, __last, _Integral());
  }

  template <class _Integer>
  void _M_assign_dispatch(_Integer __n, _Integer __val, __true_type)
    { _M_fill_assign((size_t) __n, (bool) __val); }

  template <class _InputIter>
  void _M_assign_dispatch(_InputIter __first, _InputIter __last, __false_type)
    { _M_assign_aux(__first, __last, __ITERATOR_CATEGORY(__first)); }

  template <class _InputIterator>
  void _M_assign_aux(_InputIterator __first, _InputIterator __last,
                     input_iterator_tag) {
    iterator __cur = begin();
    for ( ; __first != __last && __cur != end(); ++__cur, ++__first)
      *__cur = *__first;
    if (__first == __last)
      erase(__cur, end());
    else
      insert(end(), __first, __last);
  }

  template <class _ForwardIterator>
  void _M_assign_aux(_ForwardIterator __first, _ForwardIterator __last,
                     forward_iterator_tag) {
    size_type __len = 0;
    distance(__first, __last, __len);
    if (__len < size())
      erase(copy(__first, __last, begin()), end());
    else {
      _ForwardIterator __mid = __first;
      advance(__mid, size());
      copy(__first, __mid, begin());
      insert(end(), __mid, __last);
    }
  }    

#endif /* __STL_MEMBER_TEMPLATES */

  void reserve(size_type __n) {
    if (capacity() < __n) {
      unsigned int* __q = _M_bit_alloc(__n);
      _M_finish = copy(begin(), end(), iterator(__q, 0));
      _M_deallocate();
      _M_start = iterator(__q, 0);
      _M_end_of_storage = __q + (__n + __WORD_BIT - 1)/__WORD_BIT;
    }
  }

  reference front() { return *begin(); }
  const_reference front() const { return *begin(); }
  reference back() { return *(end() - 1); }
  const_reference back() const { return *(end() - 1); }
  void push_back(bool __x) {
    if (_M_finish._M_p != _M_end_of_storage)
      *_M_finish++ = __x;
    else
      _M_insert_aux(end(), __x);
  }
  void swap(__BVECTOR& __x) {
    __STD::swap(_M_start, __x._M_start);
    __STD::swap(_M_finish, __x._M_finish);
    __STD::swap(_M_end_of_storage, __x._M_end_of_storage);
  }
  iterator insert(iterator __position, bool __x = bool()) {
    difference_type __n = __position - begin();
    if (_M_finish._M_p != _M_end_of_storage && __position == end())
      *_M_finish++ = __x;
    else
      _M_insert_aux(__position, __x);
    return begin() + __n;
  }

#ifdef __STL_MEMBER_TEMPLATES
  // Check whether it's an integral type.  If so, it's not an iterator.

  template <class _Integer>
  void _M_insert_dispatch(iterator __pos, _Integer __n, _Integer __x,
                          __true_type) {
    _M_fill_insert(__pos, __n, __x);
  }

  template <class _InputIterator>
  void _M_insert_dispatch(iterator __pos,
                          _InputIterator __first, _InputIterator __last,
                          __false_type) {
    _M_insert_range(__pos, __first, __last, __ITERATOR_CATEGORY(__first));
  }

  template <class _InputIterator>
  void insert(iterator __position,
              _InputIterator __first, _InputIterator __last) {
    typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
    _M_insert_dispatch(__position, __first, __last, _Integral());
  }

#else /* __STL_MEMBER_TEMPLATES */
  void insert(iterator __position,
              const_iterator __first, const_iterator __last) {
    if (__first == __last) return;
    size_type __n = 0;
    distance(__first, __last, __n);
    if (capacity() - size() >= __n) {
      copy_backward(__position, end(), _M_finish + __n);
      copy(__first, __last, __position);
      _M_finish += __n;
    }
    else {
      size_type __len = size() + max(size(), __n);
      unsigned int* __q = _M_bit_alloc(__len);
      iterator __i = copy(begin(), __position, iterator(__q, 0));
      __i = copy(__first, __last, __i);
      _M_finish = copy(__position, end(), __i);
      _M_deallocate();
      _M_end_of_storage = __q + (__len + __WORD_BIT - 1)/__WORD_BIT;
      _M_start = iterator(__q, 0);
    }
  }

  void insert(iterator __position, const bool* __first, const bool* __last) {
    if (__first == __last) return;
    size_type __n = 0;
    distance(__first, __last, __n);
    if (capacity() - size() >= __n) {
      copy_backward(__position, end(), _M_finish + __n);
      copy(__first, __last, __position);
      _M_finish += __n;
    }
    else {
      size_type __len = size() + max(size(), __n);
      unsigned int* __q = _M_bit_alloc(__len);
      iterator __i = copy(begin(), __position, iterator(__q, 0));
      __i = copy(__first, __last, __i);
      _M_finish = copy(__position, end(), __i);
      _M_deallocate();
      _M_end_of_storage = __q + (__len + __WORD_BIT - 1)/__WORD_BIT;
      _M_start = iterator(__q, 0);
    }
  }
#endif /* __STL_MEMBER_TEMPLATES */

  void _M_fill_insert(iterator __position, size_type __n, bool __x) {
    if (__n == 0) return;
    if (capacity() - size() >= __n) {
      copy_backward(__position, end(), _M_finish + difference_type(__n));
      fill(__position, __position + difference_type(__n), __x);
      _M_finish += difference_type(__n);
    }
    else {
      size_type __len = size() + max(size(), __n);
      unsigned int* __q = _M_bit_alloc(__len);
      iterator __i = copy(begin(), __position, iterator(__q, 0));
      fill_n(__i, __n, __x);
      _M_finish = copy(__position, end(), __i + difference_type(__n));
      _M_deallocate();
      _M_end_of_storage = __q + (__len + __WORD_BIT - 1)/__WORD_BIT;
      _M_start = iterator(__q, 0);
    }
  }

  void insert(iterator __position, size_type __n, bool __x) {
    _M_fill_insert(__position, __n, __x);
  }

  void pop_back() { --_M_finish; }
  iterator erase(iterator __position) {
    if (__position + 1 != end())
      copy(__position + 1, end(), __position);
      --_M_finish;
    return __position;
  }
  iterator erase(iterator __first, iterator __last) {
    _M_finish = copy(__last, end(), __first);
    return __first;
  }
  void resize(size_type __new_size, bool __x = bool()) {
    if (__new_size < size()) 
      erase(begin() + difference_type(__new_size), end());
    else
      insert(end(), __new_size - size(), __x);
  }
  void flip() {
    for (unsigned int* __p = _M_start._M_p; __p != _M_end_of_storage; ++__p)
      *__p = ~*__p;
  }

  void clear() { erase(begin(), end()); }
};

#ifdef __SGI_STL_VECBOOL_TEMPLATE

// This typedef is non-standard.  It is provided for backward compatibility.
typedef vector<bool, alloc> bit_vector;

#else /* __SGI_STL_VECBOOL_TEMPLATE */

inline void swap(bit_vector& __x, bit_vector& __y) {
  __x.swap(__y);
}

inline bool 
operator==(const bit_vector& __x, const bit_vector& __y)
{
  return (__x.size() == __y.size() && 
          equal(__x.begin(), __x.end(), __y.begin()));
}

inline bool 
operator!=(const bit_vector& __x, const bit_vector& __y)
{
  return !(__x == __y);
}

inline bool 
operator<(const bit_vector& __x, const bit_vector& __y)
{
  return lexicographical_compare(__x.begin(), __x.end(), 
                                 __y.begin(), __y.end());
}

inline bool operator>(const bit_vector& __x, const bit_vector& __y)
{
  return __y < __x;
}

inline bool operator<=(const bit_vector& __x, const bit_vector& __y)
{
  return !(__y < __x);
}

inline bool operator>=(const bit_vector& __x, const bit_vector& __y)
{
  return !(__x < __y);
}

#endif /* __SGI_STL_VECBOOL_TEMPLATE */

#undef __SGI_STL_VECBOOL_TEMPLATE
#undef __BVECTOR
#undef __VECTOR
#undef __BVECTOR_BASE
#undef __BVECTOR_TMPL_LIST 

#if defined(__sgi) && !defined(__GNUC__) && (_MIPS_SIM != _MIPS_SIM_ABI32)
#pragma reset woff 1174
#pragma reset woff 1375
#endif

__STL_END_NAMESPACE 

#endif /* __SGI_STL_INTERNAL_BVECTOR_H */

// Local Variables:
// mode:C++
// End:
