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
 * Copyright (c) 1997
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

#ifndef __SGI_STL_INTERNAL_DEQUE_H
#define __SGI_STL_INTERNAL_DEQUE_H

/* Class invariants:
 *  For any nonsingular iterator i:
 *    i.node is the address of an element in the map array.  The
 *      contents of i.node is a pointer to the beginning of a node.
 *    i.first == *(i.node) 
 *    i.last  == i.first + node_size
 *    i.cur is a pointer in the range [i.first, i.last).  NOTE:
 *      the implication of this is that i.cur is always a dereferenceable
 *      pointer, even if i is a past-the-end iterator.
 *  Start and Finish are always nonsingular iterators.  NOTE: this means
 *    that an empty deque must have one node, and that a deque
 *    with N elements, where N is the buffer size, must have two nodes.
 *  For every node other than start.node and finish.node, every element
 *    in the node is an initialized object.  If start.node == finish.node,
 *    then [start.cur, finish.cur) are initialized objects, and
 *    the elements outside that range are uninitialized storage.  Otherwise,
 *    [start.cur, start.last) and [finish.first, finish.cur) are initialized
 *    objects, and [start.first, start.cur) and [finish.cur, finish.last)
 *    are uninitialized storage.
 *  [map, map + map_size) is a valid, non-empty range.  
 *  [start.node, finish.node] is a valid range contained within 
 *    [map, map + map_size).  
 *  A pointer in the range [map, map + map_size) points to an allocated node
 *    if and only if the pointer is in the range [start.node, finish.node].
 */


/*
 * In previous versions of deque, node_size was fixed by the 
 * implementation.  In this version, however, users can select
 * the node size.  Deque has three template parameters; the third,
 * a number of type size_t, is the number of elements per node.
 * If the third template parameter is 0 (which is the default), 
 * then deque will use a default node size.
 *
 * The only reason for using an alternate node size is if your application
 * requires a different performance tradeoff than the default.  If,
 * for example, your program contains many deques each of which contains
 * only a few elements, then you might want to save memory (possibly
 * by sacrificing some speed) by using smaller nodes.
 *
 * Unfortunately, some compilers have trouble with non-type template 
 * parameters; stl_config.h defines __STL_NON_TYPE_TMPL_PARAM_BUG if
 * that is the case.  If your compiler is one of them, then you will
 * not be able to use alternate node sizes; you will have to use the
 * default value.
 */

__STL_BEGIN_NAMESPACE 

#if defined(__sgi) && !defined(__GNUC__) && (_MIPS_SIM != _MIPS_SIM_ABI32)
#pragma set woff 1174
#pragma set woff 1375
#endif

// Note: this function is simply a kludge to work around several compilers'
//  bugs in handling constant expressions.
inline size_t
__deque_buf_size(size_t __n, size_t __size)
{
  return __n != 0 ? __n : (__size < 512 ? size_t(512 / __size) : size_t(1));
}

#ifndef __STL_NON_TYPE_TMPL_PARAM_BUG
template <class _Tp, class _Ref, class _Ptr, size_t __bufsiz>
struct _Deque_iterator {
  typedef _Deque_iterator<_Tp,_Tp&,_Tp*,__bufsiz>             iterator;
  typedef _Deque_iterator<_Tp,const _Tp&,const _Tp*,__bufsiz> const_iterator;
  static size_t 
    _S_buffer_size() { return __deque_buf_size(__bufsiz, sizeof(_Tp)); }
#else /* __STL_NON_TYPE_TMPL_PARAM_BUG */
template <class _Tp, class _Ref, class _Ptr>
struct _Deque_iterator {
  typedef _Deque_iterator<_Tp, _Tp&, _Tp*>             iterator;
  typedef _Deque_iterator<_Tp, const _Tp&, const _Tp*> const_iterator;
  static size_t 
    _S_buffer_size() { return __deque_buf_size(0, sizeof(_Tp)); }
#endif

  typedef random_access_iterator_tag iterator_category;
  typedef _Tp value_type;
  typedef _Ptr pointer;
  typedef _Ref reference;
  typedef size_t size_type;
  typedef ptrdiff_t difference_type;
  typedef _Tp** _Map_pointer;

  typedef _Deque_iterator _Self;

  _Tp* _M_cur;
  _Tp* _M_first;
  _Tp* _M_last;
  _Map_pointer _M_node;

  _Deque_iterator(_Tp* __x, _Map_pointer __y) 
    : _M_cur(__x), _M_first(*__y),
      _M_last(*__y + _S_buffer_size()), _M_node(__y) {}
  _Deque_iterator() : _M_cur(0), _M_first(0), _M_last(0), _M_node(0) {}
  _Deque_iterator(const iterator& __x)
    : _M_cur(__x._M_cur), _M_first(__x._M_first), 
      _M_last(__x._M_last), _M_node(__x._M_node) {}

  reference operator*() const { return *_M_cur; }
#ifndef __SGI_STL_NO_ARROW_OPERATOR
  pointer operator->() const { return _M_cur; }
#endif /* __SGI_STL_NO_ARROW_OPERATOR */

  difference_type operator-(const _Self& __x) const {
    return difference_type(_S_buffer_size()) * (_M_node - __x._M_node - 1) +
      (_M_cur - _M_first) + (__x._M_last - __x._M_cur);
  }

  _Self& operator++() {
    ++_M_cur;
    if (_M_cur == _M_last) {
      _M_set_node(_M_node + 1);
      _M_cur = _M_first;
    }
    return *this; 
  }
  _Self operator++(int)  {
    _Self __tmp = *this;
    ++*this;
    return __tmp;
  }

  _Self& operator--() {
    if (_M_cur == _M_first) {
      _M_set_node(_M_node - 1);
      _M_cur = _M_last;
    }
    --_M_cur;
    return *this;
  }
  _Self operator--(int) {
    _Self __tmp = *this;
    --*this;
    return __tmp;
  }

  _Self& operator+=(difference_type __n)
  {
    difference_type __offset = __n + (_M_cur - _M_first);
    if (__offset >= 0 && __offset < difference_type(_S_buffer_size()))
      _M_cur += __n;
    else {
      difference_type __node_offset =
        __offset > 0 ? __offset / difference_type(_S_buffer_size())
                   : -difference_type((-__offset - 1) / _S_buffer_size()) - 1;
      _M_set_node(_M_node + __node_offset);
      _M_cur = _M_first + 
        (__offset - __node_offset * difference_type(_S_buffer_size()));
    }
    return *this;
  }

  _Self operator+(difference_type __n) const
  {
    _Self __tmp = *this;
    return __tmp += __n;
  }

  _Self& operator-=(difference_type __n) { return *this += -__n; }
 
  _Self operator-(difference_type __n) const {
    _Self __tmp = *this;
    return __tmp -= __n;
  }

  reference operator[](difference_type __n) const { return *(*this + __n); }

  bool operator==(const _Self& __x) const { return _M_cur == __x._M_cur; }
  bool operator!=(const _Self& __x) const { return !(*this == __x); }
  bool operator<(const _Self& __x) const {
    return (_M_node == __x._M_node) ? 
      (_M_cur < __x._M_cur) : (_M_node < __x._M_node);
  }

  void _M_set_node(_Map_pointer __new_node) {
    _M_node = __new_node;
    _M_first = *__new_node;
    _M_last = _M_first + difference_type(_S_buffer_size());
  }
};

#ifndef __STL_CLASS_PARTIAL_SPECIALIZATION

#ifndef __STL_NON_TYPE_TMPL_PARAM_BUG

template <class _Tp, class _Ref, class _Ptr, size_t __bufsiz>
inline random_access_iterator_tag
iterator_category(const _Deque_iterator<_Tp,_Ref,_Ptr,__bufsiz>&) {
  return random_access_iterator_tag();
}

template <class _Tp, class _Ref, class _Ptr, size_t __bufsiz>
inline _Tp*
value_type(const _Deque_iterator<_Tp,_Ref,_Ptr,__bufsiz>&) {
  return 0;
}

template <class _Tp, class _Ref, class _Ptr, size_t __bufsiz>
inline ptrdiff_t*
distance_type(const _Deque_iterator<_Tp,_Ref,_Ptr,__bufsiz>&) {
  return 0;
}

#else /* __STL_NON_TYPE_TMPL_PARAM_BUG */

template <class _Tp, class _Ref, class _Ptr>
inline random_access_iterator_tag
iterator_category(const _Deque_iterator<_Tp,_Ref,_Ptr>&)
{
  return random_access_iterator_tag();
}

template <class _Tp, class _Ref, class _Ptr>
inline _Tp*
value_type(const _Deque_iterator<_Tp,_Ref,_Ptr>&) { return 0; }

template <class _Tp, class _Ref, class _Ptr>
inline ptrdiff_t*
distance_type(const _Deque_iterator<_Tp,_Ref,_Ptr>&) {
  return 0;
}

#endif /* __STL_NON_TYPE_TMPL_PARAM_BUG */

#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

// Deque base class.  It has two purposes.  First, its constructor
//  and destructor allocate (but don't initialize) storage.  This makes
//  exception safety easier.  Second, the base class encapsulates all of
//  the differences between SGI-style allocators and standard-conforming
//  allocators.

#ifdef __STL_USE_STD_ALLOCATORS

// Base class for ordinary allocators.
template <class _Tp, class _Alloc, size_t __bufsiz, bool __is_static>
class _Deque_alloc_base {
public:
  typedef typename _Alloc_traits<_Tp,_Alloc>::allocator_type allocator_type;
  allocator_type get_allocator() const { return node_allocator; }

  _Deque_alloc_base(const allocator_type& __a)
    : node_allocator(__a), map_allocator(__a), _M_map(0), _M_map_size(0)
  {}
  
protected:
  typedef typename _Alloc_traits<_Tp*, _Alloc>::allocator_type
          map_allocator_type;

  allocator_type node_allocator;
  map_allocator_type map_allocator;

  _Tp* _M_allocate_node() {
    return node_allocator.allocate(__deque_buf_size(__bufsiz,sizeof(_Tp)));
  }
  void _M_deallocate_node(_Tp* __p) {
    node_allocator.deallocate(__p, __deque_buf_size(__bufsiz,sizeof(_Tp)));
  }
  _Tp** _M_allocate_map(size_t __n) 
    { return map_allocator.allocate(__n); }
  void _M_deallocate_map(_Tp** __p, size_t __n) 
    { map_allocator.deallocate(__p, __n); }

  _Tp** _M_map;
  size_t _M_map_size;
};

// Specialization for instanceless allocators.
template <class _Tp, class _Alloc, size_t __bufsiz>
class _Deque_alloc_base<_Tp, _Alloc, __bufsiz, true>
{
public:
  typedef typename _Alloc_traits<_Tp,_Alloc>::allocator_type allocator_type;
  allocator_type get_allocator() const { return allocator_type(); }

  _Deque_alloc_base(const allocator_type&) : _M_map(0), _M_map_size(0) {}
  
protected:
  typedef typename _Alloc_traits<_Tp, _Alloc>::_Alloc_type _Node_alloc_type;
  typedef typename _Alloc_traits<_Tp*, _Alloc>::_Alloc_type _Map_alloc_type;

  _Tp* _M_allocate_node()
    { return _Node_alloc_type::allocate(__deque_buf_size(__bufsiz, 
                                                         sizeof(_Tp))); }
  void _M_deallocate_node(_Tp* __p)
    { _Node_alloc_type::deallocate(__p, __deque_buf_size(__bufsiz, 
                                                         sizeof(_Tp))); }
  _Tp** _M_allocate_map(size_t __n) 
    { return _Map_alloc_type::allocate(__n); }
  void _M_deallocate_map(_Tp** __p, size_t __n) 
    { _Map_alloc_type::deallocate(__p, __n); }

  _Tp** _M_map;
  size_t _M_map_size;
};

template <class _Tp, class _Alloc, size_t __bufsiz>
class _Deque_base
  : public _Deque_alloc_base<_Tp,_Alloc,__bufsiz, 
                              _Alloc_traits<_Tp, _Alloc>::_S_instanceless>
{
public:
  typedef _Deque_alloc_base<_Tp,_Alloc,__bufsiz,
                             _Alloc_traits<_Tp, _Alloc>::_S_instanceless>
          _Base;
  typedef typename _Base::allocator_type allocator_type;
  typedef _Deque_iterator<_Tp,_Tp&,_Tp*,__bufsiz>              iterator;
  typedef _Deque_iterator<_Tp,const _Tp&,const _Tp&, __bufsiz> const_iterator;

  _Deque_base(const allocator_type& __a, size_t __num_elements)
    : _Base(__a), _M_start(), _M_finish()
    { _M_initialize_map(__num_elements); }
  _Deque_base(const allocator_type& __a) 
    : _Base(__a), _M_start(), _M_finish() {}
  ~_Deque_base();    

protected:
  void _M_initialize_map(size_t);
  void _M_create_nodes(_Tp** __nstart, _Tp** __nfinish);
  void _M_destroy_nodes(_Tp** __nstart, _Tp** __nfinish);
  enum { _S_initial_map_size = 8 };

protected:
  iterator _M_start;
  iterator _M_finish;
};

#else /* __STL_USE_STD_ALLOCATORS */

template <class _Tp, class _Alloc, size_t __bufsiz>
class _Deque_base {
public:
#ifndef __STL_NON_TYPE_TMPL_PARAM_BUG
  typedef _Deque_iterator<_Tp,_Tp&,_Tp*,__bufsiz>              iterator;
  typedef _Deque_iterator<_Tp,const _Tp&,const _Tp*, __bufsiz> const_iterator;
#else /* __STL_NON_TYPE_TMPL_PARAM_BUG */
  typedef _Deque_iterator<_Tp,_Tp&,_Tp*>                      iterator;
  typedef _Deque_iterator<_Tp,const _Tp&,const _Tp*>          const_iterator;
#endif /* __STL_NON_TYPE_TMPL_PARAM_BUG */

  typedef _Alloc allocator_type;
  allocator_type get_allocator() const { return allocator_type(); }

  _Deque_base(const allocator_type&, size_t __num_elements)
    : _M_map(0), _M_map_size(0),  _M_start(), _M_finish() {
    _M_initialize_map(__num_elements);
  }
  _Deque_base(const allocator_type&)
    : _M_map(0), _M_map_size(0),  _M_start(), _M_finish() {}
  ~_Deque_base();    

protected:
  void _M_initialize_map(size_t);
  void _M_create_nodes(_Tp** __nstart, _Tp** __nfinish);
  void _M_destroy_nodes(_Tp** __nstart, _Tp** __nfinish);
  enum { _S_initial_map_size = 8 };

protected:
  _Tp** _M_map;
  size_t _M_map_size;  
  iterator _M_start;
  iterator _M_finish;

  typedef simple_alloc<_Tp, _Alloc>  _Node_alloc_type;
  typedef simple_alloc<_Tp*, _Alloc> _Map_alloc_type;

  _Tp* _M_allocate_node()
    { return _Node_alloc_type::allocate(__deque_buf_size(__bufsiz, 
                                                         sizeof(_Tp))); }
  void _M_deallocate_node(_Tp* __p)
    { _Node_alloc_type::deallocate(__p, __deque_buf_size(__bufsiz, 
                                                         sizeof(_Tp))); }
  _Tp** _M_allocate_map(size_t __n) 
    { return _Map_alloc_type::allocate(__n); }
  void _M_deallocate_map(_Tp** __p, size_t __n) 
    { _Map_alloc_type::deallocate(__p, __n); }
};

#endif /* __STL_USE_STD_ALLOCATORS */

// Non-inline member functions from _Deque_base.

template <class _Tp, class _Alloc, size_t __bufsiz>
_Deque_base<_Tp,_Alloc,__bufsiz>::~_Deque_base() {
  if (_M_map) {
    _M_destroy_nodes(_M_start._M_node, _M_finish._M_node + 1);
    _M_deallocate_map(_M_map, _M_map_size);
  }
}

template <class _Tp, class _Alloc, size_t __bufsiz>
void
_Deque_base<_Tp,_Alloc,__bufsiz>::_M_initialize_map(size_t __num_elements)
{
  size_t __num_nodes = 
    __num_elements / __deque_buf_size(__bufsiz, sizeof(_Tp)) + 1;

  _M_map_size = max((size_t) _S_initial_map_size, __num_nodes + 2);
  _M_map = _M_allocate_map(_M_map_size);

  _Tp** __nstart = _M_map + (_M_map_size - __num_nodes) / 2;
  _Tp** __nfinish = __nstart + __num_nodes;
    
  __STL_TRY {
    _M_create_nodes(__nstart, __nfinish);
  }
  __STL_UNWIND((_M_deallocate_map(_M_map, _M_map_size), 
                _M_map = 0, _M_map_size = 0));
  _M_start._M_set_node(__nstart);
  _M_finish._M_set_node(__nfinish - 1);
  _M_start._M_cur = _M_start._M_first;
  _M_finish._M_cur = _M_finish._M_first +
               __num_elements % __deque_buf_size(__bufsiz, sizeof(_Tp));
}

template <class _Tp, class _Alloc, size_t __bufsiz>
void
_Deque_base<_Tp,_Alloc,__bufsiz>::_M_create_nodes(_Tp** __nstart,
                                                  _Tp** __nfinish)
{
  _Tp** __cur;
  __STL_TRY {
    for (__cur = __nstart; __cur < __nfinish; ++__cur)
      *__cur = _M_allocate_node();
  }
  __STL_UNWIND(_M_destroy_nodes(__nstart, __cur));
}

template <class _Tp, class _Alloc, size_t __bufsiz>
void 
_Deque_base<_Tp,_Alloc,__bufsiz>::_M_destroy_nodes(_Tp** __nstart,
                                                   _Tp** __nfinish)
{
  for (_Tp** __n = __nstart; __n < __nfinish; ++__n)
    _M_deallocate_node(*__n);
}

// See __deque_buf_size().  The only reason that the default value is 0
//  is as a workaround for bugs in the way that some compilers handle
//  constant expressions.
template <class _Tp, class _Alloc = __STL_DEFAULT_ALLOCATOR(_Tp), 
          size_t __bufsiz = 0> 
class deque : protected _Deque_base<_Tp, _Alloc, __bufsiz> {
  typedef _Deque_base<_Tp, _Alloc, __bufsiz> _Base;
public:                         // Basic types
  typedef _Tp value_type;
  typedef value_type* pointer;
  typedef const value_type* const_pointer;
  typedef value_type& reference;
  typedef const value_type& const_reference;
  typedef size_t size_type;
  typedef ptrdiff_t difference_type;

  typedef typename _Base::allocator_type allocator_type;
  allocator_type get_allocator() const { return _Base::get_allocator(); }

public:                         // Iterators
  typedef typename _Base::iterator       iterator;
  typedef typename _Base::const_iterator const_iterator;

#ifdef __STL_CLASS_PARTIAL_SPECIALIZATION
  typedef reverse_iterator<const_iterator> const_reverse_iterator;
  typedef reverse_iterator<iterator> reverse_iterator;
#else /* __STL_CLASS_PARTIAL_SPECIALIZATION */
  typedef reverse_iterator<const_iterator, value_type, const_reference, 
                           difference_type>  
          const_reverse_iterator;
  typedef reverse_iterator<iterator, value_type, reference, difference_type>
          reverse_iterator; 
#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

protected:                      // Internal typedefs
  typedef pointer* _Map_pointer;
  static size_t _S_buffer_size()
    { return __deque_buf_size(__bufsiz, sizeof(_Tp)); }

protected:
#ifdef __STL_USE_NAMESPACES
  using _Base::_M_initialize_map;
  using _Base::_M_create_nodes;
  using _Base::_M_destroy_nodes;
  using _Base::_M_allocate_node;
  using _Base::_M_deallocate_node;
  using _Base::_M_allocate_map;
  using _Base::_M_deallocate_map;

  using _Base::_M_map;
  using _Base::_M_map_size;
  using _Base::_M_start;
  using _Base::_M_finish;
#endif /* __STL_USE_NAMESPACES */

public:                         // Basic accessors
  iterator begin() { return _M_start; }
  iterator end() { return _M_finish; }
  const_iterator begin() const { return _M_start; }
  const_iterator end() const { return _M_finish; }

  reverse_iterator rbegin() { return reverse_iterator(_M_finish); }
  reverse_iterator rend() { return reverse_iterator(_M_start); }
  const_reverse_iterator rbegin() const 
    { return const_reverse_iterator(_M_finish); }
  const_reverse_iterator rend() const 
    { return const_reverse_iterator(_M_start); }

  reference operator[](size_type __n)
    { return _M_start[difference_type(__n)]; }
  const_reference operator[](size_type __n) const 
    { return _M_start[difference_type(__n)]; }

  reference front() { return *_M_start; }
  reference back() {
    iterator __tmp = _M_finish;
    --__tmp;
    return *__tmp;
  }
  const_reference front() const { return *_M_start; }
  const_reference back() const {
    const_iterator __tmp = _M_finish;
    --__tmp;
    return *__tmp;
  }

  size_type size() const { return _M_finish - _M_start;; }
  size_type max_size() const { return size_type(-1); }
  bool empty() const { return _M_finish == _M_start; }

public:                         // Constructor, destructor.
  explicit deque(const allocator_type& __a = allocator_type()) 
    : _Base(__a, 0) {}
  deque(const deque& __x) : _Base(__x.get_allocator(), __x.size()) 
    { uninitialized_copy(__x.begin(), __x.end(), _M_start); }
  deque(size_type __n, const value_type& __value,
        const allocator_type& __a = allocator_type()) : _Base(__a, __n)
    { _M_fill_initialize(__value); }
  explicit deque(size_type __n) : _Base(allocator_type(), __n)
    { _M_fill_initialize(value_type()); }

#ifdef __STL_MEMBER_TEMPLATES

  // Check whether it's an integral type.  If so, it's not an iterator.
  template <class _InputIterator>
  deque(_InputIterator __first, _InputIterator __last,
        const allocator_type& __a = allocator_type()) : _Base(__a) {
    typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
    _M_initialize_dispatch(__first, __last, _Integral());
  }

  template <class _Integer>
  void _M_initialize_dispatch(_Integer __n, _Integer __x, __true_type) {
    _M_initialize_map(__n);
    _M_fill_initialize(__x);
  }

  template <class _InputIter>
  void _M_initialize_dispatch(_InputIter __first, _InputIter __last,
                              __false_type) {
    _M_range_initialize(__first, __last, __ITERATOR_CATEGORY(__first));
  }

#else /* __STL_MEMBER_TEMPLATES */

  deque(const value_type* __first, const value_type* __last,
        const allocator_type& __a = allocator_type()) 
    : _Base(__a, __last - __first)
    { uninitialized_copy(__first, __last, _M_start); }
  deque(const_iterator __first, const_iterator __last,
        const allocator_type& __a = allocator_type()) 
    : _Base(__a, __last - __first)
    { uninitialized_copy(__first, __last, _M_start); }

#endif /* __STL_MEMBER_TEMPLATES */

  ~deque() { destroy(_M_start, _M_finish); }

  deque& operator= (const deque& __x) {
    const size_type __len = size();
    if (&__x != this) {
      if (__len >= __x.size())
        erase(copy(__x.begin(), __x.end(), _M_start), _M_finish);
      else {
        const_iterator __mid = __x.begin() + difference_type(__len);
        copy(__x.begin(), __mid, _M_start);
        insert(_M_finish, __mid, __x.end());
      }
    }
    return *this;
  }        

  void swap(deque& __x) {
    __STD::swap(_M_start, __x._M_start);
    __STD::swap(_M_finish, __x._M_finish);
    __STD::swap(_M_map, __x._M_map);
    __STD::swap(_M_map_size, __x._M_map_size);
  }

public: 
  // assign(), a generalized assignment member function.  Two
  // versions: one that takes a count, and one that takes a range.
  // The range version is a member template, so we dispatch on whether
  // or not the type is an integer.

  void assign(size_type __n, const _Tp& __val) {
    if (__n > size()) {
      fill(begin(), end(), __val);
      insert(end(), __n - size(), __val);
    }
    else {
      erase(begin() + __n, end());
      fill(begin(), end(), __val);
    }
  }

#ifdef __STL_MEMBER_TEMPLATES

  template <class _InputIterator>
  void assign(_InputIterator __first, _InputIterator __last) {
    typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
    _M_assign_dispatch(__first, __last, _Integral());
  }

private:                        // helper functions for assign() 

  template <class _Integer>
  void _M_assign_dispatch(_Integer __n, _Integer __val, __true_type)
    { assign((size_type) __n, (_Tp) __val); }

  template <class _InputIterator>
  void _M_assign_dispatch(_InputIterator __first, _InputIterator __last,
                          __false_type) {
    _M_assign_aux(__first, __last, __ITERATOR_CATEGORY(__first));
  }

  template <class _InputIterator>
  void _M_assign_aux(_InputIterator __first, _InputIterator __last,
                     input_iterator_tag);

  template <class _ForwardIterator>
  void _M_assign_aux(_ForwardIterator __first, _ForwardIterator __last,
                     forward_iterator_tag) {
    size_type __len = 0;
    distance(__first, __last, __len);
    if (__len > size()) {
      _ForwardIterator __mid = __first;
      advance(__mid, size());
      copy(__first, __mid, begin());
      insert(end(), __mid, __last);
    }
    else
      erase(copy(__first, __last, begin()), end());
  }

#endif /* __STL_MEMBER_TEMPLATES */

public:                         // push_* and pop_*
  
  void push_back(const value_type& __t) {
    if (_M_finish._M_cur != _M_finish._M_last - 1) {
      construct(_M_finish._M_cur, __t);
      ++_M_finish._M_cur;
    }
    else
      _M_push_back_aux(__t);
  }

  void push_back() {
    if (_M_finish._M_cur != _M_finish._M_last - 1) {
      construct(_M_finish._M_cur);
      ++_M_finish._M_cur;
    }
    else
      _M_push_back_aux();
  }

  void push_front(const value_type& __t) {
    if (_M_start._M_cur != _M_start._M_first) {
      construct(_M_start._M_cur - 1, __t);
      --_M_start._M_cur;
    }
    else
      _M_push_front_aux(__t);
  }

  void push_front() {
    if (_M_start._M_cur != _M_start._M_first) {
      construct(_M_start._M_cur - 1);
      --_M_start._M_cur;
    }
    else
      _M_push_front_aux();
  }


  void pop_back() {
    if (_M_finish._M_cur != _M_finish._M_first) {
      --_M_finish._M_cur;
      destroy(_M_finish._M_cur);
    }
    else
      _M_pop_back_aux();
  }

  void pop_front() {
    if (_M_start._M_cur != _M_start._M_last - 1) {
      destroy(_M_start._M_cur);
      ++_M_start._M_cur;
    }
    else 
      _M_pop_front_aux();
  }

public:                         // Insert

  iterator insert(iterator position, const value_type& __x) {
    if (position._M_cur == _M_start._M_cur) {
      push_front(__x);
      return _M_start;
    }
    else if (position._M_cur == _M_finish._M_cur) {
      push_back(__x);
      iterator __tmp = _M_finish;
      --__tmp;
      return __tmp;
    }
    else {
      return _M_insert_aux(position, __x);
    }
  }

  iterator insert(iterator __position)
    { return insert(__position, value_type()); }

  void insert(iterator __pos, size_type __n, const value_type& __x); 

#ifdef __STL_MEMBER_TEMPLATES  

  // Check whether it's an integral type.  If so, it's not an iterator.
  template <class _InputIterator>
  void insert(iterator __pos, _InputIterator __first, _InputIterator __last) {
    typedef typename _Is_integer<_InputIterator>::_Integral _Integral;
    _M_insert_dispatch(__pos, __first, __last, _Integral());
  }

  template <class _Integer>
  void _M_insert_dispatch(iterator __pos, _Integer __n, _Integer __x,
                          __true_type) {
    insert(__pos, (size_type) __n, (value_type) __x);
  }

  template <class _InputIterator>
  void _M_insert_dispatch(iterator __pos,
                          _InputIterator __first, _InputIterator __last,
                          __false_type) {
    insert(__pos, __first, __last, __ITERATOR_CATEGORY(__first));
  }

#else /* __STL_MEMBER_TEMPLATES */

  void insert(iterator __pos,
              const value_type* __first, const value_type* __last);
  void insert(iterator __pos,
              const_iterator __first, const_iterator __last);

#endif /* __STL_MEMBER_TEMPLATES */

  void resize(size_type __new_size, const value_type& __x) {
    const size_type __len = size();
    if (__new_size < __len) 
      erase(_M_start + __new_size, _M_finish);
    else
      insert(_M_finish, __new_size - __len, __x);
  }

  void resize(size_type new_size) { resize(new_size, value_type()); }

public:                         // Erase
  iterator erase(iterator __pos) {
    iterator __next = __pos;
    ++__next;
    difference_type __index = __pos - _M_start;
    if (__index < (size() >> 1)) {
      copy_backward(_M_start, __pos, __next);
      pop_front();
    }
    else {
      copy(__next, _M_finish, __pos);
      pop_back();
    }
    return _M_start + __index;
  }

  iterator erase(iterator __first, iterator __last);
  void clear(); 

protected:                        // Internal construction/destruction

  void _M_fill_initialize(const value_type& __value);

#ifdef __STL_MEMBER_TEMPLATES  

  template <class _InputIterator>
  void _M_range_initialize(_InputIterator __first, _InputIterator __last,
                        input_iterator_tag);

  template <class _ForwardIterator>
  void _M_range_initialize(_ForwardIterator __first, _ForwardIterator __last,
                        forward_iterator_tag);

#endif /* __STL_MEMBER_TEMPLATES */

protected:                        // Internal push_* and pop_*

  void _M_push_back_aux(const value_type&);
  void _M_push_back_aux();
  void _M_push_front_aux(const value_type&);
  void _M_push_front_aux();
  void _M_pop_back_aux();
  void _M_pop_front_aux();

protected:                        // Internal insert functions

#ifdef __STL_MEMBER_TEMPLATES  

  template <class _InputIterator>
  void insert(iterator __pos, _InputIterator __first, _InputIterator __last,
              input_iterator_tag);

  template <class _ForwardIterator>
  void insert(iterator __pos,
              _ForwardIterator __first, _ForwardIterator __last,
              forward_iterator_tag);

#endif /* __STL_MEMBER_TEMPLATES */

  iterator _M_insert_aux(iterator __pos, const value_type& __x);
  iterator _M_insert_aux(iterator __pos);
  void _M_insert_aux(iterator __pos, size_type __n, const value_type& __x);

#ifdef __STL_MEMBER_TEMPLATES  

  template <class _ForwardIterator>
  void _M_insert_aux(iterator __pos, 
                     _ForwardIterator __first, _ForwardIterator __last,
                     size_type __n);

#else /* __STL_MEMBER_TEMPLATES */
  
  void _M_insert_aux(iterator __pos,
                     const value_type* __first, const value_type* __last,
                     size_type __n);

  void _M_insert_aux(iterator __pos, 
                     const_iterator __first, const_iterator __last,
                     size_type __n);
 
#endif /* __STL_MEMBER_TEMPLATES */

  iterator _M_reserve_elements_at_front(size_type __n) {
    size_type __vacancies = _M_start._M_cur - _M_start._M_first;
    if (__n > __vacancies) 
      _M_new_elements_at_front(__n - __vacancies);
    return _M_start - difference_type(__n);
  }

  iterator _M_reserve_elements_at_back(size_type __n) {
    size_type __vacancies = (_M_finish._M_last - _M_finish._M_cur) - 1;
    if (__n > __vacancies)
      _M_new_elements_at_back(__n - __vacancies);
    return _M_finish + difference_type(__n);
  }

  void _M_new_elements_at_front(size_type __new_elements);
  void _M_new_elements_at_back(size_type __new_elements);

protected:                      // Allocation of _M_map and nodes

  // Makes sure the _M_map has space for new nodes.  Does not actually
  //  add the nodes.  Can invalidate _M_map pointers.  (And consequently, 
  //  deque iterators.)

  void _M_reserve_map_at_back (size_type __nodes_to_add = 1) {
    if (__nodes_to_add + 1 > _M_map_size - (_M_finish._M_node - _M_map))
      _M_reallocate_map(__nodes_to_add, false);
  }

  void _M_reserve_map_at_front (size_type __nodes_to_add = 1) {
    if (__nodes_to_add > size_type(_M_start._M_node - _M_map))
      _M_reallocate_map(__nodes_to_add, true);
  }

  void _M_reallocate_map(size_type __nodes_to_add, bool __add_at_front);
 
#ifdef __STL_NON_TYPE_TMPL_PARAM_BUG
public:
  bool operator==(const deque<_Tp,_Alloc,0>& __x) const {
    return size() == __x.size() && equal(begin(), end(), __x.begin());
  }
  bool operator!=(const deque<_Tp,_Alloc,0>& __x) const {
    return size() != __x.size() || !equal(begin(), end(), __x.begin());
  }
  bool operator<(const deque<_Tp,_Alloc,0>& __x) const {
    return lexicographical_compare(begin(), end(), __x.begin(), __x.end());
  }
#endif /* __STL_NON_TYPE_TMPL_PARAM_BUG */
};

// Non-inline member functions

#ifdef __STL_MEMBER_TEMPLATES

template <class _Tp, class _Alloc, size_t __bufsize>
template <class _InputIter>
void deque<_Tp, _Alloc, __bufsize>
  ::_M_assign_aux(_InputIter __first, _InputIter __last, input_iterator_tag)
{
  iterator __cur = begin();
  for ( ; __first != __last && __cur != end(); ++__cur, ++__first)
    *__cur = *__first;
  if (__first == __last)
    erase(__cur, end());
  else
    insert(end(), __first, __last);
}

#endif /* __STL_MEMBER_TEMPLATES */

template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp, _Alloc, __bufsize>::insert(iterator __pos,
                                      size_type __n, const value_type& __x)
{
  if (__pos._M_cur == _M_start._M_cur) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    uninitialized_fill(__new_start, _M_start, __x);
    _M_start = __new_start;
  }
  else if (__pos._M_cur == _M_finish._M_cur) {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    uninitialized_fill(_M_finish, __new_finish, __x);
    _M_finish = __new_finish;
  }
  else 
    _M_insert_aux(__pos, __n, __x);
}

#ifndef __STL_MEMBER_TEMPLATES  

template <class _Tp, class _Alloc, size_t __bufsize>
void deque<_Tp, _Alloc, __bufsize>::insert(iterator __pos,
                                           const value_type* __first,
                                           const value_type* __last) {
  size_type __n = __last - __first;
  if (__pos._M_cur == _M_start._M_cur) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    __STL_TRY {
      uninitialized_copy(__first, __last, __new_start);
      _M_start = __new_start;
    }
    __STL_UNWIND(_M_destroy_nodes(__new_start._M_node, _M_start._M_node));
  }
  else if (__pos._M_cur == _M_finish._M_cur) {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    __STL_TRY {
      uninitialized_copy(__first, __last, _M_finish);
      _M_finish = __new_finish;
    }
    __STL_UNWIND(_M_destroy_nodes(_M_finish._M_node + 1, 
                                  __new_finish._M_node + 1));
  }
  else
    _M_insert_aux(__pos, __first, __last, __n);
}

template <class _Tp, class _Alloc, size_t __bufsize>
void deque<_Tp,_Alloc,__bufsize>::insert(iterator __pos,
                                         const_iterator __first,
                                         const_iterator __last)
{
  size_type __n = __last - __first;
  if (__pos._M_cur == _M_start._M_cur) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    __STL_TRY {
      uninitialized_copy(__first, __last, __new_start);
      _M_start = __new_start;
    }
    __STL_UNWIND(_M_destroy_nodes(__new_start._M_node, _M_start._M_node));
  }
  else if (__pos._M_cur == _M_finish._M_cur) {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    __STL_TRY {
      uninitialized_copy(__first, __last, _M_finish);
      _M_finish = __new_finish;
    }
    __STL_UNWIND(_M_destroy_nodes(_M_finish._M_node + 1, 
                 __new_finish._M_node + 1));
  }
  else
    _M_insert_aux(__pos, __first, __last, __n);
}

#endif /* __STL_MEMBER_TEMPLATES */

template <class _Tp, class _Alloc, size_t __bufsize>
deque<_Tp,_Alloc,__bufsize>::iterator 
deque<_Tp,_Alloc,__bufsize>::erase(iterator __first, iterator __last)
{
  if (__first == _M_start && __last == _M_finish) {
    clear();
    return _M_finish;
  }
  else {
    difference_type __n = __last - __first;
    difference_type __elems_before = __first - _M_start;
    if (__elems_before < (size() - __n) / 2) {
      copy_backward(_M_start, __first, __last);
      iterator __new_start = _M_start + __n;
      destroy(_M_start, __new_start);
      _M_destroy_nodes(__new_start._M_node, _M_start._M_node);
      _M_start = __new_start;
    }
    else {
      copy(__last, _M_finish, __first);
      iterator __new_finish = _M_finish - __n;
      destroy(__new_finish, _M_finish);
      _M_destroy_nodes(__new_finish._M_node + 1, _M_finish._M_node + 1);
      _M_finish = __new_finish;
    }
    return _M_start + __elems_before;
  }
}

template <class _Tp, class _Alloc, size_t __bufsize>
void deque<_Tp,_Alloc,__bufsize>::clear()
{
  for (_Map_pointer __node = _M_start._M_node + 1;
       __node < _M_finish._M_node;
       ++__node) {
    destroy(*__node, *__node + _S_buffer_size());
    _M_deallocate_node(*__node);
  }

  if (_M_start._M_node != _M_finish._M_node) {
    destroy(_M_start._M_cur, _M_start._M_last);
    destroy(_M_finish._M_first, _M_finish._M_cur);
    _M_deallocate_node(_M_finish._M_first);
  }
  else
    destroy(_M_start._M_cur, _M_finish._M_cur);

  _M_finish = _M_start;
}

// Precondition: _M_start and _M_finish have already been initialized,
// but none of the deque's elements have yet been constructed.
template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp,_Alloc,__bufsize>::_M_fill_initialize(const value_type& __value) {
  _Map_pointer __cur;
  __STL_TRY {
    for (__cur = _M_start._M_node; __cur < _M_finish._M_node; ++__cur)
      uninitialized_fill(*__cur, *__cur + _S_buffer_size(), __value);
    uninitialized_fill(_M_finish._M_first, _M_finish._M_cur, __value);
  }
  __STL_UNWIND(destroy(_M_start, iterator(*__cur, __cur)));
}

#ifdef __STL_MEMBER_TEMPLATES  

template <class _Tp, class _Alloc, size_t __bufsize>
template <class _InputIterator>
void
deque<_Tp,_Alloc,__bufsize>::_M_range_initialize(_InputIterator __first,
                                                 _InputIterator __last,
                                                 input_iterator_tag)
{
  _M_initialize_map(0);
  for ( ; __first != __last; ++__first)
    push_back(*__first);
}

template <class _Tp, class _Alloc, size_t __bufsize>
template <class _ForwardIterator>
void
deque<_Tp,_Alloc,__bufsize>::_M_range_initialize(_ForwardIterator __first,
                                                 _ForwardIterator __last,
                                                 forward_iterator_tag)
{
  size_type __n = 0;
  distance(__first, __last, __n);
  _M_initialize_map(__n);

  _Map_pointer __cur_node;
  __STL_TRY {
    for (__cur_node = _M_start._M_node; 
         __cur_node < _M_finish._M_node; 
	 ++__cur_node) {
      _ForwardIterator __mid = __first;
      advance(__mid, _S_buffer_size());
      uninitialized_copy(__first, __mid, *__cur_node);
      __first = __mid;
    }
    uninitialized_copy(__first, __last, _M_finish._M_first);
  }
  __STL_UNWIND(destroy(_M_start, iterator(*__cur_node, __cur_node)));
}

#endif /* __STL_MEMBER_TEMPLATES */

// Called only if _M_finish._M_cur == _M_finish._M_last - 1.
template <class _Tp, class _Alloc, size_t __bufsize>
void
deque<_Tp,_Alloc,__bufsize>::_M_push_back_aux(const value_type& __t)
{
  value_type __t_copy = __t;
  _M_reserve_map_at_back();
  *(_M_finish._M_node + 1) = _M_allocate_node();
  __STL_TRY {
    construct(_M_finish._M_cur, __t_copy);
    _M_finish._M_set_node(_M_finish._M_node + 1);
    _M_finish._M_cur = _M_finish._M_first;
  }
  __STL_UNWIND(_M_deallocate_node(*(_M_finish._M_node + 1)));
}

// Called only if _M_finish._M_cur == _M_finish._M_last - 1.
template <class _Tp, class _Alloc, size_t __bufsize>
void
deque<_Tp,_Alloc,__bufsize>::_M_push_back_aux()
{
  _M_reserve_map_at_back();
  *(_M_finish._M_node + 1) = _M_allocate_node();
  __STL_TRY {
    construct(_M_finish._M_cur);
    _M_finish._M_set_node(_M_finish._M_node + 1);
    _M_finish._M_cur = _M_finish._M_first;
  }
  __STL_UNWIND(_M_deallocate_node(*(_M_finish._M_node + 1)));
}

// Called only if _M_start._M_cur == _M_start._M_first.
template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp,_Alloc,__bufsize>::_M_push_front_aux(const value_type& __t)
{
  value_type __t_copy = __t;
  _M_reserve_map_at_front();
  *(_M_start._M_node - 1) = _M_allocate_node();
  __STL_TRY {
    _M_start._M_set_node(_M_start._M_node - 1);
    _M_start._M_cur = _M_start._M_last - 1;
    construct(_M_start._M_cur, __t_copy);
  }
  __STL_UNWIND((++_M_start, _M_deallocate_node(*(_M_start._M_node - 1))));
} 

// Called only if _M_start._M_cur == _M_start._M_first.
template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp,_Alloc,__bufsize>::_M_push_front_aux()
{
  _M_reserve_map_at_front();
  *(_M_start._M_node - 1) = _M_allocate_node();
  __STL_TRY {
    _M_start._M_set_node(_M_start._M_node - 1);
    _M_start._M_cur = _M_start._M_last - 1;
    construct(_M_start._M_cur);
  }
  __STL_UNWIND((++_M_start, _M_deallocate_node(*(_M_start._M_node - 1))));
} 

// Called only if _M_finish._M_cur == _M_finish._M_first.
template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp,_Alloc,__bufsize>::_M_pop_back_aux()
{
  _M_deallocate_node(_M_finish._M_first);
  _M_finish._M_set_node(_M_finish._M_node - 1);
  _M_finish._M_cur = _M_finish._M_last - 1;
  destroy(_M_finish._M_cur);
}

// Called only if _M_start._M_cur == _M_start._M_last - 1.  Note that 
// if the deque has at least one element (a precondition for this member 
// function), and if _M_start._M_cur == _M_start._M_last, then the deque 
// must have at least two nodes.
template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp,_Alloc,__bufsize>::_M_pop_front_aux()
{
  destroy(_M_start._M_cur);
  _M_deallocate_node(_M_start._M_first);
  _M_start._M_set_node(_M_start._M_node + 1);
  _M_start._M_cur = _M_start._M_first;
}      

#ifdef __STL_MEMBER_TEMPLATES  

template <class _Tp, class _Alloc, size_t __bufsize>
template <class _InputIterator>
void 
deque<_Tp,_Alloc,__bufsize>::insert(iterator __pos,
                                    _InputIterator __first,
                                    _InputIterator __last,
                                    input_iterator_tag)
{
  copy(__first, __last, inserter(*this, __pos));
}

template <class _Tp, class _Alloc, size_t __bufsize>
template <class _ForwardIterator>
void 
deque<_Tp,_Alloc,__bufsize>::insert(iterator __pos,
                                    _ForwardIterator __first,
                                    _ForwardIterator __last,
                                    forward_iterator_tag) {
  size_type __n = 0;
  distance(__first, __last, __n);
  if (__pos._M_cur == _M_start._M_cur) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    __STL_TRY {
      uninitialized_copy(__first, __last, __new_start);
      _M_start = __new_start;
    }
    __STL_UNWIND(_M_destroy_nodes(__new_start._M_node, _M_start._M_node));
  }
  else if (__pos._M_cur == _M_finish._M_cur) {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    __STL_TRY {
      uninitialized_copy(__first, __last, _M_finish);
      _M_finish = __new_finish;
    }
    __STL_UNWIND(_M_destroy_nodes(_M_finish._M_node + 1, 
                                  __new_finish._M_node + 1));
  }
  else
    _M_insert_aux(__pos, __first, __last, __n);
}

#endif /* __STL_MEMBER_TEMPLATES */

template <class _Tp, class _Alloc, size_t __bufsize>
typename deque<_Tp, _Alloc, __bufsize>::iterator
deque<_Tp,_Alloc,__bufsize>::_M_insert_aux(iterator __pos,
                                           const value_type& __x)
{
  difference_type __index = __pos - _M_start;
  value_type __x_copy = __x;
  if (__index < size() / 2) {
    push_front(front());
    iterator __front1 = _M_start;
    ++__front1;
    iterator __front2 = __front1;
    ++__front2;
    __pos = _M_start + __index;
    iterator __pos1 = __pos;
    ++__pos1;
    copy(__front2, __pos1, __front1);
  }
  else {
    push_back(back());
    iterator __back1 = _M_finish;
    --__back1;
    iterator __back2 = __back1;
    --__back2;
    __pos = _M_start + __index;
    copy_backward(__pos, __back2, __back1);
  }
  *__pos = __x_copy;
  return __pos;
}

template <class _Tp, class _Alloc, size_t __bufsize>
typename deque<_Tp,_Alloc,__bufsize>::iterator
deque<_Tp,_Alloc,__bufsize>::_M_insert_aux(iterator __pos)
{
  difference_type __index = __pos - _M_start;
  if (__index < size() / 2) {
    push_front(front());
    iterator __front1 = _M_start;
    ++__front1;
    iterator __front2 = __front1;
    ++__front2;
    __pos = _M_start + __index;
    iterator __pos1 = __pos;
    ++__pos1;
    copy(__front2, __pos1, __front1);
  }
  else {
    push_back(back());
    iterator __back1 = _M_finish;
    --__back1;
    iterator __back2 = __back1;
    --__back2;
    __pos = _M_start + __index;
    copy_backward(__pos, __back2, __back1);
  }
  *__pos = value_type();
  return __pos;
}

template <class _Tp, class _Alloc, size_t __bufsize>
void
deque<_Tp,_Alloc,__bufsize>::_M_insert_aux(iterator __pos,
                                           size_type __n,
                                           const value_type& __x)
{
  const difference_type __elems_before = __pos - _M_start;
  size_type __length = size();
  value_type __x_copy = __x;
  if (__elems_before < __length / 2) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    iterator __old_start = _M_start;
    __pos = _M_start + __elems_before;
    __STL_TRY {
      if (__elems_before >= difference_type(__n)) {
        iterator __start_n = _M_start + difference_type(__n);
        uninitialized_copy(_M_start, __start_n, __new_start);
        _M_start = __new_start;
        copy(__start_n, __pos, __old_start);
        fill(__pos - difference_type(__n), __pos, __x_copy);
      }
      else {
        __uninitialized_copy_fill(_M_start, __pos, __new_start, 
	                          _M_start, __x_copy);
        _M_start = __new_start;
        fill(__old_start, __pos, __x_copy);
      }
    }
    __STL_UNWIND(_M_destroy_nodes(__new_start._M_node, _M_start._M_node));
  }
  else {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    iterator __old_finish = _M_finish;
    const difference_type __elems_after = 
      difference_type(__length) - __elems_before;
    __pos = _M_finish - __elems_after;
    __STL_TRY {
      if (__elems_after > difference_type(__n)) {
        iterator __finish_n = _M_finish - difference_type(__n);
        uninitialized_copy(__finish_n, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy_backward(__pos, __finish_n, __old_finish);
        fill(__pos, __pos + difference_type(__n), __x_copy);
      }
      else {
        __uninitialized_fill_copy(_M_finish, __pos + difference_type(__n),
                                  __x_copy, __pos, _M_finish);
        _M_finish = __new_finish;
        fill(__pos, __old_finish, __x_copy);
      }
    }
    __STL_UNWIND(_M_destroy_nodes(_M_finish._M_node + 1, 
                                  __new_finish._M_node + 1));
  }
}

#ifdef __STL_MEMBER_TEMPLATES  

template <class _Tp, class _Alloc, size_t __bufsize>
template <class _ForwardIterator>
void
deque<_Tp,_Alloc,__bufsize>::_M_insert_aux(iterator __pos,
                                           _ForwardIterator __first,
                                           _ForwardIterator __last,
                                           size_type __n)
{
  const difference_type __elemsbefore = __pos - _M_start;
  size_type __length = size();
  if (__elemsbefore < __length / 2) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    iterator __old_start = _M_start;
    __pos = _M_start + __elemsbefore;
    __STL_TRY {
      if (__elemsbefore >= difference_type(__n)) {
        iterator __start_n = _M_start + difference_type(__n); 
        uninitialized_copy(_M_start, __start_n, __new_start);
        _M_start = __new_start;
        copy(__start_n, __pos, __old_start);
        copy(__first, __last, __pos - difference_type(__n));
      }
      else {
        _ForwardIterator __mid = __first;
        advance(__mid, difference_type(__n) - __elemsbefore);
        __uninitialized_copy_copy(_M_start, __pos, __first, __mid,
                                  __new_start);
        _M_start = __new_start;
        copy(__mid, __last, __old_start);
      }
    }
    __STL_UNWIND(_M_destroy_nodes(__new_start._M_node, _M_start._M_node));
  }
  else {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    iterator __old_finish = _M_finish;
    const difference_type __elemsafter = 
      difference_type(__length) - __elemsbefore;
    __pos = _M_finish - __elemsafter;
    __STL_TRY {
      if (__elemsafter > difference_type(__n)) {
        iterator __finish_n = _M_finish - difference_type(__n);
        uninitialized_copy(__finish_n, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy_backward(__pos, __finish_n, __old_finish);
        copy(__first, __last, __pos);
      }
      else {
        _ForwardIterator __mid = __first;
        advance(__mid, __elemsafter);
        __uninitialized_copy_copy(__mid, __last, __pos, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy(__first, __mid, __pos);
      }
    }
    __STL_UNWIND(_M_destroy_nodes(_M_finish._M_node + 1, 
                                  __new_finish._M_node + 1));
  }
}

#else /* __STL_MEMBER_TEMPLATES */

template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp,_Alloc,__bufsize>::_M_insert_aux(iterator __pos,
                                           const value_type* __first,
                                           const value_type* __last,
                                           size_type __n)
{
  const difference_type __elemsbefore = __pos - _M_start;
  size_type __length = size();
  if (__elemsbefore < __length / 2) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    iterator __old_start = _M_start;
    __pos = _M_start + __elemsbefore;
    __STL_TRY {
      if (__elemsbefore >= difference_type(__n)) {
        iterator __start_n = _M_start + difference_type(__n);
        uninitialized_copy(_M_start, __start_n, __new_start);
        _M_start = __new_start;
        copy(__start_n, __pos, __old_start);
        copy(__first, __last, __pos - difference_type(__n));
      }
      else {
        const value_type* __mid = 
	  __first + (difference_type(__n) - __elemsbefore);
        __uninitialized_copy_copy(_M_start, __pos, __first, __mid,
                                  __new_start);
        _M_start = __new_start;
        copy(__mid, __last, __old_start);
      }
    }
    __STL_UNWIND(_M_destroy_nodes(__new_start._M_node, _M_start._M_node));
  }
  else {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    iterator __old_finish = _M_finish;
    const difference_type __elemsafter = 
      difference_type(__length) - __elemsbefore;
    __pos = _M_finish - __elemsafter;
    __STL_TRY {
      if (__elemsafter > difference_type(__n)) {
        iterator __finish_n = _M_finish - difference_type(__n);
        uninitialized_copy(__finish_n, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy_backward(__pos, __finish_n, __old_finish);
        copy(__first, __last, __pos);
      }
      else {
        const value_type* __mid = __first + __elemsafter;
        __uninitialized_copy_copy(__mid, __last, __pos, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy(__first, __mid, __pos);
      }
    }
    __STL_UNWIND(_M_destroy_nodes(_M_finish._M_node + 1, 
                                  __new_finish._M_node + 1));
  }
}

template <class _Tp, class _Alloc, size_t __bufsize>
void
deque<_Tp,_Alloc,__bufsize>::_M_insert_aux(iterator __pos,
                                           const_iterator __first,
                                           const_iterator __last,
                                           size_type __n)
{
  const difference_type __elemsbefore = __pos - _M_start;
  size_type __length = size();
  if (__elemsbefore < __length / 2) {
    iterator __new_start = _M_reserve_elements_at_front(__n);
    iterator __old_start = _M_start;
    __pos = _M_start + __elemsbefore;
    __STL_TRY {
      if (__elemsbefore >= __n) {
        iterator __start_n = _M_start + __n;
        uninitialized_copy(_M_start, __start_n, __new_start);
        _M_start = __new_start;
        copy(__start_n, __pos, __old_start);
        copy(__first, __last, __pos - difference_type(__n));
      }
      else {
        const_iterator __mid = __first + (__n - __elemsbefore);
        __uninitialized_copy_copy(_M_start, __pos, __first, __mid,
                                  __new_start);
        _M_start = __new_start;
        copy(__mid, __last, __old_start);
      }
    }
    __STL_UNWIND(_M_destroy_nodes(__new_start._M_node, _M_start._M_node));
  }
  else {
    iterator __new_finish = _M_reserve_elements_at_back(__n);
    iterator __old_finish = _M_finish;
    const difference_type __elemsafter = __length - __elemsbefore;
    __pos = _M_finish - __elemsafter;
    __STL_TRY {
      if (__elemsafter > __n) {
        iterator __finish_n = _M_finish - difference_type(__n);
        uninitialized_copy(__finish_n, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy_backward(__pos, __finish_n, __old_finish);
        copy(__first, __last, __pos);
      }
      else {
        const_iterator __mid = __first + __elemsafter;
        __uninitialized_copy_copy(__mid, __last, __pos, _M_finish, _M_finish);
        _M_finish = __new_finish;
        copy(__first, __mid, __pos);
      }
    }
    __STL_UNWIND(_M_destroy_nodes(_M_finish._M_node + 1, 
                 __new_finish._M_node + 1));
  }
}

#endif /* __STL_MEMBER_TEMPLATES */

template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp,_Alloc,__bufsize>::_M_new_elements_at_front(size_type __new_elems)
{
  size_type __new_nodes
      = (__new_elems + _S_buffer_size() - 1) / _S_buffer_size();
  _M_reserve_map_at_front(__new_nodes);
  size_type __i;
  __STL_TRY {
    for (__i = 1; __i <= __new_nodes; ++__i)
      *(_M_start._M_node - __i) = _M_allocate_node();
  }
#       ifdef __STL_USE_EXCEPTIONS
  catch(...) {
    for (size_type __j = 1; __j < __i; ++__j)
      _M_deallocate_node(*(_M_start._M_node - __j));      
    throw;
  }
#       endif /* __STL_USE_EXCEPTIONS */
}

template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp,_Alloc,__bufsize>::_M_new_elements_at_back(size_type __new_elems)
{
  size_type __new_nodes
      = (__new_elems + _S_buffer_size() - 1) / _S_buffer_size();
  _M_reserve_map_at_back(__new_nodes);
  size_type __i;
  __STL_TRY {
    for (__i = 1; __i <= __new_nodes; ++__i)
      *(_M_finish._M_node + __i) = _M_allocate_node();
  }
#       ifdef __STL_USE_EXCEPTIONS
  catch(...) {
    for (size_type __j = 1; __j < __i; ++__j)
      _M_deallocate_node(*(_M_finish._M_node + __j));      
    throw;
  }
#       endif /* __STL_USE_EXCEPTIONS */
}

template <class _Tp, class _Alloc, size_t __bufsize>
void 
deque<_Tp,_Alloc,__bufsize>::_M_reallocate_map(size_type __nodes_to_add,
                                              bool __add_at_front)
{
  size_type __old_num_nodes = _M_finish._M_node - _M_start._M_node + 1;
  size_type __new_num_nodes = __old_num_nodes + __nodes_to_add;

  _Map_pointer __new_nstart;
  if (_M_map_size > 2 * __new_num_nodes) {
    __new_nstart = _M_map + (_M_map_size - __new_num_nodes) / 2 
                     + (__add_at_front ? __nodes_to_add : 0);
    if (__new_nstart < _M_start._M_node)
      copy(_M_start._M_node, _M_finish._M_node + 1, __new_nstart);
    else
      copy_backward(_M_start._M_node, _M_finish._M_node + 1, 
                    __new_nstart + __old_num_nodes);
  }
  else {
    size_type __new_map_size = 
      _M_map_size + max(_M_map_size, __nodes_to_add) + 2;

    _Map_pointer __new_map = _M_allocate_map(__new_map_size);
    __new_nstart = __new_map + (__new_map_size - __new_num_nodes) / 2
                         + (__add_at_front ? __nodes_to_add : 0);
    copy(_M_start._M_node, _M_finish._M_node + 1, __new_nstart);
    _M_deallocate_map(_M_map, _M_map_size);

    _M_map = __new_map;
    _M_map_size = __new_map_size;
  }

  _M_start._M_set_node(__new_nstart);
  _M_finish._M_set_node(__new_nstart + __old_num_nodes - 1);
}


// Nonmember functions.

#ifndef __STL_NON_TYPE_TMPL_PARAM_BUG

template <class _Tp, class _Alloc, size_t __bufsiz>
bool operator==(const deque<_Tp, _Alloc, __bufsiz>& __x,
                const deque<_Tp, _Alloc, __bufsiz>& __y)
{
  return __x.size() == __y.size() &&
         equal(__x.begin(), __x.end(), __y.begin());
}

template <class _Tp, class _Alloc, size_t __bufsiz>
bool operator<(const deque<_Tp, _Alloc, __bufsiz>& __x,
               const deque<_Tp, _Alloc, __bufsiz>& __y)
{
  return lexicographical_compare(__x.begin(), __x.end(), 
                                 __y.begin(), __y.end());
}

#endif /* __STL_NON_TYPE_TMPL_PARAM_BUG */

#if defined(__STL_FUNCTION_TMPL_PARTIAL_ORDER) && \
    !defined(__STL_NON_TYPE_TMPL_PARAM_BUG)

template <class _Tp, class _Alloc, size_t __bufsiz>
inline void 
swap(deque<_Tp,_Alloc,__bufsiz>& __x, deque<_Tp,_Alloc,__bufsiz>& __y)
{
  __x.swap(__y);
}

#endif

#if defined(__sgi) && !defined(__GNUC__) && (_MIPS_SIM != _MIPS_SIM_ABI32)
#pragma reset woff 1174
#pragma reset woff 1375
#endif
          
__STL_END_NAMESPACE 
  
#endif /* __SGI_STL_INTERNAL_DEQUE_H */

// Local Variables:
// mode:C++
// End:
