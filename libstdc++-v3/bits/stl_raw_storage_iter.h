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
 * Copyright (c) 1996
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
 * You should not attempt to use it directly.
 */

#ifndef _CPP_BITS_STL_RAW_STORAGE_ITERATOR_H
#define _CPP_BITS_STL_RAW_STORAGE_ITERATOR_H 1

__STL_BEGIN_NAMESPACE

template <class _ForwardIterator, class _Tp>
class raw_storage_iterator {
protected:
  _ForwardIterator _M_iter;
public:
  typedef output_iterator_tag iterator_category;
  typedef void                value_type;
  typedef void                difference_type;
  typedef void                pointer;
  typedef void                reference;

  explicit raw_storage_iterator(_ForwardIterator __x) : _M_iter(__x) {}
  raw_storage_iterator& operator*() { return *this; }
  raw_storage_iterator& operator=(const _Tp& __element) {
    construct(&*_M_iter, __element);
    return *this;
  }        
  raw_storage_iterator<_ForwardIterator, _Tp>& operator++() {
    ++_M_iter;
    return *this;
  }
  raw_storage_iterator<_ForwardIterator, _Tp> operator++(int) {
    raw_storage_iterator<_ForwardIterator, _Tp> __tmp = *this;
    ++_M_iter;
    return __tmp;
  }
};

#ifndef __STL_CLASS_PARTIAL_SPECIALIZATION

template <class _ForwardIterator, class _Tp>
inline output_iterator_tag
iterator_category(const raw_storage_iterator<_ForwardIterator, _Tp>&)
{
  return output_iterator_tag();
}

#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

__STL_END_NAMESPACE

#endif /* _CPP_BITS_STL_RAW_STORAGE_ITERATOR_H */

// Local Variables:
// mode:C++
// End:
