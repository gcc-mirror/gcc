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
 *   You should not attempt to use it directly.
 */

#ifndef __SGI_STL_INTERNAL_RAW_STORAGE_ITERATOR_H
#define __SGI_STL_INTERNAL_RAW_STORAGE_ITERATOR_H

__STL_BEGIN_NAMESPACE

template <class ForwardIterator, class T>
class raw_storage_iterator {
protected:
  ForwardIterator iter;
public:
  typedef output_iterator_tag iterator_category;
  typedef void                value_type;
  typedef void                difference_type;
  typedef void                pointer;
  typedef void                reference;

  explicit raw_storage_iterator(ForwardIterator x) : iter(x) {}
  raw_storage_iterator<ForwardIterator, T>& operator*() { return *this; }
  raw_storage_iterator<ForwardIterator, T>& operator=(const T& element) {
    construct(&*iter, element);
    return *this;
  }        
  raw_storage_iterator<ForwardIterator, T>& operator++() {
    ++iter;
    return *this;
  }
  raw_storage_iterator<ForwardIterator, T> operator++(int) {
    raw_storage_iterator<ForwardIterator, T> tmp = *this;
    ++iter;
    return tmp;
  }
};

#ifndef __STL_CLASS_PARTIAL_SPECIALIZATION

template <class ForwardIterator, class T>
inline output_iterator_tag
iterator_category(const raw_storage_iterator<ForwardIterator, T>&)
{
  return output_iterator_tag();
}

#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

#endif /* __SGI_STL_INTERNAL_RAW_STORAGE_ITERATOR_H */

__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:
