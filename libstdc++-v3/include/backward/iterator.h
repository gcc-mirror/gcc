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

#ifndef _CPP_BACKWARD_ITERATOR_H
#define _CPP_BACKWARD_ITERATOR_H 1

#include "backward_warning.h"
#include "function.h"
#include <stddef.h>
#include "iostream.h"
#include "iterator.h"

#include <bits/stl_construct.h>
#include <bits/stl_raw_storage_iter.h>

// Names from stl_iterator.h
using std::input_iterator_tag;
using std::output_iterator_tag;
using std::forward_iterator_tag;
using std::bidirectional_iterator_tag;
using std::random_access_iterator_tag;

#if 0
using std::iterator;
#endif
using std::input_iterator;
using std::output_iterator;
using std::forward_iterator;
using std::bidirectional_iterator;
using std::random_access_iterator;

using std::iterator_traits;

using std::iterator_category;
using std::distance_type;
using std::value_type;

using std::distance; 
using std::advance; 

using std::insert_iterator;
using std::front_insert_iterator;
using std::back_insert_iterator;
using std::inserter;
using std::front_inserter;
using std::back_inserter;

using std::reverse_iterator;
using std::reverse_bidirectional_iterator;

using std::istream_iterator;
using std::ostream_iterator;

// Names from stl_construct.h
template<class _T1, class _T2>
  inline void
  construct(_T1* __p, const _T2& __value)
  { std::_Construct(__p, __value); }

template<class _T1>
  inline void
  construct(_T1* __p)
  { std::_Construct(__p); }

template <class _Tp>
  inline void
  destroy(_Tp* __pointer)
  { std::_Destroy(__pointer); }
  
template <class _ForwardIterator>
  inline void
  destroy(_ForwardIterator __first, _ForwardIterator __last)
  { std::_Destroy(__first, __last); }
}

// Names from stl_raw_storage_iter.h
using std::raw_storage_iterator;

#endif /* _CPP_BACKWARD_ITERATOR_H */

// Local Variables:
// mode:C++
// End:
