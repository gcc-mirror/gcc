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

#ifndef __SGI_STL_ITERATOR_H
#define __SGI_STL_ITERATOR_H

#ifndef __SGI_STL_FUNCTION_H
#include <function.h>
#endif
#include <stddef.h>
#include <iostream.h>
#ifndef __SGI_STL_INTERNAL_ITERATOR_H
#include <stl_iterator.h>
#endif
#ifndef __TYPE_TRAITS_H
#include <type_traits.h>
#endif
#ifndef __SGI_STL_INTERNAL_CONSTRUCT_H
#include <stl_construct.h>
#endif
#ifndef __SGI_STL_INTERNAL_RAW_STORAGE_ITERATOR_H
#include <stl_raw_storage_iter.h>
#endif

#ifdef __STL_USE_NAMESPACES

// Names from stl_iterator.h

using __STD::input_iterator_tag;
using __STD::output_iterator_tag;
using __STD::forward_iterator_tag;
using __STD::bidirectional_iterator_tag;
using __STD::random_access_iterator_tag;

#if 0
using __STD::iterator;
#endif
using __STD::input_iterator;
using __STD::output_iterator;
using __STD::forward_iterator;
using __STD::bidirectional_iterator;
using __STD::random_access_iterator;

#ifdef __STL_CLASS_PARTIAL_SPECIALIZATION
using __STD::iterator_traits;
#endif

using __STD::iterator_category;
using __STD::distance_type;
using __STD::value_type;

using __STD::distance; 
using __STD::advance; 

using __STD::insert_iterator;
using __STD::front_insert_iterator;
using __STD::back_insert_iterator;
using __STD::inserter;
using __STD::front_inserter;
using __STD::back_inserter;

using __STD::reverse_iterator;
using __STD::reverse_bidirectional_iterator;

using __STD::istream_iterator;
using __STD::ostream_iterator;

// Names from stl_construct.h
using __STD::construct;
using __STD::destroy;

// Names from stl_raw_storage_iter.h
using __STD::raw_storage_iterator;

#endif /* __STL_USE_NAMESPACES */

#endif /* __SGI_STL_ITERATOR_H */

// Local Variables:
// mode:C++
// End:
