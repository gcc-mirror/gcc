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
 * Copyright (c) 1996,1997
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

#ifndef _CPP_BACKWARD_ALGOBASE_H
#define _CPP_BACKWARD_ALGOBASE_H 1

#ifndef _CPP_BACKWARD_PAIR_H
#include "pair.h"
#endif
#ifndef _CPP_BACKWARD_ITERATOR_H
#include "iterator.h"
#endif
#ifndef _CPP_BITS_STL__ALGOBASE_H
#include <bits/stl_algobase.h>
#endif
#ifndef _CPP_BITS_STL_UNINITIALIZED_H
#include <bits/stl_uninitialized.h>
#endif

#ifdef __STL_USE_NAMESPACES

// Names from stl_algobase.h
using __STD::iter_swap; 
using __STD::swap; 
using __STD::min; 
using __STD::max; 
using __STD::copy; 
using __STD::copy_backward; 
using __STD::copy_n; 
using __STD::fill; 
using __STD::fill_n; 
using __STD::mismatch; 
using __STD::equal; 
using __STD::lexicographical_compare; 
using __STD::lexicographical_compare_3way; 

// Names from stl_uninitialized.h
using __STD::uninitialized_copy;
using __STD::uninitialized_copy_n;
using __STD::uninitialized_fill;
using __STD::uninitialized_fill_n;

#endif /* __STL_USE_NAMESPACES */

#endif /* _CPP_BACKWARD_ALGOBASE_H */

// Local Variables:
// mode:C++
// End:
