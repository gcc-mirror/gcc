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

#include "backward_warning.h"
#include "pair.h"
#include "iterator.h"
#include <bits/stl_algobase.h>
#include <bits/stl_uninitialized.h>

// Names from stl_algobase.h
using std::iter_swap; 
using std::swap; 
using std::min; 
using std::max; 
using std::copy; 
using std::copy_backward; 
using std::copy_n; 
using std::fill; 
using std::fill_n; 
using std::mismatch; 
using std::equal; 
using std::lexicographical_compare; 
using std::lexicographical_compare_3way; 

// Names from stl_uninitialized.h
using std::uninitialized_copy;
using std::uninitialized_copy_n;
using std::uninitialized_fill;
using std::uninitialized_fill_n;

#endif /* _CPP_BACKWARD_ALGOBASE_H */

// Local Variables:
// mode:C++
// End:
