/*
 * Copyright (c) 1996-1997
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

#ifndef _CPP_BACKWARD_ALLOC_H
#define _CPP_BACKWARD_ALLOC_H 1

#include "backward_warning.h"
#include <bits/c++config.h>
#include <bits/stl_alloc.h>

using std::__malloc_alloc_template; 
using std::malloc_alloc; 
using std::simple_alloc; 
using std::debug_alloc; 
#ifndef __USE_MALLOC
using std::__default_alloc_template; 
#endif
using std::alloc; 
using std::single_client_alloc; 
using std::allocator;

#endif /* _CPP_BACKWARD_ALLOC_H */

// Local Variables:
// mode:C++
// End:
