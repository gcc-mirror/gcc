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

#ifndef _CPP_BACKWARD_TEMPBUF_H
#define _CPP_BACKWARD_TEMPBUF_H 1

#ifndef _CPP_BACKWARD_PAIR_H
#include "pair.h"
#endif
#include <iterator.h>
#include <limits.h>
#include <stddef.h> 
#include <stdlib.h> 
#ifndef _CPP_BITS_TYPE_TRAITS_H
#include <bits/type_traits.h>  
#endif
#ifndef _CPP_BITS_STL_CONSTRUCT_H
#include <bits/stl_construct.h>
#endif
#ifndef _CPP_BITS_STL_UNINITIALIZED_H
#include <bits/stl_uninitialized.h>
#endif
#ifndef _CPP_BITS_STL_TEMPBUF_H
#include <bits/stl_tempbuf.h>
#endif

#ifdef __STL_USE_NAMESPACES

using __STD::get_temporary_buffer;
using __STD::return_temporary_buffer;
using __STD::_Temporary_buffer;

#endif /* __STL_USE_NAMESPACES */

#endif /* _CPP_BACKWARD_TEMPBUF_H */

// Local Variables:
// mode:C++
// End:
