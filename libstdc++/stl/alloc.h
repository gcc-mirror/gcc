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

#ifndef __SGI_STL_ALLOC_H
#define __SGI_STL_ALLOC_H

#ifndef __STL_CONFIG_H
#include <stl_config.h>
#endif
#ifndef __SGI_STL_INTERNAL_ALLOC_H
#include <stl_alloc.h>
#endif

#ifdef __STL_USE_NAMESPACES

using __STD::__malloc_alloc_template; 
using __STD::malloc_alloc; 
using __STD::simple_alloc; 
using __STD::debug_alloc; 
using __STD::__default_alloc_template; 
using __STD::alloc; 
using __STD::single_client_alloc; 
#ifdef __STL_STATIC_TEMPLATE_MEMBER_BUG
using __STD::__malloc_alloc_oom_handler; 
#endif /* __STL_STATIC_TEMPLATE_MEMBER_BUG */


#endif /* __STL_USE_NAMESPACES */

#endif /* __SGI_STL_ALLOC_H */

// Local Variables:
// mode:C++
// End:
