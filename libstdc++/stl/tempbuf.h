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

#ifndef __SGI_STL_TEMPBUF_H
#define __SGI_STL_TEMPBUF_H

#ifndef __SGI_STL_PAIR_H
#include <pair.h>
#endif
#include <limits.h>      /* XXX should use <climits> */
#include <stddef.h>      /* XXX should use <cstddef> */
#include <stdlib.h>      /* XXX should use <cstdlib> */
#ifndef __TYPE_TRAITS_H
#include <type_traits.h>
#endif
#ifndef __SGI_STL_INTERNAL_CONSTRUCT_H
#include <stl_construct.h>
#endif
#ifndef __SGI_STL_INTERNAL_UNINITIALIZED_H
#include <stl_uninitialized.h>
#endif
#ifndef __SGI_STL_INTERNAL_TEMPBUF_H
#include <stl_tempbuf.h>
#endif

#ifdef __STL_USE_NAMESPACES

using __STD::get_temporary_buffer;
using __STD::return_temporary_buffer;
using __STD::temporary_buffer;

#endif /* __STL_USE_NAMESPACES */

#endif /* __SGI_STL_TEMPBUF_H */

// Local Variables:
// mode:C++
// End:
