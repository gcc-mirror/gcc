/*
 * Copyright (c) 1998
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

#ifndef __SGI_STL_EXCEPTION_H
#define __SGI_STL_EXCEPTION_H

// This header exists solely for portability.  Normally it just includes
// the header <exception>.

// The header <exception> contains low-level functions that interact
// with a compiler's exception-handling mechanism.  It is assumed to
// be supplied with the compiler, rather than with the library, because
// it is inherently tied very closely to the compiler itself.

// On platforms where <exception> does not exist, this header defines
// an exception base class.  This is *not* a substitute for everything
// in <exception>, but it suffices to support a bare minimum of STL
// functionality.

#include <stl_config.h>

#ifndef __STL_NO_EXCEPTION_HEADER

#include <exception>
#define __STL_EXCEPTION_BASE exception

#else /* __STL_NO_EXCEPTION_HEADER */

__STL_BEGIN_NAMESPACE

class _Exception {
public:
  virtual ~_Exception() __STL_NOTHROW {}
  virtual const char* what() const __STL_NOTHROW { return ""; }
};

#define __STL_EXCEPTION_BASE _Exception

__STL_END_NAMESPACE

#endif /* __STL_NO_EXCEPTION_HEADER */

#endif /* __SGI_STL_EXCEPTION_H */

// Local Variables:
// mode:C++
// End:
