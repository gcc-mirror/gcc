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
 * Copyright (c) 1997
 * Silicon Graphics
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */

#ifndef __STL_CONFIG_H
# define __STL_CONFIG_H

// What this file does.
//  (1) Defines bool, true, and false if the compiler doesn't do so already.
//  (2) Defines __STL_NO_DRAND48 if the compiler's standard library does
//      not support the drand48() function.
//  (3) Defines __STL_STATIC_TEMPLATE_MEMBER_BUG if the compiler can't 
//      handle static members of template classes.
//  (4) Defines 'typename' as a null macro if the compiler does not support
//      the typename keyword.
//  (5) Defines __STL_CLASS_PARTIAL_SPECIALIZATION if the compiler 
//      supports partial specialization of template classes.
//  (6) Defines __STL_MEMBER_TEMPLATES if the compiler supports
//      template members of classes.
//  (7) Defines 'explicit' as a null macro if the compiler does not support
//      the explicit keyword.    
//  (8) Defines __STL_LIMITED_DEFAULT_TEMPLATES if the compiler is
//      unable to handle default template parameters that depend on
//      previous template parameters.
//  (9) Defines __STL_NON_TYPE_TMPL_PARAM_BUG if the compiler has 
//      trouble performing function template argument deduction for
//      non-type template parameters.
//  (10) Defines __SGI_STL_NO_ARROW_OPERATOR if the compiler is unable
//       to support the -> operator for iterators.
//  (11) Defines __STL_USE_EXCEPTIONS if the compiler (in the current
//       compilation mode) supports exceptions.
//  (12) Defines __STL_SGI_THREADS if this is being compiled on an SGI
//       compiler, and if the user hasn't selected pthreads or no threads
//       instead.
//  (13) Defines __STL_WIN32THREADS if this is being compiled on a 
//       WIN32 compiler in multithreaded mode.
//  (14) Defines __stl_assert either as a test or as a null macro,
//       depending on whether or not __STL_ASSERTIONS is defined.

# if defined(__sgi) && !defined(__GNUC__)
#   if !defined(_BOOL)
#     define __STL_NEED_BOOL
#   endif
#   if !defined(_TYPENAME_IS_KEYWORD)
#     define __STL_NEED_TYPENAME
#   endif
#   ifdef _PARTIAL_SPECIALIZATION_OF_CLASS_TEMPLATES
#     define __STL_CLASS_PARTIAL_SPECIALIZATION
#   endif
#   ifdef _MEMBER_TEMPLATES
#     define __STL_MEMBER_TEMPLATES
#   endif
#   if !defined(_EXPLICIT_IS_KEYWORD)
#     define __STL_NEED_EXPLICIT
#   endif
#   ifdef __EXCEPTIONS
#     define __STL_USE_EXCEPTIONS
#   endif
#   if !defined(_NOTHREADS) && !defined(_PTHREADS)
#     define __STL_SGI_THREADS
#   endif
# endif

# ifdef __GNUC__
#   if 0 && (__GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 8))
#     define __STL_STATIC_TEMPLATE_MEMBER_BUG
#     define __STL_NEED_TYPENAME
#     define __STL_NEED_EXPLICIT
#   else
#     define __STL_CLASS_PARTIAL_SPECIALIZATION
#     define __STL_MEMBER_TEMPLATES
#   endif
#   ifdef __EXCEPTIONS
#     define __STL_USE_EXCEPTIONS
#   endif
# endif

# if defined(__SUNPRO_CC) 
#   define __STL_NEED_BOOL
#   define __STL_NEED_TYPENAME
#   define __STL_NEED_EXPLICIT
#   define __STL_USE_EXCEPTIONS
# endif

# if defined(__COMO__)
#   define __STL_MEMBER_TEMPLATES
#   define __STL_CLASS_PARTIAL_SPECIALIZATION
#   define __STL_USE_EXCEPTIONS
# endif

# if defined(_MSC_VER)
#   if _MSC_VER > 1000
#     include <yvals.h>
#   else
#     define __STL_NEED_BOOL
#   endif
#   define __STL_NO_DRAND48
#   define __STL_NEED_TYPENAME
#   if _MSC_VER < 1100
#     define __STL_NEED_EXPLICIT
#   endif
#   define __STL_NON_TYPE_TMPL_PARAM_BUG
#   define __SGI_STL_NO_ARROW_OPERATOR
#   ifdef _CPPUNWIND
#     define __STL_USE_EXCEPTIONS
#   endif
#   ifdef _MT
#     define __STL_WIN32THREADS
#   endif
# endif

# if defined(__BORLANDC__)
#   define __STL_NO_DRAND48
#   define __STL_NEED_TYPENAME
#   define __STL_LIMITED_DEFAULT_TEMPLATES
#   define __SGI_STL_NO_ARROW_OPERATOR
#   define __STL_NON_TYPE_TMPL_PARAM_BUG
#   ifdef _CPPUNWIND
#     define __STL_USE_EXCEPTIONS
#   endif
#   ifdef __MT__
#     define __STL_WIN32THREADS
#   endif
# endif


# if defined(__STL_NEED_BOOL)
    typedef int bool;
#   define true 1
#   define false 0
#   undef __STL_NEED_BOOL
# endif

# ifdef __STL_NEED_TYPENAME
#   define typename
#   undef __STL_NEED_TYPENAME
# endif

# ifdef __STL_NEED_EXPLICIT
#   define explicit
#   undef __STL_NEED_EXPLICIT
# endif

#ifdef __STL_ASSERTIONS
# include <stdio.h>
# define __stl_assert(expr) \
    if (!(expr)) { fprintf(stderr, "%s:%d STL assertion failure: %s\n", \
			  __FILE__, __LINE__, # expr); abort(); }
#else
# define __stl_assert(expr)
#endif

#endif /* __STL_CONFIG_H */
