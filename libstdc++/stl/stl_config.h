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

// Flags:
// * __STL_NO_BOOL: defined if the compiler doesn't have bool as a builtin
//   type.
// * __STL_HAS_WCHAR_T: defined if the compier has wchar_t as a builtin type.
// * __STL_NO_DRAND48: defined if the compiler doesn't have the drand48
//   function.
// * __STL_STATIC_TEMPLATE_MEMBER_BUG: defined if the compiler can't handle
//   static members of template classes.
// * __STL_CLASS_PARTIAL_SPECIALIZATION: defined if the compiler supports
//   partial specialization of template classes.
// * __STL_PARTIAL_SPECIALIZATION_SYNTAX: defined if the compiler
//   supports partial specialization syntax for full specialization of
//   class templates.  (Even if it doesn't actually support partial
//   specialization itself.)
// * __STL_FUNCTION_TMPL_PARTIAL_ORDER: defined if the compiler supports
//   partial ordering of function templates.  (a.k.a partial specialization
//   of function templates.)
// * __STL_MEMBER_TEMPLATES: defined if the compiler supports template
//   member functions of classes.
// * __STL_MEMBER_TEMPLATE_CLASSES: defined if the compiler supports
//   nested classes that are member templates of other classes.
// * __STL_EXPLICIT_FUNCTION_TMPL_ARGS: defined if the compiler
//   supports calling a function template by providing its template
//   arguments explicitly.
// * __STL_LIMITED_DEFAULT_TEMPLATES: defined if the compiler is unable
//   to handle default template parameters that depend on previous template
//   parameters.
// * __STL_NON_TYPE_TMPL_PARAM_BUG: defined if the compiler has trouble with
//   function template argument deduction for non-type template parameters.
// * __SGI_STL_NO_ARROW_OPERATOR: defined if the compiler is unable
//   to support the -> operator for iterators.
// * __STL_USE_EXCEPTIONS: defined if the compiler (in the current compilation
//   mode) supports exceptions.
// * __STL_USE_NAMESPACES: defined if the compiler has the necessary
//   support for namespaces.
// * __STL_NO_EXCEPTION_HEADER: defined if the compiler does not have a
//   standard-conforming header <exception>.
// * __STL_SGI_THREADS: defined if this is being compiled for an SGI IRIX
//   system in multithreaded mode, using native SGI threads instead of
//   pthreads.
// * __STL_WIN32THREADS: defined if this is being compiled on a WIN32
//   compiler in multithreaded mode.
// * __STL_LONG_LONG if the compiler has long long and unsigned long long
//   types.  (They're not in the C++ standard, but they are expected to be
//   included in the forthcoming C9X standard.)


// User-settable macros that control compilation:
// * __STL_USE_SGI_ALLOCATORS: if defined, then the STL will use older
//   SGI-style allocators, instead of standard-conforming allocators,
//   even if the compiler supports all of the language features needed
//   for standard-conforming allocators.
// * __STL_NO_NAMESPACES: if defined, don't put the library in namespace
//   std, even if the compiler supports namespaces.
// * __STL_ASSERTIONS: if defined, then enable runtime checking through the
//   __stl_assert macro.
// * _PTHREADS: if defined, use Posix threads for multithreading support.
// * _NOTHREADS: if defined, don't use any multithreading support.


// Other macros defined by this file:

// * bool, true, and false, if __STL_NO_BOOL is defined.
// * typename, as a null macro if it's not already a keyword.
// * explicit, as a null macro if it's not already a keyword.
// * namespace-related macros (__STD, __STL_BEGIN_NAMESPACE, etc.)
// * exception-related macros (__STL_TRY, __STL_UNWIND, etc.)
// * __stl_assert, either as a test or as a null macro, depending on
//   whether or not __STL_ASSERTIONS is defined.

#ifdef _PTHREADS
#   define __STL_PTHREADS
#endif
#ifdef _SOLTHREADS
#   define __STL_SOLTHREADS
#endif

# if defined(__sgi) && !defined(__GNUC__)
#   if !defined(_BOOL)
#     define __STL_NO_BOOL
#   endif
#   if defined(_WCHAR_T_IS_KEYWORD)
#     define __STL_HAS_WCHAR_T
#   endif
#   if !defined(_TYPENAME_IS_KEYWORD)
#     define __STL_NEED_TYPENAME
#   endif
#   ifdef _PARTIAL_SPECIALIZATION_OF_CLASS_TEMPLATES
#     define __STL_CLASS_PARTIAL_SPECIALIZATION
#   endif
#   ifdef _MEMBER_TEMPLATES
#     define __STL_MEMBER_TEMPLATES
#     define __STL_MEMBER_TEMPLATE_CLASSES
#   endif
#   if defined(_MEMBER_TEMPLATE_KEYWORD)
#     define __STL_MEMBER_TEMPLATE_KEYWORD
#   endif
#   if (_COMPILER_VERSION >= 730) && defined(_MIPS_SIM) && _MIPS_SIM != _ABIO32
#     define __STL_MEMBER_TEMPLATE_KEYWORD
#   endif
#   if !defined(_EXPLICIT_IS_KEYWORD)
#     define __STL_NEED_EXPLICIT
#   endif
#   ifdef __EXCEPTIONS
#     define __STL_USE_EXCEPTIONS
#   endif
#   if (_COMPILER_VERSION >= 721) && defined(_NAMESPACES)
#     define __STL_HAS_NAMESPACES
#   endif
#   if (_COMPILER_VERSION < 721)
#     define __STL_NO_EXCEPTION_HEADER
#   endif
#   if !defined(_NOTHREADS) && !defined(__STL_PTHREADS)
#     define __STL_SGI_THREADS
#   endif
#   if defined(_LONGLONG) && defined(_SGIAPI) && _SGIAPI
#     define __STL_LONG_LONG
#   endif
# endif

# ifdef __GNUC__
#   include <_G_config.h>
#   define __STL_HAS_WCHAR_T
#   if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 8)
#     define __STL_STATIC_TEMPLATE_MEMBER_BUG
#     define __STL_NEED_TYPENAME
#     define __STL_NEED_EXPLICIT
#   else
#     define __STL_CLASS_PARTIAL_SPECIALIZATION
#     define __STL_FUNCTION_TMPL_PARTIAL_ORDER
#     define __STL_MEMBER_TEMPLATES
#     define __STL_MEMBER_TEMPLATE_CLASSES
#     define __STL_EXPLICIT_FUNCTION_TMPL_ARGS
#     define __STL_HAS_NAMESPACES
#     define __STL_NO_NAMESPACES
#     define __SGI_STL_USE_AUTO_PTR_CONVERSIONS
#     define __STL_USE_NAMESPACES
#   endif
#   if defined(__linux__)
     /* glibc pre 2.0 is very buggy. We have to disable thread for it.
        It should be upgraded to glibc 2.0 or later. */
#    if !defined(_NOTHREADS) && __GLIBC__ >= 2 && defined(_G_USING_THUNKS)
#      define __STL_PTHREADS
#      ifdef __STRICT_ANSI__
         /* Work around a bug in the glibc 2.0.x pthread.h.  */
#        define sigset_t __sigset_t
#      endif
#    endif
#   endif
#   ifdef __EXCEPTIONS
#     define __STL_USE_EXCEPTIONS
#   endif
#   ifndef __STRICT_ANSI__
#     define __STL_LONG_LONG
#   endif
# endif

# if defined(__SUNPRO_CC)
#   define __STL_NO_BOOL
#   define __STL_NEED_TYPENAME
#   define __STL_NEED_EXPLICIT
#   define __STL_USE_EXCEPTIONS
# endif

# if defined(__COMO__)
#   define __STL_MEMBER_TEMPLATES
#   define __STL_MEMBER_TEMPLATE_CLASSES
#   define __STL_CLASS_PARTIAL_SPECIALIZATION
#   define __STL_USE_EXCEPTIONS
#   define __STL_HAS_NAMESPACES
# endif

# if defined(__MINGW32__)
#   define __STL_NO_DRAND48
# endif

# if defined(__CYGWIN__)
#   define __STL_NO_DRAND48
# endif

# if defined(_MSC_VER)
#   define __STL_NO_DRAND48
#   define __STL_NEED_TYPENAME
#   if _MSC_VER < 1100  /* 1000 is version 4.0, 1100 is 5.0, 1200 is 6.0. */
#     define __STL_NEED_EXPLICIT
#     define __STL_NO_BOOL
#     if  _MSC_VER > 1000
#       include <yvals.h>
#       define __STL_DONT_USE_BOOL_TYPEDEF
#     endif
#   endif
#   define __STL_NON_TYPE_TMPL_PARAM_BUG
#   define __SGI_STL_NO_ARROW_OPERATOR
#   ifdef _CPPUNWIND
#     define __STL_USE_EXCEPTIONS
#   endif
#   ifdef _MT
#     define __STL_WIN32THREADS
#   endif
#   if _MSC_VER >= 1200
#     define __STL_PARTIAL_SPECIALIZATION_SYNTAX
#     define __STL_HAS_NAMESPACES
#     define __STL_NO_NAMESPACES
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

# if defined(__STL_NO_BOOL) && !defined(__STL_DONT_USE_BOOL_TYPEDEF)
    typedef int bool;
#   define true 1
#   define false 0
# endif

# ifdef __STL_NEED_TYPENAME
#   define typename
# endif

# ifdef __STL_MEMBER_TEMPLATE_KEYWORD
#   define __STL_TEMPLATE template
# else
#   define __STL_TEMPLATE
# endif

# ifdef __STL_NEED_EXPLICIT
#   define explicit
# endif

# ifdef __STL_EXPLICIT_FUNCTION_TMPL_ARGS
#   define __STL_NULL_TMPL_ARGS <>
# else
#   define __STL_NULL_TMPL_ARGS
# endif

# if defined(__STL_CLASS_PARTIAL_SPECIALIZATION) \
     || defined (__STL_PARTIAL_SPECIALIZATION_SYNTAX)
#   define __STL_TEMPLATE_NULL template<>
# else
#   define __STL_TEMPLATE_NULL
# endif

// Use standard-conforming allocators if we have the necessary language
// features.  __STL_USE_SGI_ALLOCATORS is a hook so that users can
// disable new-style allocators, and continue to use the same kind of
// allocators as before, without having to edit library headers.
# if defined(__STL_CLASS_PARTIAL_SPECIALIZATION) && \
     defined(__STL_MEMBER_TEMPLATES) && \
     defined(__STL_MEMBER_TEMPLATE_CLASSES) && \
    !defined(__STL_NO_BOOL) && \
    !defined(__STL_NON_TYPE_TMPL_PARAM_BUG) && \
    !defined(__STL_LIMITED_DEFAULT_TEMPLATES) && \
    !defined(__STL_USE_SGI_ALLOCATORS)
#   define __STL_USE_STD_ALLOCATORS
# endif

# ifndef __STL_DEFAULT_ALLOCATOR
#   ifdef __STL_USE_STD_ALLOCATORS
#     define __STL_DEFAULT_ALLOCATOR(T) allocator<T>
#   else
#     define __STL_DEFAULT_ALLOCATOR(T) alloc
#   endif
# endif

// __STL_NO_NAMESPACES is a hook so that users can disable namespaces
// without having to edit library headers.
# if defined(__STL_HAS_NAMESPACES) && !defined(__STL_NO_NAMESPACES)
#   define __STD std
#   define __STL_BEGIN_NAMESPACE namespace std {
#   define __STL_END_NAMESPACE }
#   define __STL_USE_NAMESPACE_FOR_RELOPS
#   define __STL_BEGIN_RELOPS_NAMESPACE namespace std {
#   define __STL_END_RELOPS_NAMESPACE }
#   define __STD_RELOPS std
#   define __STL_USE_NAMESPACES
# else
#   define __STD
#   define __STL_BEGIN_NAMESPACE
#   define __STL_END_NAMESPACE
#   undef  __STL_USE_NAMESPACE_FOR_RELOPS
#   define __STL_BEGIN_RELOPS_NAMESPACE
#   define __STL_END_RELOPS_NAMESPACE
#   define __STD_RELOPS
#   undef  __STL_USE_NAMESPACES
# endif

# ifdef __STL_USE_EXCEPTIONS
#   define __STL_TRY try
#   define __STL_CATCH_ALL catch(...)
#   define __STL_THROW(x) throw x
#   define __STL_RETHROW throw
#   define __STL_NOTHROW throw()
#   define __STL_UNWIND(action) catch(...) { action; throw; }
# else
#   define __STL_TRY
#   define __STL_CATCH_ALL if (false)
#   define __STL_THROW(x)
#   define __STL_RETHROW
#   define __STL_NOTHROW
#   define __STL_UNWIND(action)
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

// Local Variables:
// mode:C++
// End:
