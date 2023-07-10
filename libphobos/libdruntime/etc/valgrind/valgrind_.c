/**
 * This file contains wrapper functions for macro-defined valgrind routines.
 *
 * Copyright: Copyright: Copyright (C) D Language Foundation 2023
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Source:    $(DRUNTIMESRC etc/valgrind/valgrind.c)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
#include "config.h"

#ifdef ENABLE_VALGRIND_CHECKING

#ifdef HAVE_STDDEF_H
# include <stddef.h>
#endif
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif

#ifdef HAVE_VALGRIND_MEMCHECK_H
# include <valgrind/memcheck.h>
#elif defined HAVE_MEMCHECK_H
# include <memcheck.h>
#else
# include <valgrind.h>
#endif

#ifndef VALGRIND_MAKE_MEM_NOACCESS
# define VALGRIND_MAKE_MEM_NOACCESS VALGRIND_MAKE_NOACCESS
#endif
#ifndef VALGRIND_MAKE_MEM_UNDEFINED
# define VALGRIND_MAKE_MEM_UNDEFINED VALGRIND_MAKE_WRITABLE
#endif
#ifndef VALGRIND_MAKE_MEM_DEFINED
# define VALGRIND_MAKE_MEM_DEFINED VALGRIND_MAKE_READABLE
#endif

#ifndef VALGRIND_GET_VBITS
# define VALGRIND_GET_VBITS(a,b,l) 0
#endif
#ifndef VALGRIND_SET_VBITS
# define VALGRIND_SET_VBITS(a,b,l) 0
#endif
#ifndef VALGRIND_DISABLE_ADDR_ERROR_REPORTING_IN_RANGE
# define VALGRIND_DISABLE_ADDR_ERROR_REPORTING_IN_RANGE(a,l)
#endif
#ifndef VALGRIND_ENABLE_ADDR_ERROR_REPORTING_IN_RANGE
# define VALGRIND_ENABLE_ADDR_ERROR_REPORTING_IN_RANGE(a,l)
#endif

#define MAYBE_UNUSED __attribute__((unused))

void _d_valgrind_make_mem_noaccess(const void* addr, size_t len)
{
    VALGRIND_DISCARD(VALGRIND_MAKE_MEM_NOACCESS(addr, len));
}

void _d_valgrind_make_mem_undefined(const void* addr, size_t len)
{
    VALGRIND_DISCARD(VALGRIND_MAKE_MEM_UNDEFINED(addr, len));
}

void _d_valgrind_make_mem_defined(const void* addr, size_t len)
{
    VALGRIND_DISCARD(VALGRIND_MAKE_MEM_DEFINED(addr, len));
}

unsigned _d_valgrind_get_vbits(const void* addr MAYBE_UNUSED,
                               char* bits MAYBE_UNUSED,
                               size_t len MAYBE_UNUSED)
{
    return VALGRIND_GET_VBITS(addr, bits, len);
}

unsigned _d_valgrind_set_vbits(const void* addr MAYBE_UNUSED,
                               char* bits MAYBE_UNUSED,
                               size_t len MAYBE_UNUSED)
{
    return VALGRIND_SET_VBITS(addr, bits, len);
}

void _d_valgrind_disable_addr_reporting_in_range(const void* addr MAYBE_UNUSED,
                                                 size_t len MAYBE_UNUSED)
{
    VALGRIND_DISCARD(VALGRIND_DISABLE_ADDR_ERROR_REPORTING_IN_RANGE(addr, len));
}

void _d_valgrind_enable_addr_reporting_in_range(const void* addr MAYBE_UNUSED,
                                                size_t len MAYBE_UNUSED)
{
    VALGRIND_DISCARD(VALGRIND_ENABLE_ADDR_ERROR_REPORTING_IN_RANGE(addr, len));
}

#endif  /* ENABLE_VALGRIND_CHECKING  */
