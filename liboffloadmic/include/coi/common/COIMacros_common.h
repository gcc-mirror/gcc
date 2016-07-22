/*
 * Copyright 2010-2016 Intel Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, version 2.1.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * Disclaimer: The codes contained in these modules may be specific
 * to the Intel Software Development Platform codenamed Knights Ferry,
 * and the Intel product codenamed Knights Corner, and are not backward
 * compatible with other Intel products. Additionally, Intel will NOT
 * support the codes or instruction set in future products.
 *
 * Intel offers no warranty of any kind regarding the code. This code is
 * licensed on an "AS IS" basis and Intel is not obligated to provide
 * any support, assistance, installation, training, or other services
 * of any kind. Intel is also not obligated to provide any updates,
 * enhancements or extensions. Intel specifically disclaims any warranty
 * of merchantability, non-infringement, fitness for any particular
 * purpose, and any other warranty.
 *
 * Further, Intel disclaims all liability of any kind, including but
 * not limited to liability for infringement of any proprietary rights,
 * relating to the use of the code, even if Intel is notified of the
 * possibility of such liability. Except as expressly stated in an Intel
 * license agreement provided with this code and agreed upon with Intel,
 * no license, express or implied, by estoppel or otherwise, to any
 * intellectual property rights is granted herein.
 */

#ifndef _COIMACROS_COMMON_H
#define _COIMACROS_COMMON_H

#include <string.h>
#include "../source/COIPipeline_source.h"
#include "../common/COITypes_common.h"

/// @file common/COIMacros_common.h
/// Commonly used macros

// Note that UNUSUED_ATTR means that it is "possibly" unused, not "definitely".
// This should compile out in release mode if indeed it is unused.
    #define UNUSED_ATTR __attribute__((unused))
    #include <sched.h>
#ifndef UNREFERENCED_CONST_PARAM
#define UNREFERENCED_CONST_PARAM(P)     { void* x UNUSED_ATTR = \
            (void*)(uint64_t)P; \
}
#endif

// This seems to work on everything.
#ifndef UNREFERENCED_PARAM
    #define UNREFERENCED_PARAM(P)          (P = P)
#endif

#ifndef SYMBOL_VERSION

        /* Linux support: */

        #define SYMBOL_VERSION( SYMBOL , VERSION ) SYMBOL ## VERSION

#endif

/* The following are static inline definitions of functions used for manipulating
   COI_CPU_MASK info (The COI_CPU_MASK type is declared as an array of 16 uint64_t's
   in COITypes_common.h "typedef uint64_t COI_CPU_MASK[16]").

   These static inlined functions are intended on being roughly the same as the Linux
   CPU_* macros defined in sched.h - with the important difference being a different
   fundamental type difference: cpu_set_t versus COI_CPU_MASK.

   The motivation for writing this code was to ease portability on the host side of COI
   applications to both Windows and Linux.
*/

/* Roughly equivalent to CPU_ISSET(). */
static inline uint64_t COI_CPU_MASK_ISSET(int bitNumber, const COI_CPU_MASK cpu_mask)
{
    if ((size_t)bitNumber < sizeof(COI_CPU_MASK) * 8)
        return ((cpu_mask)[bitNumber / 64] & (((uint64_t)1) << (bitNumber % 64)));
    return 0;
}

/* Roughly equivalent to CPU_SET(). */
static inline void COI_CPU_MASK_SET(int bitNumber, COI_CPU_MASK cpu_mask)
{
    if ((size_t)bitNumber < sizeof(COI_CPU_MASK) * 8)
        ((cpu_mask)[bitNumber / 64] |= (((uint64_t)1) << (bitNumber % 64)));
}

/* Roughly equivalent to CPU_ZERO(). */
static inline void COI_CPU_MASK_ZERO(COI_CPU_MASK cpu_mask)
{
    memset(cpu_mask, 0, sizeof(COI_CPU_MASK));
}

/* Roughly equivalent to CPU_AND(). */
static inline void COI_CPU_MASK_AND(COI_CPU_MASK dst, const COI_CPU_MASK src1, const COI_CPU_MASK src2)
{
    const unsigned int loopIterations = sizeof(COI_CPU_MASK) / sizeof(dst[0]);
    unsigned int i = 0;

    for (; i < loopIterations; ++i)
        dst[i] = src1[i] & src2[i];
}

/* Roughly equivalent to CPU_XOR(). */
static inline void COI_CPU_MASK_XOR(COI_CPU_MASK dst, const COI_CPU_MASK src1, const COI_CPU_MASK src2)
{
    const unsigned int loopIterations = sizeof(COI_CPU_MASK) / sizeof(dst[0]);
    unsigned int i = 0;

    for (; i < loopIterations; ++i)
        dst[i] = src1[i] ^ src2[i];
}

/* Roughly equivalent to CPU_OR(). */
static inline void COI_CPU_MASK_OR(COI_CPU_MASK dst, const COI_CPU_MASK src1, const COI_CPU_MASK src2)
{
    const unsigned int loopIterations = sizeof(COI_CPU_MASK) / sizeof(dst[0]);
    unsigned int i = 0;

    for (; i < loopIterations; ++i)
        dst[i] = src1[i] | src2[i];
}

/* Utility function for COI_CPU_MASK_COUNT() below. */
static inline int __COI_CountBits(uint64_t n)
{
    int cnt = 0;

    for (; n; cnt++)
        n &= (n - 1);
    return cnt;
}

/* Roughly equivalent to CPU_COUNT(). */
static inline int COI_CPU_MASK_COUNT(const COI_CPU_MASK cpu_mask)
{
    int cnt = 0;
    const unsigned int loopIterations = sizeof(COI_CPU_MASK) / sizeof(cpu_mask[0]);
    unsigned int i = 0;

    for (; i < loopIterations; ++i)
    {
        cnt += __COI_CountBits(cpu_mask[i]);
    }
    return cnt;
}

/* Roughly equivalent to CPU_EQUAL(). */
static inline int COI_CPU_MASK_EQUAL(const COI_CPU_MASK cpu_mask1, const COI_CPU_MASK cpu_mask2)
{
    const unsigned int loopIterations = sizeof(COI_CPU_MASK) / sizeof(cpu_mask1[0]);
    unsigned int i = 0;

    for (; i < loopIterations; ++i)
    {
        if (cpu_mask1[i] != cpu_mask2[i])
            return 0;
    }
    return 1;
}


/* Utility function to translate from cpu_set * to COI_CPU_MASK. */
static inline void COI_CPU_MASK_XLATE(COI_CPU_MASK dest, const cpu_set_t *src)
{
    unsigned int i;
    unsigned int j;
    COI_CPU_MASK_ZERO(dest);
    #if 0
    /* Slightly slower version than the following #else/#endif block. Left here only to
         document the intent of the code. */
    for (i = 0; i < sizeof(cpu_set_t) * 8; ++i)
        if (CPU_ISSET(i, src))
            COI_CPU_MASK_SET(i, dest);
    #else
    for (i = 0; i < sizeof(COI_CPU_MASK) / sizeof(dest[0]); ++i)
    {
        for (j = 0; j < 64; ++j)
        {
            if (CPU_ISSET(i * 64 + j, src))
                dest[i] |= ((uint64_t)1) << j;
        }
    }
    #endif
}

/* Utility function to translate from COI_CPU_MASK to cpu_set *. */
static inline void COI_CPU_MASK_XLATE_EX(cpu_set_t *dest, const COI_CPU_MASK src)
{
    unsigned int i;
    unsigned int j;
    CPU_ZERO(dest);
    #if 0
    /* Slightly slower version than the following #else/#endif block. Left here only to
         document the intent of the code. */
    for (i = 0; i < sizeof(COI_CPU_MASK) * 8; ++i)
        if (COI_CPU_MASK_ISSET(i, src))
            CPU_SET(i, dest);
    #else
    for (i = 0; i < sizeof(COI_CPU_MASK) / sizeof(src[0]); ++i)
    {
        const uint64_t cpu_mask = src[i];

        for (j = 0; j < 64; ++j)
        {
            const uint64_t bit = ((uint64_t)1) << j;

            if (bit & cpu_mask)
                CPU_SET(i * 64 + j, dest);
        }
    }
    #endif
}


#endif /* _COIMACROS_COMMON_H */
