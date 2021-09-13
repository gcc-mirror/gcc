/* FreeBSD specific atomic operations for ARM EABI.
   Copyright (C) 2015-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include <sys/types.h>

#define HIDDEN __attribute__ ((visibility ("hidden")))

#define ARM_VECTORS_HIGH 0xffff0000U
#define ARM_TP_ADDRESS   (ARM_VECTORS_HIGH + 0x1000)
#define ARM_RAS_START    (ARM_TP_ADDRESS + 4)

void HIDDEN
__sync_synchronize (void)
{
#if defined (__ARM_ARCH_6__) || defined (__ARM_ARCH_6J__)       \
    || defined (__ARM_ARCH_6K__) || defined (__ARM_ARCH_6T2__)  \
    || defined (__ARM_ARCH_6Z__) || defined (__ARM_ARCH_6ZK__)  \
    || defined (__ARM_ARCH_7__) || defined (__ARM_ARCH_7A__)
#if defined (__ARM_ARCH_7__) || defined (__ARM_ARCH_7A__)
    __asm __volatile ("dmb" : : : "memory");
#else
    __asm __volatile ("mcr p15, 0, r0, c7, c10, 5" : : : "memory");
#endif
#else
    __asm __volatile ("nop" : : : "memory");
#endif
}

#if defined (__ARM_ARCH_6__) || defined (__ARM_ARCH_6J__)        \
    || defined (__ARM_ARCH_6K__) || defined (__ARM_ARCH_6T2__)   \
    || defined (__ARM_ARCH_6Z__) || defined (__ARM_ARCH_6ZK__)   \
    || defined (__ARM_ARCH_7__) || defined (__ARM_ARCH_7A__)

/* These systems should be supported by the compiler.  */

#else /* __ARM_ARCH_5__  */

#define	SYNC_LOCK_TEST_AND_SET_N(N, TYPE, LDR, STR)			\
TYPE HIDDEN    								\
__sync_lock_test_and_set_##N (TYPE *mem, TYPE val)			\
{									\
        unsigned int old, temp, ras_start;                              \
                                                                        \
        ras_start = ARM_RAS_START;					\
        __asm volatile (						\
                /* Set up Restartable Atomic Sequence.  */		\
                "1:"							\
                "\tadr   %2, 1b\n"					\
                "\tstr   %2, [%5]\n"					\
                "\tadr   %2, 2f\n"					\
                "\tstr   %2, [%5, #4]\n"				\
                                                                        \
                "\t"LDR" %0, %4\n"	/* Load old value.  */		\
                "\t"STR" %3, %1\n"	/* Store new value.  */		\
                                                                        \
                /* Tear down Restartable Atomic Sequence.  */		\
                "2:"							\
                "\tmov   %2, #0x00000000\n"				\
                "\tstr   %2, [%5]\n"					\
                "\tmov   %2, #0xffffffff\n"				\
                "\tstr   %2, [%5, #4]\n"				\
                : "=&r" (old), "=m" (*mem), "=&r" (temp)		\
                : "r" (val), "m" (*mem), "r" (ras_start));		\
        return (old);							\
}

#define SYNC_LOCK_RELEASE_N(N, TYPE)					\
void HIDDEN								\
__sync_lock_release_##N (TYPE *ptr)     				\
{					       				\
    /* All writes before this point must be seen before we release	\
       the lock itself.  */						\
    __sync_synchronize ();     						\
    *ptr = 0;								\
}

#define	SYNC_VAL_CAS_N(N, TYPE, LDR, STREQ)             		\
TYPE HIDDEN    								\
__sync_val_compare_and_swap_##N (TYPE *mem, TYPE expected,		\
    TYPE desired)							\
{									\
        unsigned int old, temp, ras_start;                              \
                                                                        \
        ras_start = ARM_RAS_START;					\
        __asm volatile (						\
                /* Set up Restartable Atomic Sequence.  */		\
                "1:"							\
                "\tadr   %2, 1b\n"					\
                "\tstr   %2, [%6]\n"					\
                "\tadr   %2, 2f\n"					\
                "\tstr   %2, [%6, #4]\n"				\
                                                                        \
                "\t"LDR" %0, %5\n"     /* Load old value.  */		\
                "\tcmp   %0, %3\n"     /* Compare to expected value.  */\
                "\t"STREQ" %4, %1\n"   /* Store new value.  */		\
                                                                        \
                /* Tear down Restartable Atomic Sequence.  */		\
                "2:"							\
                "\tmov   %2, #0x00000000\n"				\
                "\tstr   %2, [%6]\n"					\
                "\tmov   %2, #0xffffffff\n"				\
                "\tstr   %2, [%6, #4]\n"				\
                : "=&r" (old), "=m" (*mem), "=&r" (temp)		\
                : "r" (expected), "r" (desired), "m" (*mem),		\
                  "r" (ras_start));					\
        return (old);							\
}

typedef unsigned char bool;

#define SYNC_BOOL_CAS_N(N, TYPE)                                        \
bool HIDDEN								\
__sync_bool_compare_and_swap_##N (TYPE *ptr, TYPE oldval,		\
                                  TYPE newval)                          \
{									\
    TYPE actual_oldval							\
      = __sync_val_compare_and_swap_##N (ptr, oldval, newval);          \
    return (oldval == actual_oldval);					\
}

#define	SYNC_FETCH_AND_OP_N(N, TYPE, LDR, STR, NAME, OP)		\
TYPE HIDDEN								\
__sync_fetch_and_##NAME##_##N (TYPE *mem, TYPE val)	       		\
{									\
        unsigned int old, temp, ras_start;                              \
                                                                        \
        ras_start = ARM_RAS_START;					\
        __asm volatile (						\
                /* Set up Restartable Atomic Sequence.  */		\
                "1:"							\
                "\tadr   %2, 1b\n"					\
                "\tstr   %2, [%5]\n"					\
                "\tadr   %2, 2f\n"					\
                "\tstr   %2, [%5, #4]\n"				\
                                                                        \
                "\t"LDR" %0, %4\n"	/* Load old value.  */		\
                "\t"OP"  %2, %0, %3\n"	/* Calculate new value.  */	\
                "\t"STR" %2, %1\n"	/* Store new value.  */		\
                                                                        \
                /* Tear down Restartable Atomic Sequence.  */		\
                "2:"							\
                "\tmov   %2, #0x00000000\n"				\
                "\tstr   %2, [%5]\n"					\
                "\tmov   %2, #0xffffffff\n"				\
                "\tstr   %2, [%5, #4]\n"				\
                : "=&r" (old), "=m" (*mem), "=&r" (temp)		\
                : "r" (val), "m" (*mem), "r" (ras_start));		\
        return (old);							\
}

#define	SYNC_OP_AND_FETCH_N(N, TYPE, LDR, STR, NAME, OP)		\
TYPE HIDDEN   								\
__sync_##NAME##_and_fetch_##N (TYPE *mem, TYPE val)			\
{									\
        unsigned int old, temp, ras_start, res;                         \
                                                                        \
        ras_start = ARM_RAS_START;					\
        __asm volatile (						\
                /* Set up Restartable Atomic Sequence.  */		\
                "1:"							\
                "\tadr   %2, 1b\n"					\
                "\tstr   %2, [%6]\n"					\
                "\tadr   %2, 2f\n"					\
                "\tstr   %2, [%6, #4]\n"				\
                                                                        \
                "\t"LDR" %0, %5\n"	/* Load old value.  */		\
                "\t"OP"  %3, %0, %4\n"	/* Calculate new value.  */	\
                "\t"STR" %3, %1\n"	/* Store new value.  */		\
                                                                        \
                /* Tear down Restartable Atomic Sequence.  */		\
                "2:"							\
                "\tmov   %2, #0x00000000\n"				\
                "\tstr   %2, [%6]\n"					\
                "\tmov   %2, #0xffffffff\n"				\
                "\tstr   %2, [%6, #4]\n"				\
                : "=&r" (old), "=m" (*mem), "=&r" (temp), "=&r" (res)	\
                : "r" (val), "m" (*mem), "r" (ras_start));		\
        return (res);							\
}

#define	EMIT_ALL_OPS_N(N, TYPE, LDR, STR, STREQ)			\
SYNC_LOCK_TEST_AND_SET_N (N, TYPE, LDR, STR)				\
SYNC_LOCK_RELEASE_N (N, TYPE)                                           \
SYNC_VAL_CAS_N (N, TYPE, LDR, STREQ)	                		\
SYNC_BOOL_CAS_N (N, TYPE)                                               \
SYNC_FETCH_AND_OP_N (N, TYPE, LDR, STR, add, "add")		        \
SYNC_FETCH_AND_OP_N (N, TYPE, LDR, STR, and, "and")		        \
SYNC_FETCH_AND_OP_N (N, TYPE, LDR, STR, or, "orr")		        \
SYNC_FETCH_AND_OP_N (N, TYPE, LDR, STR, sub, "sub")		        \
SYNC_FETCH_AND_OP_N (N, TYPE, LDR, STR, xor, "eor")                     \
SYNC_OP_AND_FETCH_N (N, TYPE, LDR, STR, add, "add")		        \
SYNC_OP_AND_FETCH_N (N, TYPE, LDR, STR, and, "and")		        \
SYNC_OP_AND_FETCH_N (N, TYPE, LDR, STR, or, "orr")		        \
SYNC_OP_AND_FETCH_N (N, TYPE, LDR, STR, sub, "sub")		        \
SYNC_OP_AND_FETCH_N (N, TYPE, LDR, STR, xor, "eor")



EMIT_ALL_OPS_N (1, unsigned char, "ldrb", "strb", "streqb")
EMIT_ALL_OPS_N (2, unsigned short, "ldrh", "strh", "streqh")
EMIT_ALL_OPS_N (4, unsigned int, "ldr", "str", "streq")

#endif
