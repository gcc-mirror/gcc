/* Copyright (C) 2012-2019 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file contains data types and function declarations that are
   private to the implementation of libatomic.  */

#ifndef LIBATOMIC_H
#define LIBATOMIC_H 1

#include "auto-config.h"
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <limits.h>
#include <string.h>


/* Symbol concatenation macros.  */
#define C2_(X,Y)	X ## Y
#define C2(X,Y)		C2_(X,Y)
#define C3_(X,Y,Z)	X ## Y ## Z
#define C3(X,Y,Z)	C3_(X,Y,Z)
#define C4_(W,X,Y,Z)	W ## X ## Y ## Z
#define C4(W,X,Y,Z)	C4_(W,X,Y,Z)

/* Stringification macros.  */
#define S2(X)		#X
#define S(X)		S2(X)

/* All of the primitive types on which we operate.  */
typedef unsigned U_1 __attribute__((mode(QI)));
#if HAVE_INT2
typedef unsigned U_2 __attribute__((mode(HI)));
#endif
#if HAVE_INT4
typedef unsigned U_4 __attribute__((mode(SI)));
#endif
#if HAVE_INT8
typedef unsigned U_8 __attribute__((mode(DI)));
#endif
#if HAVE_INT16
typedef unsigned U_16 __attribute__((mode(TI)));
#endif

/* The widest type that we support.  */
#if HAVE_INT16
# define MAX_SIZE	16
#elif HAVE_INT8
# define MAX_SIZE	8
#elif HAVE_INT4
# define MAX_SIZE	4
#elif HAVE_INT2
# define MAX_SIZE	2
#else
# define MAX_SIZE	1
#endif
typedef C2(U_,MAX_SIZE) U_MAX;

/* Provide dummy fallback types so that stuff is syntactically correct
   without having to overdo the ifdefs.  The code using these should
   always be protected with the HAVE_INT{n} macros.  */
#if !HAVE_INT2
typedef U_MAX U_2;
#endif
#if !HAVE_INT4
typedef U_MAX U_4;
#endif
#if !HAVE_INT8
typedef U_MAX U_8;
#endif
#if !HAVE_INT16
typedef U_MAX U_16;
#endif

union max_size_u
{
  U_1 b[MAX_SIZE];
  U_2 i2;
  U_4 i4;
  U_8 i8;
  U_16 i16;
};

/* The "word" size of the machine.  */
typedef unsigned UWORD __attribute__((mode(word)));

/* Macros for handing sub-word sized quantities.  */
#define MASK_1		((UWORD)0xff)
#define MASK_2		((UWORD)0xffff)
#define MASK_4		((UWORD)0xffffffff)
#define INVERT_MASK_1	((UWORD)WORDS_BIGENDIAN << ((WORDSIZE - 1) * CHAR_BIT))
#define INVERT_MASK_2	((UWORD)WORDS_BIGENDIAN << ((WORDSIZE - 2) * CHAR_BIT))
#define INVERT_MASK_4	((UWORD)WORDS_BIGENDIAN << ((WORDSIZE - 4) * CHAR_BIT))

/* Most of the files in this library are compiled multiple times with
   N defined to be a power of 2 between 1 and 16.  The SIZE macro is
   then used to append _N to the symbol being manipulated.  */
#define SIZE(X)		C3(X,_,N)
#define WSIZE(X)	C3(X,_,WORDSIZE)
#define PTR(N,X)	((C2(U_,N) *)X)

/* And thus, the type on which this compilation will be operating.  */
#define ITYPE		SIZE(I)
#define UTYPE		SIZE(U)

/* Utility macros for GCC attributes.  */
#define UNUSED		__attribute__((unused))
#ifdef HAVE_ATTRIBUTE_VISIBILITY
# define HIDDEN		__attribute__((visibility("hidden")))
#else
# define HIDDEN
#endif

/* Occasionally we have to play games with internal and external symbol
   names, in order to work around builtin functions of the same name.
   This macro sets the external name of the function appropriately.  */
#define ASMNAME(X)	__asm__(S(C2(__USER_LABEL_PREFIX__,X)))

/* Locking for a "small" operation.  In the bare-metal single processor
   cases this could be implemented by disabling interrupts.  Thus the extra
   word passed between the two functions, saving the interrupt level.
   It is assumed that the object being locked does not cross the locking
   granularity.

   Not actually declared here so that they can be defined static inline
   in a target-specfic <host-config.h>.

UWORD protect_start (void *ptr);
void protect_end (void *ptr, UWORD);
*/

/* Locking for a "large' operation.  This should always be some sort of
   test-and-set operation, as we assume that the interrupt latency would
   be unreasonably large.  */
void libat_lock_n (void *ptr, size_t n);
void libat_unlock_n (void *ptr, size_t n);

/* We'll need to declare all of the sized functions a few times...  */
#define DECLARE_ALL_SIZED(N)  DECLARE_ALL_SIZED_(N,C2(U_,N))
#define DECLARE_ALL_SIZED_(N,T)						\
  DECLARE_1(T,    C2(load_,N), (T *mptr, int));				\
  DECLARE_1(void, C2(store_,N), (T *mptr, T val, int));			\
  DECLARE_1(T,    C2(exchange_,N), (T *mptr, T, int));			\
  DECLARE_1(bool, C2(compare_exchange_,N), (T *mptr, T *, T, int, int)); \
  DECLARE_1(bool, C2(test_and_set_,N), (T *mptr, int));			\
  DECLARE_1(T,    C2(fetch_add_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(fetch_sub_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(fetch_and_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(fetch_xor_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(fetch_or_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(fetch_nand_,N), (T *mptr, T, int));		\
  DECLARE_1(T,    C2(add_fetch_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(sub_fetch_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(and_fetch_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(xor_fetch_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(or_fetch_,N), (T *mptr, T, int));			\
  DECLARE_1(T,    C2(nand_fetch_,N), (T *mptr, T, int))

/* All sized operations are implemented in hidden functions prefixed with
   "libat_".  These are either renamed or aliased to the expected prefix
   of "__atomic".  Some amount of renaming is required to avoid hiding or
   conflicting with the builtins of the same name, but this additional
   use of hidden symbols (where appropriate) avoids unnecessary PLT entries
   on relevant targets.  */

#if IFUNC_ALT
# define MAN(X)			ASMNAME(C4(libat_,X,_i,IFUNC_ALT)) HIDDEN
#elif defined(HAVE_ATTRIBUTE_ALIAS)
# define MAN(X)			HIDDEN
#else
# define MAN(X)			ASMNAME(C2(__atomic_,X))
#endif

#if !defined(N) && HAVE_IFUNC
# define DECLARE_1(RET,NAME,ARGS) \
	RET C2(libat_,NAME) ARGS MAN(NAME); \
	RET C2(ifunc_,NAME) ARGS ASMNAME(C2(__atomic_,NAME))
#else
# define DECLARE_1(RET,NAME,ARGS)	RET C2(libat_,NAME) ARGS MAN(NAME)
#endif

/* Prefix to use when calling internal, possibly ifunc'ed functions.  */
#if HAVE_IFUNC
# define local_ ifunc_
#else
# define local_ libat_
#endif

DECLARE_ALL_SIZED(1);
DECLARE_ALL_SIZED(2);
DECLARE_ALL_SIZED(4);
DECLARE_ALL_SIZED(8);
DECLARE_ALL_SIZED(16);

#undef DECLARE_1
#undef DECLARE_ALL_SIZED
#undef DECLARE_ALL_SIZED_

/* And the generic sized versions.  */
void libat_load (size_t, void *, void *, int) MAN(load);
void libat_store (size_t, void *, void *, int) MAN(store);
void libat_exchange (size_t, void *, void *, void *, int) MAN(exchange);
bool libat_compare_exchange (size_t, void *, void *, void *, int, int)
	MAN(compare_exchange);
bool libat_is_lock_free (size_t, void *) MAN(is_lock_free);

#undef MAN

#include <host-config.h>

/* We don't have IFUNC_NCOND until after host-config.h.  */
#if !HAVE_IFUNC
# define IFUNC_NCOND(N) 0
#endif

#if IFUNC_ALT
# define EXPORT_ALIAS(X)	/* exported symbol in non-alternate file */
#elif defined(N) && IFUNC_NCOND(N)
# if IFUNC_NCOND(N) == 1
#  define GEN_SELECTOR(X)					\
	extern typeof(C2(libat_,X)) C3(libat_,X,_i1) HIDDEN;	\
	static typeof(C2(libat_,X)) * C2(select_,X) (IFUNC_RESOLVER_ARGS) \
	{							\
	  if (IFUNC_COND_1)					\
	    return C3(libat_,X,_i1);				\
	  return C2(libat_,X);					\
	}
# elif IFUNC_NCOND(N) == 2
#  define GEN_SELECTOR(X)					\
	extern typeof(C2(libat_,X)) C3(libat_,X,_i1) HIDDEN;	\
	extern typeof(C2(libat_,X)) C3(libat_,X,_i2) HIDDEN;	\
	static typeof(C2(libat_,X)) * C2(select_,X) (IFUNC_RESOLVER_ARGS) \
	{							\
	  if (IFUNC_COND_1)					\
	    return C3(libat_,X,_i1);				\
	  if (IFUNC_COND_2)					\
	    return C3(libat_,X,_i2);				\
	  return C2(libat_,X);					\
	}
# elif IFUNC_NCOND(N) == 3
#  define GEN_SELECTOR(X)					\
	extern typeof(C2(libat_,X)) C3(libat_,X,_i1) HIDDEN;	\
	extern typeof(C2(libat_,X)) C3(libat_,X,_i2) HIDDEN;	\
	extern typeof(C2(libat_,X)) C3(libat_,X,_i3) HIDDEN;	\
	static typeof(C2(libat_,X)) * C2(select_,X) (IFUNC_RESOLVER_ARGS) \
	{							\
	  if (IFUNC_COND_1)					\
	    return C3(libat_,X,_i1);				\
	  if (IFUNC_COND_2)					\
	    return C3(libat_,X,_i2);				\
	  if (IFUNC_COND_3)					\
	    return C3(libat_,X,_i3);				\
	  return C2(libat_,X);					\
	}
# else
#  error "Unsupported number of ifunc alternatives."
# endif
# define EXPORT_ALIAS(X)					\
	GEN_SELECTOR(X)						\
	typeof(C2(libat_,X)) C2(ifunc_,X)			\
	  ASMNAME(C2(__atomic_,X))				\
	  __attribute__((ifunc(S(C2(select_,X)))))
#elif defined(HAVE_ATTRIBUTE_ALIAS)
# define EXPORT_ALIAS(X)					\
	extern typeof(C2(libat_,X)) C2(export_,X)		\
	  ASMNAME(C2(__atomic_,X))				\
	  __attribute__((alias(S(C2(libat_,X)))))
#else
# define EXPORT_ALIAS(X)	/* original symbol is exported */
#endif

#endif /* LIBATOMIC_H */
