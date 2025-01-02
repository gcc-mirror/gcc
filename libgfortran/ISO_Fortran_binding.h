/* Declarations for ISO Fortran binding.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
   Contributed by Daniel Celis Garza  <celisdanieljr@gmail.com>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef ISO_FORTRAN_BINDING_H
#define ISO_FORTRAN_BINDING_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>  /* Standard ptrdiff_t tand size_t. */
#include <stdint.h>  /* Integer types. */

/* Constants, defined as macros. */
#define CFI_VERSION 1
#define CFI_MAX_RANK 15

/* Attributes. */
#define CFI_attribute_pointer 0
#define CFI_attribute_allocatable 1
#define CFI_attribute_other 2

/* Error codes.
   Note that CFI_FAILURE and CFI_INVALID_STRIDE are specific to GCC
   and not part of the Fortran standard   */
#define CFI_SUCCESS 0
#define CFI_FAILURE 1
#define CFI_ERROR_BASE_ADDR_NULL 2
#define CFI_ERROR_BASE_ADDR_NOT_NULL 3
#define CFI_INVALID_ELEM_LEN 4
#define CFI_INVALID_RANK 5
#define CFI_INVALID_TYPE 6
#define CFI_INVALID_ATTRIBUTE 7
#define CFI_INVALID_EXTENT 8
#define CFI_INVALID_STRIDE 9
#define CFI_INVALID_DESCRIPTOR 10
#define CFI_ERROR_MEM_ALLOCATION 11
#define CFI_ERROR_OUT_OF_BOUNDS 12

/* CFI type definitions. */
typedef ptrdiff_t CFI_index_t;
typedef int8_t CFI_rank_t;
typedef int8_t CFI_attribute_t;
typedef int16_t CFI_type_t;

/* CFI_dim_t. */
typedef struct CFI_dim_t
  {
    CFI_index_t lower_bound;
    CFI_index_t extent;
    CFI_index_t sm;
  }
CFI_dim_t;

/* CFI_cdesc_t, C descriptors are cast to this structure as follows:
   CFI_CDESC_T(CFI_MAX_RANK) foo;
   CFI_cdesc_t * bar = (CFI_cdesc_t *) &foo;
 */
typedef struct CFI_cdesc_t
 {
    void *base_addr;
    size_t elem_len;
    int version;
    CFI_rank_t rank;
    CFI_attribute_t attribute;
    CFI_type_t type;
    CFI_dim_t dim[];
 }
CFI_cdesc_t;

/* CFI_CDESC_T with an explicit type. */
#define CFI_CDESC_TYPE_T(r, base_type) \
	struct { \
		base_type *base_addr; \
		size_t elem_len; \
		int version; \
		CFI_rank_t rank; \
		CFI_attribute_t attribute; \
		CFI_type_t type; \
		CFI_dim_t dim[r]; \
	}
#define CFI_CDESC_T(r) CFI_CDESC_TYPE_T (r, void)

/* CFI function declarations. */
extern void *CFI_address (const CFI_cdesc_t *, const CFI_index_t []);
extern int CFI_allocate (CFI_cdesc_t *, const CFI_index_t [], const CFI_index_t [],
			 size_t);
extern int CFI_deallocate (CFI_cdesc_t *);
extern int CFI_establish (CFI_cdesc_t *, void *, CFI_attribute_t, CFI_type_t, size_t,
			  CFI_rank_t, const CFI_index_t []);
extern int CFI_is_contiguous (const CFI_cdesc_t *);
extern int CFI_section (CFI_cdesc_t *, const CFI_cdesc_t *, const CFI_index_t [],
			const CFI_index_t [], const CFI_index_t []);
extern int CFI_select_part (CFI_cdesc_t *, const CFI_cdesc_t *, size_t, size_t);
extern int CFI_setpointer (CFI_cdesc_t *, CFI_cdesc_t *, const CFI_index_t []);

/* Types and kind numbers. Allows bitwise and to reveal the intrinsic type of a kind type. It also allows us to find the kind parameter by inverting the bit-shift equation.
   CFI_type_kind_shift = 8
   CFI_intrinsic_type  = 0 0 0 0 0 0 0 0 0 0 1 0
   CFI_type_kind       = 0 0 0 0 0 0 0 0 1 0 0 0
   CFI_type_example    = CFI_intrinsic_type + (CFI_type_kind << CFI_type_kind_shift)
   Defining the CFI_type_example.
   CFI_type_kind       = 0 0 0 0 0 0 0 0 1 0 0 0  << CFI_type_kind_shift
			-------------------------
			 1 0 0 0 0 0 0 0 0 0 0 0  +
   CFI_intrinsic_type  = 0 0 0 0 0 0 0 0 0 0 1 0
			-------------------------
   CFI_type_example    = 1 0 0 0 0 0 0 0 0 0 1 0
   Finding the intrinsic type with the logical mask.
   CFI_type_example    = 1 0 0 0 0 0 0 0 0 0 1 0  &
   CFI_type_mask       = 0 0 0 0 1 1 1 1 1 1 1 1
			-------------------------
   CFI_intrinsic_type  = 0 0 0 0 0 0 0 0 0 0 1 0
   Using the intrinsic type and kind shift to find the kind value of the type.
   CFI_type_kind = (CFI_type_example - CFI_intrinsic_type) >> CFI_type_kind_shift
   CFI_type_example   = 1 0 0 0 0 0 0 0 0 0 1 0  -
   CFI_intrinsic_type = 0 0 0 0 0 0 0 0 0 0 1 0
			-------------------------
			1 0 0 0 0 0 0 0 0 0 0 0  >> CFI_type_kind_shift
			-------------------------
   CFI_type_kind      = 0 0 0 0 0 0 0 0 1 0 0 0
 */
#define CFI_type_mask 0xFF
#define CFI_type_kind_shift 8

/* Intrinsic types. Their kind number defines their storage size. */
#define CFI_type_Integer 1
#define CFI_type_Logical 2
#define CFI_type_Real 3
#define CFI_type_Complex 4
#define CFI_type_Character 5

/* Types with no kind.  */
#define CFI_type_struct 6
#define CFI_type_cptr 7
#define CFI_type_cfunptr 8
#define CFI_type_other -1

/* Types with kind parameter.
   The kind parameter represents the type's byte size.  The exception is
   real kind = 10, which has byte size of 128 bits but 80 bit precision.
   Complex variables are double the byte size of their real counterparts.
   The ucs4_char matches wchar_t if sizeof (wchar_t) == 4.
 */
#define CFI_type_char (CFI_type_Character + (1 << CFI_type_kind_shift))
#define CFI_type_ucs4_char (CFI_type_Character + (4 << CFI_type_kind_shift))

/* C-Fortran Interoperability types. */
#define CFI_type_signed_char (CFI_type_Integer + (sizeof (char) << CFI_type_kind_shift))
#define CFI_type_short (CFI_type_Integer + (sizeof (short) << CFI_type_kind_shift))
#define CFI_type_int (CFI_type_Integer + (sizeof (int) << CFI_type_kind_shift))
#define CFI_type_long (CFI_type_Integer + (sizeof (long) << CFI_type_kind_shift))
#define CFI_type_long_long (CFI_type_Integer + (sizeof (long long) << CFI_type_kind_shift))
#define CFI_type_size_t (CFI_type_Integer + (sizeof (size_t) << CFI_type_kind_shift))
#define CFI_type_int8_t (CFI_type_Integer + (sizeof (int8_t) << CFI_type_kind_shift))
#define CFI_type_int16_t (CFI_type_Integer + (sizeof (int16_t) << CFI_type_kind_shift))
#define CFI_type_int32_t (CFI_type_Integer + (sizeof (int32_t) << CFI_type_kind_shift))
#define CFI_type_int64_t (CFI_type_Integer + (sizeof (int64_t) << CFI_type_kind_shift))
#define CFI_type_int_least8_t (CFI_type_Integer + (sizeof (int_least8_t) << CFI_type_kind_shift))
#define CFI_type_int_least16_t (CFI_type_Integer + (sizeof (int_least16_t) << CFI_type_kind_shift))
#define CFI_type_int_least32_t (CFI_type_Integer + (sizeof (int_least32_t) << CFI_type_kind_shift))
#define CFI_type_int_least64_t (CFI_type_Integer + (sizeof (int_least64_t) << CFI_type_kind_shift))
#define CFI_type_int_fast8_t (CFI_type_Integer + (sizeof (int_fast8_t) << CFI_type_kind_shift))
#define CFI_type_int_fast16_t (CFI_type_Integer + (sizeof (int_fast16_t) << CFI_type_kind_shift))
#define CFI_type_int_fast32_t (CFI_type_Integer + (sizeof (int_fast32_t) << CFI_type_kind_shift))
#define CFI_type_int_fast64_t (CFI_type_Integer + (sizeof (int_fast64_t) << CFI_type_kind_shift))
#define CFI_type_intmax_t (CFI_type_Integer + (sizeof (intmax_t) << CFI_type_kind_shift))
#define CFI_type_intptr_t (CFI_type_Integer + (sizeof (intptr_t) << CFI_type_kind_shift))
#define CFI_type_ptrdiff_t (CFI_type_Integer + (sizeof (ptrdiff_t) << CFI_type_kind_shift))
#define CFI_type_Bool (CFI_type_Logical + (sizeof (_Bool) << CFI_type_kind_shift))
#define CFI_type_float (CFI_type_Real + (sizeof (float) << CFI_type_kind_shift))
#define CFI_type_double (CFI_type_Real + (sizeof (double) << CFI_type_kind_shift))
#define CFI_type_float_Complex (CFI_type_Complex + (sizeof (float) << CFI_type_kind_shift))
#define CFI_type_double_Complex (CFI_type_Complex + (sizeof (double) << CFI_type_kind_shift))

/* If GCC supports int128_t on this target, it predefines
   __SIZEOF_INT128__ to 16.  */
#if defined(__SIZEOF_INT128__)
#if (__SIZEOF_INT128__ == 16)
#define CFI_type_int128_t (CFI_type_Integer + (16 << CFI_type_kind_shift))
#define CFI_type_int_least128_t (CFI_type_Integer + (16 << CFI_type_kind_shift))
#define CFI_type_int_fast128_t (CFI_type_Integer + (16 << CFI_type_kind_shift))
#else
#error "Can't determine kind of int128_t"
#endif
#else
#define CFI_type_int128_t -2
#define CFI_type_int_least128_t -2
#define CFI_type_int_fast128_t -2
#endif

/* The situation with long double support is more complicated; we need to
   examine the type in more detail to figure out its kind.
   GCC and some other compilers predefine the __LDBL* macros; otherwise
   get the parameters we need from float.h.  */

#if (defined (__LDBL_MANT_DIG__) \
     && defined (__LDBL_MIN_EXP__) \
     && defined (__LDBL_MAX_EXP__) \
     && defined (__DBL_MANT_DIG__) \
     && defined (__DBL_MIN_EXP__) \
     && defined (__DBL_MAX_EXP__))
#define __CFI_LDBL_MANT_DIG__ __LDBL_MANT_DIG__
#define __CFI_LDBL_MIN_EXP__ __LDBL_MIN_EXP__
#define __CFI_LDBL_MAX_EXP__ __LDBL_MAX_EXP__
#define __CFI_DBL_MANT_DIG__ __DBL_MANT_DIG__
#define __CFI_DBL_MIN_EXP__ __DBL_MIN_EXP__
#define __CFI_DBL_MAX_EXP__ __DBL_MAX_EXP__

#else
#include <float.h>

#if (defined (LDBL_MANT_DIG) \
     && defined (LDBL_MIN_EXP) \
     && defined (LDBL_MAX_EXP) \
     && defined (DBL_MANT_DIG) \
     && defined (DBL_MIN_EXP) \
     && defined (DBL_MAX_EXP))
#define __CFI_LDBL_MANT_DIG__ LDBL_MANT_DIG
#define __CFI_LDBL_MIN_EXP__ LDBL_MIN_EXP
#define __CFI_LDBL_MAX_EXP__ LDBL_MAX_EXP
#define __CFI_DBL_MANT_DIG__ DBL_MANT_DIG
#define __CFI_DBL_MIN_EXP__ DBL_MIN_EXP
#define __CFI_DBL_MAX_EXP__ DBL_MAX_EXP

#else
#define CFI_no_long_double 1

#endif  /* Definitions from float.h.  */
#endif  /* Definitions from compiler builtins.  */

/* Can't determine anything about long double support?  */
#if (defined (CFI_no_long_double))
#define CFI_type_long_double -2
#define CFI_type_long_double_Complex -2

/* Long double is the same kind as double.  */
#elif (__CFI_LDBL_MANT_DIG__ == __CFI_DBL_MANT_DIG__ \
     && __CFI_LDBL_MIN_EXP__ == __CFI_DBL_MIN_EXP__ \
     && __CFI_LDBL_MAX_EXP__ == __CFI_DBL_MAX_EXP__)
#define CFI_type_long_double CFI_type_double
#define CFI_type_long_double_Complex CFI_type_double_Complex

/* This is the 80-bit encoding on x86; Fortran assigns it kind 10.  */
#elif ((__CFI_LDBL_MANT_DIG__ == 64 || __CFI_LDBL_MANT_DIG__ == 53) \
       && __CFI_LDBL_MIN_EXP__ == -16381 \
       && __CFI_LDBL_MAX_EXP__ == 16384)
#define CFI_type_long_double (CFI_type_Real + (10 << CFI_type_kind_shift))
#define CFI_type_long_double_Complex (CFI_type_Complex + (10 << CFI_type_kind_shift))

/* This is the 96-bit encoding on m68k; Fortran assigns it kind 10.  */
#elif (__CFI_LDBL_MANT_DIG__ == 64 \
       && __CFI_LDBL_MIN_EXP__ == -16382 \
       && __CFI_LDBL_MAX_EXP__ == 16384)
#define CFI_type_long_double (CFI_type_Real + (10 << CFI_type_kind_shift))
#define CFI_type_long_double_Complex (CFI_type_Complex + (10 << CFI_type_kind_shift))

/* This is the IEEE 128-bit encoding, same as _Float128.  */
#elif (__CFI_LDBL_MANT_DIG__ == 113 \
       && __CFI_LDBL_MIN_EXP__ == -16381 \
       && __CFI_LDBL_MAX_EXP__ == 16384)
#define CFI_type_long_double (CFI_type_Real + (16 << CFI_type_kind_shift))
#define CFI_type_long_double_Complex (CFI_type_Complex + (16 << CFI_type_kind_shift))

/* This is the IBM128 encoding used on PowerPC; also assigned kind 16.  */
#elif (__CFI_LDBL_MANT_DIG__ == 106 \
       && __CFI_LDBL_MIN_EXP__ == -968 \
       && __CFI_LDBL_MAX_EXP__ == 1024)
#define CFI_type_long_double (CFI_type_Real + (16 << CFI_type_kind_shift))
#define CFI_type_long_double_Complex (CFI_type_Complex + (16 << CFI_type_kind_shift))
#define CFI_no_float128 1

/* It's a bug if we get here.  If you've got a target that has some other
   long double encoding, you need add something here for Fortran to
   recognize it.  */
#else
#error "Can't determine kind of long double"
#endif

/* Similarly for _Float128.  This always refers to the IEEE encoding
   and not some other 128-bit representation, so if we already used
   kind 16 for a non-IEEE representation, this one must be unsupported
   in Fortran even if it's available in C.  */
#if (!defined (CFI_no_float128) \
     && defined(__FLT128_MANT_DIG__) && __FLT128_MANT_DIG__ == 113  \
     && defined(__FLT128_MIN_EXP__) && __FLT128_MIN_EXP__ == -16381 \
     && defined(__FLT128_MAX_EXP__) && __FLT128_MAX_EXP__ == 16384)
#define CFI_type_float128 (CFI_type_Real + (16 << CFI_type_kind_shift))
#define CFI_type_float128_Complex (CFI_type_Complex + (16 << CFI_type_kind_shift))
#else
#define CFI_type_float128 -2
#define CFI_type_float128_Complex -2
#endif

#ifdef __cplusplus
}
#endif

#endif /* ISO_FORTRAN_BINDING_H */
