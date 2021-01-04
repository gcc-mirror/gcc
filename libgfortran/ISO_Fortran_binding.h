/* Declarations for ISO Fortran binding.
   Copyright (C) 2018-2021 Free Software Foundation, Inc.
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
   CFI_INVALID_STRIDE should be defined in the standard because they are useful to the implementation of the functions.
 */
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

/* Types with no kind. */
#define CFI_type_struct 6
#define CFI_type_cptr 7
#define CFI_type_cfunptr 8
#define CFI_type_other -1

/* Types with kind parameter.
   The kind parameter represents the type's byte size. The exception is kind = 10, which has byte size of 64 but 80 bit precision. Complex variables are double the byte size of their real counterparts. The ucs4_char matches wchar_t if sizeof (wchar_t) == 4.
 */
#define CFI_type_char (CFI_type_Character + (1 << CFI_type_kind_shift))
#define CFI_type_ucs4_char (CFI_type_Character + (4 << CFI_type_kind_shift))

/* C-Fortran Interoperability types. */
#define CFI_type_signed_char (CFI_type_Integer + (1 << CFI_type_kind_shift))
#define CFI_type_short (CFI_type_Integer + (2 << CFI_type_kind_shift))
#define CFI_type_int (CFI_type_Integer + (4 << CFI_type_kind_shift))
#define CFI_type_long (CFI_type_Integer + (8 << CFI_type_kind_shift))
#define CFI_type_long_long (CFI_type_Integer + (8 << CFI_type_kind_shift))
#define CFI_type_size_t (CFI_type_Integer + (8 << CFI_type_kind_shift))
#define CFI_type_int8_t (CFI_type_Integer + (1 << CFI_type_kind_shift))
#define CFI_type_int16_t (CFI_type_Integer + (2 << CFI_type_kind_shift))
#define CFI_type_int32_t (CFI_type_Integer + (4 << CFI_type_kind_shift))
#define CFI_type_int64_t (CFI_type_Integer + (8 << CFI_type_kind_shift))
#define CFI_type_int_least8_t (CFI_type_Integer + (1 << CFI_type_kind_shift))
#define CFI_type_int_least16_t (CFI_type_Integer + (2 << CFI_type_kind_shift))
#define CFI_type_int_least32_t (CFI_type_Integer + (4 << CFI_type_kind_shift))
#define CFI_type_int_least64_t (CFI_type_Integer + (8 << CFI_type_kind_shift))
#define CFI_type_int_fast8_t (CFI_type_Integer + (1 << CFI_type_kind_shift))
#define CFI_type_int_fast16_t (CFI_type_Integer + (2 << CFI_type_kind_shift))
#define CFI_type_int_fast32_t (CFI_type_Integer + (4 << CFI_type_kind_shift))
#define CFI_type_int_fast64_t (CFI_type_Integer + (8 << CFI_type_kind_shift))
#define CFI_type_intmax_t (CFI_type_Integer + (8 << CFI_type_kind_shift))
#define CFI_type_intptr_t (CFI_type_Integer + (8 << CFI_type_kind_shift))
#define CFI_type_ptrdiff_t (CFI_type_Integer + (8 << CFI_type_kind_shift))
#define CFI_type_int128_t (CFI_type_Integer + (16 << CFI_type_kind_shift))
#define CFI_type_int_least128_t (CFI_type_Integer + (16 << CFI_type_kind_shift))
#define CFI_type_int_fast128_t (CFI_type_Integer + (16 << CFI_type_kind_shift))
#define CFI_type_Bool (CFI_type_Logical + (1 << CFI_type_kind_shift))
#define CFI_type_float (CFI_type_Real + (4 << CFI_type_kind_shift))
#define CFI_type_double (CFI_type_Real + (8 << CFI_type_kind_shift))
#define CFI_type_long_double (CFI_type_Real + (10 << CFI_type_kind_shift))
#define CFI_type_float128 (CFI_type_Real + (16 << CFI_type_kind_shift))
#define CFI_type_float_Complex (CFI_type_Complex + (4 << CFI_type_kind_shift))
#define CFI_type_double_Complex (CFI_type_Complex + (8 << CFI_type_kind_shift))
#define CFI_type_long_double_Complex (CFI_type_Complex + (10 << CFI_type_kind_shift))
#define CFI_type_float128_Complex (CFI_type_Complex + (16 << CFI_type_kind_shift))

#ifdef __cplusplus
}
#endif

#endif /* ISO_FORTRAN_BINDING_H */
