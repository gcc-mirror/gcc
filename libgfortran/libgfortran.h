/* Common declarations for all of libgfortran.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
   2011
   Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>, and
   Andy Vaught <andy@xena.eas.asu.edu>

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

#ifndef LIBGFOR_H
#define LIBGFOR_H

/* Ensure that ANSI conform stdio is used. This needs to be set before
   any system header file is included.  */
#if defined __MINGW32__
#  define _POSIX 1
#  define gfc_printf gnu_printf
#else
#  define gfc_printf __printf__
#endif

/* config.h MUST be first because it can affect system headers.  */
#include "config.h"

#include <stdio.h>
#include <math.h>
#include <stddef.h>
#include <float.h>
#include <stdarg.h>

/* If we're support quad-precision floating-point type, include the
   header to our support library.  */
#ifdef HAVE_FLOAT128
#  include "quadmath_weak.h"
#endif

#ifdef __MINGW32__
extern float __strtof (const char *, char **);
#define gfc_strtof __strtof
extern double __strtod (const char *, char **);
#define gfc_strtod __strtod
extern long double __strtold (const char *, char **);
#define gfc_strtold __strtold
#else
#define gfc_strtof strtof
#define gfc_strtod strtod
#define gfc_strtold strtold
#endif

#if HAVE_COMPLEX_H
# include <complex.h>
#else
#define complex __complex__
#endif

#include "../gcc/fortran/libgfortran.h"

#include "c99_protos.h"

#if HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

#include "gstdint.h"

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef __MINGW32__
typedef off64_t gfc_offset;
#else
typedef off_t gfc_offset;
#endif

#ifndef NULL
#define NULL (void *) 0
#endif

#ifndef __GNUC__
#define __attribute__(x)
#define likely(x)       (x)
#define unlikely(x)     (x)
#else
#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)
#endif


/* Make sure we have ptrdiff_t. */
#ifndef HAVE_PTRDIFF_T
typedef intptr_t ptrdiff_t;
#endif

/* On mingw, work around the buggy Windows snprintf() by using the one
   mingw provides, __mingw_snprintf().  We also provide a prototype for
   __mingw_snprintf(), because the mingw headers currently don't have one.  */
#if HAVE_MINGW_SNPRINTF
extern int __mingw_snprintf (char *, size_t, const char *, ...)
     __attribute__ ((format (gnu_printf, 3, 4)));
#undef snprintf
#define snprintf __mingw_snprintf
/* Fallback to sprintf if target does not have snprintf.  */
#elif !defined(HAVE_SNPRINTF)
#undef snprintf
#define snprintf(str, size, ...) sprintf (str, __VA_ARGS__)
#endif


/* For a library, a standard prefix is a requirement in order to partition
   the namespace.  IPREFIX is for symbols intended to be internal to the
   library.  */
#define PREFIX(x)	_gfortran_ ## x
#define IPREFIX(x)	_gfortrani_ ## x

/* Magic to rename a symbol at the compiler level.  You continue to refer
   to the symbol as OLD in the source, but it'll be named NEW in the asm.  */
#define sym_rename(old, new) sym_rename1(old, __USER_LABEL_PREFIX__, new)
#define sym_rename1(old, ulp, new) sym_rename2(old, ulp, new)
#define sym_rename2(old, ulp, new) extern __typeof(old) old __asm__(#ulp #new)

/* There are several classifications of routines:

     (1) Symbols used only within the library,
     (2) Symbols to be exported from the library,
     (3) Symbols to be exported from the library, but
	 also used inside the library.

   By telling the compiler about these different classifications we can
   tightly control the interface seen by the user, and get better code
   from the compiler at the same time.

   One of the following should be used immediately after the declaration
   of each symbol:

     internal_proto	Marks a symbol used only within the library,
			and adds IPREFIX to the assembly-level symbol
			name.  The later is important for maintaining
			the namespace partition for the static library.

     export_proto	Marks a symbol to be exported, and adds PREFIX
			to the assembly-level symbol name.

     export_proto_np	Marks a symbol to be exported without adding PREFIX.

     iexport_proto	Marks a function to be exported, but with the 
			understanding that it can be used inside as well.

     iexport_data_proto	Similarly, marks a data symbol to be exported.
			Unfortunately, some systems can't play the hidden
			symbol renaming trick on data symbols, thanks to
			the horribleness of COPY relocations.

   If iexport_proto or iexport_data_proto is used, you must also use
   iexport or iexport_data after the *definition* of the symbol.  */

#if defined(HAVE_ATTRIBUTE_VISIBILITY)
# define internal_proto(x) \
	sym_rename(x, IPREFIX (x)) __attribute__((__visibility__("hidden")))
#else
# define internal_proto(x)	sym_rename(x, IPREFIX(x))
#endif

#if defined(HAVE_ATTRIBUTE_VISIBILITY) && defined(HAVE_ATTRIBUTE_ALIAS)
# define export_proto(x)	sym_rename(x, PREFIX(x))
# define export_proto_np(x)	extern char swallow_semicolon
# define iexport_proto(x)	internal_proto(x)
# define iexport(x)		iexport1(x, IPREFIX(x))
# define iexport1(x,y)		iexport2(x,y)
# define iexport2(x,y) \
	extern __typeof(x) PREFIX(x) __attribute__((__alias__(#y)))
#else
# define export_proto(x)	sym_rename(x, PREFIX(x))
# define export_proto_np(x)	extern char swallow_semicolon
# define iexport_proto(x)	export_proto(x)
# define iexport(x)		extern char swallow_semicolon
#endif

/* TODO: detect the case when we *can* hide the symbol.  */
#define iexport_data_proto(x)	export_proto(x)
#define iexport_data(x)		extern char swallow_semicolon

/* The only reliable way to get the offset of a field in a struct
   in a system independent way is via this macro.  */
#ifndef offsetof
#define offsetof(TYPE, MEMBER)  ((size_t) &((TYPE *) 0)->MEMBER)
#endif

/* The C99 classification macros isfinite, isinf, isnan, isnormal
   and signbit are broken or inconsistent on quite a few targets.
   So, we use GCC's builtins instead.

   Another advantage for GCC's builtins for these type-generic macros
   is that it handles floating-point types that the system headers
   may not support (like __float128).  */

#undef isnan
#define isnan(x) __builtin_isnan(x)
#undef isfinite
#define isfinite(x) __builtin_isfinite(x)
#undef isinf
#define isinf(x) __builtin_isinf(x)
#undef isnormal
#define isnormal(x) __builtin_isnormal(x)
#undef signbit
#define signbit(x) __builtin_signbit(x)

/* TODO: find the C99 version of these an move into above ifdef.  */
#define REALPART(z) (__real__(z))
#define IMAGPART(z) (__imag__(z))
#define COMPLEX_ASSIGN(z_, r_, i_) {__real__(z_) = (r_); __imag__(z_) = (i_);}

#include "kinds.h"

/* Define the type used for the current record number for large file I/O.
   The size must be consistent with the size defined on the compiler side.  */
#ifdef HAVE_GFC_INTEGER_8
typedef GFC_INTEGER_8 GFC_IO_INT;
#else
#ifdef HAVE_GFC_INTEGER_4
typedef GFC_INTEGER_4 GFC_IO_INT;
#else
#error "GFC_INTEGER_4 should be available for the library to compile".
#endif
#endif

/* The following two definitions must be consistent with the types used
   by the compiler.  */
/* The type used of array indices, amongst other things.  */
typedef ptrdiff_t index_type;

/* The type used for the lengths of character variables.  */
typedef GFC_INTEGER_4 gfc_charlen_type;

/* Definitions of CHARACTER data types:
     - CHARACTER(KIND=1) corresponds to the C char type,
     - CHARACTER(KIND=4) corresponds to an unsigned 32-bit integer.  */
typedef GFC_UINTEGER_4 gfc_char4_t;

/* Byte size of character kinds.  For the kinds currently supported, it's
   simply equal to the kind parameter itself.  */
#define GFC_SIZE_OF_CHAR_KIND(kind) (kind)

/* This will be 0 on little-endian machines and one on big-endian machines.  */
extern int big_endian;
internal_proto(big_endian);

#define GFOR_POINTER_TO_L1(p, kind) \
  (big_endian * (kind - 1) + (GFC_LOGICAL_1 *)(p))

#define GFC_INTEGER_1_HUGE \
  (GFC_INTEGER_1)((((GFC_UINTEGER_1)1) << 7) - 1)
#define GFC_INTEGER_2_HUGE \
  (GFC_INTEGER_2)((((GFC_UINTEGER_2)1) << 15) - 1)
#define GFC_INTEGER_4_HUGE \
  (GFC_INTEGER_4)((((GFC_UINTEGER_4)1) << 31) - 1)
#define GFC_INTEGER_8_HUGE \
  (GFC_INTEGER_8)((((GFC_UINTEGER_8)1) << 63) - 1)
#ifdef HAVE_GFC_INTEGER_16
#define GFC_INTEGER_16_HUGE \
  (GFC_INTEGER_16)((((GFC_UINTEGER_16)1) << 127) - 1)
#endif

/* M{IN,AX}{LOC,VAL} need also infinities and NaNs if supported.  */

#ifdef __FLT_HAS_INFINITY__
# define GFC_REAL_4_INFINITY __builtin_inff ()
#endif
#ifdef __DBL_HAS_INFINITY__
# define GFC_REAL_8_INFINITY __builtin_inf ()
#endif
#ifdef __LDBL_HAS_INFINITY__
# ifdef HAVE_GFC_REAL_10
#  define GFC_REAL_10_INFINITY __builtin_infl ()
# endif
# ifdef HAVE_GFC_REAL_16
#  ifdef GFC_REAL_16_IS_LONG_DOUBLE
#   define GFC_REAL_16_INFINITY __builtin_infl ()
#  else
#   define GFC_REAL_16_INFINITY __builtin_infq ()
#  endif
# endif
#endif
#ifdef __FLT_HAS_QUIET_NAN__
# define GFC_REAL_4_QUIET_NAN __builtin_nanf ("")
#endif
#ifdef __DBL_HAS_QUIET_NAN__
# define GFC_REAL_8_QUIET_NAN __builtin_nan ("")
#endif
#ifdef __LDBL_HAS_QUIET_NAN__
# ifdef HAVE_GFC_REAL_10
#  define GFC_REAL_10_QUIET_NAN __builtin_nanl ("")
# endif
# ifdef HAVE_GFC_REAL_16
#  ifdef GFC_REAL_16_IS_LONG_DOUBLE
#   define GFC_REAL_16_QUIET_NAN __builtin_nanl ("")
#  else
#   define GFC_REAL_16_QUIET_NAN nanq ("")
#  endif
# endif
#endif

typedef struct descriptor_dimension
{
  index_type _stride;
  index_type _lbound;
  index_type _ubound;
}

descriptor_dimension;

#define GFC_ARRAY_DESCRIPTOR(r, type) \
struct {\
  type *data;\
  size_t offset;\
  index_type dtype;\
  descriptor_dimension dim[r];\
}

/* Commonly used array descriptor types.  */
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, void) gfc_array_void;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, char) gfc_array_char;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_INTEGER_1) gfc_array_i1;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_INTEGER_2) gfc_array_i2;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_INTEGER_4) gfc_array_i4;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_INTEGER_8) gfc_array_i8;
#ifdef HAVE_GFC_INTEGER_16
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_INTEGER_16) gfc_array_i16;
#endif
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_REAL_4) gfc_array_r4;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_REAL_8) gfc_array_r8;
#ifdef HAVE_GFC_REAL_10
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_REAL_10) gfc_array_r10;
#endif
#ifdef HAVE_GFC_REAL_16
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_REAL_16) gfc_array_r16;
#endif
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_COMPLEX_4) gfc_array_c4;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_COMPLEX_8) gfc_array_c8;
#ifdef HAVE_GFC_COMPLEX_10
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_COMPLEX_10) gfc_array_c10;
#endif
#ifdef HAVE_GFC_COMPLEX_16
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_COMPLEX_16) gfc_array_c16;
#endif
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_LOGICAL_1) gfc_array_l1;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_LOGICAL_2) gfc_array_l2;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_LOGICAL_4) gfc_array_l4;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_LOGICAL_8) gfc_array_l8;
#ifdef HAVE_GFC_LOGICAL_16
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_LOGICAL_16) gfc_array_l16;
#endif


#define GFC_DESCRIPTOR_RANK(desc) ((desc)->dtype & GFC_DTYPE_RANK_MASK)
#define GFC_DESCRIPTOR_TYPE(desc) (((desc)->dtype & GFC_DTYPE_TYPE_MASK) \
                                   >> GFC_DTYPE_TYPE_SHIFT)
#define GFC_DESCRIPTOR_SIZE(desc) ((desc)->dtype >> GFC_DTYPE_SIZE_SHIFT)
#define GFC_DESCRIPTOR_DATA(desc) ((desc)->data)
#define GFC_DESCRIPTOR_DTYPE(desc) ((desc)->dtype)

#define GFC_DIMENSION_LBOUND(dim) ((dim)._lbound)
#define GFC_DIMENSION_UBOUND(dim) ((dim)._ubound)
#define GFC_DIMENSION_STRIDE(dim) ((dim)._stride)
#define GFC_DIMENSION_EXTENT(dim) ((dim)._ubound + 1 - (dim)._lbound)
#define GFC_DIMENSION_SET(dim,lb,ub,str) \
  do \
    { \
      (dim)._lbound = lb;			\
      (dim)._ubound = ub;			\
      (dim)._stride = str;			\
    } while (0)
	    

#define GFC_DESCRIPTOR_LBOUND(desc,i) ((desc)->dim[i]._lbound)
#define GFC_DESCRIPTOR_UBOUND(desc,i) ((desc)->dim[i]._ubound)
#define GFC_DESCRIPTOR_EXTENT(desc,i) ((desc)->dim[i]._ubound + 1 \
				      - (desc)->dim[i]._lbound)
#define GFC_DESCRIPTOR_EXTENT_BYTES(desc,i) \
  (GFC_DESCRIPTOR_EXTENT(desc,i) * GFC_DESCRIPTOR_SIZE(desc))

#define GFC_DESCRIPTOR_STRIDE(desc,i) ((desc)->dim[i]._stride)
#define GFC_DESCRIPTOR_STRIDE_BYTES(desc,i) \
  (GFC_DESCRIPTOR_STRIDE(desc,i) * GFC_DESCRIPTOR_SIZE(desc))

/* Macros to get both the size and the type with a single masking operation  */

#define GFC_DTYPE_SIZE_MASK \
  ((~((index_type) 0) >> GFC_DTYPE_SIZE_SHIFT) << GFC_DTYPE_SIZE_SHIFT)
#define GFC_DTYPE_TYPE_SIZE_MASK (GFC_DTYPE_SIZE_MASK | GFC_DTYPE_TYPE_MASK)

#define GFC_DTYPE_TYPE_SIZE(desc) ((desc)->dtype & GFC_DTYPE_TYPE_SIZE_MASK)

#define GFC_DTYPE_INTEGER_1 ((BT_INTEGER << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_1) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_INTEGER_2 ((BT_INTEGER << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_2) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_INTEGER_4 ((BT_INTEGER << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_4) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_INTEGER_8 ((BT_INTEGER << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_8) << GFC_DTYPE_SIZE_SHIFT))
#ifdef HAVE_GFC_INTEGER_16
#define GFC_DTYPE_INTEGER_16 ((BT_INTEGER << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_16) << GFC_DTYPE_SIZE_SHIFT))
#endif

#define GFC_DTYPE_LOGICAL_1 ((BT_LOGICAL << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_LOGICAL_1) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_LOGICAL_2 ((BT_LOGICAL << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_LOGICAL_2) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_LOGICAL_4 ((BT_LOGICAL << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_LOGICAL_4) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_LOGICAL_8 ((BT_LOGICAL << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_LOGICAL_8) << GFC_DTYPE_SIZE_SHIFT))
#ifdef HAVE_GFC_LOGICAL_16
#define GFC_DTYPE_LOGICAL_16 ((BT_LOGICAL << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_LOGICAL_16) << GFC_DTYPE_SIZE_SHIFT))
#endif

#define GFC_DTYPE_REAL_4 ((BT_REAL << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_REAL_4) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_REAL_8 ((BT_REAL << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_REAL_8) << GFC_DTYPE_SIZE_SHIFT))
#ifdef HAVE_GFC_REAL_10
#define GFC_DTYPE_REAL_10  ((BT_REAL << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_REAL_10) << GFC_DTYPE_SIZE_SHIFT))
#endif
#ifdef HAVE_GFC_REAL_16
#define GFC_DTYPE_REAL_16 ((BT_REAL << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_REAL_16) << GFC_DTYPE_SIZE_SHIFT))
#endif

#define GFC_DTYPE_COMPLEX_4 ((BT_COMPLEX << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_COMPLEX_4) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_COMPLEX_8 ((BT_COMPLEX << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_COMPLEX_8) << GFC_DTYPE_SIZE_SHIFT))
#ifdef HAVE_GFC_COMPLEX_10
#define GFC_DTYPE_COMPLEX_10 ((BT_COMPLEX << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_COMPLEX_10) << GFC_DTYPE_SIZE_SHIFT))
#endif
#ifdef HAVE_GFC_COMPLEX_16
#define GFC_DTYPE_COMPLEX_16 ((BT_COMPLEX << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_COMPLEX_16) << GFC_DTYPE_SIZE_SHIFT))
#endif

#define GFC_DTYPE_DERIVED_1 ((BT_DERIVED << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_1) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_DERIVED_2 ((BT_DERIVED << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_2) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_DERIVED_4 ((BT_DERIVED << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_4) << GFC_DTYPE_SIZE_SHIFT))
#define GFC_DTYPE_DERIVED_8 ((BT_DERIVED << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_8) << GFC_DTYPE_SIZE_SHIFT))
#ifdef HAVE_GFC_INTEGER_16
#define GFC_DTYPE_DERIVED_16 ((BT_DERIVED << GFC_DTYPE_TYPE_SHIFT) \
   | (sizeof(GFC_INTEGER_16) << GFC_DTYPE_SIZE_SHIFT))
#endif

/* Macros to determine the alignment of pointers.  */

#define GFC_UNALIGNED_2(x) (((uintptr_t)(x)) & \
			    (__alignof__(GFC_INTEGER_2) - 1))
#define GFC_UNALIGNED_4(x) (((uintptr_t)(x)) & \
			    (__alignof__(GFC_INTEGER_4) - 1))
#define GFC_UNALIGNED_8(x) (((uintptr_t)(x)) & \
			    (__alignof__(GFC_INTEGER_8) - 1))
#ifdef HAVE_GFC_INTEGER_16
#define GFC_UNALIGNED_16(x) (((uintptr_t)(x)) & \
			     (__alignof__(GFC_INTEGER_16) - 1))
#endif

#define GFC_UNALIGNED_C4(x) (((uintptr_t)(x)) & \
			     (__alignof__(GFC_COMPLEX_4) - 1))

#define GFC_UNALIGNED_C8(x) (((uintptr_t)(x)) & \
			     (__alignof__(GFC_COMPLEX_8) - 1))

/* Runtime library include.  */
#define stringize(x) expand_macro(x)
#define expand_macro(x) # x

/* Runtime options structure.  */

typedef struct
{
  int stdin_unit, stdout_unit, stderr_unit, optional_plus;
  int locus;

  int separator_len;
  const char *separator;

  int all_unbuffered, unbuffered_preconnected, default_recl;
  int fpe, backtrace;
}
options_t;

extern options_t options;
internal_proto(options);

extern void backtrace_handler (int);
internal_proto(backtrace_handler);


/* Compile-time options that will influence the library.  */

typedef struct
{
  int warn_std;
  int allow_std;
  int pedantic;
  int convert;
  int backtrace;
  int sign_zero;
  size_t record_marker;
  int max_subrecord_length;
  int bounds_check;
  int range_check;
}
compile_options_t;

extern compile_options_t compile_options;
internal_proto(compile_options);

extern void init_compile_options (void);
internal_proto(init_compile_options);

#define GFC_MAX_SUBRECORD_LENGTH 2147483639   /* 2**31 - 9 */

/* Structure for statement options.  */

typedef struct
{
  const char *name;
  int value;
}
st_option;


/* This is returned by notification_std to know if, given the flags
   that were given (-std=, -pedantic) we should issue an error, a warning
   or nothing.  */
typedef enum
{ NOTIFICATION_SILENT, NOTIFICATION_WARNING, NOTIFICATION_ERROR }
notification;

/* This is returned by notify_std and several io functions.  */
typedef enum
{ SUCCESS = 1, FAILURE }
try;

/* The filename and line number don't go inside the globals structure.
   They are set by the rest of the program and must be linked to.  */

/* Location of the current library call (optional).  */
extern unsigned line;
iexport_data_proto(line);

extern char *filename;
iexport_data_proto(filename);

/* Avoid conflicting prototypes of alloca() in system headers by using 
   GCC's builtin alloca().  */
#define gfc_alloca(x)  __builtin_alloca(x)


/* Directory for creating temporary files.  Only used when none of the
   following environment variables exist: GFORTRAN_TMPDIR, TMP and TEMP.  */
#define DEFAULT_TEMPDIR "/tmp"

/* The default value of record length for preconnected units is defined
   here. This value can be overriden by an environment variable.
   Default value is 1 Gb.  */
#define DEFAULT_RECL 1073741824


#define CHARACTER2(name) \
              gfc_charlen_type name ## _len; \
              char * name

typedef struct st_parameter_common
{
  GFC_INTEGER_4 flags;
  GFC_INTEGER_4 unit;
  const char *filename;
  GFC_INTEGER_4 line;
  CHARACTER2 (iomsg);
  GFC_INTEGER_4 *iostat;
}
st_parameter_common;

#undef CHARACTER2

#define IOPARM_LIBRETURN_MASK           (3 << 0)
#define IOPARM_LIBRETURN_OK             (0 << 0)
#define IOPARM_LIBRETURN_ERROR          (1 << 0)
#define IOPARM_LIBRETURN_END            (2 << 0)
#define IOPARM_LIBRETURN_EOR            (3 << 0)
#define IOPARM_ERR                      (1 << 2)
#define IOPARM_END                      (1 << 3)
#define IOPARM_EOR                      (1 << 4)
#define IOPARM_HAS_IOSTAT               (1 << 5)
#define IOPARM_HAS_IOMSG                (1 << 6)

#define IOPARM_COMMON_MASK              ((1 << 7) - 1)

#define IOPARM_OPEN_HAS_RECL_IN         (1 << 7)
#define IOPARM_OPEN_HAS_FILE            (1 << 8)
#define IOPARM_OPEN_HAS_STATUS          (1 << 9)
#define IOPARM_OPEN_HAS_ACCESS          (1 << 10)
#define IOPARM_OPEN_HAS_FORM            (1 << 11)
#define IOPARM_OPEN_HAS_BLANK           (1 << 12)
#define IOPARM_OPEN_HAS_POSITION        (1 << 13)
#define IOPARM_OPEN_HAS_ACTION          (1 << 14)
#define IOPARM_OPEN_HAS_DELIM           (1 << 15)
#define IOPARM_OPEN_HAS_PAD             (1 << 16)
#define IOPARM_OPEN_HAS_CONVERT         (1 << 17)
#define IOPARM_OPEN_HAS_DECIMAL		(1 << 18)
#define IOPARM_OPEN_HAS_ENCODING	(1 << 19)
#define IOPARM_OPEN_HAS_ROUND		(1 << 20)
#define IOPARM_OPEN_HAS_SIGN		(1 << 21)
#define IOPARM_OPEN_HAS_ASYNCHRONOUS	(1 << 22)
#define IOPARM_OPEN_HAS_NEWUNIT		(1 << 23)

/* library start function and end macro.  These can be expanded if needed
   in the future.  cmp is st_parameter_common *cmp  */

extern void library_start (st_parameter_common *);
internal_proto(library_start);

#define library_end()

/* main.c */

extern void stupid_function_name_for_static_linking (void);
internal_proto(stupid_function_name_for_static_linking);

extern void set_args (int, char **);
iexport_proto(set_args);

extern void get_args (int *, char ***);
internal_proto(get_args);

extern void store_exe_path (const char *);
export_proto(store_exe_path);

extern char * full_exe_path (void);
internal_proto(full_exe_path);

extern void find_addr2line (void);
internal_proto(find_addr2line);

/* backtrace.c */

extern void show_backtrace (void);
internal_proto(show_backtrace);

/* error.c */

#if defined(HAVE_GFC_REAL_16)
#define GFC_LARGEST_BUF (sizeof (GFC_REAL_16))
#elif defined(HAVE_GFC_INTEGER_16)
#define GFC_LARGEST_BUF (sizeof (GFC_INTEGER_LARGEST))
#elif defined(HAVE_GFC_REAL_10)
#define GFC_LARGEST_BUF (sizeof (GFC_REAL_10))
#else
#define GFC_LARGEST_BUF (sizeof (GFC_INTEGER_LARGEST))
#endif

#define GFC_ITOA_BUF_SIZE (sizeof (GFC_INTEGER_LARGEST) * 3 + 2)
#define GFC_XTOA_BUF_SIZE (GFC_LARGEST_BUF * 2 + 1)
#define GFC_OTOA_BUF_SIZE (GFC_LARGEST_BUF * 3 + 1)
#define GFC_BTOA_BUF_SIZE (GFC_LARGEST_BUF * 8 + 1)

extern void sys_abort (void) __attribute__ ((noreturn));
internal_proto(sys_abort);

extern ssize_t estr_write (const char *);
internal_proto(estr_write);

extern int st_vprintf (const char *, va_list);
internal_proto(st_vprintf);

extern int st_printf (const char *, ...)
  __attribute__((format (gfc_printf, 1, 2)));
internal_proto(st_printf);

extern const char *gfc_xtoa (GFC_UINTEGER_LARGEST, char *, size_t);
internal_proto(gfc_xtoa);

extern void os_error (const char *) __attribute__ ((noreturn));
iexport_proto(os_error);

extern void show_locus (st_parameter_common *);
internal_proto(show_locus);

extern void runtime_error (const char *, ...)
     __attribute__ ((noreturn, format (gfc_printf, 1, 2)));
iexport_proto(runtime_error);

extern void runtime_error_at (const char *, const char *, ...)
     __attribute__ ((noreturn, format (gfc_printf, 2, 3)));
iexport_proto(runtime_error_at);

extern void runtime_warning_at (const char *, const char *, ...)
     __attribute__ ((format (gfc_printf, 2, 3)));
iexport_proto(runtime_warning_at);

extern void internal_error (st_parameter_common *, const char *)
  __attribute__ ((noreturn));
internal_proto(internal_error);

extern const char *translate_error (int);
internal_proto(translate_error);

extern void generate_error (st_parameter_common *, int, const char *);
iexport_proto(generate_error);

extern void generate_warning (st_parameter_common *, const char *);
internal_proto(generate_warning);

extern try notify_std (st_parameter_common *, int, const char *);
internal_proto(notify_std);

extern notification notification_std(int);
internal_proto(notification_std);

extern char *gf_strerror (int, char *, size_t);
internal_proto(gf_strerror);

/* fpu.c */

extern void set_fpu (void);
internal_proto(set_fpu);

/* memory.c */

extern void *get_mem (size_t) __attribute__ ((malloc));
internal_proto(get_mem);

extern void *internal_malloc_size (size_t) __attribute__ ((malloc));
internal_proto(internal_malloc_size);

/* environ.c */

extern int check_buffered (int);
internal_proto(check_buffered);

extern void init_variables (void);
internal_proto(init_variables);

extern void show_variables (void);
internal_proto(show_variables);

unit_convert get_unformatted_convert (int);
internal_proto(get_unformatted_convert);

/* string.c */

extern int find_option (st_parameter_common *, const char *, gfc_charlen_type,
			const st_option *, const char *);
internal_proto(find_option);

extern gfc_charlen_type fstrlen (const char *, gfc_charlen_type);
internal_proto(fstrlen);

extern gfc_charlen_type fstrcpy (char *, gfc_charlen_type, const char *, gfc_charlen_type);
internal_proto(fstrcpy);

extern gfc_charlen_type cf_strcpy (char *, gfc_charlen_type, const char *);
internal_proto(cf_strcpy);

extern gfc_charlen_type string_len_trim (gfc_charlen_type, const char *);
export_proto(string_len_trim);

extern gfc_charlen_type string_len_trim_char4 (gfc_charlen_type,
					       const gfc_char4_t *);
export_proto(string_len_trim_char4);

/* io/intrinsics.c */

extern void flush_all_units (void);
internal_proto(flush_all_units);

/* io.c */

extern void init_units (void);
internal_proto(init_units);

extern void close_units (void);
internal_proto(close_units);

extern int unit_to_fd (int);
internal_proto(unit_to_fd);

extern char * filename_from_unit (int);
internal_proto(filename_from_unit);

/* stop.c */

extern void stop_string (const char *, GFC_INTEGER_4)
  __attribute__ ((noreturn));
export_proto(stop_string);

/* reshape_packed.c */

extern void reshape_packed (char *, index_type, const char *, index_type,
			    const char *, index_type);
internal_proto(reshape_packed);

/* Repacking functions.  These are called internally by internal_pack
   and internal_unpack.  */

GFC_INTEGER_1 *internal_pack_1 (gfc_array_i1 *);
internal_proto(internal_pack_1);

GFC_INTEGER_2 *internal_pack_2 (gfc_array_i2 *);
internal_proto(internal_pack_2);

GFC_INTEGER_4 *internal_pack_4 (gfc_array_i4 *);
internal_proto(internal_pack_4);

GFC_INTEGER_8 *internal_pack_8 (gfc_array_i8 *);
internal_proto(internal_pack_8);

#if defined HAVE_GFC_INTEGER_16
GFC_INTEGER_16 *internal_pack_16 (gfc_array_i16 *);
internal_proto(internal_pack_16);
#endif

GFC_REAL_4 *internal_pack_r4 (gfc_array_r4 *);
internal_proto(internal_pack_r4);

GFC_REAL_8 *internal_pack_r8 (gfc_array_r8 *);
internal_proto(internal_pack_r8);

#if defined HAVE_GFC_REAL_10
GFC_REAL_10 *internal_pack_r10 (gfc_array_r10 *);
internal_proto(internal_pack_r10);
#endif

#if defined HAVE_GFC_REAL_16
GFC_REAL_16 *internal_pack_r16 (gfc_array_r16 *);
internal_proto(internal_pack_r16);
#endif

GFC_COMPLEX_4 *internal_pack_c4 (gfc_array_c4 *);
internal_proto(internal_pack_c4);

GFC_COMPLEX_8 *internal_pack_c8 (gfc_array_c8 *);
internal_proto(internal_pack_c8);

#if defined HAVE_GFC_COMPLEX_10
GFC_COMPLEX_10 *internal_pack_c10 (gfc_array_c10 *);
internal_proto(internal_pack_c10);
#endif

#if defined HAVE_GFC_COMPLEX_16
GFC_COMPLEX_16 *internal_pack_c16 (gfc_array_c16 *);
internal_proto(internal_pack_c16);
#endif

extern void internal_unpack_1 (gfc_array_i1 *, const GFC_INTEGER_1 *);
internal_proto(internal_unpack_1);

extern void internal_unpack_2 (gfc_array_i2 *, const GFC_INTEGER_2 *);
internal_proto(internal_unpack_2);

extern void internal_unpack_4 (gfc_array_i4 *, const GFC_INTEGER_4 *);
internal_proto(internal_unpack_4);

extern void internal_unpack_8 (gfc_array_i8 *, const GFC_INTEGER_8 *);
internal_proto(internal_unpack_8);

#if defined HAVE_GFC_INTEGER_16
extern void internal_unpack_16 (gfc_array_i16 *, const GFC_INTEGER_16 *);
internal_proto(internal_unpack_16);
#endif

extern void internal_unpack_r4 (gfc_array_r4 *, const GFC_REAL_4 *);
internal_proto(internal_unpack_r4);

extern void internal_unpack_r8 (gfc_array_r8 *, const GFC_REAL_8 *);
internal_proto(internal_unpack_r8);

#if defined HAVE_GFC_REAL_10
extern void internal_unpack_r10 (gfc_array_r10 *, const GFC_REAL_10 *);
internal_proto(internal_unpack_r10);
#endif

#if defined HAVE_GFC_REAL_16
extern void internal_unpack_r16 (gfc_array_r16 *, const GFC_REAL_16 *);
internal_proto(internal_unpack_r16);
#endif

extern void internal_unpack_c4 (gfc_array_c4 *, const GFC_COMPLEX_4 *);
internal_proto(internal_unpack_c4);

extern void internal_unpack_c8 (gfc_array_c8 *, const GFC_COMPLEX_8 *);
internal_proto(internal_unpack_c8);

#if defined HAVE_GFC_COMPLEX_10
extern void internal_unpack_c10 (gfc_array_c10 *, const GFC_COMPLEX_10 *);
internal_proto(internal_unpack_c10);
#endif

#if defined HAVE_GFC_COMPLEX_16
extern void internal_unpack_c16 (gfc_array_c16 *, const GFC_COMPLEX_16 *);
internal_proto(internal_unpack_c16);
#endif

/* Internal auxiliary functions for the pack intrinsic.  */

extern void pack_i1 (gfc_array_i1 *, const gfc_array_i1 *,
		     const gfc_array_l1 *, const gfc_array_i1 *);
internal_proto(pack_i1);

extern void pack_i2 (gfc_array_i2 *, const gfc_array_i2 *,
		     const gfc_array_l1 *, const gfc_array_i2 *);
internal_proto(pack_i2);

extern void pack_i4 (gfc_array_i4 *, const gfc_array_i4 *,
		     const gfc_array_l1 *, const gfc_array_i4 *);
internal_proto(pack_i4);

extern void pack_i8 (gfc_array_i8 *, const gfc_array_i8 *,
		     const gfc_array_l1 *, const gfc_array_i8 *);
internal_proto(pack_i8);

#ifdef HAVE_GFC_INTEGER_16
extern void pack_i16 (gfc_array_i16 *, const gfc_array_i16 *,
		     const gfc_array_l1 *, const gfc_array_i16 *);
internal_proto(pack_i16);
#endif

extern void pack_r4 (gfc_array_r4 *, const gfc_array_r4 *,
		     const gfc_array_l1 *, const gfc_array_r4 *);
internal_proto(pack_r4);

extern void pack_r8 (gfc_array_r8 *, const gfc_array_r8 *,
		     const gfc_array_l1 *, const gfc_array_r8 *);
internal_proto(pack_r8);

#ifdef HAVE_GFC_REAL_10
extern void pack_r10 (gfc_array_r10 *, const gfc_array_r10 *,
		     const gfc_array_l1 *, const gfc_array_r10 *);
internal_proto(pack_r10);
#endif

#ifdef HAVE_GFC_REAL_16
extern void pack_r16 (gfc_array_r16 *, const gfc_array_r16 *,
		     const gfc_array_l1 *, const gfc_array_r16 *);
internal_proto(pack_r16);
#endif

extern void pack_c4 (gfc_array_c4 *, const gfc_array_c4 *,
		     const gfc_array_l1 *, const gfc_array_c4 *);
internal_proto(pack_c4);

extern void pack_c8 (gfc_array_c8 *, const gfc_array_c8 *,
		     const gfc_array_l1 *, const gfc_array_c8 *);
internal_proto(pack_c8);

#ifdef HAVE_GFC_REAL_10
extern void pack_c10 (gfc_array_c10 *, const gfc_array_c10 *,
		     const gfc_array_l1 *, const gfc_array_c10 *);
internal_proto(pack_c10);
#endif

#ifdef HAVE_GFC_REAL_16
extern void pack_c16 (gfc_array_c16 *, const gfc_array_c16 *,
		     const gfc_array_l1 *, const gfc_array_c16 *);
internal_proto(pack_c16);
#endif

/* Internal auxiliary functions for the unpack intrinsic.  */

extern void unpack0_i1 (gfc_array_i1 *, const gfc_array_i1 *,
			const gfc_array_l1 *, const GFC_INTEGER_1 *);
internal_proto(unpack0_i1);

extern void unpack0_i2 (gfc_array_i2 *, const gfc_array_i2 *,
			const gfc_array_l1 *, const GFC_INTEGER_2 *);
internal_proto(unpack0_i2);

extern void unpack0_i4 (gfc_array_i4 *, const gfc_array_i4 *,
			const gfc_array_l1 *, const GFC_INTEGER_4 *);
internal_proto(unpack0_i4);

extern void unpack0_i8 (gfc_array_i8 *, const gfc_array_i8 *,
			const gfc_array_l1 *, const GFC_INTEGER_8 *);
internal_proto(unpack0_i8);

#ifdef HAVE_GFC_INTEGER_16

extern void unpack0_i16 (gfc_array_i16 *, const gfc_array_i16 *,
			 const gfc_array_l1 *, const GFC_INTEGER_16 *);
internal_proto(unpack0_i16);

#endif

extern void unpack0_r4 (gfc_array_r4 *, const gfc_array_r4 *,
			const gfc_array_l1 *, const GFC_REAL_4 *);
internal_proto(unpack0_r4);

extern void unpack0_r8 (gfc_array_r8 *, const gfc_array_r8 *,
			const gfc_array_l1 *, const GFC_REAL_8 *);
internal_proto(unpack0_r8);

#ifdef HAVE_GFC_REAL_10

extern void unpack0_r10 (gfc_array_r10 *, const gfc_array_r10 *,
			 const gfc_array_l1 *, const GFC_REAL_10 *);
internal_proto(unpack0_r10);

#endif

#ifdef HAVE_GFC_REAL_16

extern void unpack0_r16 (gfc_array_r16 *, const gfc_array_r16 *,
			 const gfc_array_l1 *, const GFC_REAL_16 *);
internal_proto(unpack0_r16);

#endif

extern void unpack0_c4 (gfc_array_c4 *, const gfc_array_c4 *,
			const gfc_array_l1 *, const GFC_COMPLEX_4 *);
internal_proto(unpack0_c4);

extern void unpack0_c8 (gfc_array_c8 *, const gfc_array_c8 *,
			const gfc_array_l1 *, const GFC_COMPLEX_8 *);
internal_proto(unpack0_c8);

#ifdef HAVE_GFC_COMPLEX_10

extern void unpack0_c10 (gfc_array_c10 *, const gfc_array_c10 *,
			 const gfc_array_l1 *mask, const GFC_COMPLEX_10 *);
internal_proto(unpack0_c10);

#endif

#ifdef HAVE_GFC_COMPLEX_16

extern void unpack0_c16 (gfc_array_c16 *, const gfc_array_c16 *,
			 const gfc_array_l1 *, const GFC_COMPLEX_16 *);
internal_proto(unpack0_c16);

#endif

extern void unpack1_i1 (gfc_array_i1 *, const gfc_array_i1 *,
			const gfc_array_l1 *, const gfc_array_i1 *);
internal_proto(unpack1_i1);

extern void unpack1_i2 (gfc_array_i2 *, const gfc_array_i2 *,
			const gfc_array_l1 *, const gfc_array_i2 *);
internal_proto(unpack1_i2);

extern void unpack1_i4 (gfc_array_i4 *, const gfc_array_i4 *,
			const gfc_array_l1 *, const gfc_array_i4 *);
internal_proto(unpack1_i4);

extern void unpack1_i8 (gfc_array_i8 *, const gfc_array_i8 *,
			const gfc_array_l1 *, const gfc_array_i8 *);
internal_proto(unpack1_i8);

#ifdef HAVE_GFC_INTEGER_16
extern void unpack1_i16 (gfc_array_i16 *, const gfc_array_i16 *,
			 const gfc_array_l1 *, const gfc_array_i16 *);
internal_proto(unpack1_i16);
#endif

extern void unpack1_r4 (gfc_array_r4 *, const gfc_array_r4 *,
			const gfc_array_l1 *, const gfc_array_r4 *);
internal_proto(unpack1_r4);

extern void unpack1_r8 (gfc_array_r8 *, const gfc_array_r8 *,
			const gfc_array_l1 *, const gfc_array_r8 *);
internal_proto(unpack1_r8);

#ifdef HAVE_GFC_REAL_10
extern void unpack1_r10 (gfc_array_r10 *, const gfc_array_r10 *,
			 const gfc_array_l1 *, const gfc_array_r10 *);
internal_proto(unpack1_r10);
#endif

#ifdef HAVE_GFC_REAL_16
extern void unpack1_r16 (gfc_array_r16 *, const gfc_array_r16 *,
			 const gfc_array_l1 *, const gfc_array_r16 *);
internal_proto(unpack1_r16);
#endif

extern void unpack1_c4 (gfc_array_c4 *, const gfc_array_c4 *,
			const gfc_array_l1 *, const gfc_array_c4 *);
internal_proto(unpack1_c4);

extern void unpack1_c8 (gfc_array_c8 *, const gfc_array_c8 *,
			const gfc_array_l1 *, const gfc_array_c8 *);
internal_proto(unpack1_c8);

#ifdef HAVE_GFC_COMPLEX_10
extern void unpack1_c10 (gfc_array_c10 *, const gfc_array_c10 *,
			 const gfc_array_l1 *, const gfc_array_c10 *);
internal_proto(unpack1_c10);
#endif

#ifdef HAVE_GFC_COMPLEX_16
extern void unpack1_c16 (gfc_array_c16 *, const gfc_array_c16 *,
			 const gfc_array_l1 *, const gfc_array_c16 *);
internal_proto(unpack1_c16);
#endif

/* Helper functions for spread.  */

extern void spread_i1 (gfc_array_i1 *, const gfc_array_i1 *,
		       const index_type, const index_type);
internal_proto(spread_i1);

extern void spread_i2 (gfc_array_i2 *, const gfc_array_i2 *,
		       const index_type, const index_type);
internal_proto(spread_i2);

extern void spread_i4 (gfc_array_i4 *, const gfc_array_i4 *,
		       const index_type, const index_type);
internal_proto(spread_i4);

extern void spread_i8 (gfc_array_i8 *, const gfc_array_i8 *,
		       const index_type, const index_type);
internal_proto(spread_i8);

#ifdef HAVE_GFC_INTEGER_16
extern void spread_i16 (gfc_array_i16 *, const gfc_array_i16 *,
		       const index_type, const index_type);
internal_proto(spread_i16);

#endif

extern void spread_r4 (gfc_array_r4 *, const gfc_array_r4 *,
		       const index_type, const index_type);
internal_proto(spread_r4);

extern void spread_r8 (gfc_array_r8 *, const gfc_array_r8 *,
		       const index_type, const index_type);
internal_proto(spread_r8);

#ifdef HAVE_GFC_REAL_10
extern void spread_r10 (gfc_array_r10 *, const gfc_array_r10 *,
		       const index_type, const index_type);
internal_proto(spread_r10);

#endif

#ifdef HAVE_GFC_REAL_16
extern void spread_r16 (gfc_array_r16 *, const gfc_array_r16 *,
		       const index_type, const index_type);
internal_proto(spread_r16);

#endif

extern void spread_c4 (gfc_array_c4 *, const gfc_array_c4 *,
		       const index_type, const index_type);
internal_proto(spread_c4);

extern void spread_c8 (gfc_array_c8 *, const gfc_array_c8 *,
		       const index_type, const index_type);
internal_proto(spread_c8);

#ifdef HAVE_GFC_COMPLEX_10
extern void spread_c10 (gfc_array_c10 *, const gfc_array_c10 *,
		       const index_type, const index_type);
internal_proto(spread_c10);

#endif

#ifdef HAVE_GFC_COMPLEX_16
extern void spread_c16 (gfc_array_c16 *, const gfc_array_c16 *,
		       const index_type, const index_type);
internal_proto(spread_c16);

#endif

extern void spread_scalar_i1 (gfc_array_i1 *, const GFC_INTEGER_1 *,
			      const index_type, const index_type);
internal_proto(spread_scalar_i1);

extern void spread_scalar_i2 (gfc_array_i2 *, const GFC_INTEGER_2 *,
			      const index_type, const index_type);
internal_proto(spread_scalar_i2);

extern void spread_scalar_i4 (gfc_array_i4 *, const GFC_INTEGER_4 *,
			      const index_type, const index_type);
internal_proto(spread_scalar_i4);

extern void spread_scalar_i8 (gfc_array_i8 *, const GFC_INTEGER_8 *,
			      const index_type, const index_type);
internal_proto(spread_scalar_i8);

#ifdef HAVE_GFC_INTEGER_16
extern void spread_scalar_i16 (gfc_array_i16 *, const GFC_INTEGER_16 *,
			       const index_type, const index_type);
internal_proto(spread_scalar_i16);

#endif

extern void spread_scalar_r4 (gfc_array_r4 *, const GFC_REAL_4 *,
			      const index_type, const index_type);
internal_proto(spread_scalar_r4);

extern void spread_scalar_r8 (gfc_array_r8 *, const GFC_REAL_8 *,
			      const index_type, const index_type);
internal_proto(spread_scalar_r8);

#ifdef HAVE_GFC_REAL_10
extern void spread_scalar_r10 (gfc_array_r10 *, const GFC_REAL_10 *,
			       const index_type, const index_type);
internal_proto(spread_scalar_r10);

#endif

#ifdef HAVE_GFC_REAL_16
extern void spread_scalar_r16 (gfc_array_r16 *, const GFC_REAL_16 *,
			       const index_type, const index_type);
internal_proto(spread_scalar_r16);

#endif

extern void spread_scalar_c4 (gfc_array_c4 *, const GFC_COMPLEX_4 *,
			      const index_type, const index_type);
internal_proto(spread_scalar_c4);

extern void spread_scalar_c8 (gfc_array_c8 *, const GFC_COMPLEX_8 *,
			      const index_type, const index_type);
internal_proto(spread_scalar_c8);

#ifdef HAVE_GFC_COMPLEX_10
extern void spread_scalar_c10 (gfc_array_c10 *, const GFC_COMPLEX_10 *,
			       const index_type, const index_type);
internal_proto(spread_scalar_c10);

#endif

#ifdef HAVE_GFC_COMPLEX_16
extern void spread_scalar_c16 (gfc_array_c16 *, const GFC_COMPLEX_16 *,
			       const index_type, const index_type);
internal_proto(spread_scalar_c16);

#endif

/* string_intrinsics.c */

extern int compare_string (gfc_charlen_type, const char *,
			   gfc_charlen_type, const char *);
iexport_proto(compare_string);

extern int compare_string_char4 (gfc_charlen_type, const gfc_char4_t *,
				 gfc_charlen_type, const gfc_char4_t *);
iexport_proto(compare_string_char4);

extern int memcmp_char4 (const void *, const void *, size_t);
internal_proto(memcmp_char4);


/* random.c */

extern void random_seed_i4 (GFC_INTEGER_4 * size, gfc_array_i4 * put,
			    gfc_array_i4 * get);
iexport_proto(random_seed_i4);
extern void random_seed_i8 (GFC_INTEGER_8 * size, gfc_array_i8 * put,
			    gfc_array_i8 * get);
iexport_proto(random_seed_i8);

/* size.c */

typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, void) array_t;

extern index_type size0 (const array_t * array); 
iexport_proto(size0);

/* bounds.c */

extern void bounds_equal_extents (array_t *, array_t *, const char *,
				  const char *);
internal_proto(bounds_equal_extents);

extern void bounds_reduced_extents (array_t *, array_t *, int, const char *,
			     const char *intrinsic);
internal_proto(bounds_reduced_extents);

extern void bounds_iforeach_return (array_t *, array_t *, const char *);
internal_proto(bounds_iforeach_return);

extern void bounds_ifunction_return (array_t *, const index_type *,
				     const char *, const char *);
internal_proto(bounds_ifunction_return);

extern index_type count_0 (const gfc_array_l1 *);

internal_proto(count_0);

/* Internal auxiliary functions for cshift */

void cshift0_i1 (gfc_array_i1 *, const gfc_array_i1 *, ptrdiff_t, int);
internal_proto(cshift0_i1);

void cshift0_i2 (gfc_array_i2 *, const gfc_array_i2 *, ptrdiff_t, int);
internal_proto(cshift0_i2);

void cshift0_i4 (gfc_array_i4 *, const gfc_array_i4 *, ptrdiff_t, int);
internal_proto(cshift0_i4);

void cshift0_i8 (gfc_array_i8 *, const gfc_array_i8 *, ptrdiff_t, int);
internal_proto(cshift0_i8);

#ifdef HAVE_GFC_INTEGER_16
void cshift0_i16 (gfc_array_i16 *, const gfc_array_i16 *, ptrdiff_t, int);
internal_proto(cshift0_i16);
#endif

void cshift0_r4 (gfc_array_r4 *, const gfc_array_r4 *, ptrdiff_t, int);
internal_proto(cshift0_r4);

void cshift0_r8 (gfc_array_r8 *, const gfc_array_r8 *, ptrdiff_t, int);
internal_proto(cshift0_r8);

#ifdef HAVE_GFC_REAL_10
void cshift0_r10 (gfc_array_r10 *, const gfc_array_r10 *, ptrdiff_t, int);
internal_proto(cshift0_r10);
#endif

#ifdef HAVE_GFC_REAL_16
void cshift0_r16 (gfc_array_r16 *, const gfc_array_r16 *, ptrdiff_t, int);
internal_proto(cshift0_r16);
#endif

void cshift0_c4 (gfc_array_c4 *, const gfc_array_c4 *, ptrdiff_t, int);
internal_proto(cshift0_c4);

void cshift0_c8 (gfc_array_c8 *, const gfc_array_c8 *, ptrdiff_t, int);
internal_proto(cshift0_c8);

#ifdef HAVE_GFC_COMPLEX_10
void cshift0_c10 (gfc_array_c10 *, const gfc_array_c10 *, ptrdiff_t, int);
internal_proto(cshift0_c10);
#endif

#ifdef HAVE_GFC_COMPLEX_16
void cshift0_c16 (gfc_array_c16 *, const gfc_array_c16 *, ptrdiff_t, int);
internal_proto(cshift0_c16);
#endif

#endif  /* LIBGFOR_H  */
