/* Common declarations for all of libgfor.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>, and
   Andy Vaught <andy@xena.eas.asu.edu>

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfor is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfor is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#ifndef LIBGFOR_H
#define LIBGFOR_H

#include <math.h>
#include <stddef.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif

#include "config.h"

#if HAVE_COMPLEX_H
# include <complex.h>
#else
#define complex __complex__
#endif

#if HAVE_STDINT_H
#include <stdint.h>
#endif

#if HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
typedef off_t gfc_offset;

#ifndef NULL
#define NULL (void *) 0
#endif

#ifndef __GNUC__
#define __attribute__(x)
#endif

/* For a library, a standard prefix is a requirement in order to
   partition the namespace.  It's ugly to look at and a pain to type,
   so we hide it behind macros.  */
#define prefix(x) _gfortran_ ## x

/* The only reliable way to get the offset of a field in a struct
   in a system independent way is via this macro.  */
#ifndef offsetof
#define offsetof(TYPE, MEMBER)  ((size_t) &((TYPE *) 0)->MEMBER)
#endif

/* TODO: find the C99 version of these an move into above ifdef.  */
#define REALPART(z) (__real__(z))
#define IMAGPART(z) (__imag__(z))
#define COMPLEX_ASSIGN(z_, r_, i_) {__real__(z_) = (r_); __imag__(z_) = (i_);}

typedef int32_t GFC_INTEGER_4;
typedef int64_t GFC_INTEGER_8;
typedef uint32_t GFC_UINTEGER_4;
typedef uint64_t GFC_UINTEGER_8;
typedef GFC_INTEGER_4 GFC_LOGICAL_4;
typedef GFC_INTEGER_8 GFC_LOGICAL_8;
typedef float GFC_REAL_4;
typedef double GFC_REAL_8;
typedef complex float GFC_COMPLEX_4;
typedef complex double GFC_COMPLEX_8;

/* The following two definitions must be consistent with the types used
   by the compiler.  */
/* The type used of array indices, amongst other things.  */
typedef size_t index_type;
/* The type used for the lengths of character variables.  */
typedef GFC_INTEGER_4 gfc_charlen_type;

/* This will be 0 on little-endian machines and one on big-endian machines.  */
#define l8_to_l4_offset prefix(l8_to_l4_offset)
extern int l8_to_l4_offset;

#define GFOR_POINTER_L8_TO_L4(p8) \
  (l8_to_l4_offset + (GFC_LOGICAL_4 *)(p8))

#define GFC_INTEGER_4_HUGE \
  (GFC_INTEGER_4)((((GFC_UINTEGER_4)1) << 31) - 1)
#define GFC_INTEGER_8_HUGE \
  (GFC_INTEGER_8)((((GFC_UINTEGER_8)1) << 63) - 1)
#define GFC_REAL_4_HUGE FLT_MAX
#define GFC_REAL_8_HUGE DBL_MAX

#ifndef GFC_MAX_DIMENSIONS
#define GFC_MAX_DIMENSIONS 7
#endif

typedef struct descriptor_dimension
{
  index_type stride;
  index_type lbound;
  index_type ubound;
}
descriptor_dimension;

#define GFC_ARRAY_DESCRIPTOR(r, type) \
struct {\
  type *data;\
  type *base;\
  index_type dtype;\
  descriptor_dimension dim[r];\
}

/* Commonly used array descriptor types.  */
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, void) gfc_array_void;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, char) gfc_array_char;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_INTEGER_4) gfc_array_i4;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_INTEGER_8) gfc_array_i8;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_REAL_4) gfc_array_r4;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_REAL_8) gfc_array_r8;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_COMPLEX_4) gfc_array_c4;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_COMPLEX_8) gfc_array_c8;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_LOGICAL_4) gfc_array_l4;
typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, GFC_LOGICAL_8) gfc_array_l8;

#define GFC_DTYPE_RANK_MASK 0x07
#define GFC_DTYPE_TYPE_SHIFT 3
#define GFC_DTYPE_TYPE_MASK 0x38
#define GFC_DTYPE_SIZE_SHIFT 6

enum
{
  GFC_DTYPE_UNKNOWN = 0,
  GFC_DTYPE_INTEGER,
  /* TODO: recognize logical types.  */
  GFC_DTYPE_LOGICAL,
  GFC_DTYPE_REAL,
  GFC_DTYPE_COMPLEX,
  GFC_DTYPE_DERIVED,
  GFC_DTYPE_CHARACTER
};

#define GFC_DESCRIPTOR_RANK(desc) ((desc)->dtype & GFC_DTYPE_RANK_MASK)
#define GFC_DESCRIPTOR_TYPE(desc) (((desc)->dtype & GFC_DTYPE_TYPE_MASK) \
                                   >> GFC_DTYPE_TYPE_SHIFT)
#define GFC_DESCRIPTOR_SIZE(desc) ((desc)->dtype >> GFC_DTYPE_SIZE_SHIFT)
#define GFC_DESCRIPTOR_DATA(desc) ((desc)->data)
#define GFC_DESCRIPTOR_DTYPE(desc) ((desc)->dtype)

/* Runtime library include.  */
#define stringize(x) expand_macro(x)
#define expand_macro(x) # x

/* Runtime options structure.  */

typedef struct
{
  int stdin_unit, stdout_unit, optional_plus;
  int allocate_init_flag, allocate_init_value;
  int locus;

  int separator_len;
  const char *separator;

  int mem_check;
  int use_stderr, all_unbuffered, default_recl;

  int fpu_round, fpu_precision, fpu_invalid, fpu_denormal, fpu_zerodiv,
    fpu_overflow, fpu_underflow, fpu_precision_loss;

  int sighup, sigint;
}
options_t;


#define options prefix(options)
extern options_t options;


/* Structure for statement options.  */

typedef struct
{
  const char *name;
  int value;
}
st_option;

/* Runtime errors.  The EOR and EOF errors are required to be negative.  */

typedef enum
{
  ERROR_FIRST = -3,		/* Marker for the first error.  */
  ERROR_EOR = -2,
  ERROR_END = -1,
  ERROR_OK = 0,			/* Indicates success, must be zero.  */
  ERROR_OS,			/* Operating system error, more info in errno.  */
  ERROR_OPTION_CONFLICT,
  ERROR_BAD_OPTION,
  ERROR_MISSING_OPTION,
  ERROR_ALREADY_OPEN,
  ERROR_BAD_UNIT,
  ERROR_FORMAT,
  ERROR_BAD_ACTION,
  ERROR_ENDFILE,
  ERROR_BAD_US,
  ERROR_READ_VALUE,
  ERROR_READ_OVERFLOW,
  ERROR_LAST			/* Not a real error, the last error # + 1.  */
}
error_codes;


/* The filename and line number don't go inside the globals structure.
   They are set by the rest of the program and must be linked to.  */

#define line prefix(line)
extern unsigned line;		/* Location of the current libray call (optional).  */

#define filename prefix(filename)
extern char *filename;

/* Avoid conflicting prototypes of alloca() in system headers by using 
   GCC's builtin alloca().  */

#define gfc_alloca(x)  __builtin_alloca(x)


/* main.c */

#define library_start prefix(library_start)
void library_start (void);

#define library_end prefix(library_end)
void library_end (void);

#define set_args prefix(set_args)
void set_args (int, char **);

#define get_args prefix(get_args)
void get_args (int *, char ***);


/* error.c */
#define itoa prefix(itoa)
char *itoa (int64_t);

#define xtoa prefix(xtoa)
char *xtoa (uint64_t);

#define os_error prefix(os_error)
void os_error (const char *) __attribute__ ((noreturn));

#define show_locus prefix(show_locus)
void show_locus (void);

#define runtime_error prefix(runtime_error)
void runtime_error (const char *) __attribute__ ((noreturn));

#define internal_error prefix(internal_error)
void internal_error (const char *) __attribute__ ((noreturn));

#define get_oserror prefix(get_oserror)
const char *get_oserror (void);

#define write_error prefix(write_error)
void write_error (const char *);

#define sys_exit prefix(sys_exit)
void sys_exit (int) __attribute__ ((noreturn));

#define st_printf prefix(st_printf)
int st_printf (const char *, ...) __attribute__ ((format (printf, 1, 2)));

#define st_sprintf prefix(st_sprintf)
void st_sprintf (char *, const char *, ...) __attribute__ ((format (printf, 2, 3)));

#define translate_error prefix(translate_error)
const char *translate_error (int);

#define generate_error prefix(generate_error)
void generate_error (int, const char *);


/* memory.c */

#define memory_init	prefix(memory_init)
void memory_init (void);

#define runtime_cleanup	prefix(runtime_cleanup)
void runtime_cleanup (void);

#define get_mem		prefix(get_mem)
void *get_mem (size_t) __attribute__ ((malloc));

#define free_mem	prefix(free_mem)
void free_mem (void *);

#define internal_malloc_size	prefix(internal_malloc_size)
void *internal_malloc_size (size_t);

#define internal_malloc	prefix(internal_malloc)
void *internal_malloc (GFC_INTEGER_4);

#define internal_malloc64 prefix(internal_malloc64)
void *internal_malloc64 (GFC_INTEGER_8);

#define internal_free	prefix(internal_free)
void internal_free (void *);

#define allocate	prefix(allocate)
void allocate (void **, GFC_INTEGER_4, GFC_INTEGER_4 *);

#define allocate64	prefix(allocate64)
void allocate64 (void **, GFC_INTEGER_8, GFC_INTEGER_4 *);

#define deallocate	prefix(deallocate)
void deallocate (void **, GFC_INTEGER_4 *);


/* environ.c */

#define check_buffered prefix(check_buffered)
int check_buffered (int);

#define init_variables prefix(init_variables)
void init_variables (void);

#define show_variables prefix(show_variables)
void show_variables (void);


/* string.c */

#define find_option prefix(find_option)
int find_option (const char *, int, st_option *, const char *);

#define fstrlen prefix(fstrlen)
int fstrlen (const char *, int);

#define fstrcpy prefix(fstrcpy)
void fstrcpy (char *, int, const char *, int);

#define cf_strcpy prefix(cf_strcpy)
void cf_strcpy (char *, int, const char *);

/* io.c */

#define init_units prefix(init_units)
void init_units (void);

#define close_units prefix(close_units)
void close_units (void);

/* stop.c */
#define stop_numeric prefix(stop_numeric)
void stop_numeric (GFC_INTEGER_4);

/* reshape_packed.c */
#define reshape_packed prefix(reshape_packed)
void reshape_packed (char *, index_type, const char *, index_type,
		     const char *, index_type);

/* Repacking functions.  */
#define internal_pack prefix(internal_pack)
void *internal_pack (gfc_array_char *);

#define internal_unpack prefix(internal_unpack)
void internal_unpack (gfc_array_char *, const void *);

#define internal_pack_4 prefix(internal_pack_4)
GFC_INTEGER_4 *internal_pack_4 (gfc_array_i4 *);

#define internal_pack_8 prefix(internal_pack_8)
GFC_INTEGER_8 *internal_pack_8 (gfc_array_i8 *);

#define internal_unpack_4 prefix(internal_unpack_4)
void internal_unpack_4 (gfc_array_i4 *, const GFC_INTEGER_4 *);

#define internal_unpack_8 prefix(internal_unpack_8)
void internal_unpack_8 (gfc_array_i8 *, const GFC_INTEGER_8 *);

/* date_and_time.c */

#define date_and_time prefix(date_and_time)
void date_and_time (char *, char *, char *, gfc_array_i4 *,
                   GFC_INTEGER_4, GFC_INTEGER_4, GFC_INTEGER_4);

/* string_intrinsics.c */

#define compare_string prefix(compare_string)
GFC_INTEGER_4 compare_string (GFC_INTEGER_4, const char *,
			      GFC_INTEGER_4, const char *);

/* random.c */

#define random_seed prefix(random_seed)
void random_seed (GFC_INTEGER_4 * size, gfc_array_i4 * put,
		  gfc_array_i4 * get);

/* normalize.c */

#define normalize_r4_i4 prefix(normalize_r4_i4)
GFC_REAL_4 normalize_r4_i4 (GFC_UINTEGER_4, GFC_UINTEGER_4);

#define normalize_r8_i8 prefix(normalize_r8_i8)
GFC_REAL_8 normalize_r8_i8 (GFC_UINTEGER_8, GFC_UINTEGER_8);

/* size.c */

typedef GFC_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, void) array_t;

#define size0 prefix(size0)
index_type size0 (const array_t * array); 

#endif

