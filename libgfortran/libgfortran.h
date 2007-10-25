/* Common declarations for all of libgfortran.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>, and
   Andy Vaught <andy@xena.eas.asu.edu>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


#ifndef LIBGFOR_H
#define LIBGFOR_H

/* config.h MUST be first because it can affect system headers.  */
#include "config.h"

#include <stdio.h>
#include <math.h>
#include <stddef.h>
#include <float.h>
#include <stdarg.h>

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
typedef off_t gfc_offset;

#ifndef NULL
#define NULL (void *) 0
#endif

#ifndef __GNUC__
#define __attribute__(x)
#endif


/* On mingw, work around the buggy Windows snprintf() by using the one
   mingw provides, __mingw_snprintf().  We also provide a prototype for
   __mingw_snprintf(), because the mingw headers currently don't have one.  */
#if HAVE_MINGW_SNPRINTF
extern int __mingw_snprintf (char *, size_t, const char *, ...);
#undef snprintf
#define snprintf __mingw_snprintf
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
/* ??? We're not currently building a dll, and it's wrong to add dllexport
   to objects going into a static library archive.  */
#elif 0 && defined(HAVE_ATTRIBUTE_DLLEXPORT)
# define export_proto_np(x)	extern __typeof(x) x __attribute__((dllexport))
# define export_proto(x)    sym_rename(x, PREFIX(x)) __attribute__((dllexport))
# define iexport_proto(x)	export_proto(x)
# define iexport(x)		extern char swallow_semicolon
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

/* The isfinite macro is only available with C99, but some non-C99
   systems still provide fpclassify, and there is a `finite' function
   in BSD.

   Also, isfinite is broken on Cygwin.

   When isfinite is not available, try to use one of the
   alternatives, or bail out.  */

#if defined(HAVE_BROKEN_ISFINITE) || defined(__CYGWIN__)
#undef isfinite
#endif

#if defined(HAVE_BROKEN_ISNAN)
#undef isnan
#endif

#if defined(HAVE_BROKEN_FPCLASSIFY)
#undef fpclassify
#endif

#if !defined(isfinite)
#if !defined(fpclassify)
#define isfinite(x) ((x) - (x) == 0)
#else
#define isfinite(x) (fpclassify(x) != FP_NAN && fpclassify(x) != FP_INFINITE)
#endif /* !defined(fpclassify) */
#endif /* !defined(isfinite)  */

#if !defined(isnan)
#if !defined(fpclassify)
#define isnan(x) ((x) != (x))
#else
#define isnan(x) (fpclassify(x) == FP_NAN)
#endif /* !defined(fpclassify) */
#endif /* !defined(isfinite)  */

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
typedef ssize_t index_type;
/* The type used for the lengths of character variables.  */
typedef GFC_INTEGER_4 gfc_charlen_type;

/* This will be 0 on little-endian machines and one on big-endian machines.  */
extern int l8_to_l4_offset;
internal_proto(l8_to_l4_offset);

#define GFOR_POINTER_TO_L1(p, kind) \
  (l8_to_l4_offset * (kind - 1) + (GFC_LOGICAL_1 *)(p))

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

  int use_stderr, all_unbuffered, unbuffered_preconnected, default_recl;
  int fpe, dump_core, backtrace;
}
options_t;

extern options_t options;
internal_proto(options);

extern void handler (int);
internal_proto(handler);


/* Compile-time options that will influence the library.  */

typedef struct
{
  int warn_std;
  int allow_std;
  int pedantic;
  int convert;
  int dump_core;
  int backtrace;
  int sign_zero;
  size_t record_marker;
  int max_subrecord_length;
  int bounds_check;
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
{ SILENT, WARNING, ERROR }
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

/* library start function and end macro.  These can be expanded if needed
   in the future.  cmp is st_parameter_common *cmp  */

extern void library_start (st_parameter_common *);
internal_proto(library_start);

#define library_end()

/* main.c */

extern void stupid_function_name_for_static_linking (void);
internal_proto(stupid_function_name_for_static_linking);

extern void set_args (int, char **);
export_proto(set_args);

extern void get_args (int *, char ***);
internal_proto(get_args);

extern void store_exe_path (const char *);
export_proto(store_exe_path);

extern char * full_exe_path (void);
internal_proto(full_exe_path);

/* backtrace.c */

extern void show_backtrace (void);
internal_proto(show_backtrace);

/* error.c */

#define GFC_ITOA_BUF_SIZE (sizeof (GFC_INTEGER_LARGEST) * 3 + 2)
#define GFC_XTOA_BUF_SIZE (sizeof (GFC_UINTEGER_LARGEST) * 2 + 1)
#define GFC_OTOA_BUF_SIZE (sizeof (GFC_INTEGER_LARGEST) * 3 + 1)
#define GFC_BTOA_BUF_SIZE (sizeof (GFC_INTEGER_LARGEST) * 8 + 1)

extern void sys_exit (int) __attribute__ ((noreturn));
internal_proto(sys_exit);

extern const char *gfc_itoa (GFC_INTEGER_LARGEST, char *, size_t);
internal_proto(gfc_itoa);

extern const char *xtoa (GFC_UINTEGER_LARGEST, char *, size_t);
internal_proto(xtoa);

extern void os_error (const char *) __attribute__ ((noreturn));
iexport_proto(os_error);

extern void show_locus (st_parameter_common *);
internal_proto(show_locus);

extern void runtime_error (const char *, ...)
     __attribute__ ((noreturn, format (printf, 1, 2)));
iexport_proto(runtime_error);

extern void runtime_error_at (const char *, const char *, ...)
     __attribute__ ((noreturn, format (printf, 2, 3)));
iexport_proto(runtime_error_at);

extern void internal_error (st_parameter_common *, const char *)
  __attribute__ ((noreturn));
internal_proto(internal_error);

extern const char *get_oserror (void);
internal_proto(get_oserror);

extern const char *translate_error (int);
internal_proto(translate_error);

extern void generate_error (st_parameter_common *, int, const char *);
iexport_proto(generate_error);

extern try notify_std (st_parameter_common *, int, const char *);
internal_proto(notify_std);

extern notification notification_std(int);
internal_proto(notification_std);

/* fpu.c */

extern void set_fpu (void);
internal_proto(set_fpu);

/* memory.c */

extern void *get_mem (size_t) __attribute__ ((malloc));
internal_proto(get_mem);

extern void free_mem (void *);
internal_proto(free_mem);

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

extern int st_printf (const char *, ...)
  __attribute__ ((format (printf, 1, 2)));
internal_proto(st_printf);

extern int st_vprintf (const char *, va_list);
internal_proto(st_vprintf);

extern char * filename_from_unit (int);
internal_proto(filename_from_unit);

/* stop.c */

extern void stop_numeric (GFC_INTEGER_4) __attribute__ ((noreturn));
iexport_proto(stop_numeric);

/* reshape_packed.c */

extern void reshape_packed (char *, index_type, const char *, index_type,
			    const char *, index_type);
internal_proto(reshape_packed);

/* Repacking functions.  */

/* ??? These aren't currently used by the compiler, though we
   certainly could do so.  */
GFC_INTEGER_4 *internal_pack_4 (gfc_array_i4 *);
internal_proto(internal_pack_4);

GFC_INTEGER_8 *internal_pack_8 (gfc_array_i8 *);
internal_proto(internal_pack_8);

#if defined HAVE_GFC_INTEGER_16
GFC_INTEGER_16 *internal_pack_16 (gfc_array_i16 *);
internal_proto(internal_pack_16);
#endif

GFC_COMPLEX_4 *internal_pack_c4 (gfc_array_c4 *);
internal_proto(internal_pack_c4);

GFC_COMPLEX_8 *internal_pack_c8 (gfc_array_c8 *);
internal_proto(internal_pack_c8);

#if defined HAVE_GFC_COMPLEX_10
GFC_COMPLEX_10 *internal_pack_c10 (gfc_array_c10 *);
internal_proto(internal_pack_c10);
#endif

extern void internal_unpack_4 (gfc_array_i4 *, const GFC_INTEGER_4 *);
internal_proto(internal_unpack_4);

extern void internal_unpack_8 (gfc_array_i8 *, const GFC_INTEGER_8 *);
internal_proto(internal_unpack_8);

#if defined HAVE_GFC_INTEGER_16
extern void internal_unpack_16 (gfc_array_i16 *, const GFC_INTEGER_16 *);
internal_proto(internal_unpack_16);
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

/* string_intrinsics.c */

extern int compare_string (GFC_INTEGER_4, const char *,
			   GFC_INTEGER_4, const char *);
iexport_proto(compare_string);

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

#endif  /* LIBGFOR_H  */
