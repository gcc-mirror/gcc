/* -----------------------------------------------------------------------
   ffi_common.h - Copyright (c) 1996  Cygnus Solutions

   $Id: ffi_common.h,v 1.1.1.1 1998/11/29 16:48:16 green Exp $

   Common internal definitions and macros. Only necessary for building
   libffi.
   ----------------------------------------------------------------------- */

#ifndef FFI_COMMON_H
#define FFI_COMMON_H

#ifdef __cplusplus
extern "C" {
#endif

/* Do not move this. Some versions of AIX are very picky about where
   this is positioned. */
#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

/* Check for the existence of memcpy. */
#if STDC_HEADERS
# include <string.h>
#else
# ifndef HAVE_MEMCPY
#  define memcpy(d, s, n) bcopy ((s), (d), (n))
# endif
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE (!FALSE)
#endif

#ifndef __cplusplus
/* bool is a keyword in C++ */
/*@-cppnames@*/
typedef int bool;
/*@=cppnames@*/
#endif

#ifdef FFI_DEBUG

/* Debugging functions */
/*@exits@*/ int ffi_assert(/*@temp@*/ char *file, int line);
void ffi_stop_here(void);
bool ffi_type_test(/*@temp@*/ /*@out@*/ ffi_type *a);

#define FFI_ASSERT(x) ((x) ? 0 : ffi_assert(__FILE__,__LINE__))

#else

#define FFI_ASSERT(x) 

#endif

/* Perform machine dependent cif processing */
ffi_status ffi_prep_cif_machdep(ffi_cif *cif);

/* Extended cif, used in callback from assembly routine */
typedef struct
{
  /*@dependent@*/ ffi_cif *cif;
  /*@dependent@*/ void *rvalue;
  /*@dependent@*/ void **avalue;
} extended_cif;

#ifdef __cplusplus
}
#endif

#endif


