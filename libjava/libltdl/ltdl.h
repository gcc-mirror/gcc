/* ltdl.h -- generic dlopen functions
   Copyright (C) 1998-1999 Free Software Foundation, Inc.
   Originally by Thomas Tanner <tanner@ffii.org>
   This file is part of GNU Libtool.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

As a special exception to the GNU Library General Public License,
if you distribute this file as part of a program that uses GNU libtool
to create libraries and programs, you may include it under the same
distribution terms that you use for the rest of that program.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free
Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307  USA
*/

/* Only include this header file once. */
#ifndef _LTDL_H_
#define _LTDL_H_ 1

/* __BEGIN_DECLS should be used at the beginning of your declarations,
   so that C++ compilers don't mangle their names.  Use __END_DECLS at
   the end of C declarations. */
#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS /* empty */
# define __END_DECLS /* empty */
#endif

/* LTDL_PARAMS is a macro used to wrap function prototypes, so that compilers
   that don't understand ANSI C prototypes still work, and ANSI C
   compilers can issue warnings about type mismatches. */
#undef LTDL_PARAMS
#undef lt_ptr_t
#if defined (__STDC__) || defined (_AIX) || (defined (__mips) && defined (_SYSTYPE_SVR4)) || defined(WIN32) || defined(__cplusplus)
# define LTDL_PARAMS(protos)	protos
# define lt_ptr_t	void*
#else
# define LTDL_PARAMS(protos)	()
# define lt_ptr_t	char*
#endif

#include <stdlib.h>

#ifdef _LTDL_COMPILE_
typedef	struct lt_dlhandle_t *lt_dlhandle;
#else
typedef	lt_ptr_t lt_dlhandle;
#endif

typedef struct {
	const char *name;
	lt_ptr_t address;
} lt_dlsymlist;

__BEGIN_DECLS
extern int lt_dlinit LTDL_PARAMS((void));
extern int lt_dlpreload LTDL_PARAMS((const lt_dlsymlist *preloaded));
extern int lt_dlpreload_default LTDL_PARAMS((const lt_dlsymlist *preloaded));
extern int lt_dlexit LTDL_PARAMS((void));
extern lt_dlhandle lt_dlopen LTDL_PARAMS((const char *filename));
extern lt_dlhandle lt_dlopenext LTDL_PARAMS((const char *filename));
extern int lt_dlclose LTDL_PARAMS((lt_dlhandle handle));
extern lt_ptr_t lt_dlsym LTDL_PARAMS((lt_dlhandle handle, const char *name));
extern const char *lt_dlerror LTDL_PARAMS((void));
extern int lt_dladdsearchdir LTDL_PARAMS((const char *search_dir));
extern int lt_dlsetsearchpath LTDL_PARAMS((const char *search_path));
extern const char *lt_dlgetsearchpath LTDL_PARAMS((void));

extern const lt_dlsymlist lt_preloaded_symbols[];
#define LTDL_SET_PRELOADED_SYMBOLS() lt_dlpreload_default(lt_preloaded_symbols)

extern lt_ptr_t (*lt_dlmalloc)LTDL_PARAMS((size_t size));
extern void (*lt_dlfree)LTDL_PARAMS((lt_ptr_t ptr));

__END_DECLS

#endif /* !_LTDL_H_ */
