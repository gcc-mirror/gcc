/* ltdl.c -- system independent dlopen wrapper
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Originally by Thomas Tanner <tanner@ffii.org>
   This file is part of GNU Libtool.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

As a special exception to the GNU Lesser General Public License,
if you distribute this file as part of a program or library that
is built using GNU libtool, you may include it under the same
distribution terms that you use for the rest of that program.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307  USA

*/

#define _LTDL_COMPILE_

#if HAVE_CONFIG_H
#include <config.h>
#endif

#if HAVE_STRING_H
#include <string.h>
#endif

#if HAVE_STRINGS_H
#include <strings.h>
#endif

#if HAVE_CTYPE_H
#include <ctype.h>
#endif

#if HAVE_MALLOC_H
#include <malloc.h>
#endif

#if HAVE_MEMORY_H
#include <memory.h>
#endif

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if HAVE_STDIO_H
#include <stdio.h>
#endif

#if HAVE_BOEHM_GC
#include <gc.h>
#endif

#include "ltdl.h"

#ifdef DLL_EXPORT
#  define LTDL_GLOBAL_DATA	__declspec(dllexport)
#else
#  define LTDL_GLOBAL_DATA
#endif

/* max. filename length */
#ifndef LTDL_FILENAME_MAX
#define LTDL_FILENAME_MAX 1024
#endif

#undef	LTDL_READTEXT_MODE
/* fopen() mode flags for reading a text file */
#ifdef _WIN32
#define LTDL_READTEXT_MODE "rt"
#else
#define LTDL_READTEXT_MODE "r"
#endif

#undef	LTDL_SYMBOL_LENGTH
/* This is the maximum symbol size that won't require malloc/free */
#define LTDL_SYMBOL_LENGTH	128

#undef	LTDL_SYMBOL_OVERHEAD
/* This accounts for the _LTX_ separator */
#define LTDL_SYMBOL_OVERHEAD	5

/* NOTE: typedefed in ltdl.h
   This structure is used for the list of registered loaders. */
struct lt_dlloader_t {
	struct lt_dlloader_t *next;
	const char *loader_name; /* identifying name for each loader */
	const char *sym_prefix;	 /* prefix for symbols */
	lt_module_open_t *module_open;
	lt_module_close_t *module_close;
	lt_find_sym_t *find_sym;
	lt_dlloader_exit_t *dlloader_exit;
  	lt_dlloader_data_t dlloader_data;
};

typedef	struct lt_dlhandle_t {
	struct lt_dlhandle_t *next;
	lt_dlloader_t *loader;	/* dlopening interface */
	lt_dlinfo info;
	int	depcount;	/* number of dependencies */
	lt_dlhandle *deplibs;	/* dependencies */
	lt_module_t module;	/* system module handle */
	lt_ptr_t system;	/* system specific data */
	lt_ptr_t app_private;	/* application private data */
} lt_dlhandle_t;

static const char objdir[] = LTDL_OBJDIR;
#ifdef	LTDL_SHLIB_EXT
static const char shlib_ext[] = LTDL_SHLIB_EXT;
#endif
#ifdef	LTDL_SYSSEARCHPATH
static const char sys_search_path[] = LTDL_SYSSEARCHPATH;
#endif

/* Extract the diagnostic strings from the error table macro in the same
   order as the enumberated indices in ltdl.h. */
#define LTDL_ERROR(name, diagnostic)	(diagnostic),
static const char *ltdl_error_strings[] = {
	ltdl_error_table
	0
};
#undef LTDL_ERROR

#ifdef __STDC__
#  define LT_DLSTRERROR(name)	ltdl_error_strings[LTDL_ERROR_##name]
#else
#  define LT_DLSTRERROR(name)	ltdl_error_strings[LTDL_ERROR_/**/name]
#endif

static const char *last_error = 0;

LTDL_GLOBAL_DATA lt_ptr_t (*lt_dlmalloc) LTDL_PARAMS((size_t size)) = (lt_ptr_t(*)LTDL_PARAMS((size_t)))malloc;
LTDL_GLOBAL_DATA void	 (*lt_dlfree)  LTDL_PARAMS((lt_ptr_t ptr)) = (void(*)LTDL_PARAMS((lt_ptr_t)))free;

#undef strdup
#define strdup xstrdup

static inline char *
strdup(str)
	const char *str;
{
	char *tmp;

	if (!str)
		return 0;
	tmp = (char*) lt_dlmalloc(strlen(str)+1);
	if (tmp)
		strcpy(tmp, str);
	return tmp;
}

#if ! HAVE_STRCMP

#undef strcmp
#define strcmp xstrcmp

static inline int
strcmp (str1, str2)
	const char *str1;
	const char *str2;
{
	if (str1 == str2)
		return 0;
	if (str1 == 0)
		return -1;
	if (str2 == 0)
		return 1;
		
	for (;*str1 && *str2; str1++, str2++)
		if (*str1 != *str2)
			break;
	
	return (int)(*str1 - *str2);
}
#endif
		

#if ! HAVE_STRCHR

# if HAVE_INDEX

#  define strchr index

# else

#  define strchr xstrchr

static inline const char*
strchr(str, ch)
	const char *str;
	int ch;
{
	const char *p;

	for (p = str; *p != (char)ch && *p != '\0'; p++)
		/*NOWORK*/;

	return (*p == (char)ch) ? p : 0;
}

# endif

#endif

#if ! HAVE_STRRCHR

# if HAVE_RINDEX

#  define strrchr rindex

# else

#  define strrchr xstrrchr

static inline const char*
strrchr(str, ch)
	const char *str;
	int ch;
{
	const char *p;

	for (p = str; *p != '\0'; p++)
		/*NOWORK*/;

	while (*p != (char)ch && p >= str)
		p--;

	return (*p == (char)ch) ? p : 0;
}

# endif

#endif

/* The Cygwin dlopen implementation prints a spurious error message to
   stderr if its call to LoadLibrary() fails for any reason.  We can
   mitigate this by not using the Cygwin implementation, and falling
   back to our own LoadLibrary() wrapper. */
#if HAVE_LIBDL && !defined(__CYGWIN__)

/* dynamic linking with dlopen/dlsym */

#if HAVE_DLFCN_H
# include <dlfcn.h>
#endif

#ifdef RTLD_GLOBAL
# define LTDL_GLOBAL	RTLD_GLOBAL
#else
# ifdef DL_GLOBAL
#  define LTDL_GLOBAL	DL_GLOBAL
# else
#  define LTDL_GLOBAL	0
# endif
#endif

/* We may have to define LTDL_LAZY_OR_NOW in the command line if we
   find out it does not work in some platform. */
#ifndef LTDL_LAZY_OR_NOW
# ifdef RTLD_LAZY
#  define LTDL_LAZY_OR_NOW	RTLD_LAZY
# else
#  ifdef DL_LAZY
#   define LTDL_LAZY_OR_NOW	DL_LAZY
#  else
#   ifdef RTLD_NOW
#    define LTDL_LAZY_OR_NOW	RTLD_NOW
#   else
#    ifdef DL_NOW
#     define LTDL_LAZY_OR_NOW	DL_NOW
#    else
#     define LTDL_LAZY_OR_NOW	0
#    endif
#   endif
#  endif
# endif
#endif

static lt_module_t
sys_dl_open (loader_data, filename)
	lt_dlloader_data_t loader_data;
	const char *filename;
{
	lt_module_t module = dlopen(filename, LTDL_GLOBAL | LTDL_LAZY_OR_NOW);
	if (!module) {
#if HAVE_DLERROR
		last_error = dlerror();
#else
		last_error = LT_DLSTRERROR(CANNOT_OPEN);
#endif
	}
	return module;
}

static int
sys_dl_close (loader_data, module)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
{
	if (dlclose(module) != 0) {
#if HAVE_DLERROR
		last_error = dlerror();
#else
		last_error = LT_DLSTRERROR(CANNOT_CLOSE);
#endif
		return 1;
	}
	return 0;
}

static lt_ptr_t
sys_dl_sym (loader_data, module, symbol)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
	const char *symbol;
{
	lt_ptr_t address = dlsym(module, symbol);
	
	if (!address)
#if HAVE_DLERROR
		last_error = dlerror();
#else
		last_error = LT_DLSTRERROR(SYMBOL_NOT_FOUND);
#endif
	return address;
}

static struct lt_user_dlloader sys_dl = {
#  ifdef NEED_USCORE
	   "_",
#  else
	   0,
#  endif
	   sys_dl_open, sys_dl_close, sys_dl_sym, 0, 0 };
#endif

#if HAVE_SHL_LOAD

/* dynamic linking with shl_load (HP-UX) (comments from gmodule) */

#ifdef HAVE_DL_H
#include <dl.h>
#endif

/* some flags are missing on some systems, so we provide
 * harmless defaults.
 *
 * Mandatory:
 * BIND_IMMEDIATE  - Resolve symbol references when the library is loaded.
 * BIND_DEFERRED   - Delay code symbol resolution until actual reference.
 *
 * Optionally:
 * BIND_FIRST	   - Place the library at the head of the symbol search order.
 * BIND_NONFATAL   - The default BIND_IMMEDIATE behavior is to treat all unsatisfied
 *		     symbols as fatal.	This flag allows binding of unsatisfied code
 *		     symbols to be deferred until use.
 *		     [Perl: For certain libraries, like DCE, deferred binding often
 *		     causes run time problems.	Adding BIND_NONFATAL to BIND_IMMEDIATE
 *		     still allows unresolved references in situations like this.]
 * BIND_NOSTART	   - Do not call the initializer for the shared library when the
 *		     library is loaded, nor on a future call to shl_unload().
 * BIND_VERBOSE	   - Print verbose messages concerning possible unsatisfied symbols.
 *
 * hp9000s700/hp9000s800:
 * BIND_RESTRICTED - Restrict symbols visible by the library to those present at
 *		     library load time.
 * DYNAMIC_PATH	   - Allow the loader to dynamically search for the library specified
 *		     by the path argument.
 */

#ifndef	DYNAMIC_PATH
#define	DYNAMIC_PATH	0
#endif	/* DYNAMIC_PATH */
#ifndef	BIND_RESTRICTED
#define	BIND_RESTRICTED	0
#endif	/* BIND_RESTRICTED */

#define	LTDL_BIND_FLAGS	(BIND_IMMEDIATE | BIND_NONFATAL | DYNAMIC_PATH)

static lt_module_t
sys_shl_open (loader_data, filename)
	lt_dlloader_data_t loader_data;
	const char *filename;
{
	lt_module_t module = shl_load(filename, LTDL_BIND_FLAGS, 0L);
	if (!module) {
		last_error = LT_DLSTRERROR(CANNOT_OPEN);
	}
	return module;
}

static int
sys_shl_close (loader_data, module)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
{
	if (shl_unload((shl_t) (module)) != 0) {
		last_error = LT_DLSTRERROR(CANNOT_CLOSE);
		return 1;
	}
	return 0;
}

static lt_ptr_t
sys_shl_sym (loader_data, module, symbol)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
	const char *symbol;
{
	lt_ptr_t address;

	if (module && shl_findsym((shl_t*) &module,
	    symbol, TYPE_UNDEFINED, &address) == 0)
		if (address)
			return address;
	last_error = LT_DLSTRERROR(SYMBOL_NOT_FOUND);
	return 0;
}

static struct lt_user_dlloader
sys_shl = { 0, sys_shl_open, sys_shl_close, sys_shl_sym, 0, 0 };

#undef LTDL_TYPE_TOP
#define LTDL_TYPE_TOP &sys_shl

#endif

#ifdef _WIN32

/* dynamic linking for Win32 */

#include <windows.h>

/* Forward declaration; required to implement handle search below. */
static lt_dlhandle handles;

static lt_module_t
sys_wll_open (loader_data, filename)
	lt_dlloader_data_t loader_data;
	const char *filename;
{
	lt_dlhandle cur;
	lt_module_t module;
	char *searchname = 0;
        char *ext;
        char self_name_buf[MAX_PATH];

	if (!filename) {
		/* Get the name of main module */
		*self_name_buf = 0;
		GetModuleFileName(NULL, self_name_buf, sizeof(self_name_buf));
		filename = ext = self_name_buf;
	}
	else ext = strrchr(filename, '.');

	if (ext) {
		/* FILENAME already has an extension. */
		searchname = strdup(filename);
	} else {
		/* Append a `.' to stop Windows from adding an
		   implicit `.dll' extension. */
		searchname = (char*)lt_dlmalloc(2+ strlen(filename));
		if (!searchname) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			return 0;
		}
		strcpy(searchname, filename);
		strcat(searchname, ".");
	}

	module = LoadLibrary(searchname);
	lt_dlfree(searchname);
	
	/* libltdl expects this function to fail if it is unable
	   to physically load the library.  Sadly, LoadLibrary
	   will search the loaded libraries for a match and return
	   one of them if the path search load fails.

	   We check whether LoadLibrary is returning a handle to
	   an already loaded module, and simulate failure if we
	   find one. */
	cur = handles;
	while (cur) {
		if (!cur->module) {
			cur = 0;
			break;
		}
		if (cur->module == module)
			break;
		cur = cur->next;
	}

	if (cur || !module) {
		last_error = LT_DLSTRERROR(CANNOT_OPEN);
		return 0;
	}

	return module;
}

static int
sys_wll_close (loader_data, module)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
{
	if (FreeLibrary(module) == 0) {
		last_error = LT_DLSTRERROR(CANNOT_CLOSE);
		return 1;
	}
	return 0;
}

static lt_ptr_t
sys_wll_sym (loader_data, module, symbol)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
	const char *symbol;
{
	lt_ptr_t address = GetProcAddress(module, symbol);
	
	if (!address)
		last_error = LT_DLSTRERROR(SYMBOL_NOT_FOUND);
	return address;
}

static struct lt_user_dlloader
sys_wll = { 0, sys_wll_open, sys_wll_close, sys_wll_sym, 0, 0 };

#endif

#ifdef __BEOS__

/* dynamic linking for BeOS */

#include <kernel/image.h>

static lt_module_t
sys_bedl_open (loader_data, filename)
	lt_dlloader_data_t loader_data;
	const char *filename;
{
	image_id image = 0;
	
	if (filename) {
		image = load_add_on(filename);
	} else {
		image_info info; 
		int32 cookie = 0; 
		if (get_next_image_info(0, &cookie, &info) == B_OK)
			image = load_add_on(info.name);
	}
	if (image <= 0) {
		last_error = LT_DLSTRERROR(CANNOT_OPEN);
		return 0;
	}

	return (lt_module_t) image;
}

static int
sys_bedl_close (loader_data, module)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
{
	if (unload_add_on((image_id)module) != B_OK) {
		last_error = LT_DLSTRERROR(CANNOT_CLOSE);
		return 1;
	}
	return 0;
}

static lt_ptr_t
sys_bedl_sym (loader_data, module, symbol)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
	const char *symbol;
{
	lt_ptr_t address = 0;
	image_id image = (image_id)module;
   
	if (get_image_symbol(image, symbol, B_SYMBOL_TYPE_ANY,
		&address) != B_OK) {
		last_error = LT_DLSTRERROR(SYMBOL_NOT_FOUND);
		return 0;
	}
	return address;
}

static struct lt_user_dlloader
sys_bedl = { 0, sys_bedl_open, sys_bedl_close, sys_bedl_sym, 0, 0 };

#endif

#if HAVE_DLD

/* dynamic linking with dld */

#if HAVE_DLD_H
#include <dld.h>
#endif

static lt_module_t
sys_dld_open (loader_data, filename)
	lt_dlloader_data_t loader_data;
	const char *filename;
{
	lt_module_t module = strdup(filename);
	if (!module) {
		last_error = LT_DLSTRERROR(NO_MEMORY);
		return 0;
	}
	if (dld_link(filename) != 0) {
		last_error = LT_DLSTRERROR(CANNOT_OPEN);
		lt_dlfree(module);
		return 0;
	}
	return module;
}

static int
sys_dld_close (loader_data, module)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
{
	if (dld_unlink_by_file((char*)(module), 1) != 0) {
		last_error = LT_DLSTRERROR(CANNOT_CLOSE);
		return 1;
	}
	lt_dlfree(module);
	return 0;
}

static lt_ptr_t
sys_dld_sym (loader_data, module, symbol)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
	const char *symbol;
{
	lt_ptr_t address = dld_get_func(symbol);

	if (!address)
		last_error = LT_DLSTRERROR(SYMBOL_NOT_FOUND);
	return address;
}

static struct lt_user_dlloader
sys_dld = { 0, sys_dld_open, sys_dld_close, sys_dld_sym, 0, 0 };

#endif

/* emulate dynamic linking using preloaded_symbols */

typedef struct lt_dlsymlists_t {
	struct lt_dlsymlists_t *next;
	const lt_dlsymlist *syms;
} lt_dlsymlists_t;

static const lt_dlsymlist *default_preloaded_symbols = 0;
static lt_dlsymlists_t *preloaded_symbols = 0;

static int
presym_init (loader_data)
	lt_dlloader_data_t loader_data;
{
	preloaded_symbols = 0;
	if (default_preloaded_symbols)
		return lt_dlpreload(default_preloaded_symbols);
	return 0;
}

static int
presym_free_symlists LTDL_PARAMS((void))
{
	lt_dlsymlists_t	*lists = preloaded_symbols;
	
	while (lists) {
		lt_dlsymlists_t	*tmp = lists;
		
		lists = lists->next;
		lt_dlfree(tmp);
	}
	preloaded_symbols = 0;
	return 0;
}

static int
presym_exit (loader_data)
	lt_dlloader_data_t loader_data;
{
	presym_free_symlists();
	return 0;
}

static int
presym_add_symlist (preloaded)
	const lt_dlsymlist *preloaded;
{
	lt_dlsymlists_t *tmp;
	lt_dlsymlists_t *lists = preloaded_symbols;
	
	while (lists) {
		if (lists->syms == preloaded)
			return 0;
		lists = lists->next;
	}

	tmp = (lt_dlsymlists_t*) lt_dlmalloc(sizeof(lt_dlsymlists_t));
	if (!tmp) {
		last_error = LT_DLSTRERROR(NO_MEMORY);
		return 1;
	}
	tmp->syms = preloaded;
	tmp->next = preloaded_symbols;
	preloaded_symbols = tmp;
	return 0;
}

static lt_module_t
presym_open (loader_data, filename)
	lt_dlloader_data_t loader_data;
	const char *filename;
{
	lt_dlsymlists_t *lists = preloaded_symbols;

	if (!lists) {
		last_error = LT_DLSTRERROR(NO_SYMBOLS);
		return 0;
	}
	if (!filename)
		filename = "@PROGRAM@";
	while (lists) {
		const lt_dlsymlist *syms = lists->syms;
	
		while (syms->name) {
			if (!syms->address &&
			    strcmp(syms->name, filename) == 0) {
				return (lt_module_t) syms;
			}
			syms++;
		}
		lists = lists->next;
	}
	last_error = LT_DLSTRERROR(FILE_NOT_FOUND);
	return 0;
}

static int
presym_close (loader_data, module)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
{
	/* Just to silence gcc -Wall */
	module = 0;
	return 0;
}

static lt_ptr_t
presym_sym (loader_data, module, symbol)
	lt_dlloader_data_t loader_data;
	lt_module_t module;
	const char *symbol;
{
	lt_dlsymlist *syms = (lt_dlsymlist*)(module);

	syms++;
	while (syms->address) {
		if (strcmp(syms->name, symbol) == 0)
			return syms->address;
		syms++;
	}
	last_error = LT_DLSTRERROR(SYMBOL_NOT_FOUND);
	return 0;
}

static struct lt_user_dlloader
presym = { 0, presym_open, presym_close, presym_sym, presym_exit, 0 };


static char *user_search_path = 0;
static lt_dlloader_t *loaders = 0;
static lt_dlhandle handles = 0;
static int initialized = 0;

int
lt_dlinit LTDL_PARAMS((void))
{
	/* initialize libltdl */
	int errors = 0;

	if (initialized) {	/* Initialize only at first call. */
		initialized++;
		return 0;
	}
	handles = 0;
	user_search_path = 0; /* empty search path */
	
#if HAVE_LIBDL && !defined(__CYGWIN__)
	errors += lt_dlloader_add (lt_dlloader_next(0), &sys_dl, "dlopen");
#endif	
#if HAVE_SHL_LOAD
	errors += lt_dlloader_add (lt_dlloader_next(0), &sys_shl, "dlopen");
#endif
#ifdef _WIN32
	errors += lt_dlloader_add (lt_dlloader_next(0), &sys_wll, "dlopen");
#endif
#ifdef __BEOS__
	errors += lt_dlloader_add (lt_dlloader_next(0), &sys_bedl, "dlopen");
#endif
#if HAVE_DLD
	errors += lt_dlloader_add (lt_dlloader_next(0), &sys_dld, "dld");
#endif
	errors += lt_dlloader_add (lt_dlloader_next(0), &presym, "dlpreload");
	if (presym_init(presym.dlloader_data)) {
		last_error = LT_DLSTRERROR(INIT_LOADER);
			return 1;
	}

	if (errors != 0) {
		last_error = LT_DLSTRERROR(DLOPEN_NOT_SUPPORTED);
		return 1;
	}
	last_error = 0;
	initialized = 1;
	return 0;
}

int
lt_dlpreload (preloaded)
	const lt_dlsymlist *preloaded;
{
	if (preloaded)
		return presym_add_symlist(preloaded);
	presym_free_symlists();
	if (default_preloaded_symbols)
		return lt_dlpreload(default_preloaded_symbols);
	return 0;
}

int
lt_dlpreload_default (preloaded)
	const lt_dlsymlist *preloaded;
{
	default_preloaded_symbols = preloaded;
	return 0;
}

int
lt_dlexit LTDL_PARAMS((void))
{
	/* shut down libltdl */
	lt_dlloader_t *loader = loaders;
	int	errors, level;
	
	if (!initialized) {
		last_error = LT_DLSTRERROR(SHUTDOWN);
		return 1;
	}
	if (initialized != 1) { /* shut down only at last call. */
		initialized--;
		return 0;
	}
	/* close all modules */
	errors = 0;
	for (level = 1; handles; level++) {
		lt_dlhandle cur = handles;
		while (cur) {
			lt_dlhandle tmp = cur;
			cur = cur->next;
			if (tmp->info.ref_count <= level)
				if (lt_dlclose(tmp))
					errors++;
		}
	}
	/* close all loaders */
	while (loader) {
	  	lt_dlloader_t *next = loader->next;
		lt_dlloader_data_t data = loader->dlloader_data;
		if (loader->dlloader_exit && loader->dlloader_exit(data))
			errors++;
		lt_dlfree (loader);
		loader = next;
	}

	initialized = 0;
	return errors;
}

static int
tryall_dlopen (handle, filename)
	lt_dlhandle *handle;
	const char *filename;
{
	lt_dlhandle cur = handles;
	lt_dlloader_t *loader = loaders;
	const char *saved_error = last_error;
	
	/* check whether the module was already opened */
	while (cur) {
		/* try to dlopen the program itself? */
		if (!cur->info.filename && !filename)
			break;
		if (cur->info.filename && filename && 
		    strcmp(cur->info.filename, filename) == 0)
			break;
		cur = cur->next;
	}

	if (cur) {
		cur->info.ref_count++;
		*handle = cur;
		return 0;
	}
	
	cur = *handle;
	if (filename) {
		cur->info.filename = strdup(filename);
		if (!cur->info.filename) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			return 1;
		}
	} else
		cur->info.filename = 0;
	while (loader) {
		lt_dlloader_data_t data = loader->dlloader_data;
		cur->module = loader->module_open(data, filename);
		if (cur->module != 0)
			break;
		loader = loader->next;
	}
	if (!loader) {
		if (cur->info.filename)
			lt_dlfree(cur->info.filename);
		return 1;
	}
	cur->loader = loader;
	last_error = saved_error;
	return 0;
}

static int
find_module (handle, dir, libdir, dlname, old_name, installed)
	lt_dlhandle *handle;
	const char *dir;
	const char *libdir;
	const char *dlname;
	const char *old_name;
	int installed;
{
	int	error;
	char	*filename;
	/* try to open the old library first; if it was dlpreopened, 
	   we want the preopened version of it, even if a dlopenable
	   module is available */
	if (old_name && tryall_dlopen(handle, old_name) == 0)
		return 0;
	/* try to open the dynamic library */
	if (dlname) {
		/* try to open the installed module */
		if (installed && libdir) {
			filename = (char*)
				lt_dlmalloc(strlen(libdir)+1+strlen(dlname)+1);
			if (!filename) {
				last_error = LT_DLSTRERROR(NO_MEMORY);
				return 1;
			}
			sprintf (filename, "%s/%s", libdir, dlname);
			error = tryall_dlopen(handle, filename) != 0;
			lt_dlfree(filename);
			if (!error)
				return 0;
		}
		/* try to open the not-installed module */
		if (!installed) {
			filename = (char*)
				lt_dlmalloc((dir ? strlen(dir) : 0)
				       + strlen(objdir)	+ strlen(dlname) + 1);
			if (!filename) {
				last_error = LT_DLSTRERROR(NO_MEMORY);
				return 1;
			}
			if (dir)
				strcpy(filename, dir);
			else
				*filename = 0;
			strcat(filename, objdir);
			strcat(filename, dlname);

			error = tryall_dlopen(handle, filename) != 0;
			lt_dlfree(filename);
			if (!error)
				return 0;
		}
		/* maybe it was moved to another directory */
		{
			filename = (char*)
				lt_dlmalloc((dir ? strlen(dir) : 0)
				       + strlen(dlname) + 1);
			if (dir)
				strcpy(filename, dir);
			else
				*filename = 0;
			strcat(filename, dlname);
			error = tryall_dlopen(handle, filename) != 0;
			lt_dlfree(filename);
			if (!error)
				return 0;
		}
	}
	return 1;
}

static char*
canonicalize_path (path)
	const char *path;
{
	char *canonical = 0;
	
	if (path && *path) {
		char *ptr = strdup (path);
		canonical = ptr;
#ifdef LTDL_DIRSEP_CHAR
		/* Avoid this overhead where '/' is the only separator. */
		while (ptr = strchr (ptr, LTDL_DIRSEP_CHAR))
			*ptr++ = '/';
#endif
	}

	return canonical;
}

static lt_ptr_t
find_file (basename, search_path, pdir, handle)
	const char *basename;
	const char *search_path;
	char **pdir;
	lt_dlhandle *handle;
{
	/* when handle != NULL search a library, otherwise a file */
	/* return NULL on failure, otherwise the file/handle */

	lt_ptr_t result = 0;
	char	*filename = 0;
	int     filenamesize = 0;
	int	lenbase = strlen(basename);
	char	*canonical = 0, *next = 0;
	
	if (!search_path || !*search_path) {
		last_error = LT_DLSTRERROR(FILE_NOT_FOUND);
		return 0;
	}
	canonical = canonicalize_path (search_path);
	if (!canonical) {
		last_error = LT_DLSTRERROR(NO_MEMORY);
		goto cleanup;
	}
	next = canonical;
	while (next) {
		int lendir;
		char *cur = next;

		next = strchr(cur, LTDL_PATHSEP_CHAR);
		if (!next)
			next = cur + strlen(cur);
		lendir = next - cur;
		if (*next == LTDL_PATHSEP_CHAR)
			++next;
		else
			next = 0;
		if (lendir == 0)
			continue;
		if (lendir + 1 + lenbase >= filenamesize) {
			if (filename)
				lt_dlfree(filename);
			filenamesize = lendir + 1 + lenbase + 1;
			filename = (char*) lt_dlmalloc(filenamesize);
			if (!filename) {
				last_error = LT_DLSTRERROR(NO_MEMORY);
				goto cleanup;
			}
		}
		strncpy(filename, cur, lendir);
		if (filename[lendir-1] != '/')
			filename[lendir++] = '/';
		strcpy(filename+lendir, basename);
		if (handle) {
			if (tryall_dlopen(handle, filename) == 0) {
				result = (lt_ptr_t) handle;
				goto cleanup;
			}
		} else {
			FILE *file = fopen(filename, LTDL_READTEXT_MODE);
			if (file) {
				if (*pdir)
					lt_dlfree(*pdir);
				filename[lendir] = '\0';
				*pdir = strdup(filename);
				if (!*pdir) {
					/* We could have even avoided the
					   strdup, but there would be some
					   memory overhead. */
					*pdir = filename;
					filename = 0;
				}
				result = (lt_ptr_t) file;
				goto cleanup;
			}
		}
	}
	last_error = LT_DLSTRERROR(FILE_NOT_FOUND);
cleanup:
	if (filename)
		lt_dlfree(filename);
	if (canonical)
		lt_dlfree(canonical);
	return result;
}

static int
load_deplibs(handle, deplibs)
	lt_dlhandle handle;
	char *deplibs;
{
	char	*p, *save_search_path;
	int	i;
	int	ret = 1, depcount = 0;
	char	**names = 0;
	lt_dlhandle *handles = 0;

	handle->depcount = 0;
	if (!deplibs)
		return 0;
	save_search_path = strdup(user_search_path);
	if (user_search_path && !save_search_path) {
		last_error = LT_DLSTRERROR(NO_MEMORY);
		return 1;
	}
	p = deplibs;
	/* extract search paths and count deplibs */
	while (*p) {
		if (!isspace(*p)) {
			char *end = p+1;
			while (*end && !isspace(*end)) end++;
			if (strncmp(p, "-L", 2) == 0 ||
			    strncmp(p, "-R", 2) == 0) {
				char save = *end;
				*end = 0; /* set a temporary string terminator */
				if (lt_dladdsearchdir(p+2))
					goto cleanup;
				*end = save;
			} else
				depcount++;
			p = end;
		} else
			p++;
	}
	if (!depcount) {
		ret = 0;
		goto cleanup;
	}
	names = (char**)lt_dlmalloc(depcount * sizeof(char*));
	if (!names)
		goto cleanup;
	handles = (lt_dlhandle*)lt_dlmalloc(depcount * sizeof(lt_dlhandle*));
	if (!handles)
		goto cleanup;
	depcount = 0;
	/* now only extract the actual deplibs */
	p = deplibs;
	while (*p) {
		if (!isspace(*p)) {
			char *end = p+1;
			while (*end && !isspace(*end)) end++;
			if (strncmp(p, "-L", 2) != 0 &&
			    strncmp(p, "-R", 2) != 0) {
				char *name;
				char save = *end;
				*end = 0; /* set a temporary string terminator */
				if (strncmp(p, "-l", 2) == 0) {
					name = lt_dlmalloc(3+ /* "lib" */
							   strlen(p+2)+1);
					if (name)
						sprintf (name, "lib%s", p+2);
				} else
					name = strdup(p);
				if (name)
					names[depcount++] = name;
				else
					goto cleanup_names;
				*end = save;
			}
			p = end;
		} else
			p++;
	}
	/* load the deplibs (in reverse order) */
	for (i = 0; i < depcount; i++) {
		lt_dlhandle handle = lt_dlopenext(names[depcount-1-i]);
		if (!handle) {
			int j;
			for (j = 0; j < i; j++)
				lt_dlclose(handles[j]);
			last_error = LT_DLSTRERROR(DEPLIB_NOT_FOUND);
			goto cleanup_names;
		}
		handles[i] = handle;	
	}
	handle->depcount = depcount;
	handle->deplibs = handles;
	handles = 0;
	ret = 0;
cleanup_names:
	for (i = 0; i < depcount; i++)
		lt_dlfree(names[i]);
cleanup:
	if (names)
		lt_dlfree(names);
	if (handles)
		lt_dlfree(handles);
	/* restore the old search path */
	if (user_search_path)
		lt_dlfree(user_search_path);
	user_search_path = save_search_path;
	return ret;
}

static int
unload_deplibs(handle)
	lt_dlhandle handle;
{
	int i;
	int errors = 0;
	
	if (!handle->depcount)
		return 0;
	for (i = 0; i < handle->depcount; i++)
		errors += lt_dlclose(handle->deplibs[i]);		
	return errors;
}

static inline int
trim (dest, str)
	char **dest;
	const char *str;
{
	/* remove the leading and trailing "'" from str 
	   and store the result in dest */
	char *tmp;
	const char *end = strrchr(str, '\'');
	int len = strlen(str);

	if (*dest)
		lt_dlfree(*dest);
	if (len > 3 && str[0] == '\'') {
		tmp = (char*) lt_dlmalloc(end - str);
		if (!tmp) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			return 1;
		}
		strncpy(tmp, &str[1], (end - str) - 1);
		tmp[len-3] = '\0';
		*dest = tmp;
	} else
		*dest = 0;
	return 0;
}

static inline int
free_vars( dlname, oldname, libdir, deplibs)
	char *dlname;
	char *oldname;
	char *libdir;
	char *deplibs;
{
	if (dlname)
		lt_dlfree(dlname);
	if (oldname)
		lt_dlfree(oldname);
	if (libdir)
		lt_dlfree(libdir);
	if (deplibs)
		lt_dlfree(deplibs);
	return 0;
}

lt_dlhandle
lt_dlopen (filename)
	const char *filename;
{
	lt_dlhandle handle = 0, newhandle;
	const char *ext;
	const char *saved_error = last_error;
	char	*canonical = 0, *basename = 0, *dir = 0, *name = 0;
	
	if (!filename) {
		handle = (lt_dlhandle) lt_dlmalloc(sizeof(lt_dlhandle_t));
		if (!handle) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			return 0;
		}
		handle->info.ref_count = 0;
		handle->depcount = 0;
		handle->deplibs = 0;
		newhandle = handle;
		if (tryall_dlopen(&newhandle, 0) != 0) {
			lt_dlfree(handle);
			return 0;
		}
		goto register_handle;
	}
	canonical = canonicalize_path (filename);
	if (!canonical) {
		last_error = LT_DLSTRERROR(NO_MEMORY);
		if (handle)
			lt_dlfree(handle);
		return 0;
	}
	basename = strrchr(canonical, '/');
	if (basename) {
		basename++;
		dir = (char*) lt_dlmalloc(basename - canonical + 1);
		if (!dir) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			handle = 0;
			goto cleanup;
		}
		strncpy(dir, canonical, basename - canonical);
		dir[basename - canonical] = '\0';
	} else
		basename = canonical;
	/* check whether we open a libtool module (.la extension) */
	ext = strrchr(basename, '.');
	if (ext && strcmp(ext, ".la") == 0) {
		/* this seems to be a libtool module */
		FILE	*file;
		int	i;
		char	*dlname = 0, *old_name = 0;
		char	*libdir = 0, *deplibs = 0;
		char	*line;
		int	error = 0;
		/* if we can't find the installed flag, it is probably an
		   installed libtool archive, produced with an old version
		   of libtool */
		int     installed = 1; 

		/* extract the module name from the file name */
		name = (char*) lt_dlmalloc(ext - basename + 1);
		if (!name) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			handle = 0;
			goto cleanup;
		}
		/* canonicalize the module name */
		for (i = 0; i < ext - basename; i++)
			if (isalnum((int)(basename[i])))
				name[i] = basename[i];
			else
				name[i] = '_';
		name[ext - basename] = '\0';
		/* now try to open the .la file */
		file = fopen(filename, LTDL_READTEXT_MODE);
		if (!file)
			last_error = LT_DLSTRERROR(FILE_NOT_FOUND);
		if (!file && !dir) {
			/* try other directories */
			file = (FILE*) find_file(basename, 
						 user_search_path,
						 &dir, 0);
			if (!file)
				file = (FILE*) find_file(basename,
						 getenv("LTDL_LIBRARY_PATH"),
						 &dir, 0);
#ifdef LTDL_SHLIBPATH_VAR
			if (!file)
				file = (FILE*) find_file(basename,
						 getenv(LTDL_SHLIBPATH_VAR),
						 &dir, 0);
#endif
#ifdef LTDL_SYSSEARCHPATH
			if (!file)
				file = (FILE*) find_file(basename,
						 sys_search_path,
						 &dir, 0);
#endif
		}
		if (!file) {
			handle = 0;
			goto cleanup;
		}
		line = (char*) lt_dlmalloc(LTDL_FILENAME_MAX);
		if (!line) {
			fclose(file);
			last_error = LT_DLSTRERROR(NO_MEMORY);
			handle = 0;
			goto cleanup;
		}
		/* read the .la file */
		while (!feof(file)) {
			if (!fgets(line, LTDL_FILENAME_MAX, file))
				break;
			if (line[0] == '\n' || line[0] == '#')
				continue;
#			undef  STR_DLNAME
#			define STR_DLNAME	"dlname="
			if (strncmp(line, STR_DLNAME,
				sizeof(STR_DLNAME) - 1) == 0)
				error = trim(&dlname,
					&line[sizeof(STR_DLNAME) - 1]);
			else
#			undef  STR_OLD_LIBRARY
#			define STR_OLD_LIBRARY	"old_library="
			if (strncmp(line, STR_OLD_LIBRARY,
				sizeof(STR_OLD_LIBRARY) - 1) == 0)
				error = trim(&old_name,
					&line[sizeof(STR_OLD_LIBRARY) - 1]);
			else
#			undef  STR_LIBDIR
#			define STR_LIBDIR	"libdir="
			if (strncmp(line, STR_LIBDIR,
				sizeof(STR_LIBDIR) - 1) == 0)
				error = trim(&libdir,
					&line[sizeof(STR_LIBDIR) - 1]);
			else
#			undef  STR_DL_DEPLIBS
#			define STR_DL_DEPLIBS	"dl_dependency_libs="
			if (strncmp(line, STR_DL_DEPLIBS,
				sizeof(STR_DL_DEPLIBS) - 1) == 0)
				error = trim(&deplibs,
					&line[sizeof(STR_DL_DEPLIBS) - 1]);
			else
			if (strcmp(line, "installed=yes\n") == 0)
				installed = 1;
			else
			if (strcmp(line, "installed=no\n") == 0)
				installed = 0;
			else
#			undef  STR_LIBRARY_NAMES
#			define STR_LIBRARY_NAMES "library_names="
			if (! dlname &&
			    strncmp(line, STR_LIBRARY_NAMES,
				sizeof(STR_LIBRARY_NAMES) - 1) == 0) {
			  char *last_libname;
			  error = trim(&dlname,
				       &line[sizeof(STR_LIBRARY_NAMES) - 1]);
			  if (! error && dlname &&
			      (last_libname = strrchr(dlname, ' ')) != NULL) {
			    last_libname = strdup(last_libname + 1);
			    free(dlname);
			    dlname = last_libname;
			  }
			}
			if (error)
				break;
		}
		fclose(file);
		lt_dlfree(line);
		/* allocate the handle */
		handle = (lt_dlhandle) lt_dlmalloc(sizeof(lt_dlhandle_t));
		if (!handle || error) {
			if (handle)
				lt_dlfree(handle);
			if (!error)
				last_error = LT_DLSTRERROR(NO_MEMORY);
			free_vars(dlname, old_name, libdir, deplibs);
			/* handle is already set to 0 */
			goto cleanup;
		}
		handle->info.ref_count = 0;
		if (load_deplibs(handle, deplibs) == 0) {
			newhandle = handle;
			/* find_module may replace newhandle */
			if (find_module(&newhandle, dir, libdir, 
					dlname, old_name, installed)) {
				unload_deplibs(handle);
				error = 1;
			}
		} else
			error = 1;
		free_vars(dlname, old_name, libdir, deplibs);
		if (error) {
			lt_dlfree(handle);
			handle = 0;
			goto cleanup;
		}
		if (handle != newhandle)
			unload_deplibs(handle);
	} else {
		/* not a libtool module */
		handle = (lt_dlhandle) lt_dlmalloc(sizeof(lt_dlhandle_t));
		if (!handle) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			/* handle is already set to 0 */
			goto cleanup;
		}
		handle->info.ref_count = 0;
		/* non-libtool modules don't have dependencies */
		handle->depcount = 0;
		handle->deplibs = 0;
		newhandle = handle;
		if (tryall_dlopen(&newhandle, filename)
		    && (dir
			|| (!find_file(basename, user_search_path,
					  0, &newhandle)
			    && !find_file(basename,
					  getenv("LTDL_LIBRARY_PATH"),
					  0, &newhandle)
#ifdef LTDL_SHLIBPATH_VAR
			    && !find_file(basename,
					  getenv(LTDL_SHLIBPATH_VAR),
					  0, &newhandle)
#endif
#ifdef LTDL_SYSSEARCHPATH
			    && !find_file(basename, sys_search_path,
					  0, &newhandle)
#endif
				))) {
			lt_dlfree(handle);
			handle = 0;
			goto cleanup;
		}
	}
register_handle:
	if (newhandle != handle) {
		lt_dlfree(handle);
		handle = newhandle;
	}
	if (!handle->info.ref_count) {
		handle->info.ref_count = 1;
		handle->info.name = name;
		handle->next = handles;
		handles = handle;
		name = 0;	/* don't free this during `cleanup' */
	}
	last_error = saved_error;
cleanup:
	if (dir)
		lt_dlfree(dir);
	if (name)
		lt_dlfree(name);
	if (canonical)
		lt_dlfree(canonical);
	return handle;
}

lt_dlhandle
lt_dlopenext (filename)
	const char *filename;
{
	lt_dlhandle handle;
	char	*tmp;
	int	len;
	const char *saved_error = last_error;
	
	if (!filename)
		return lt_dlopen(filename);
	len = strlen(filename);
	if (!len) {
		last_error = LT_DLSTRERROR(FILE_NOT_FOUND);
		return 0;
	}
	/* try the normal file name */
	handle = lt_dlopen(filename);
	if (handle)
		return handle;
	/* try "filename.la" */
	tmp = (char*) lt_dlmalloc(len+4);
	if (!tmp) {
		last_error = LT_DLSTRERROR(NO_MEMORY);
		return 0;
	}
	strcpy(tmp, filename);
	strcat(tmp, ".la");
	handle = lt_dlopen(tmp);
	if (handle) {
		last_error = saved_error;
		lt_dlfree(tmp);
		return handle;
	}
#ifdef LTDL_SHLIB_EXT
	/* try "filename.EXT" */
	if (strlen(shlib_ext) > 3) {
		lt_dlfree(tmp);
		tmp = (char*) lt_dlmalloc(len + strlen(shlib_ext) + 1);
		if (!tmp) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			return 0;
		}
		strcpy(tmp, filename);
	} else
		tmp[len] = '\0';
	strcat(tmp, shlib_ext);
	handle = lt_dlopen(tmp);
	if (handle) {
		last_error = saved_error;
		lt_dlfree(tmp);
		return handle;
	}
#endif	
	last_error = LT_DLSTRERROR(FILE_NOT_FOUND);
	lt_dlfree(tmp);
	return 0;
}

int
lt_dlclose (handle)
	lt_dlhandle handle;
{
	lt_dlhandle cur, last;
	
	/* check whether the handle is valid */
	last = cur = handles;
	while (cur && handle != cur) {
		last = cur;
		cur = cur->next;
	}
	if (!cur) {
		last_error = LT_DLSTRERROR(INVALID_HANDLE);
		return 1;
	}
	handle->info.ref_count--;
	if (!handle->info.ref_count) {
		int	error;
	  	lt_dlloader_data_t data = handle->loader->dlloader_data;
	
		if (handle != handles)
			last->next = handle->next;
		else
			handles = handle->next;
		error = handle->loader->module_close(data, handle->module);
		error += unload_deplibs(handle);
		if (handle->info.filename)
			lt_dlfree(handle->info.filename);
		if (handle->info.name)
			lt_dlfree(handle->info.name);
		lt_dlfree(handle);
		return error;
	}
	return 0;
}

lt_ptr_t
lt_dlsym (handle, symbol)
	lt_dlhandle handle;
	const char *symbol;
{
	int	lensym;
	char	lsym[LTDL_SYMBOL_LENGTH];
	char	*sym;
	lt_ptr_t address;
	lt_dlloader_data_t data;

	if (!handle) {
		last_error = LT_DLSTRERROR(INVALID_HANDLE);
		return 0;
	}
	if (!symbol) {
		last_error = LT_DLSTRERROR(SYMBOL_NOT_FOUND);
		return 0;
	}
	lensym = strlen(symbol);
	if (handle->loader->sym_prefix)
		lensym += strlen(handle->loader->sym_prefix);
	if (handle->info.name)
		lensym += strlen(handle->info.name);
	if (lensym + LTDL_SYMBOL_OVERHEAD < LTDL_SYMBOL_LENGTH)
		sym = lsym;
	else
		sym = (char*) lt_dlmalloc(lensym + LTDL_SYMBOL_OVERHEAD + 1);
	if (!sym) {
		last_error = LT_DLSTRERROR(BUFFER_OVERFLOW);
		return 0;
	}
	data = handle->loader->dlloader_data;
	if (handle->info.name) {
		const char *saved_error = last_error;
		
		/* this is a libtool module */
		if (handle->loader->sym_prefix) {
			strcpy(sym, handle->loader->sym_prefix);
			strcat(sym, handle->info.name);
		} else
			strcpy(sym, handle->info.name);
		strcat(sym, "_LTX_");
		strcat(sym, symbol);
		/* try "modulename_LTX_symbol" */
		address = handle->loader->find_sym(data, handle->module, sym);
		if (address) {
			if (sym != lsym)
				lt_dlfree(sym);
			return address;
		}
		last_error = saved_error;
	}
	/* otherwise try "symbol" */
	if (handle->loader->sym_prefix) {
		strcpy(sym, handle->loader->sym_prefix);
		strcat(sym, symbol);
	} else
		strcpy(sym, symbol);
	address = handle->loader->find_sym(data, handle->module, sym);
	if (sym != lsym)
		lt_dlfree(sym);
	return address;
}

const char *
lt_dlerror LTDL_PARAMS((void))
{
	const char *error = last_error;
	
	last_error = 0;
	return error;
}

int
lt_dladdsearchdir (search_dir)
	const char *search_dir;
{
	if (!search_dir || !strlen(search_dir))
		return 0;
	if (!user_search_path) {
		user_search_path = strdup(search_dir);
		if (!user_search_path) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			return 1;
		}
	} else {
		char	*new_search_path = (char*)
			lt_dlmalloc(strlen(user_search_path) + 
				strlen(search_dir) + 2); /* ':' + '\0' == 2 */
		if (!new_search_path) {
			last_error = LT_DLSTRERROR(NO_MEMORY);
			return 1;
		}
		sprintf (new_search_path, "%s%c%s", user_search_path,
			 LTDL_PATHSEP_CHAR, search_dir);
		lt_dlfree(user_search_path);
		user_search_path = new_search_path;
	}
	return 0;
}

int
lt_dlsetsearchpath (search_path)
	const char *search_path;
{
	if (user_search_path)
		lt_dlfree(user_search_path);
	user_search_path = 0; /* reset the search path */
	if (!search_path || !strlen(search_path))
		return 0;
	user_search_path = strdup(search_path);
	if (!user_search_path)
		return 1;
	return 0;
}

const char *
lt_dlgetsearchpath LTDL_PARAMS((void))
{
	return user_search_path;
}

const lt_dlinfo *
lt_dlgetinfo (handle)
	lt_dlhandle handle;
{
	if (!handle) {
		last_error = LT_DLSTRERROR(INVALID_HANDLE);
		return 0;
	}
	return &(handle->info);
}

int
lt_dlforeach (func, data)
	int (*func) LTDL_PARAMS((lt_dlhandle handle, lt_ptr_t data));
	lt_ptr_t data;
{
	lt_dlhandle cur = handles;
	while (cur) {
		lt_dlhandle tmp = cur;
		cur = cur->next;
		if (func(tmp, data))
			return 1;
	}
	return 0;
}

int
lt_dlloader_add (place, dlloader, loader_name)
	lt_dlloader_t *place;
	const struct lt_user_dlloader *dlloader;
	const char *loader_name;
{
	lt_dlloader_t *node = 0, *ptr = 0;
	
	if ((dlloader == 0)	/* diagnose null parameters */
	    || (dlloader->module_open == 0)
	    || (dlloader->module_close == 0)
	    || (dlloader->find_sym == 0)) {
		last_error = LT_DLSTRERROR(INVALID_LOADER);
		return 1;
	}

	/* Create a new dlloader node with copies of the user callbacks.  */
	node = (lt_dlloader_t *) lt_dlmalloc (sizeof (lt_dlloader_t));
	if (node == 0) {
		last_error = LT_DLSTRERROR(NO_MEMORY);
		return 1;
	}
	node->next = 0;
	node->loader_name = loader_name;
	node->sym_prefix = dlloader->sym_prefix;
	node->dlloader_exit = dlloader->dlloader_exit;
	node->module_open = dlloader->module_open;
	node->module_close = dlloader->module_close;
	node->find_sym = dlloader->find_sym;
	node->dlloader_data = dlloader->dlloader_data;
	
	if (!loaders)
		/* If there are no loaders, NODE becomes the list! */
		loaders = node;
	else if (!place) {
		/* If PLACE is not set, add NODE to the end of the
		   LOADERS list. */
		for (ptr = loaders; ptr->next; ptr = ptr->next)
			/*NOWORK*/;
		ptr->next = node;
	} else if (loaders == place) {
		/* If PLACE is the first loader, NODE goes first. */
		node->next = place;
		loaders = node;
	} else {
		/* Find the node immediately preceding PLACE. */
		for (ptr = loaders; ptr->next != place; ptr = ptr->next)
			/*NOWORK*/;

		if (ptr->next != place) {
			last_error = LT_DLSTRERROR(INVALID_LOADER);
			return 1;
		}

		/* Insert NODE between PTR and PLACE. */
		node->next = place;
		ptr->next = node;
	}

	return 0;
}

int
lt_dlloader_remove (loader_name)
	const char *loader_name;
{
	lt_dlloader_t *place = lt_dlloader_find (loader_name);
	lt_dlhandle handle;
	int result = 0;

	if (!place) {
		last_error = LT_DLSTRERROR(INVALID_LOADER);
		return 1;
	}

	/* Fail if there are any open modules which use this loader. */
	for  (handle = handles; handle; handle = handle->next)
		if (handle->loader == place) {
			last_error = LT_DLSTRERROR(REMOVE_LOADER);
			return 1;
		}
	
	if (place == loaders)
		/* PLACE is the first loader in the list. */
		loaders = loaders->next;
	else {
		/* Find the loader before the one being removed. */
		lt_dlloader_t *prev;
		for (prev = loaders; prev->next; prev = prev->next)
			if (!strcmp (prev->next->loader_name, loader_name))
				break;

		place = prev->next;
		prev->next = prev->next->next;
	}
	if (place->dlloader_exit)
		result = place->dlloader_exit (place->dlloader_data);
	lt_dlfree (place);

	return result;
}

lt_dlloader_t *
lt_dlloader_next (place)
	lt_dlloader_t *place;
{
	return place ? place->next : loaders;
}
	
const char *
lt_dlloader_name (place)
	lt_dlloader_t *place;
{
	if (!place)
		last_error =  LT_DLSTRERROR(INVALID_LOADER);
	return place ? place->loader_name : 0;
}	

lt_dlloader_data_t *
lt_dlloader_data (place)
	lt_dlloader_t *place;
{
	if (!place)
		last_error =  LT_DLSTRERROR(INVALID_LOADER);
	return place ? &(place->dlloader_data) : 0;
}	

lt_dlloader_t *
lt_dlloader_find (loader_name)
	const char *loader_name;
{
	lt_dlloader_t *place = 0;

	for (place = loaders; place; place = place->next)
		if (strcmp (place->loader_name, loader_name) == 0)
			break;

	return place;
}

static const char **user_error_strings = 0;
static int errorcode = LTDL_ERROR_MAX;

int
lt_dladderror (diagnostic)
	const char *diagnostic;
{
	int index = errorcode - LTDL_ERROR_MAX;
	const char **temp = 0;

	/* realloc is not entirely portable, so simulate it using
	   lt_dlmalloc and lt_dlfree. */
	temp = (const char **) lt_dlmalloc ((1+index) * sizeof(const char*));
	if (temp == 0) {
		last_error = LT_DLSTRERROR(NO_MEMORY);
		return -1;
	}

	/* Build the new vector in the memory addressed by temp. */
	temp[index] = diagnostic;
	while (--index >= 0)
		temp[index] = user_error_strings[index];

	lt_dlfree (user_error_strings);
	user_error_strings = temp;
	return errorcode++;
}

int
lt_dlseterror (index)
	int index;
{
	if (index >= errorcode || index < 0) {
		last_error = LT_DLSTRERROR(INVALID_ERRORCODE);
		return 1;
	}
	
	if (index < LTDL_ERROR_MAX)
		last_error = ltdl_error_strings[errorcode];
	else
		last_error = user_error_strings[errorcode - LTDL_ERROR_MAX];

	return 0;
}
