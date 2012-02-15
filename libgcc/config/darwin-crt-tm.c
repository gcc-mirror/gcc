/* Provide the runtime infrastructure for the transactional memory lib.
   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by Iain Sandoe <iains@gcc.gnu.org>

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

#include "tsystem.h"
#include <stddef.h>
#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>

#ifdef __LP64__
#define GET_DATA_TMCT(mh,size) \
  getsectdatafromheader_64 ((struct mach_header_64*) mh, \
			    "__DATA", "__tm_clone_table", (uint64_t *)size)
#else
#define GET_DATA_TMCT(mh,size) \
  getsectdatafromheader (mh, "__DATA", "__tm_clone_table", (uint32_t *)size)
#endif

#define WEAK __attribute__((weak))
#define UNUSED __attribute__((unused))

extern void _ITM_registerTMCloneTable (void *, size_t) WEAK;
extern void _ITM_deregisterTMCloneTable (void *) WEAK;

#if defined(START) || defined(END)
static inline void *getTMCloneTable (const void *f, size_t *tmct_siz)
{
  char *tmct_fixed, *tmct = NULL;
  unsigned int i, img_count; 
  struct mach_header *mh;
  Dl_info info;
  
  if (! dladdr (f, &info) || info.dli_fbase == NULL)
    abort ();
  
  mh = (struct mach_header *) info.dli_fbase;
  tmct_fixed = GET_DATA_TMCT (mh, tmct_siz);
  *tmct_siz /= (sizeof (size_t) * 2);
  /* No tm_clone_table or no clones. */
  if (tmct_fixed == NULL || *tmct_siz == 0)
    return NULL; 

  img_count = _dyld_image_count();
  for (i = 0; i < img_count && tmct == NULL; i++)
    {
      if (mh == _dyld_get_image_header(i))
	tmct = tmct_fixed + (unsigned long)_dyld_get_image_vmaddr_slide(i); 
    }

  return tmct;
}
#endif

#ifdef START

void __doTMRegistrations (void) __attribute__ ((constructor));

void __doTMRegistrations (void)
{
  size_t tmct_siz;
  void *tmct;

  tmct = getTMCloneTable ((const void *)&__doTMRegistrations, &tmct_siz);
  if (_ITM_registerTMCloneTable != NULL && tmct != NULL)
    _ITM_registerTMCloneTable (tmct, (size_t)tmct_siz);
}

#endif

#ifdef END

void __doTMdeRegistrations (void) __attribute__ ((destructor));

void __doTMdeRegistrations (void)
{
  size_t tmct_siz;
  void *tmct;

  tmct = getTMCloneTable ((const void *)&__doTMdeRegistrations, &tmct_siz);
  if (_ITM_deregisterTMCloneTable != NULL && tmct != NULL)
    _ITM_deregisterTMCloneTable (tmct);
}

#endif

/* Provide dummy functions to satisfy linkage for versions of the Darwin 
   tool-chain that that can't handle undefined weak refs at the link stage.
   ??? Define these dummy functions only when !HAVE_ELF_STYLE_WEAKREF. */

extern void *__cxa_allocate_exception (size_t) WEAK;
extern void __cxa_throw (void *, void *, void *) WEAK;
extern void *__cxa_begin_catch (void *) WEAK;
extern void *__cxa_end_catch (void) WEAK;
extern void __cxa_tm_cleanup (void *, void *, unsigned int) WEAK;

extern void *_ZnwX (size_t) WEAK;
extern void _ZdlPv (void *) WEAK;
extern void *_ZnaX (size_t) WEAK;
extern void _ZdaPv (void *) WEAK;

typedef const struct nothrow_t { } *c_nothrow_p;

extern void *_ZnwXRKSt9nothrow_t (size_t, c_nothrow_p) WEAK;
extern void _ZdlPvRKSt9nothrow_t (void *, c_nothrow_p) WEAK;
extern void *_ZnaXRKSt9nothrow_t (size_t, c_nothrow_p) WEAK;
extern void _ZdaPvRKSt9nothrow_t (void *, c_nothrow_p) WEAK;

void *__cxa_allocate_exception (size_t s UNUSED) { return NULL; }
void __cxa_throw (void * a UNUSED, void * b UNUSED, void * c UNUSED)
  { return; }
void *__cxa_begin_catch (void * a UNUSED) { return NULL; }
void *__cxa_end_catch (void) { return NULL; }
void __cxa_tm_cleanup (void * a UNUSED, void * b UNUSED, unsigned int c UNUSED)
  { return; }

void *_ZnwX (size_t s UNUSED) { return NULL; }
void _ZdlPv (void * a UNUSED) { return; }
void *_ZnaX (size_t s UNUSED) { return NULL; }
void _ZdaPv (void * a UNUSED) { return; }

void *_ZnwXRKSt9nothrow_t (size_t s UNUSED, c_nothrow_p b UNUSED)
  { return NULL; }
void _ZdlPvRKSt9nothrow_t (void * a UNUSED, c_nothrow_p b UNUSED)  { return; }
void *_ZnaXRKSt9nothrow_t (size_t s UNUSED, c_nothrow_p b UNUSED)
  { return NULL; }
void _ZdaPvRKSt9nothrow_t (void * a UNUSED, c_nothrow_p b UNUSED) { return; }

