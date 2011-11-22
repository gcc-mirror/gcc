/* Provide the runtime intrastructure the transactional memory lib.
   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by Iain Sandoe <iains@gcc.gnu.org>

   This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <mach-o/dyld.h>

/* not listed in mach-o/dyld.h for some reason.  */
extern char * getsectdata (const char*,const char*,unsigned long*); 

#define WEAK __attribute__((weak))

#ifndef ATTRIBUTE_UNUSED
#define ATTRIBUTE_UNUSED __attribute__((unused))
#endif

extern void _ITM_registerTMCloneTable (void *, size_t) WEAK;
extern void _ITM_deregisterTMCloneTable (void *) WEAK;

#ifdef START

void __doTMRegistrations (void) __attribute__ ((constructor));

void __doTMRegistrations (void)
{
  char * tm_clone_table_sect_data;
  unsigned long tmct_siz;
  
  tm_clone_table_sect_data = getsectdata ("__DATA",
					  "__tm_clone_table",
					  &tmct_siz);
  tmct_siz /= (sizeof (size_t) * 2);
  if (_ITM_registerTMCloneTable != NULL
      && tm_clone_table_sect_data != NULL
      && tmct_siz > 0)
    _ITM_registerTMCloneTable (tm_clone_table_sect_data, (size_t)tmct_siz);
}

#endif

#ifdef END

void __doTMdeRegistrations (void) __attribute__ ((destructor));

void __doTMdeRegistrations (void)
{
  char * tm_clone_table_sect_data;
  unsigned long tmct_siz;
  
  tm_clone_table_sect_data = getsectdata ("__DATA",
					  "__tm_clone_table",
					  &tmct_siz);
  
  if (_ITM_deregisterTMCloneTable != NULL
      && tm_clone_table_sect_data != NULL
      && tmct_siz > 0)
    _ITM_deregisterTMCloneTable (tm_clone_table_sect_data);

}

/* Provide dumy funcs for the weak ones - needed on most Darwin versions
   for now.  */

void _ITM_registerTMCloneTable (void *n ATTRIBUTE_UNUSED, size_t s ATTRIBUTE_UNUSED)
{}
void _ITM_deregisterTMCloneTable (void *n ATTRIBUTE_UNUSED)
{}
#endif
