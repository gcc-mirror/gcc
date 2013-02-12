/* Stub functions.
   Copyright (C) 2006-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
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

#include "tsystem.h"

#ifdef L_register_frame_info
struct object;
void  __register_frame_info (const void *, struct object *);
void
__register_frame_info (__attribute__((unused)) const void *p,
		       __attribute__((unused)) struct object *ob)
{
}
#endif

#ifdef L_deregister_frame_info
void *__deregister_frame_info (const void *);
void *
__deregister_frame_info (__attribute__((unused)) const void *p)
{
  return (void *)0;
}
#endif

#ifdef L_ITM_registerTMCloneTable
struct object;
void  _ITM_registerTMCloneTable (const void *, size_t);
void
_ITM_registerTMCloneTable (__attribute__((unused)) const void *p,
			   __attribute__((unused)) size_t s)
{
}
#endif

#ifdef L_ITM_deregisterTMCloneTable
void _ITM_deregisterTMCloneTable (const void *);
void
_ITM_deregisterTMCloneTable (__attribute__((unused)) const void *p)
{
}
#endif

#ifdef L_cxa_finalize
void __cxa_finalize (void *);
void
__cxa_finalize (__attribute__((unused)) void *p)
{
}
#endif

#ifdef L_Jv_RegisterClasses
void _Jv_RegisterClasses (void *);
void
_Jv_RegisterClasses (__attribute__((unused)) void *p)
{
}
#endif

#ifdef L_pthread_default_stacksize_np
int pthread_default_stacksize_np (unsigned long, unsigned long *);
int
pthread_default_stacksize_np (__attribute__((unused)) unsigned long new,
			      unsigned long *old)
{
  if (old)
    *old = 0;
  return 0;
}
#endif

#ifdef L_pthread_mutex_lock
int pthread_mutex_lock (void);
int
pthread_mutex_lock (void)
{
  return 0;
}
#endif

#ifdef L_pthread_mutex_unlock
int pthread_mutex_unlock (void);
int
pthread_mutex_unlock (void)
{
  return 0;
}
#endif

#ifdef L_pthread_once
int pthread_once (void);
int
pthread_once (void)
{
  return 0;
}
#endif
