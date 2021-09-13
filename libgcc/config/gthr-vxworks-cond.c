/* Copyright (C) 2002-2021 Free Software Foundation, Inc.

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

/* Threads compatibility routines for libgcc2 for VxWorks.

   This file implements the GTHREAD_HAS_COND part of the interface
   exposed by gthr-vxworks.h.  */

#include "gthr.h"

#if __GTHREAD_HAS_COND

#include <taskLib.h>

/* --------------------------- Condition Variables ------------------------ */

void
__gthread_cond_init (__gthread_cond_t *cond)
{
  if (!cond)
    return;
  *cond = semBCreate (SEM_Q_FIFO, SEM_EMPTY);
}

int
__gthread_cond_destroy (__gthread_cond_t *cond)
{
  if (!cond)
    return ERROR;
  return __CHECK_RESULT (semDelete (*cond));
}

int
__gthread_cond_broadcast (__gthread_cond_t *cond)
{
  if (!cond)
    return ERROR;

  return __CHECK_RESULT (semFlush (*cond));
}

int
__gthread_cond_wait (__gthread_cond_t *cond,
		     __gthread_mutex_t *mutex)
{
  if (!cond)
    return ERROR;

  if (!mutex)
    return ERROR;

  int ret = __CHECK_RESULT (semExchange (*mutex, *cond, WAIT_FOREVER));

  __RETURN_ERRNO_IF_NOT_OK (semTake (*mutex, WAIT_FOREVER));

  return ret;
}

int
__gthread_cond_wait_recursive (__gthread_cond_t *cond,
			       __gthread_recursive_mutex_t *mutex)
{
  return __gthread_cond_wait (cond, mutex);
}

#endif
