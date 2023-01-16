/* Copyright (C) 1999-2023 Free Software Foundation, Inc.

   NOTE: This source is derived from an old version taken from the GNU C
   Library (glibc).

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

#include <stdlib.h>
#include "exit.h"


/* This is defined by newer gcc version unique for each module.  */
extern void *__dso_handle __attribute__ ((__weak__));


/* Register FUNC to be executed by `exit'.  */
int
#ifndef atexit
attribute_hidden
#endif
atexit (void (*func) (void))
{
  return __cxa_atexit ((void (*) (void *)) func, NULL,
		       &__dso_handle == NULL ? NULL : __dso_handle);
}
