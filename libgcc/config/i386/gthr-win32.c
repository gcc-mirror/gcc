/* Implementation of threads compatibility routines for libgcc2.  */

/* Copyright (C) 1999-2025 Free Software Foundation, Inc.

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

/* Get the out-of-line version of the inline routines.  */

#define __GTHREAD_WIN32_ACTIVE_P() 1
#define __GTHREAD_WIN32_INLINE

#define __gthread_detach __gthr_win32_detach
#define __gthread_equal __gthr_win32_equal
#define __gthread_yield __gthr_win32_yield
#define __gthread_once __gthr_win32_once
#define __gthread_key_create __gthr_win32_key_create
#define __gthread_key_delete __gthr_win32_key_delete
#define __gthread_getspecific __gthr_win32_getspecific
#define __gthread_setspecific __gthr_win32_setspecific
#define __gthread_mutex_init_function __gthr_win32_mutex_init_function
#define __gthread_mutex_destroy __gthr_win32_mutex_destroy
#define __gthread_mutex_lock __gthr_win32_mutex_lock
#define __gthread_mutex_trylock __gthr_win32_mutex_trylock
#define __gthread_mutex_unlock __gthr_win32_mutex_unlock
#define __gthread_recursive_mutex_trylock __gthr_win32_recursive_mutex_trylock

#include "gthr-win32.h"

/* Check the sizes of the local version of the Win32 types.  */

#define CHECK_SIZE_OF(TYPE) \
  typedef int assertion[sizeof(__gthr_win32_##TYPE) == sizeof(TYPE) ? 1 : -1];

CHECK_SIZE_OF (DWORD)
CHECK_SIZE_OF (HANDLE)
CHECK_SIZE_OF (CRITICAL_SECTION)
