/* __dso_handle initialization for AIX.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
   Written by David Edelsohn, IBM.

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

#ifdef SHARED
void *__dso_handle = &__dso_handle;
#else
void *__dso_handle = 0;
#endif

extern void __cxa_finalize (void *);

/* Add __cxa_finalize call to beginning of destructors list.  */
void __init_aix_libgcc_cxa_atexit (void) __attribute__ ((destructor (65535)));

void
__init_aix_libgcc_cxa_atexit (void)
{
  __cxa_finalize (__dso_handle);
}

