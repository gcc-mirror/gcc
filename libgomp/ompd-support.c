/* Copyright (C) The GNU Toolchain Authors.
   Contributed by Mohamed Atef <mohamedatef1698@gmail.com>.
   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).
   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.
   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.
   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.
   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "ompd-support.h"

#ifdef __ELF__
#define OMPD_SECTION __attribute__ ((section ("OMPD")))
#else
#define OMPD_SECTION
#endif

#ifndef GOMP_NEEDS_THREAD_HANDLE
const unsigned short gompd_access_gomp_thread_handle
  __attribute__ ((used)) OMPD_SECTION = 0;
const unsigned short gompd_sizeof_gomp_thread_handle
  __attribute__ ((used)) OMPD_SECTION = 0;
#endif

/* Get offset of the member m in struct t.  */
#define gompd_get_offset(t, m) \
  const unsigned short gompd_access_##t##_##m __attribute__ ((used)) \
    OMPD_SECTION \
      = (unsigned short) offsetof (struct t, m);
  GOMPD_FOREACH_ACCESS (gompd_get_offset)
#undef gompd_get_offset
/* Get size of member m in struct t.  */
#define gompd_get_sizeof_member(t, m) \
  const unsigned short gompd_sizeof_##t##_##m __attribute__ ((used)) \
    OMPD_SECTION \
      = sizeof (((struct t *) NULL)->m);
  GOMPD_FOREACH_ACCESS (gompd_get_sizeof_member)
#undef gompd_get_sizeof_member
/* Get size of struct t.  */
#define gompd_get_size(t) \
  const unsigned short gompd_sizeof_##t##_ __attribute__ ((used)) \
    OMPD_SECTION \
      = sizeof (struct t);
  GOMPD_SIZES (gompd_get_size)
#undef gompd_get_size

const char **ompd_dll_locations = NULL;
unsigned short gompd_state;

void
gompd_load (void)
{
  gomp_debug (2, "OMP OMPD active\n");
  static const char *ompd_dll_locations_array[2]
    = {"libgompd" SONAME_SUFFIX (1) , NULL};
  gompd_state |= OMPD_ENABLED;
  ompd_dll_locations = &ompd_dll_locations_array[0];
  ompd_dll_locations_valid ();
}

#ifndef __ELF__
/* Dummy functions. they shoud not be optimized.  */

void __attribute__ ((noipa))
ompd_dll_locations_valid (void)
{
}

void __attribute__ ((noipa))
ompd_bp_parallel_begin (void)
{
}

void __attribute__ ((noipa))
ompd_bp_parallel_end (void)
{
}

void __attribute__ ((noipa))
ompd_bp_task_begin (void)
{
}

void __attribute__ ((noipa))
ompd_bp_task_end (void)
{
}

void __attribute__ ((noipa))
ompd_bp_thread_begin (void)
{
}

void __attribute__ ((noipa))
ompd_bp_thread_end (void)
{
}

void __attribute__ ((noipa))
ompd_bp_device_begin (void)
{
}

void __attribute__ ((noipa))
ompd_bp_device_end (void)
{
}
#endif /* __ELF__*/
