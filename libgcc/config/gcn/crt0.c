/* Copyright (C) 2017-2020 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

typedef long long size_t;

/* Provide an entry point symbol to silence a linker warning.  */
void _start() {}

#ifdef USE_NEWLIB_INITFINI

extern void __libc_init_array (void) __attribute__((weak));
extern void __libc_fini_array (void) __attribute__((weak));

__attribute__((amdgpu_hsa_kernel ()))
void _init_array()
{
  __libc_init_array ();
}

__attribute__((amdgpu_hsa_kernel ()))
void _fini_array()
{
  __libc_fini_array ();
}

#endif

/* These magic symbols are provided by the linker.  */
extern void (*__preinit_array_start []) (void) __attribute__((weak));
extern void (*__preinit_array_end []) (void) __attribute__((weak));
extern void (*__init_array_start []) (void) __attribute__((weak));
extern void (*__init_array_end []) (void) __attribute__((weak));
extern void (*__fini_array_start []) (void) __attribute__((weak));
extern void (*__fini_array_end []) (void) __attribute__((weak));

__attribute__((amdgpu_hsa_kernel ()))
void _init_array()
{
  /* Iterate over all the init routines.  */
  size_t count;
  size_t i;

  count = __preinit_array_end - __preinit_array_start;
  for (i = 0; i < count; i++)
    __preinit_array_start[i] ();

  count = __init_array_end - __init_array_start;
  for (i = 0; i < count; i++)
    __init_array_start[i] ();
}

__attribute__((amdgpu_hsa_kernel ()))
void _fini_array()
{
  size_t count;
  size_t i;

  count = __fini_array_end - __fini_array_start;
  for (i = count; i > 0; i--)
    __fini_array_start[i-1] ();
}
