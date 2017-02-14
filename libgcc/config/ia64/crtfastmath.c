/* Copyright (C) 2001-2017 Free Software Foundation, Inc.
   Contributed by David Mosberger <davidm@hpl.hp.com>.

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

/* We could call fesetenv() here but that would create a confusing
   dependency on libm (since that is where fesetenv() gets defined.
   To avoid this, just do everything locally.  */
#define FE_NONIEEE_ENV 0x0009a04d0270037f

static void __attribute__((constructor))
__ia64_set_fast_math (void)
{
  __asm__ __volatile__ ("mov.m ar.fpsr=%0" : : "r"(FE_NONIEEE_ENV));
}
