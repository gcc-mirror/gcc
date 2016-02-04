/* EABI unaligned read/write functions.

   Copyright (C) 2005-2016 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

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

int __aeabi_uread4 (void *);
int __aeabi_uwrite4 (int, void *);
long long __aeabi_uread8 (void *);
long long __aeabi_uwrite8 (long long, void *);

struct __attribute__((packed)) u4 { int data; };
struct __attribute__((packed)) u8 { long long data; };

int
__aeabi_uread4 (void *ptr)
{
  return ((struct u4 *) ptr)->data;
}

int
__aeabi_uwrite4 (int data, void *ptr)
{
  ((struct u4 *) ptr)->data = data;
  return data;
}

long long
__aeabi_uread8 (void *ptr)
{
  return ((struct u8 *) ptr)->data;
}

long long
__aeabi_uwrite8 (long long data, void *ptr)
{
  ((struct u8 *) ptr)->data = data;
  return data;
}
