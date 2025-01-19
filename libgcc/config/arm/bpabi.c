/* Miscellaneous BPABI functions.

   Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

extern long long __divdi3 (long long, long long);
extern unsigned long long __udivdi3 (unsigned long long,
				     unsigned long long);
extern long long __gnu_ldivmod_helper (long long, long long, long long *);


long long
__gnu_ldivmod_helper (long long a,
		      long long b,
		      long long *remainder)
{
  long long quotient;

  quotient = __divdi3 (a, b);
  *remainder = a - b * quotient;
  return quotient;
}

