/*
 * Copyright (C) 2001-2016 Free Software Foundation, Inc.
 * Contributed by Richard Henderson (rth@redhat.com)
 *
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 *
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

/* Assume OSF/1 compatible interfaces.  */

extern void __ieee_set_fp_control (unsigned long int);

#define IEEE_MAP_DMZ  (1UL<<12)       /* Map denorm inputs to zero */
#define IEEE_MAP_UMZ  (1UL<<13)       /* Map underflowed outputs to zero */

static void __attribute__((constructor))
set_fast_math (void)
{
  __ieee_set_fp_control (IEEE_MAP_DMZ | IEEE_MAP_UMZ);
}
