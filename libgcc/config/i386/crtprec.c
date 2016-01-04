/*
 * Copyright (C) 2007-2016 Free Software Foundation, Inc.
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

#ifndef _SOFT_FLOAT
#if __PREC == 32
 #define X87CW		(0 << 8)	/* Single precision (24 bits) */
#elif __PREC == 64
 #define X87CW		(2 << 8)	/* Double precision (53 bits) */
#elif __PREC == 80
 #define X87CW		(3 << 8)	/* Extended precision (64 bits) */
#else
 #error "Wrong precision requested."
#endif

#define X87CW_PCMASK	(3 << 8)

static void __attribute__((constructor))
set_precision (void)
{
  unsigned short int cwd;

  asm volatile ("fstcw\t%0" : "=m" (cwd));

  cwd &= ~X87CW_PCMASK;
  cwd |= X87CW;

  asm volatile ("fldcw\t%0" : : "m" (cwd));
}
#endif
