/* Xtensa configuration settings.
   Copyright (C) 2001,2002,2003 Free Software Foundation, Inc.
   Contributed by Bob Wilson (bwilson@tensilica.com) at Tensilica.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef XTENSA_CONFIG_H
#define XTENSA_CONFIG_H

/* The macros defined here match those with the same names in the Xtensa
   compile-time HAL (Hardware Abstraction Layer).  Please refer to the
   Xtensa System Software Reference Manual for documentation of these
   macros.  */

#define XCHAL_HAVE_BE			1
#define XCHAL_HAVE_DENSITY		1
#define XCHAL_HAVE_CONST16		0
#define XCHAL_HAVE_ABS			1
#define XCHAL_HAVE_ADDX			1
#define XCHAL_HAVE_L32R			1
#define XCHAL_HAVE_MAC16		0
#define XCHAL_HAVE_MUL16		0
#define XCHAL_HAVE_MUL32		0
#define XCHAL_HAVE_DIV32		0
#define XCHAL_HAVE_NSA			1
#define XCHAL_HAVE_MINMAX		0
#define XCHAL_HAVE_SEXT			0
#define XCHAL_HAVE_LOOPS		1
#define XCHAL_HAVE_BOOLEANS		0
#define XCHAL_HAVE_FP			0
#define XCHAL_HAVE_FP_DIV		0
#define XCHAL_HAVE_FP_RECIP		0
#define XCHAL_HAVE_FP_SQRT		0
#define XCHAL_HAVE_FP_RSQRT		0
#define XCHAL_HAVE_WINDOWED		1

#define XCHAL_ICACHE_SIZE		8192
#define XCHAL_DCACHE_SIZE		8192
#define XCHAL_ICACHE_LINESIZE		16
#define XCHAL_DCACHE_LINESIZE		16
#define XCHAL_ICACHE_LINEWIDTH		4
#define XCHAL_DCACHE_LINEWIDTH		4
#define XCHAL_DCACHE_IS_WRITEBACK	0

#define XCHAL_HAVE_MMU			1
#define XCHAL_MMU_MIN_PTE_PAGE_SIZE	12

#define XCHAL_HAVE_DEBUG		1
#define XCHAL_NUM_IBREAK		2
#define XCHAL_NUM_DBREAK		2
#define XCHAL_DEBUGLEVEL		4

#define XCHAL_EXTRA_SA_SIZE             0
#define XCHAL_EXTRA_SA_ALIGN            1

#endif /* !XTENSA_CONFIG_H */
