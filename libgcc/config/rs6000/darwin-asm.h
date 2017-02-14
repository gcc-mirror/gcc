/*  Macro definitions to used to support 32/64-bit code in Darwin's
 *  assembly files.
 *
 *   Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

/* These are donated from /usr/include/architecture/ppc . */

#if defined(__ppc64__)
#define MODE_CHOICE(x, y) y
#else
#define MODE_CHOICE(x, y) x
#endif

#define cmpg    MODE_CHOICE(cmpw, cmpd)
#define lg      MODE_CHOICE(lwz, ld)
#define stg     MODE_CHOICE(stw, std)
#define lgx     MODE_CHOICE(lwzx, ldx)
#define stgx    MODE_CHOICE(stwx, stdx)
#define lgu     MODE_CHOICE(lwzu, ldu)
#define stgu    MODE_CHOICE(stwu, stdu)
#define lgux    MODE_CHOICE(lwzux, ldux)
#define stgux   MODE_CHOICE(stwux, stdux)
#define lgwa    MODE_CHOICE(lwz, lwa)

#define g_long  MODE_CHOICE(long, quad)         /* usage is ".g_long" */

#define GPR_BYTES       MODE_CHOICE(4,8)        /* size of a GPR in bytes */
#define LOG2_GPR_BYTES  MODE_CHOICE(2,3)        /* log2(GPR_BYTES) */

#define SAVED_LR_OFFSET MODE_CHOICE(8,16)	/* position of saved
						   LR in frame */
