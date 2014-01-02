/* Copyright (C) 2007-2013 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.

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

/* This file selects the single-precision parts of fp-bit.c that are
   still needed for some ARC hardware variants; it also renames functions
   that duplicate asm-coded functionality so that their results can be
   used to compare with the optimized versions for debugging.  */

#define ARC_FP_DEBUG 1
#define FINE_GRAINED_LIBRARIES
#if !defined (__ARC_NORM__) || ARC_FP_DEBUG
#define L_pack_sf
#define L_unpack_sf
#define L_make_sf
#define L_thenan_sf
#endif
#ifndef __ARC_NORM__
#define L_addsub_sf
#define L_mul_sf
#define L_div_sf
#define L_sf_to_df
#define L_si_to_sf
#define L_sf_to_si
#define L_usi_to_sf
#elif ARC_FP_DEBUG
#define L_addsub_sf
#define __addsf3 __addsf3_c
#define __subsf3 __subsf3_c
#define L_mul_sf
#define __mulsf3 __mulsf3_c
#define L_div_sf
#define __divsf3 __divsf3_c
#define L_sf_to_df
#define __extendsfdf2 __extendsfdf2_c
#define L_si_to_sf
#define __floatsisf __floatsisf_c
#define L_sf_to_si
#define __fixsfsi __fixsfsi_c
#define L_usi_to_sf
#define __floatunsisf __floatunsisf_c
#endif
#ifndef __ARC_NORM__
#define L_fpcmp_parts_sf
#define L_compare_sf
#define L_eq_sf
#define L_ne_sf
#define L_gt_sf
#define L_ge_sf
#define L_lt_sf
#define L_le_sf
#define L_unord_sf
#define L_negate_sf
#elif ARC_FP_DEBUG
#define L_fpcmp_parts_sf
#define L_eq_sf
#define __eqsf2 __eqsf2_c
#define L_gt_sf
#define __gtsf2 __gtsf2_c
#define L_ge_sf
#define __gesf2 __gesf2_c
#define L_unord_sf
#define __unordsf2 __unordsf2_c
#endif
