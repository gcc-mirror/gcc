/* Header file for RX ABI versions of libgcc functions.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Make __COM_<RX_NAME> an alias for __<GCC_NAME>.  */
#define RENAME_LIBRARY(GCC_NAME, RX_NAME)		\
  __asm__ (".globl\t__COM_" #RX_NAME "\n"		\
	   ".set\t__COM_" #RX_NAME ", ___" #GCC_NAME "\n");


/* The long-long aliases...  */

#ifdef L_muldi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (muldi3, MUL64)
#endif

#ifdef L_divdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (divdi3, DIV64s)
#endif

#ifdef L_udivdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (udivdi3, DIV64u)
#endif

#ifdef L_ashldi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (ashldi3, SHLL64)
#endif

#ifdef L_lshrdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (lshrdi3, SHLR64)
#endif

#ifdef L_ashrdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (ashrdi3, SHAR64)
#endif

#ifdef L_fixsfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixsfdi, CONVf64s)
#endif

#ifdef L_fixunssfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixunssfdi, CONVf64u)
#endif

#ifdef L_floatdisf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatdisf, CONV64sf)
#endif

#ifdef L_floatundisf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatundisf, CONV64uf)
#endif

#ifdef L_moddi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (moddi3, MOD64s)
#endif

#ifdef L_umoddi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (umoddi3, MOD64u)
#endif



#ifdef __RX_64BIT_DOUBLES__

/* Float (32-bit) aliases...  */

#ifdef L_sf_to_si
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixsfsi, CONVf32s)
#endif

#ifdef L_fixunssfsi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixunssfsi, CONVf32u)
#endif

#ifdef L_addsub_sf
#define DECLARE_LIBRARY_RENAMES \
  RENAME_LIBRARY (addsf3, ADDf) \
  RENAME_LIBRARY (subsf3, SUBf)
#endif

#ifdef L_mul_sf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (mulsf3, MULf)
#endif

#ifdef L_div_sf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (divsf3, DIVf)
#endif

/* Double (64-bit) aliases...  */

#ifdef L_addsub_df
#define DECLARE_LIBRARY_RENAMES \
  RENAME_LIBRARY (adddf3, ADDd) \
  RENAME_LIBRARY (subdf3, SUBd)
#endif

#ifdef L_mul_df
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (muldf3, MULd)
#endif

#ifdef L_div_df
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (divdf3, DIVd)
#endif

#ifdef L_fixdfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixdfdi, CONVd64s)
#endif

#ifdef L_fixunsdfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixunsdfdi, CONVd64u)
#endif

#ifdef L_floatdidf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatdisf, CONV64sd)
#endif

#ifdef L_floatundidf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatdisf, CONV64ud)
#endif

#ifdef L_df_to_si
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixdfsi, CONVd32s)
#endif

#ifdef L_fixunsdfsi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixunsdfsi, CONVd32u)
#endif

#ifdef L_si_to_df
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatsidf, CONV32sd)
#endif

#ifdef L_usi_to_df
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatunsidf, CONV32ud)
#endif

#ifdef L_sf_to_df
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (extendsfdf2, CONVfd)
#endif

#ifdef L_df_to_sf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (truncdfsf2, CONVdf)
#endif

#ifdef L_negate_df
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (negdf2, NEGd)
#endif

#ifdef L_si_to_sf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatsisf, CONV32sf)
#endif

#ifdef L_usi_to_sf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatunsisf, CONV32uf)
#endif

/* The 64-bit comparison functions do not have aliases because libgcc2
   does not provide them.  Instead they have to be supplied in
   rx-abi-functions.c.  */


#else /* 32-bit doubles.  */


#ifdef L_addsub_sf
#define DECLARE_LIBRARY_RENAMES \
  RENAME_LIBRARY (addsf3, ADDd) \
  RENAME_LIBRARY (subsf3, SUBd) \
  RENAME_LIBRARY (addsf3, ADDf) \
  RENAME_LIBRARY (subsf3, SUBf)
#endif

#ifdef L_mul_sf
#define DECLARE_LIBRARY_RENAMES \
  RENAME_LIBRARY (mulsf3, MULd) \
  RENAME_LIBRARY (mulsf3, MULf)
#endif

#ifdef L_div_sf
#define DECLARE_LIBRARY_RENAMES \
  RENAME_LIBRARY (divsf3, DIVd) \
  RENAME_LIBRARY (divsf3, DIVf)
#endif

#ifdef L_sf_to_si
#define DECLARE_LIBRARY_RENAMES \
  RENAME_LIBRARY (fixsfsi, CONVd32s) \
  RENAME_LIBRARY (fixsfsi, CONVf32s)
#endif

#ifdef L_fixunssfsi
#define DECLARE_LIBRARY_RENAMES \
  RENAME_LIBRARY (fixunssfsi, CONVd32u) \
  RENAME_LIBRARY (fixunssfsi, CONVf32u)
#endif

#ifdef L_si_to_sf
#define DECLARE_LIBRARY_RENAMES \
  RENAME_LIBRARY (floatsisf, CONV32sd) \
  RENAME_LIBRARY (floatsisf, CONV32sf)
#endif

#ifdef L_usi_to_sf
#define DECLARE_LIBRARY_RENAMES \
  RENAME_LIBRARY (floatunsisf, CONV32ud) \
  RENAME_LIBRARY (floatunsisf, CONV32uf)
#endif

#ifdef L_negate_sf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (negsf2, NEGd)
#endif

#endif /* 64-bit vs 32-bit doubles.  */
