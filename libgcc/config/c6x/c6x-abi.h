/* Header file for C6X ABI versions of libgcc functions.
   Copyright (C) 2011-2025 Free Software Foundation, Inc.
   Contributed by Bernd Schmidt <bernds@codesourcery.com>

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

/* Make __c6xabi_AEABI_NAME an alias for __GCC_NAME.  */
#define RENAME_LIBRARY(GCC_NAME, AEABI_NAME)			\
  __asm__ (".globl\t__c6xabi_" #AEABI_NAME "\n"		\
	   ".set\t__c6xabi_" #AEABI_NAME			\
	   ", __gnu_" #GCC_NAME "\n");

/* Rename helper functions to the names specified in the C6000 ELF ABI.  */
#ifdef L_divsi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (divsi3, divi)
#endif
#ifdef L_divdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (divdi3, divlli)
#endif
#ifdef L_udivsi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (udivsi3, divu)
#endif
#ifdef L_udivdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (udivdi3, divull)
#endif
#ifdef L_udivmoddi4
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (udivmoddi4, divremull)
#endif
#ifdef L_modsi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (modsi3, remi)
#endif
#ifdef L_moddi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (moddi3, remlli)
#endif
#ifdef L_umodsi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (umodsi3, remu)
#endif
#ifdef L_umoddi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (umoddi3, remull)
#endif
#ifdef L_negdi2
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (negdi2, negll)
#endif
#ifdef L_muldi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (muldi3, mpyll)
#endif
#ifdef L_ashrdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (ashrdi3, llshr)
#endif
#ifdef L_lshrdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (lshrdi3, llshru)
#endif
#ifdef L_ashldi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (ashldi3, llshl)
#endif

/* The following are excluded from softfp due to softfp_exclude_libgcc2,
   so we rename them here rather than in sfp-machine.h.  */
#ifdef L_fixdfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixdfdi, fixdlli)
#endif
#ifdef L_fixunsdfsi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixunsdfsi, fixdu)
#endif
#ifdef L_fixunsdfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixunsdfdi, fixdull)
#endif
#ifdef L_fixsfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixsfdi, fixflli)
#endif
#ifdef L_fixunssfsi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixunssfsi, fixfu)
#endif
#ifdef L_fixunssfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixunssfdi, fixfull)
#endif
#ifdef L_floatdidf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatdidf, fltllid)
#endif
#ifdef L_floatundidf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatundidf, fltulld)
#endif
#ifdef L_floatdisf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatdisf, fltllif)
#endif
#ifdef L_floatundisf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatundisf, fltullf)
#endif

#define LIBGCC2_GNU_PREFIX
