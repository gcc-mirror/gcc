/* Xtensa configuration settings.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.

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
   Foundation, 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef XTENSA_DYNCONFIG_H
#define XTENSA_DYNCONFIG_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Config versioning.
 *
 * When new config entries need to be passed through dynconfig
 * create new xtensa_config_v<N> structure and put them there.
 * Declare new function xtensa_get_config_v<N> (void).
 * Define corresponding X*HAL_* macros by accessing xtensa_get_config_v<N> ().
 * Define macro XTENSA_CONFIG_V<N>_ENTRY_LIST by listing
 * XTENSA_CONFIG_ENTRY for every entry in the new structure.
 * Add constant definition for the new xtensa_config_v<N> to the
 * XTENSA_CONFIG_INSTANCE_LIST.
 * Add XTENSA_CONFIG_V<N>_ENTRY_LIST to the XTENSA_CONFIG_ENTRY_LIST.
 *
 * On the user side (gcc/binutils/...) add definition for the function
 * xtensa_get_config_v<N> (void).
 */

struct xtensa_config_v1
{
  int xchal_have_be;
  int xchal_have_density;
  int xchal_have_const16;
  int xchal_have_abs;
  int xchal_have_addx;
  int xchal_have_l32r;
  int xshal_use_absolute_literals;
  int xshal_have_text_section_literals;
  int xchal_have_mac16;
  int xchal_have_mul16;
  int xchal_have_mul32;
  int xchal_have_mul32_high;
  int xchal_have_div32;
  int xchal_have_nsa;
  int xchal_have_minmax;
  int xchal_have_sext;
  int xchal_have_loops;
  int xchal_have_threadptr;
  int xchal_have_release_sync;
  int xchal_have_s32c1i;
  int xchal_have_booleans;
  int xchal_have_fp;
  int xchal_have_fp_div;
  int xchal_have_fp_recip;
  int xchal_have_fp_sqrt;
  int xchal_have_fp_rsqrt;
  int xchal_have_fp_postinc;
  int xchal_have_dfp;
  int xchal_have_dfp_div;
  int xchal_have_dfp_recip;
  int xchal_have_dfp_sqrt;
  int xchal_have_dfp_rsqrt;
  int xchal_have_windowed;
  int xchal_num_aregs;
  int xchal_have_wide_branches;
  int xchal_have_predicted_branches;
  int xchal_icache_size;
  int xchal_dcache_size;
  int xchal_icache_linesize;
  int xchal_dcache_linesize;
  int xchal_icache_linewidth;
  int xchal_dcache_linewidth;
  int xchal_dcache_is_writeback;
  int xchal_have_mmu;
  int xchal_mmu_min_pte_page_size;
  int xchal_have_debug;
  int xchal_num_ibreak;
  int xchal_num_dbreak;
  int xchal_debuglevel;
  int xchal_max_instruction_size;
  int xchal_inst_fetch_width;
  int xshal_abi;
  int xthal_abi_windowed;
  int xthal_abi_call0;
};

struct xtensa_config_v2
{
  int xchal_m_stage;
  int xtensa_march_latest;
  int xtensa_march_earliest;
};

struct xtensa_config_v3
{
  int xchal_have_clamps;
  int xchal_have_depbits;
  int xchal_have_exclusive;
  int xchal_have_xea3;
};

struct xtensa_config_v4
{
  int xchal_data_width;
  int xchal_unaligned_load_exception;
  int xchal_unaligned_store_exception;
  int xchal_unaligned_load_hw;
  int xchal_unaligned_store_hw;
};

extern const void *xtensa_load_config (const char *name,
				       const void *no_plugin_def,
				       const void *no_name_def);
extern const struct xtensa_config_v1 *xtensa_get_config_v1 (void);
extern const struct xtensa_config_v2 *xtensa_get_config_v2 (void);
extern const struct xtensa_config_v3 *xtensa_get_config_v3 (void);
extern const struct xtensa_config_v4 *xtensa_get_config_v4 (void);

#ifdef XTENSA_CONFIG_DEFINITION

#ifndef XCHAL_HAVE_MUL32_HIGH
#define XCHAL_HAVE_MUL32_HIGH 0
#endif

#ifndef XCHAL_HAVE_RELEASE_SYNC
#define XCHAL_HAVE_RELEASE_SYNC 0
#endif

#ifndef XCHAL_HAVE_S32C1I
#define XCHAL_HAVE_S32C1I 0
#endif

#ifndef XCHAL_HAVE_THREADPTR
#define XCHAL_HAVE_THREADPTR 0
#endif

#ifndef XCHAL_HAVE_FP_POSTINC
#define XCHAL_HAVE_FP_POSTINC 0
#endif

#ifndef XCHAL_HAVE_DFP
#define XCHAL_HAVE_DFP 0
#endif

#ifndef XCHAL_HAVE_DFP_DIV
#define XCHAL_HAVE_DFP_DIV 0
#endif

#ifndef XCHAL_HAVE_DFP_RECIP
#define XCHAL_HAVE_DFP_RECIP 0
#endif

#ifndef XCHAL_HAVE_DFP_SQRT
#define XCHAL_HAVE_DFP_SQRT 0
#endif

#ifndef XCHAL_HAVE_DFP_RSQRT
#define XCHAL_HAVE_DFP_RSQRT 0
#endif

#ifndef XSHAL_HAVE_TEXT_SECTION_LITERALS
#define XSHAL_HAVE_TEXT_SECTION_LITERALS 0
#endif

#ifndef XCHAL_MMU_MIN_PTE_PAGE_SIZE
#define XCHAL_MMU_MIN_PTE_PAGE_SIZE 1
#endif

#ifndef XTHAL_ABI_WINDOWED
#define XTHAL_ABI_WINDOWED 0
#endif

#ifndef XTHAL_ABI_CALL0
#define XTHAL_ABI_CALL0 1
#endif

#ifndef XCHAL_M_STAGE
#define XCHAL_M_STAGE 0
#endif

#ifndef XTENSA_MARCH_LATEST
#define XTENSA_MARCH_LATEST 0
#endif

#ifndef XTENSA_MARCH_EARLIEST
#define XTENSA_MARCH_EARLIEST 0
#endif

#ifndef XCHAL_HAVE_CLAMPS
#define XCHAL_HAVE_CLAMPS 0
#endif

#ifndef XCHAL_HAVE_DEPBITS
#define XCHAL_HAVE_DEPBITS 0
#endif

#ifndef XCHAL_HAVE_EXCLUSIVE
#define XCHAL_HAVE_EXCLUSIVE 0
#endif

#ifndef XCHAL_HAVE_XEA3
#define XCHAL_HAVE_XEA3 0
#endif

#ifndef XCHAL_DATA_WIDTH
#define XCHAL_DATA_WIDTH 16
#endif

#ifndef XCHAL_UNALIGNED_LOAD_EXCEPTION
#define XCHAL_UNALIGNED_LOAD_EXCEPTION 1
#endif

#ifndef XCHAL_UNALIGNED_STORE_EXCEPTION
#define XCHAL_UNALIGNED_STORE_EXCEPTION 1
#endif

#ifndef XCHAL_UNALIGNED_LOAD_HW
#define XCHAL_UNALIGNED_LOAD_HW 0
#endif

#ifndef XCHAL_UNALIGNED_STORE_HW
#define XCHAL_UNALIGNED_STORE_HW 0
#endif

#define XTENSA_CONFIG_ENTRY(a) a

#define XTENSA_CONFIG_V1_ENTRY_LIST \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_BE), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_DENSITY), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_CONST16), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_ABS), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_ADDX), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_L32R), \
    XTENSA_CONFIG_ENTRY(XSHAL_USE_ABSOLUTE_LITERALS), \
    XTENSA_CONFIG_ENTRY(XSHAL_HAVE_TEXT_SECTION_LITERALS), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_MAC16), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_MUL16), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_MUL32), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_MUL32_HIGH), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_DIV32), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_NSA), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_MINMAX), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_SEXT), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_LOOPS), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_THREADPTR), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_RELEASE_SYNC), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_S32C1I), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_BOOLEANS), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_FP), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_FP_DIV), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_FP_RECIP), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_FP_SQRT), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_FP_RSQRT), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_FP_POSTINC), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_DFP), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_DFP_DIV), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_DFP_RECIP), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_DFP_SQRT), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_DFP_RSQRT), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_WINDOWED), \
    XTENSA_CONFIG_ENTRY(XCHAL_NUM_AREGS), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_WIDE_BRANCHES), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_PREDICTED_BRANCHES), \
    XTENSA_CONFIG_ENTRY(XCHAL_ICACHE_SIZE), \
    XTENSA_CONFIG_ENTRY(XCHAL_DCACHE_SIZE), \
    XTENSA_CONFIG_ENTRY(XCHAL_ICACHE_LINESIZE), \
    XTENSA_CONFIG_ENTRY(XCHAL_DCACHE_LINESIZE), \
    XTENSA_CONFIG_ENTRY(XCHAL_ICACHE_LINEWIDTH), \
    XTENSA_CONFIG_ENTRY(XCHAL_DCACHE_LINEWIDTH), \
    XTENSA_CONFIG_ENTRY(XCHAL_DCACHE_IS_WRITEBACK), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_MMU), \
    XTENSA_CONFIG_ENTRY(XCHAL_MMU_MIN_PTE_PAGE_SIZE), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_DEBUG), \
    XTENSA_CONFIG_ENTRY(XCHAL_NUM_IBREAK), \
    XTENSA_CONFIG_ENTRY(XCHAL_NUM_DBREAK), \
    XTENSA_CONFIG_ENTRY(XCHAL_DEBUGLEVEL), \
    XTENSA_CONFIG_ENTRY(XCHAL_MAX_INSTRUCTION_SIZE), \
    XTENSA_CONFIG_ENTRY(XCHAL_INST_FETCH_WIDTH), \
    XTENSA_CONFIG_ENTRY(XSHAL_ABI), \
    XTENSA_CONFIG_ENTRY(XTHAL_ABI_WINDOWED), \
    XTENSA_CONFIG_ENTRY(XTHAL_ABI_CALL0)

#define XTENSA_CONFIG_V2_ENTRY_LIST \
    XTENSA_CONFIG_ENTRY(XCHAL_M_STAGE), \
    XTENSA_CONFIG_ENTRY(XTENSA_MARCH_LATEST), \
    XTENSA_CONFIG_ENTRY(XTENSA_MARCH_EARLIEST)

#define XTENSA_CONFIG_V3_ENTRY_LIST \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_CLAMPS), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_DEPBITS), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_EXCLUSIVE), \
    XTENSA_CONFIG_ENTRY(XCHAL_HAVE_XEA3)

#define XTENSA_CONFIG_V4_ENTRY_LIST \
    XTENSA_CONFIG_ENTRY(XCHAL_DATA_WIDTH), \
    XTENSA_CONFIG_ENTRY(XCHAL_UNALIGNED_LOAD_EXCEPTION), \
    XTENSA_CONFIG_ENTRY(XCHAL_UNALIGNED_STORE_EXCEPTION), \
    XTENSA_CONFIG_ENTRY(XCHAL_UNALIGNED_LOAD_HW), \
    XTENSA_CONFIG_ENTRY(XCHAL_UNALIGNED_STORE_HW)

#define XTENSA_CONFIG_INSTANCE_LIST \
const struct xtensa_config_v1 xtensa_config_v1 = { \
    XTENSA_CONFIG_V1_ENTRY_LIST, \
}; \
const struct xtensa_config_v2 xtensa_config_v2 = { \
    XTENSA_CONFIG_V2_ENTRY_LIST, \
}; \
const struct xtensa_config_v3 xtensa_config_v3 = { \
    XTENSA_CONFIG_V3_ENTRY_LIST, \
}; \
const struct xtensa_config_v4 xtensa_config_v4 = { \
    XTENSA_CONFIG_V4_ENTRY_LIST, \
}

#define XTENSA_CONFIG_ENTRY_LIST \
    XTENSA_CONFIG_V1_ENTRY_LIST, \
    XTENSA_CONFIG_V2_ENTRY_LIST, \
    XTENSA_CONFIG_V3_ENTRY_LIST, \
    XTENSA_CONFIG_V4_ENTRY_LIST

#else /* XTENSA_CONFIG_DEFINITION */

#undef XCHAL_HAVE_BE
#define XCHAL_HAVE_BE			(xtensa_get_config_v1 ()->xchal_have_be)

#undef XCHAL_HAVE_DENSITY
#define XCHAL_HAVE_DENSITY		(xtensa_get_config_v1 ()->xchal_have_density)

#undef XCHAL_HAVE_CONST16
#define XCHAL_HAVE_CONST16		(xtensa_get_config_v1 ()->xchal_have_const16)

#undef XCHAL_HAVE_ABS
#define XCHAL_HAVE_ABS			(xtensa_get_config_v1 ()->xchal_have_abs)

#undef XCHAL_HAVE_ADDX
#define XCHAL_HAVE_ADDX			(xtensa_get_config_v1 ()->xchal_have_addx)

#undef XCHAL_HAVE_L32R
#define XCHAL_HAVE_L32R			(xtensa_get_config_v1 ()->xchal_have_l32r)

#undef XSHAL_USE_ABSOLUTE_LITERALS
#define XSHAL_USE_ABSOLUTE_LITERALS	(xtensa_get_config_v1 ()->xshal_use_absolute_literals)

#undef XSHAL_HAVE_TEXT_SECTION_LITERALS
#define XSHAL_HAVE_TEXT_SECTION_LITERALS (xtensa_get_config_v1 ()->xshal_have_text_section_literals)

#undef XCHAL_HAVE_MAC16
#define XCHAL_HAVE_MAC16		(xtensa_get_config_v1 ()->xchal_have_mac16)

#undef XCHAL_HAVE_MUL16
#define XCHAL_HAVE_MUL16		(xtensa_get_config_v1 ()->xchal_have_mul16)

#undef XCHAL_HAVE_MUL32
#define XCHAL_HAVE_MUL32		(xtensa_get_config_v1 ()->xchal_have_mul32)

#undef XCHAL_HAVE_MUL32_HIGH
#define XCHAL_HAVE_MUL32_HIGH		(xtensa_get_config_v1 ()->xchal_have_mul32_high)

#undef XCHAL_HAVE_DIV32
#define XCHAL_HAVE_DIV32		(xtensa_get_config_v1 ()->xchal_have_div32)

#undef XCHAL_HAVE_NSA
#define XCHAL_HAVE_NSA			(xtensa_get_config_v1 ()->xchal_have_nsa)

#undef XCHAL_HAVE_MINMAX
#define XCHAL_HAVE_MINMAX		(xtensa_get_config_v1 ()->xchal_have_minmax)

#undef XCHAL_HAVE_SEXT
#define XCHAL_HAVE_SEXT			(xtensa_get_config_v1 ()->xchal_have_sext)

#undef XCHAL_HAVE_LOOPS
#define XCHAL_HAVE_LOOPS		(xtensa_get_config_v1 ()->xchal_have_loops)

#undef XCHAL_HAVE_THREADPTR
#define XCHAL_HAVE_THREADPTR		(xtensa_get_config_v1 ()->xchal_have_threadptr)

#undef XCHAL_HAVE_RELEASE_SYNC
#define XCHAL_HAVE_RELEASE_SYNC		(xtensa_get_config_v1 ()->xchal_have_release_sync)

#undef XCHAL_HAVE_S32C1I
#define XCHAL_HAVE_S32C1I		(xtensa_get_config_v1 ()->xchal_have_s32c1i)

#undef XCHAL_HAVE_BOOLEANS
#define XCHAL_HAVE_BOOLEANS		(xtensa_get_config_v1 ()->xchal_have_booleans)

#undef XCHAL_HAVE_FP
#define XCHAL_HAVE_FP			(xtensa_get_config_v1 ()->xchal_have_fp)

#undef XCHAL_HAVE_FP_DIV
#define XCHAL_HAVE_FP_DIV		(xtensa_get_config_v1 ()->xchal_have_fp_div)

#undef XCHAL_HAVE_FP_RECIP
#define XCHAL_HAVE_FP_RECIP		(xtensa_get_config_v1 ()->xchal_have_fp_recip)

#undef XCHAL_HAVE_FP_SQRT
#define XCHAL_HAVE_FP_SQRT		(xtensa_get_config_v1 ()->xchal_have_fp_sqrt)

#undef XCHAL_HAVE_FP_RSQRT
#define XCHAL_HAVE_FP_RSQRT		(xtensa_get_config_v1 ()->xchal_have_fp_rsqrt)

#undef XCHAL_HAVE_FP_POSTINC
#define XCHAL_HAVE_FP_POSTINC		(xtensa_get_config_v1 ()->xchal_have_fp_postinc)

#undef XCHAL_HAVE_DFP
#define XCHAL_HAVE_DFP			(xtensa_get_config_v1 ()->xchal_have_dfp)

#undef XCHAL_HAVE_DFP_DIV
#define XCHAL_HAVE_DFP_DIV		(xtensa_get_config_v1 ()->xchal_have_dfp_div)

#undef XCHAL_HAVE_DFP_RECIP
#define XCHAL_HAVE_DFP_RECIP		(xtensa_get_config_v1 ()->xchal_have_dfp_recip)

#undef XCHAL_HAVE_DFP_SQRT
#define XCHAL_HAVE_DFP_SQRT		(xtensa_get_config_v1 ()->xchal_have_dfp_sqrt)

#undef XCHAL_HAVE_DFP_RSQRT
#define XCHAL_HAVE_DFP_RSQRT		(xtensa_get_config_v1 ()->xchal_have_dfp_rsqrt)

#undef XCHAL_HAVE_WINDOWED
#define XCHAL_HAVE_WINDOWED		(xtensa_get_config_v1 ()->xchal_have_windowed)

#undef XCHAL_NUM_AREGS
#define XCHAL_NUM_AREGS			(xtensa_get_config_v1 ()->xchal_num_aregs)

#undef XCHAL_HAVE_WIDE_BRANCHES
#define XCHAL_HAVE_WIDE_BRANCHES	(xtensa_get_config_v1 ()->xchal_have_wide_branches)

#undef XCHAL_HAVE_PREDICTED_BRANCHES
#define XCHAL_HAVE_PREDICTED_BRANCHES	(xtensa_get_config_v1 ()->xchal_have_predicted_branches)


#undef XCHAL_ICACHE_SIZE
#define XCHAL_ICACHE_SIZE		(xtensa_get_config_v1 ()->xchal_icache_size)

#undef XCHAL_DCACHE_SIZE
#define XCHAL_DCACHE_SIZE		(xtensa_get_config_v1 ()->xchal_dcache_size)

#undef XCHAL_ICACHE_LINESIZE
#define XCHAL_ICACHE_LINESIZE		(xtensa_get_config_v1 ()->xchal_icache_linesize)

#undef XCHAL_DCACHE_LINESIZE
#define XCHAL_DCACHE_LINESIZE		(xtensa_get_config_v1 ()->xchal_dcache_linesize)

#undef XCHAL_ICACHE_LINEWIDTH
#define XCHAL_ICACHE_LINEWIDTH		(xtensa_get_config_v1 ()->xchal_icache_linewidth)

#undef XCHAL_DCACHE_LINEWIDTH
#define XCHAL_DCACHE_LINEWIDTH		(xtensa_get_config_v1 ()->xchal_dcache_linewidth)

#undef XCHAL_DCACHE_IS_WRITEBACK
#define XCHAL_DCACHE_IS_WRITEBACK	(xtensa_get_config_v1 ()->xchal_dcache_is_writeback)


#undef XCHAL_HAVE_MMU
#define XCHAL_HAVE_MMU			(xtensa_get_config_v1 ()->xchal_have_mmu)

#undef XCHAL_MMU_MIN_PTE_PAGE_SIZE
#define XCHAL_MMU_MIN_PTE_PAGE_SIZE	(xtensa_get_config_v1 ()->xchal_mmu_min_pte_page_size)


#undef XCHAL_HAVE_DEBUG
#define XCHAL_HAVE_DEBUG		(xtensa_get_config_v1 ()->xchal_have_debug)

#undef XCHAL_NUM_IBREAK
#define XCHAL_NUM_IBREAK		(xtensa_get_config_v1 ()->xchal_num_ibreak)

#undef XCHAL_NUM_DBREAK
#define XCHAL_NUM_DBREAK		(xtensa_get_config_v1 ()->xchal_num_dbreak)

#undef XCHAL_DEBUGLEVEL
#define XCHAL_DEBUGLEVEL		(xtensa_get_config_v1 ()->xchal_debuglevel)


#undef XCHAL_MAX_INSTRUCTION_SIZE
#define XCHAL_MAX_INSTRUCTION_SIZE	(xtensa_get_config_v1 ()->xchal_max_instruction_size)

#undef XCHAL_INST_FETCH_WIDTH
#define XCHAL_INST_FETCH_WIDTH		(xtensa_get_config_v1 ()->xchal_inst_fetch_width)


#undef XSHAL_ABI
#undef XTHAL_ABI_WINDOWED
#undef XTHAL_ABI_CALL0
#define XSHAL_ABI			(xtensa_get_config_v1 ()->xshal_abi)
#define XTHAL_ABI_WINDOWED		(xtensa_get_config_v1 ()->xthal_abi_windowed)
#define XTHAL_ABI_CALL0			(xtensa_get_config_v1 ()->xthal_abi_call0)


#undef XCHAL_M_STAGE
#define XCHAL_M_STAGE			(xtensa_get_config_v2 ()->xchal_m_stage)

#undef XTENSA_MARCH_LATEST
#define XTENSA_MARCH_LATEST		(xtensa_get_config_v2 ()->xtensa_march_latest)

#undef XTENSA_MARCH_EARLIEST
#define XTENSA_MARCH_EARLIEST		(xtensa_get_config_v2 ()->xtensa_march_earliest)


#undef XCHAL_HAVE_CLAMPS
#define XCHAL_HAVE_CLAMPS		(xtensa_get_config_v3 ()->xchal_have_clamps)

#undef XCHAL_HAVE_DEPBITS
#define XCHAL_HAVE_DEPBITS		(xtensa_get_config_v3 ()->xchal_have_depbits)

#undef XCHAL_HAVE_EXCLUSIVE
#define XCHAL_HAVE_EXCLUSIVE		(xtensa_get_config_v3 ()->xchal_have_exclusive)

#undef XCHAL_HAVE_XEA3
#define XCHAL_HAVE_XEA3			(xtensa_get_config_v3 ()->xchal_have_xea3)


#undef XCHAL_DATA_WIDTH
#define XCHAL_DATA_WIDTH		(xtensa_get_config_v4 ()->xchal_data_width)

#undef XCHAL_UNALIGNED_LOAD_EXCEPTION
#define XCHAL_UNALIGNED_LOAD_EXCEPTION	(xtensa_get_config_v4 ()->xchal_unaligned_load_exception)

#undef XCHAL_UNALIGNED_STORE_EXCEPTION
#define XCHAL_UNALIGNED_STORE_EXCEPTION	(xtensa_get_config_v4 ()->xchal_unaligned_store_exception)

#undef XCHAL_UNALIGNED_LOAD_HW
#define XCHAL_UNALIGNED_LOAD_HW		(xtensa_get_config_v4 ()->xchal_unaligned_load_hw)

#undef XCHAL_UNALIGNED_STORE_HW
#define XCHAL_UNALIGNED_STORE_HW	(xtensa_get_config_v4 ()->xchal_unaligned_store_hw)

#endif /* XTENSA_CONFIG_DEFINITION */

#ifdef __cplusplus
}
#endif
#endif /* !XTENSA_DYNCONFIG_H */
