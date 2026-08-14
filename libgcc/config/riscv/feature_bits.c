/* Helper function for function multi-versioning for RISC-V.

   Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

#define RISCV_FEATURE_BITS_LENGTH 2

struct {
  unsigned length;
  unsigned long long features[RISCV_FEATURE_BITS_LENGTH];
} __riscv_feature_bits __attribute__ ((visibility ("hidden"), nocommon));

struct {
  unsigned mvendorid;
  unsigned long long marchid;
  unsigned long long mimpid;
} __riscv_cpu_model __attribute__ ((visibility ("hidden"), nocommon));

#define A_GROUPID 0
#define A_BITMASK (1ULL << 0)
#define B_GROUPID 0
#define B_BITMASK (1ULL << 1)
#define C_GROUPID 0
#define C_BITMASK (1ULL << 2)
#define D_GROUPID 0
#define D_BITMASK (1ULL << 3)
#define E_GROUPID 0
#define E_BITMASK (1ULL << 4)
#define F_GROUPID 0
#define F_BITMASK (1ULL << 5)
#define H_GROUPID 0
#define H_BITMASK (1ULL << 7)
#define I_GROUPID 0
#define I_BITMASK (1ULL << 8)
#define M_GROUPID 0
#define M_BITMASK (1ULL << 12)
#define V_GROUPID 0
#define V_BITMASK (1ULL << 21)
#define ZACAS_GROUPID 0
#define ZACAS_BITMASK (1ULL << 26)
#define ZBA_GROUPID 0
#define ZBA_BITMASK (1ULL << 27)
#define ZBB_GROUPID 0
#define ZBB_BITMASK (1ULL << 28)
#define ZBC_GROUPID 0
#define ZBC_BITMASK (1ULL << 29)
#define ZBKB_GROUPID 0
#define ZBKB_BITMASK (1ULL << 30)
#define ZBKC_GROUPID 0
#define ZBKC_BITMASK (1ULL << 31)
#define ZBKX_GROUPID 0
#define ZBKX_BITMASK (1ULL << 32)
#define ZBS_GROUPID 0
#define ZBS_BITMASK (1ULL << 33)
#define ZFA_GROUPID 0
#define ZFA_BITMASK (1ULL << 34)
#define ZFH_GROUPID 0
#define ZFH_BITMASK (1ULL << 35)
#define ZFHMIN_GROUPID 0
#define ZFHMIN_BITMASK (1ULL << 36)
#define ZICBOZ_GROUPID 0
#define ZICBOZ_BITMASK (1ULL << 37)
#define ZICOND_GROUPID 0
#define ZICOND_BITMASK (1ULL << 38)
#define ZIHINTNTL_GROUPID 0
#define ZIHINTNTL_BITMASK (1ULL << 39)
#define ZIHINTPAUSE_GROUPID 0
#define ZIHINTPAUSE_BITMASK (1ULL << 40)
#define ZKND_GROUPID 0
#define ZKND_BITMASK (1ULL << 41)
#define ZKNE_GROUPID 0
#define ZKNE_BITMASK (1ULL << 42)
#define ZKNH_GROUPID 0
#define ZKNH_BITMASK (1ULL << 43)
#define ZKSED_GROUPID 0
#define ZKSED_BITMASK (1ULL << 44)
#define ZKSH_GROUPID 0
#define ZKSH_BITMASK (1ULL << 45)
#define ZKT_GROUPID 0
#define ZKT_BITMASK (1ULL << 46)
#define ZTSO_GROUPID 0
#define ZTSO_BITMASK (1ULL << 47)
#define ZVBB_GROUPID 0
#define ZVBB_BITMASK (1ULL << 48)
#define ZVBC_GROUPID 0
#define ZVBC_BITMASK (1ULL << 49)
#define ZVFH_GROUPID 0
#define ZVFH_BITMASK (1ULL << 50)
#define ZVFHMIN_GROUPID 0
#define ZVFHMIN_BITMASK (1ULL << 51)
#define ZVKB_GROUPID 0
#define ZVKB_BITMASK (1ULL << 52)
#define ZVKG_GROUPID 0
#define ZVKG_BITMASK (1ULL << 53)
#define ZVKNED_GROUPID 0
#define ZVKNED_BITMASK (1ULL << 54)
#define ZVKNHA_GROUPID 0
#define ZVKNHA_BITMASK (1ULL << 55)
#define ZVKNHB_GROUPID 0
#define ZVKNHB_BITMASK (1ULL << 56)
#define ZVKSED_GROUPID 0
#define ZVKSED_BITMASK (1ULL << 57)
#define ZVKSH_GROUPID 0
#define ZVKSH_BITMASK (1ULL << 58)
#define ZVKT_GROUPID 0
#define ZVKT_BITMASK (1ULL << 59)
#define ZVE32X_GROUPID 0
#define ZVE32X_BITMASK (1ULL << 60)
#define ZVE32F_GROUPID 0
#define ZVE32F_BITMASK (1ULL << 61)
#define ZVE64X_GROUPID 0
#define ZVE64X_BITMASK (1ULL << 62)
#define ZVE64F_GROUPID 0
#define ZVE64F_BITMASK (1ULL << 63)
#define ZVE64D_GROUPID 1
#define ZVE64D_BITMASK (1ULL << 0)
#define ZIMOP_GROUPID 1
#define ZIMOP_BITMASK (1ULL << 1)
#define ZCA_GROUPID 1
#define ZCA_BITMASK (1ULL << 2)
#define ZCB_GROUPID 1
#define ZCB_BITMASK (1ULL << 3)
#define ZCD_GROUPID 1
#define ZCD_BITMASK (1ULL << 4)
#define ZCF_GROUPID 1
#define ZCF_BITMASK (1ULL << 5)
#define ZCMOP_GROUPID 1
#define ZCMOP_BITMASK (1ULL << 6)
#define ZAWRS_GROUPID 1
#define ZAWRS_BITMASK (1ULL << 7)
#define ZILSD_GROUPID 1
#define ZILSD_BITMASK (1ULL << 8)
#define ZCLSD_GROUPID 1
#define ZCLSD_BITMASK (1ULL << 9)
#define ZCMP_GROUPID 1
#define ZCMP_BITMASK (1ULL << 10)
#define ZIFENCEI_GROUPID 1
#define ZIFENCEI_BITMASK (1ULL << 11)
#define ZMMUL_GROUPID 1
#define ZMMUL_BITMASK (1ULL << 12)
#define SUPM_GROUPID 1
#define SUPM_BITMASK (1ULL << 14)
#define ZICNTR_GROUPID 1
#define ZICNTR_BITMASK (1ULL << 15)
#define ZIHPM_GROUPID 1
#define ZIHPM_BITMASK (1ULL << 16)
#define ZFBFMIN_GROUPID 1
#define ZFBFMIN_BITMASK (1ULL << 17)
#define ZVFBFMIN_GROUPID 1
#define ZVFBFMIN_BITMASK (1ULL << 18)
#define ZVFBFWMA_GROUPID 1
#define ZVFBFWMA_BITMASK (1ULL << 19)
#define ZICBOM_GROUPID 1
#define ZICBOM_BITMASK (1ULL << 20)
#define ZAAMO_GROUPID 1
#define ZAAMO_BITMASK (1ULL << 21)
#define ZALRSC_GROUPID 1
#define ZALRSC_BITMASK (1ULL << 22)
#define ZABHA_GROUPID 1
#define ZABHA_BITMASK (1ULL << 23)
#define ZALASR_GROUPID 1
#define ZALASR_BITMASK (1ULL << 24)
#define ZICBOP_GROUPID 1
#define ZICBOP_BITMASK (1ULL << 25)
#define ZICFILP_GROUPID 1
#define ZICFILP_BITMASK (1ULL << 26)
#define ZICFISS_GROUPID 1
#define ZICFISS_BITMASK (1ULL << 27)

#define SET_EXT(EXT) features[EXT##_GROUPID] |= EXT##_BITMASK

#ifdef __linux
#include "common/config/riscv/riscv-hwprobe.h"

static void __init_riscv_features_bits_linux ()
{
  struct riscv_hwprobe hwprobes[] = {
    {RISCV_HWPROBE_KEY_MVENDORID, 0},
    {RISCV_HWPROBE_KEY_MARCHID, 0},
    {RISCV_HWPROBE_KEY_MIMPID, 0},
    {RISCV_HWPROBE_KEY_BASE_BEHAVIOR, 0},
    {RISCV_HWPROBE_KEY_IMA_EXT_0, 0},
    {RISCV_HWPROBE_KEY_IMA_EXT_1, 0},
  };
  const int npairs = sizeof (hwprobes) / sizeof (hwprobes[0]);

  long res = riscv_hwprobe (hwprobes, npairs);

  if (res)
    return;

  const struct riscv_hwprobe hwprobe_mvendorid = hwprobes[0];

  __riscv_cpu_model.mvendorid = hwprobe_mvendorid.value;

  const struct riscv_hwprobe hwprobe_marchid = hwprobes[1];

  __riscv_cpu_model.marchid = hwprobe_marchid.value;

  const struct riscv_hwprobe hwprobe_mimpid = hwprobes[2];

  __riscv_cpu_model.mimpid = hwprobe_mimpid.value;

  const struct riscv_hwprobe hwprobe_base_behavior = hwprobes[3];
  unsigned long long features[RISCV_FEATURE_BITS_LENGTH];
  int i;
  for (i = 0; i < RISCV_FEATURE_BITS_LENGTH; ++i)
    features[i] = 0;

  if (hwprobe_base_behavior.value & RISCV_HWPROBE_BASE_BEHAVIOR_IMA)
    {
      SET_EXT (I);
      SET_EXT (M);
      SET_EXT (A);
    }

  const struct riscv_hwprobe hwprobe_ima_ext = hwprobes[4];
  /* Every time we add new extensions, we should check if previous extensions
     imply the new extension and set the corresponding bit.
     We don't need to handle cases where:
     1.  The new extension implies a previous extension (e.g., Zve32f -> F).
     2.  The extensions imply some other extensions appear in the same release
	 version of Linux Kernel (e.g., Zbc - > Zbkc).  */
  if (hwprobe_ima_ext.value & (RISCV_HWPROBE_EXT_F | RISCV_HWPROBE_EXT_D))
    {
      SET_EXT (F);
      SET_EXT (D);
    }

  if (hwprobe_ima_ext.value & RISCV_HWPROBE_EXT_C)
    {
      SET_EXT (C);
      SET_EXT (ZCA);
      if (hwprobe_ima_ext.value & (RISCV_HWPROBE_EXT_F | RISCV_HWPROBE_EXT_D))
	{
#if __riscv_xlen == 32
	  SET_EXT (ZCF);
#endif
	  SET_EXT (ZCD);
	}
    }

  /* Added since Linux v6.5.  */
  if (hwprobe_ima_ext.value & RISCV_HWPROBE_EXT_V)
    {
      SET_EXT (V);
      SET_EXT (ZVE32X);
      SET_EXT (ZVE32F);
      SET_EXT (ZVE64X);
      SET_EXT (ZVE64F);
      SET_EXT (ZVE64D);
    }

#define RISCV_HWPROBE_EXT(NAME, UPPERCASE_NAME, KEY, BIT, XLEN)		\
  if (riscv_hwprobe_value (hwprobes, npairs, KEY) & (1ULL << (BIT)))	\
    SET_EXT (UPPERCASE_NAME);
#include "common/config/riscv/riscv-hwprobe.def"

  for (i = 0; i < RISCV_FEATURE_BITS_LENGTH; ++i)
    __riscv_feature_bits.features[i] = features[i];

  __riscv_feature_bits.length = RISCV_FEATURE_BITS_LENGTH;
}
#endif


static int __init = 0;

void __init_riscv_feature_bits ();

void
__attribute__ ((constructor (101)))
__init_riscv_feature_bits ()
{
  if (__init)
    return;

#ifdef __linux
  __init_riscv_features_bits_linux ();
#else
  /* Unsupported, just initialize that into all zeros.  */
  __riscv_feature_bits.length = 0;
  __riscv_cpu_model.mvendorid = 0;
  __riscv_cpu_model.marchid = 0;
  __riscv_cpu_model.mimpid = 0;
#endif

  __init = 1;
}
