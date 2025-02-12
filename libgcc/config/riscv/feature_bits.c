/* Helper function for function multi-versioning for RISC-V.

   Copyright (C) 2024-2025 Free Software Foundation, Inc.

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
#define C_GROUPID 0
#define C_BITMASK (1ULL << 2)
#define D_GROUPID 0
#define D_BITMASK (1ULL << 3)
#define F_GROUPID 0
#define F_BITMASK (1ULL << 5)
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

#define SET_EXT(EXT) features[EXT##_GROUPID] |= EXT##_BITMASK

#ifdef __linux

#define __NR_riscv_hwprobe 258
#define RISCV_HWPROBE_KEY_MVENDORID 0
#define RISCV_HWPROBE_KEY_MARCHID 1
#define RISCV_HWPROBE_KEY_MIMPID 2
#define RISCV_HWPROBE_KEY_BASE_BEHAVIOR 3
#define RISCV_HWPROBE_BASE_BEHAVIOR_IMA (1ULL << 0)
#define RISCV_HWPROBE_KEY_IMA_EXT_0 4
#define RISCV_HWPROBE_IMA_FD (1ULL << 0)
#define RISCV_HWPROBE_IMA_C (1ULL << 1)
#define RISCV_HWPROBE_IMA_V (1ULL << 2)
#define RISCV_HWPROBE_EXT_ZBA (1ULL << 3)
#define RISCV_HWPROBE_EXT_ZBB (1ULL << 4)
#define RISCV_HWPROBE_EXT_ZBS (1ULL << 5)
#define RISCV_HWPROBE_EXT_ZICBOZ (1ULL << 6)
#define RISCV_HWPROBE_EXT_ZBC (1ULL << 7)
#define RISCV_HWPROBE_EXT_ZBKB (1ULL << 8)
#define RISCV_HWPROBE_EXT_ZBKC (1ULL << 9)
#define RISCV_HWPROBE_EXT_ZBKX (1ULL << 10)
#define RISCV_HWPROBE_EXT_ZKND (1ULL << 11)
#define RISCV_HWPROBE_EXT_ZKNE (1ULL << 12)
#define RISCV_HWPROBE_EXT_ZKNH (1ULL << 13)
#define RISCV_HWPROBE_EXT_ZKSED (1ULL << 14)
#define RISCV_HWPROBE_EXT_ZKSH (1ULL << 15)
#define RISCV_HWPROBE_EXT_ZKT (1ULL << 16)
#define RISCV_HWPROBE_EXT_ZVBB (1ULL << 17)
#define RISCV_HWPROBE_EXT_ZVBC (1ULL << 18)
#define RISCV_HWPROBE_EXT_ZVKB (1ULL << 19)
#define RISCV_HWPROBE_EXT_ZVKG (1ULL << 20)
#define RISCV_HWPROBE_EXT_ZVKNED (1ULL << 21)
#define RISCV_HWPROBE_EXT_ZVKNHA (1ULL << 22)
#define RISCV_HWPROBE_EXT_ZVKNHB (1ULL << 23)
#define RISCV_HWPROBE_EXT_ZVKSED (1ULL << 24)
#define RISCV_HWPROBE_EXT_ZVKSH (1ULL << 25)
#define RISCV_HWPROBE_EXT_ZVKT (1ULL << 26)
#define RISCV_HWPROBE_EXT_ZFH (1ULL << 27)
#define RISCV_HWPROBE_EXT_ZFHMIN (1ULL << 28)
#define RISCV_HWPROBE_EXT_ZIHINTNTL (1ULL << 29)
#define RISCV_HWPROBE_EXT_ZVFH (1ULL << 30)
#define RISCV_HWPROBE_EXT_ZVFHMIN (1ULL << 31)
#define RISCV_HWPROBE_EXT_ZFA (1ULL << 32)
#define RISCV_HWPROBE_EXT_ZTSO (1ULL << 33)
#define RISCV_HWPROBE_EXT_ZACAS (1ULL << 34)
#define RISCV_HWPROBE_EXT_ZICOND (1ULL << 35)
#define RISCV_HWPROBE_EXT_ZIHINTPAUSE (1ULL << 36)
#define RISCV_HWPROBE_EXT_ZVE32X (1ULL << 37)
#define RISCV_HWPROBE_EXT_ZVE32F (1ULL << 38)
#define RISCV_HWPROBE_EXT_ZVE64X (1ULL << 39)
#define RISCV_HWPROBE_EXT_ZVE64F (1ULL << 40)
#define RISCV_HWPROBE_EXT_ZVE64D (1ULL << 41)
#define RISCV_HWPROBE_EXT_ZIMOP  (1ULL << 42)
#define RISCV_HWPROBE_EXT_ZCA    (1ULL << 43)
#define RISCV_HWPROBE_EXT_ZCB    (1ULL << 44)
#define RISCV_HWPROBE_EXT_ZCD    (1ULL << 45)
#define RISCV_HWPROBE_EXT_ZCF    (1ULL << 46)
#define RISCV_HWPROBE_EXT_ZCMOP  (1ULL << 47)
#define RISCV_HWPROBE_EXT_ZAWRS  (1ULL << 48)
#define RISCV_HWPROBE_KEY_CPUPERF_0 5
#define RISCV_HWPROBE_MISALIGNED_UNKNOWN (0 << 0)
#define RISCV_HWPROBE_MISALIGNED_EMULATED (1ULL << 0)
#define RISCV_HWPROBE_MISALIGNED_SLOW (2 << 0)
#define RISCV_HWPROBE_MISALIGNED_FAST (3 << 0)
#define RISCV_HWPROBE_MISALIGNED_UNSUPPORTED (4 << 0)
#define RISCV_HWPROBE_MISALIGNED_MASK (7 << 0)
#define RISCV_HWPROBE_KEY_ZICBOZ_BLOCK_SIZE 6

struct riscv_hwprobe {
  long long key;
  unsigned long long value;
};

static long syscall_5_args (long number, long arg1, long arg2, long arg3,
			    long arg4, long arg5)
{
  register long a7 __asm__ ("a7") = number;
  register long a0 __asm__ ("a0") = arg1;
  register long a1 __asm__ ("a1") = arg2;
  register long a2 __asm__ ("a2") = arg3;
  register long a3 __asm__ ("a3") = arg4;
  register long a4 __asm__ ("a4") = arg5;
  __asm__ __volatile__ ("ecall\n\t"
			: "=r"(a0)
			: "r"(a7), "r"(a0), "r"(a1), "r"(a2), "r"(a3), "r"(a4)
			: "memory");
  return a0;
}

#define SET_FROM_HWPROBE(HWPROBE_VAR, EXT) \
  if (HWPROBE_VAR.value & RISCV_HWPROBE_EXT_##EXT) \
    SET_EXT (EXT)

#define SET_FROM_IMA_EXT(EXT) \
  SET_FROM_HWPROBE (hwprobe_ima_ext, EXT)

static void __init_riscv_features_bits_linux ()
{
  struct riscv_hwprobe hwprobes[] = {
    {RISCV_HWPROBE_KEY_MVENDORID, 0},
    {RISCV_HWPROBE_KEY_MARCHID, 0},
    {RISCV_HWPROBE_KEY_MIMPID, 0},
    {RISCV_HWPROBE_KEY_BASE_BEHAVIOR, 0},
    {RISCV_HWPROBE_KEY_IMA_EXT_0, 0},
  };

  long res = syscall_5_args (__NR_riscv_hwprobe, (long)&hwprobes,
			     sizeof (hwprobes) / sizeof (hwprobes[0]), 0,
			     0, 0);

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

  if (hwprobe_ima_ext.value & RISCV_HWPROBE_IMA_FD)
    {
      SET_EXT (F);
      SET_EXT (D);
    }

  if (hwprobe_ima_ext.value & RISCV_HWPROBE_IMA_C)
    {
      SET_EXT (C);
      SET_EXT (ZCA);
      if (hwprobe_ima_ext.value & RISCV_HWPROBE_IMA_FD)
	{
#if __riscv_xlen == 32
	  SET_EXT (ZCF);
#endif
	  SET_EXT (ZCD);
	}
    }

  /* Added since Linux v6.5.  */
  if (hwprobe_ima_ext.value & RISCV_HWPROBE_IMA_V)
    {
      SET_EXT (V);
      SET_EXT (ZVE32X);
      SET_EXT (ZVE32F);
      SET_EXT (ZVE64X);
      SET_EXT (ZVE64F);
      SET_EXT (ZVE64D);
    }

  SET_FROM_IMA_EXT (ZBA);
  SET_FROM_IMA_EXT (ZBB);
  SET_FROM_IMA_EXT (ZBS);
  /* Added since Linux v6.7.  */
  SET_FROM_IMA_EXT (ZICBOZ);
  /* Added since Linux v6.8.  */
  SET_FROM_IMA_EXT (ZBC);
  SET_FROM_IMA_EXT (ZBKB);
  SET_FROM_IMA_EXT (ZBKC);
  SET_FROM_IMA_EXT (ZBKX);
  SET_FROM_IMA_EXT (ZKND);
  SET_FROM_IMA_EXT (ZKNE);
  SET_FROM_IMA_EXT (ZKNH);
  SET_FROM_IMA_EXT (ZKSED);
  SET_FROM_IMA_EXT (ZKSH);
  SET_FROM_IMA_EXT (ZKT);
  SET_FROM_IMA_EXT (ZVBB);
  SET_FROM_IMA_EXT (ZVBC);
  SET_FROM_IMA_EXT (ZVKB);
  SET_FROM_IMA_EXT (ZVKG);
  SET_FROM_IMA_EXT (ZVKNED);
  SET_FROM_IMA_EXT (ZVKNHA);
  SET_FROM_IMA_EXT (ZVKNHB);
  SET_FROM_IMA_EXT (ZVKSED);
  SET_FROM_IMA_EXT (ZVKSH);
  SET_FROM_IMA_EXT (ZVKT);
  SET_FROM_IMA_EXT (ZFH);
  SET_FROM_IMA_EXT (ZFHMIN);
  SET_FROM_IMA_EXT (ZIHINTNTL);
  SET_FROM_IMA_EXT (ZVFH);
  SET_FROM_IMA_EXT (ZVFHMIN);
  SET_FROM_IMA_EXT (ZFA);
  SET_FROM_IMA_EXT (ZTSO);
  SET_FROM_IMA_EXT (ZACAS);
  SET_FROM_IMA_EXT (ZICOND);
  /* Added since Linux v6.10.  */
  SET_FROM_IMA_EXT (ZIHINTPAUSE);
  /* Added since Linux v6.11.  */
  SET_FROM_IMA_EXT (ZVE32X);
  SET_FROM_IMA_EXT (ZVE32F);
  SET_FROM_IMA_EXT (ZVE64X);
  SET_FROM_IMA_EXT (ZVE64F);
  SET_FROM_IMA_EXT (ZVE64D);
  SET_FROM_IMA_EXT (ZIMOP);
  SET_FROM_IMA_EXT (ZCA);
  SET_FROM_IMA_EXT (ZCB);
  SET_FROM_IMA_EXT (ZCD);
  SET_FROM_IMA_EXT (ZCF);
  SET_FROM_IMA_EXT (ZCMOP);
  SET_FROM_IMA_EXT (ZAWRS);

  for (i = 0; i < RISCV_FEATURE_BITS_LENGTH; ++i)
    __riscv_feature_bits.features[i] = features[i];

  __riscv_feature_bits.length = RISCV_FEATURE_BITS_LENGTH;
}
#endif


static int __init = 0;

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
