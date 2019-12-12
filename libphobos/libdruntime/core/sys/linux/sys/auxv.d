/**
 * D header file for GNU/Linux.
 *
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Kai Nacke
 */
module core.sys.linux.sys.auxv;

import core.stdc.config;

version (linux):
extern (C):

version (MIPS32)  version = MIPS_Any;
version (MIPS64)  version = MIPS_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (S390)    version = IBMZ_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;
version (SystemZ) version = IBMZ_Any;

c_ulong getauxval(c_ulong type) nothrow pure @nogc @system;

version (ARM)
{
  // See https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/arm/bits/hwcap.h

  enum HWCAP_ARM_SWP                      = 1;
  enum HWCAP_ARM_HALF                     = 2;
  enum HWCAP_ARM_THUMB                    = 4;
  enum HWCAP_ARM_26BIT                    = 8;
  enum HWCAP_ARM_FAST_MULT                = 16;
  enum HWCAP_ARM_FPA                      = 32;
  enum HWCAP_ARM_VFP                      = 64;
  enum HWCAP_ARM_EDSP                     = 128;
  enum HWCAP_ARM_JAVA                     = 256;
  enum HWCAP_ARM_IWMMXT                   = 512;
  enum HWCAP_ARM_CRUNCH                   = 1024;
  enum HWCAP_ARM_THUMBEE                  = 2048;
  enum HWCAP_ARM_NEON                     = 4096;
  enum HWCAP_ARM_VFPv3                    = 8192;
  enum HWCAP_ARM_VFPv3D16                 = 16384;
  enum HWCAP_ARM_TLS                      = 32768;
  enum HWCAP_ARM_VFPv4                    = 65536;
  enum HWCAP_ARM_IDIVA                    = 131072;
  enum HWCAP_ARM_IDIVT                    = 262144;
  enum HWCAP_ARM_VFPD32                   = 524288;
  enum HWCAP_ARM_LPAE                     = 1048576;
  enum HWCAP_ARM_EVTSTRM                  = 2097152;
}
else version (AArch64)
{
  // See https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/aarch64/bits/hwcap.h

  enum HWCAP_FP                           = 1;
  enum HWCAP_ASIMD                        = 2;
  enum HWCAP_EVTSTRM                      = 4;
  enum HWCAP_AES                          = 8;
  enum HWCAP_PMULL                        = 16;
  enum HWCAP_SHA1                         = 32;
  enum HWCAP_SHA2                         = 64;
  enum HWCAP_CRC32                        = 128;
  enum HWCAP_ATOMICS                      = 256;
  enum HWCAP_FPHP                         = 512;
  enum HWCAP_ASIMDHP                      = 1024;
}
else version (PPC_Any)
{
  // See https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/powerpc/bits/hwcap.h

  enum PPC_FEATURE_32                     = 0x80000000;
  enum PPC_FEATURE_64                     = 0x40000000;
  enum PPC_FEATURE_601_INSTR              = 0x20000000;
  enum PPC_FEATURE_HAS_ALTIVEC            = 0x10000000;
  enum PPC_FEATURE_HAS_FPU                = 0x08000000;
  enum PPC_FEATURE_HAS_MMU                = 0x04000000;
  enum PPC_FEATURE_HAS_4xxMAC             = 0x02000000;
  enum PPC_FEATURE_UNIFIED_CACHE          = 0x01000000;
  enum PPC_FEATURE_HAS_SPE                = 0x00800000;
  enum PPC_FEATURE_HAS_EFP_SINGLE         = 0x00400000;
  enum PPC_FEATURE_HAS_EFP_DOUBLE         = 0x00200000;
  enum PPC_FEATURE_NO_TB                  = 0x00100000;
  enum PPC_FEATURE_POWER4                 = 0x00080000;
  enum PPC_FEATURE_POWER5                 = 0x00040000;
  enum PPC_FEATURE_POWER5_PLUS            = 0x00020000;
  enum PPC_FEATURE_CELL_BE                = 0x00010000;
  enum PPC_FEATURE_BOOKE                  = 0x00008000;
  enum PPC_FEATURE_SMT                    = 0x00004000;

  enum PPC_FEATURE_ICACHE_SNOOP           = 0x00002000;
  enum PPC_FEATURE_ARCH_2_05              = 0x00001000;
  enum PPC_FEATURE_PA6T                   = 0x00000800;
  enum PPC_FEATURE_HAS_DFP                = 0x00000400;
  enum PPC_FEATURE_POWER6_EXT             = 0x00000200;
  enum PPC_FEATURE_ARCH_2_06              = 0x00000100;
  enum PPC_FEATURE_HAS_VSX                = 0x00000080;
  enum PPC_FEATURE_PSERIES_PERFMON_COMPAT = 0x00000040;
  enum PPC_FEATURE_TRUE_LE                = 0x00000002;
  enum PPC_FEATURE_PPC_LE                 = 0x00000001;

  enum PPC_FEATURE2_ARCH_2_07             = 0x80000000;
  enum PPC_FEATURE2_HAS_HTM               = 0x40000000;
  enum PPC_FEATURE2_HAS_DSCR              = 0x20000000;
  enum PPC_FEATURE2_HAS_EBB               = 0x10000000;
  enum PPC_FEATURE2_HAS_ISEL              = 0x08000000;
  enum PPC_FEATURE2_HAS_TAR               = 0x04000000;
  enum PPC_FEATURE2_HAS_VEC_CRYPTO        = 0x02000000;
}
else version (SPARC_Any)
{
  // See https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/sparc/bits/hwcap.h

  enum HWCAP_SPARC_FLUSH                  = 0x00000001;
  enum HWCAP_SPARC_STBAR                  = 0x00000002;
  enum HWCAP_SPARC_SWAP                   = 0x00000004;
  enum HWCAP_SPARC_MULDIV                 = 0x00000008;
  enum HWCAP_SPARC_V9                     = 0x00000010;
  enum HWCAP_SPARC_ULTRA3                 = 0x00000020;
  enum HWCAP_SPARC_BLKINIT                = 0x00000040;
  enum HWCAP_SPARC_N2                     = 0x00000080;
  enum HWCAP_SPARC_MUL32                  = 0x00000100;
  enum HWCAP_SPARC_DIV32                  = 0x00000200;
  enum HWCAP_SPARC_FSMULD                 = 0x00000400;
  enum HWCAP_SPARC_V8PLUS                 = 0x00000800;
  enum HWCAP_SPARC_POPC                   = 0x00001000;
  enum HWCAP_SPARC_VIS                    = 0x00002000;
  enum HWCAP_SPARC_VIS2                   = 0x00004000;
  enum HWCAP_SPARC_ASI_BLK_INIT           = 0x00008000;
  enum HWCAP_SPARC_FMAF                   = 0x00010000;
  enum HWCAP_SPARC_VIS3                   = 0x00020000;
  enum HWCAP_SPARC_HPC                    = 0x00040000;
  enum HWCAP_SPARC_RANDOM                 = 0x00080000;
  enum HWCAP_SPARC_TRANS                  = 0x00100000;
  enum HWCAP_SPARC_FJFMAU                 = 0x00200000;
  enum HWCAP_SPARC_IMA                    = 0x00400000;
  enum HWCAP_SPARC_ASI_CACHE_SPARING      = 0x00800000;
  enum HWCAP_SPARC_PAUSE                  = 0x01000000;
  enum HWCAP_SPARC_CBCOND                 = 0x02000000;
  enum HWCAP_SPARC_CRYPTO                 = 0x04000000;
}
else version (IBMZ_Any)
{
  // See https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/s390/bits/hwcap.h

  enum HWCAP_S390_ESAN3                   = 1;
  enum HWCAP_S390_ZARCH                   = 2;
  enum HWCAP_S390_STFLE                   = 4;
  enum HWCAP_S390_MSA                     = 8;
  enum HWCAP_S390_LDISP                   = 16;
  enum HWCAP_S390_EIMM                    = 32;
  enum HWCAP_S390_DFP                     = 64;
  enum HWCAP_S390_HPAGE                   = 128;
  enum HWCAP_S390_ETF3EH                  = 256;
  enum HWCAP_S390_HIGH_GPRS               = 512;
  enum HWCAP_S390_TE                      = 1024;
  enum HWCAP_S390_VX                      = 2048;
}
