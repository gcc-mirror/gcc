/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.ucontext;

private import core.sys.posix.config;
public import core.sys.posix.signal; // for sigset_t, stack_t
private import core.stdc.stdint : uintptr_t;

version (Posix):
extern (C):
nothrow:
@nogc:

version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;
version (S390)    version = IBMZ_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;
version (SystemZ) version = IBMZ_Any;
version (X86)     version = X86_Any;
version (X86_64)  version = X86_Any;

//
// XOpen (XSI)
//
/*
mcontext_t

struct ucontext_t
{
    ucontext_t* uc_link;
    sigset_t    uc_sigmask;
    stack_t     uc_stack;
    mcontext_t  uc_mcontext;
}
*/

version (CRuntime_Glibc)
{

    version (X86_64)
    {
        enum
        {
            REG_R8 = 0,
            REG_R9,
            REG_R10,
            REG_R11,
            REG_R12,
            REG_R13,
            REG_R14,
            REG_R15,
            REG_RDI,
            REG_RSI,
            REG_RBP,
            REG_RBX,
            REG_RDX,
            REG_RAX,
            REG_RCX,
            REG_RSP,
            REG_RIP,
            REG_EFL,
            REG_CSGSFS,     /* Actually short cs, gs, fs, __pad0.  */
            REG_ERR,
            REG_TRAPNO,
            REG_OLDMASK,
            REG_CR2
        }

        private
        {
            struct _libc_fpxreg
            {
                ushort[4] significand;
                ushort    exponent;
                ushort[3] padding;
            }

            struct _libc_xmmreg
            {
                uint[4] element;
            }

            struct _libc_fpstate
            {
                ushort           cwd;
                ushort           swd;
                ushort           ftw;
                ushort           fop;
                ulong            rip;
                ulong            rdp;
                uint             mxcsr;
                uint             mxcr_mask;
                _libc_fpxreg[8]  _st;
                _libc_xmmreg[16] _xmm;
                uint[24]         padding;
            }

            enum NGREG = 23;

            alias c_long            greg_t;
            alias greg_t[NGREG]     gregset_t;
            alias _libc_fpstate*    fpregset_t;
        }

        struct mcontext_t
        {
            gregset_t   gregs;
            fpregset_t  fpregs;
            c_ulong[8]  __reserved1;
        }

        struct ucontext_t
        {
            c_ulong         uc_flags;
            ucontext_t*     uc_link;
            stack_t         uc_stack;
            mcontext_t      uc_mcontext;
            sigset_t        uc_sigmask;
            _libc_fpstate   __fpregs_mem;
        }
    }
    else version (X86)
    {
        enum
        {
            REG_GS = 0,
            REG_FS,
            REG_ES,
            REG_DS,
            REG_EDI,
            REG_ESI,
            REG_EBP,
            REG_ESP,
            REG_EBX,
            REG_EDX,
            REG_ECX,
            REG_EAX,
            REG_TRAPNO,
            REG_ERR,
            REG_EIP,
            REG_CS,
            REG_EFL,
            REG_UESP,
            REG_SS
        }

        private
        {
            struct _libc_fpreg
            {
              ushort[4] significand;
              ushort    exponent;
            }

            struct _libc_fpstate
            {
              c_ulong           cw;
              c_ulong           sw;
              c_ulong           tag;
              c_ulong           ipoff;
              c_ulong           cssel;
              c_ulong           dataoff;
              c_ulong           datasel;
              _libc_fpreg[8]    _st;
              c_ulong           status;
            }

            enum NGREG = 19;

            alias int               greg_t;
            alias greg_t[NGREG]     gregset_t;
            alias _libc_fpstate*    fpregset_t;
        }

        struct mcontext_t
        {
            gregset_t   gregs;
            fpregset_t  fpregs;
            c_ulong     oldmask;
            c_ulong     cr2;
        }

        struct ucontext_t
        {
            c_ulong         uc_flags;
            ucontext_t*     uc_link;
            stack_t         uc_stack;
            mcontext_t      uc_mcontext;
            sigset_t        uc_sigmask;
            _libc_fpstate   __fpregs_mem;
        }
    }
    else version (HPPA)
    {
        private
        {
            enum NGREG  = 80;
            enum NFPREG = 32;

            alias c_ulong greg_t;

            struct gregset_t
            {
                greg_t[32] g_regs;
                greg_t[8] sr_regs;
                greg_t[24] cr_regs;
                greg_t[16] g_pad;
            }

            struct fpregset_t
            {
                double[32] fpregs;
            }
        }

        struct mcontext_t
        {
            greg_t sc_flags;
            greg_t[32] sc_gr;
            fpregset_t sc_fr;
            greg_t[2] sc_iasq;
            greg_t[2] sc_iaoq;
            greg_t sc_sar;
        }

        struct ucontext_t
        {
            c_ulong uc_flags;
            ucontext_t* uc_link;
            stack_t uc_stack;
            mcontext_t uc_mcontext;
            sigset_t uc_sigmask;
        }
    }
    else version (MIPS32)
    {
        private
        {
            enum NGREG  = 32;
            enum NFPREG = 32;

            alias ulong         greg_t;
            alias greg_t[NGREG] gregset_t;

            struct fpregset_t
            {
                union fp_r_t
                {
                    double[NFPREG]  fp_dregs;
                    static struct fp_fregs_t
                    {
                        float   _fp_fregs;
                        uint    _fp_pad;
                    } fp_fregs_t[NFPREG] fp_fregs;
                } fp_r_t fp_r;
            }
        }

        version (MIPS_O32)
        {
            struct mcontext_t
            {
                uint regmask;
                uint status;
                greg_t pc;
                gregset_t gregs;
                fpregset_t fpregs;
                uint fp_owned;
                uint fpc_csr;
                uint fpc_eir;
                uint used_math;
                uint dsp;
                greg_t mdhi;
                greg_t mdlo;
                c_ulong hi1;
                c_ulong lo1;
                c_ulong hi2;
                c_ulong lo2;
                c_ulong hi3;
                c_ulong lo3;
            }
        }
        else
        {
            struct mcontext_t
            {
                gregset_t gregs;
                fpregset_t fpregs;
                greg_t mdhi;
                greg_t hi1;
                greg_t hi2;
                greg_t hi3;
                greg_t mdlo;
                greg_t lo1;
                greg_t lo2;
                greg_t lo3;
                greg_t pc;
                uint fpc_csr;
                uint used_math;
                uint dsp;
                uint reserved;
            }
        }

        struct ucontext_t
        {
            c_ulong     uc_flags;
            ucontext_t* uc_link;
            stack_t     uc_stack;
            mcontext_t  uc_mcontext;
            sigset_t    uc_sigmask;
        }
    }
    else version (MIPS64)
    {
        private
        {
            enum NGREG  = 32;
            enum NFPREG = 32;

            alias ulong         greg_t;
            alias greg_t[NGREG] gregset_t;

            struct fpregset_t
            {
                union fp_r_t
                {
                    double[NFPREG]  fp_dregs;
                    static struct fp_fregs_t
                    {
                        float   _fp_fregs;
                        uint    _fp_pad;
                    } fp_fregs_t[NFPREG] fp_fregs;
                } fp_r_t fp_r;
            }
        }

        struct mcontext_t
        {
            gregset_t gregs;
            fpregset_t fpregs;
            greg_t mdhi;
            greg_t hi1;
            greg_t hi2;
            greg_t hi3;
            greg_t mdlo;
            greg_t lo1;
            greg_t lo2;
            greg_t lo3;
            greg_t pc;
            uint fpc_csr;
            uint used_math;
            uint dsp;
            uint reserved;
        }

        struct ucontext_t
        {
            c_ulong     uc_flags;
            ucontext_t* uc_link;
            stack_t     uc_stack;
            mcontext_t  uc_mcontext;
            sigset_t    uc_sigmask;
        }
    }
    else version (PPC)
    {
        private
        {
            enum NGREG  = 48;

            alias c_ulong        greg_t;
            alias greg_t[NGREG]  gregset_t;

            struct fpregset_t
            {
                double[32] fpregs;
                double fpscr;
                uint[2] _pad;
            }

            struct vrregset_t
            {
                uint[32][4] vrregs;
                uint        vrsave;
                uint[2]     __pad;
                uint vscr;
            }

            struct pt_regs
            {
                c_ulong[32] gpr;
                c_ulong     nip;
                c_ulong     msr;
                c_ulong     orig_gpr3;
                c_ulong     ctr;
                c_ulong     link;
                c_ulong     xer;
                c_ulong     ccr;
                c_ulong     mq;
                c_ulong     trap;
                c_ulong     dar;
                c_ulong     dsisr;
                c_ulong     result;
            }
        }

        struct mcontext_t
        {
            gregset_t gregs;
            fpregset_t fpregs;
            align(16) vrregset_t vrregs;
        }

        struct ucontext_t
        {
            c_ulong     uc_flags;
            ucontext_t* uc_link;
            stack_t     uc_stack;
            int[7]      uc_pad;
            union uc_mcontext
            {
                pt_regs*     regs;
                mcontext_t*  uc_regs;
            }
            sigset_t    uc_sigmask;
            char[mcontext_t.sizeof + 12] uc_reg_space = 0;
        }
    }
    else version (PPC64)
    {
        private
        {
            enum NGREG  = 48;
            enum NFPREG = 33;
            enum NVRREG = 34;

            alias c_ulong        greg_t;
            alias greg_t[NGREG]  gregset_t;
            alias double[NFPREG] fpregset_t;

            struct vscr_t
            {
                uint[3] __pad;
                uint    vscr_word;
            }

            struct vrregset_t
            {
                uint[32][4] vrregs;
                vscr_t      vscr;
                uint        vrsave;
                uint[3]     __pad;
            }

            struct pt_regs
            {
                c_ulong[32] gpr;
                c_ulong     nip;
                c_ulong     msr;
                c_ulong     orig_gpr3;
                c_ulong     ctr;
                c_ulong     link;
                c_ulong     xer;
                c_ulong     ccr;
                c_ulong     softe;
                c_ulong     trap;
                c_ulong     dar;
                c_ulong     dsisr;
                c_ulong     result;
            }
        }

        struct mcontext_t
        {
            c_ulong[4] __unused;
            int signal;
            int __pad0;
            c_ulong handler;
            c_ulong oldmask;
            pt_regs* regs;
            gregset_t gp_regs;
            fpregset_t fp_regs;
            vrregset_t *v_regs;
            c_long[NVRREG+NVRREG+1] vmx_reserve;
        }

        struct ucontext_t
        {
            c_ulong     uc_flags;
            ucontext_t* uc_link;
            stack_t     uc_stack;
            sigset_t    uc_sigmask;
            mcontext_t  uc_mcontext;
        }
    }
    else version (ARM)
    {
        enum
        {
            R0 = 0,
            R1 = 1,
            R2 = 2,
            R3 = 3,
            R4 = 4,
            R5 = 5,
            R6 = 6,
            R7 = 7,
            R8 = 8,
            R9 = 9,
            R10 = 10,
            R11 = 11,
            R12 = 12,
            R13 = 13,
            R14 = 14,
            R15 = 15
        }

        struct sigcontext
        {
            c_ulong trap_no;
            c_ulong error_code;
            c_ulong oldmask;
            c_ulong arm_r0;
            c_ulong arm_r1;
            c_ulong arm_r2;
            c_ulong arm_r3;
            c_ulong arm_r4;
            c_ulong arm_r5;
            c_ulong arm_r6;
            c_ulong arm_r7;
            c_ulong arm_r8;
            c_ulong arm_r9;
            c_ulong arm_r10;
            c_ulong arm_fp;
            c_ulong arm_ip;
            c_ulong arm_sp;
            c_ulong arm_lr;
            c_ulong arm_pc;
            c_ulong arm_cpsr;
            c_ulong fault_address;
        }

        //alias elf_fpregset_t fpregset_t;
        alias sigcontext mcontext_t;

        struct ucontext_t
        {
            c_ulong uc_flags;
            ucontext_t* uc_link;
            stack_t uc_stack;
            mcontext_t uc_mcontext;
            sigset_t uc_sigmask;
            align(8) c_ulong[128] uc_regspace;
        }
    }
    else version (AArch64)
    {
        alias int greg_t;

        struct sigcontext {
            ulong           fault_address;
            /* AArch64 registers */
            ulong[31]       regs;
            ulong           sp;
            ulong           pc;
            ulong           pstate;
            /* 4K reserved for FP/SIMD state and future expansion */
            align(16) ubyte[4096] __reserved;
        }

        alias sigcontext mcontext_t;

        struct ucontext_t
        {
            c_ulong     uc_flags;
            ucontext_t* uc_link;
            stack_t     uc_stack;
            sigset_t    uc_sigmask;
            mcontext_t  uc_mcontext;
        }
    }
    else version (RISCV_Any)
    {
        private
        {
            alias c_ulong[32] __riscv_mc_gp_state;

            struct __riscv_mc_f_ext_state
            {
                uint[32] __f;
                uint __fcsr;
            }

            struct __riscv_mc_d_ext_state
            {
                ulong[32] __f;
                uint __fcsr;
            }

            struct __riscv_mc_q_ext_state
            {
                align(16) ulong[64] __f;
                uint __fcsr;
                uint[3] __reserved;
            }

            union __riscv_mc_fp_state
            {
                __riscv_mc_f_ext_state __f;
                __riscv_mc_d_ext_state __d;
                __riscv_mc_q_ext_state __q;
            }
        }

        struct mcontext_t
        {
            __riscv_mc_gp_state __gregs;
            __riscv_mc_fp_state __fpregs;
        }

        struct ucontext_t
        {
            c_ulong     __uc_flags;
            ucontext_t* uc_link;
            stack_t     uc_stack;
            sigset_t    uc_sigmask;
            char[1024 / 8 - sigset_t.sizeof] __reserved = 0;
            mcontext_t  uc_mcontext;
        }
    }
    else version (SPARC64)
    {
        enum MC_NGREG = 19;
        alias mc_greg_t = c_ulong;
        alias mc_gregset_t = mc_greg_t[MC_NGREG];

        struct mc_fq
        {
            c_ulong* mcfq_addr;
            uint     mcfq_insn;
        }

        struct mc_fpu_t
        {
            union mcfpu_fregs_t
            {
                uint[32]    sregs;
                c_ulong[32] dregs;
                real[16]    qregs;
            }
            mcfpu_fregs_t mcfpu_fregs;
            c_ulong       mcfpu_fsr;
            c_ulong       mcfpu_fprs;
            c_ulong       mcfpu_gsr;
            mc_fq*        mcfpu_fq;
            ubyte         mcfpu_qcnt;
            ubyte         mcfpu_qentsz;
            ubyte         mcfpu_enab;
        }

        struct mcontext_t
        {
            mc_gregset_t mc_gregs;
            mc_greg_t    mc_fp;
            mc_greg_t    mc_i7;
            mc_fpu_t     mc_fpregs;
        }

        struct ucontext_t
        {
            ucontext_t* uc_link;
            c_ulong     uc_flags;
            c_ulong     __uc_sigmask;
            mcontext_t  uc_mcontext;
            stack_t     uc_stack;
            sigset_t    uc_sigmask;
        }

        /* Location of the users' stored registers relative to R0.
         * Usage is as an index into a gregset_t array. */
        enum
        {
            REG_PSR = 0,
            REG_PC  = 1,
            REG_nPC = 2,
            REG_Y   = 3,
            REG_G1  = 4,
            REG_G2  = 5,
            REG_G3  = 6,
            REG_G4  = 7,
            REG_G5  = 8,
            REG_G6  = 9,
            REG_G7  = 10,
            REG_O0  = 11,
            REG_O1  = 12,
            REG_O2  = 13,
            REG_O3  = 14,
            REG_O4  = 15,
            REG_O5  = 16,
            REG_O6  = 17,
            REG_O7  = 18,
            REG_ASI = 19,
            REG_FPRS = 20,
        }

        enum NGREG = 21;
        alias greg_t = c_ulong;
        alias gregset_t = greg_t[NGREG];
    }
    else version (IBMZ_Any)
    {
        public import core.sys.posix.signal : sigset_t;

        enum NGREG = 27;

        alias greg_t = c_ulong;
        alias gregset_t = align(8) greg_t[NGREG];

        align(8) struct __psw_t
        {
            c_ulong mask;
            c_ulong addr;
        }

        union fpreg_t
        {
            double d;
            float  f;
        }

        struct fpregset_t
        {
            uint        fpc;
            fpreg_t[16] fprs;
        }

        struct  mcontext_t
        {
            __psw_t     psw;
            c_ulong[16] gregs;
            uint[16]    aregs;
            fpregset_t  fpregs;
        }

        struct ucontext
        {
            c_ulong    uc_flags;
            ucontext*  uc_link;
            stack_t    uc_stack;
            mcontext_t uc_mcontext;
            sigset_t   uc_sigmask;
        }

        alias ucontext_t = ucontext;
    }
    else
        static assert(0, "unimplemented");
}
else version (FreeBSD)
{
    // <machine/ucontext.h>
    version (X86_64)
    {
      alias long __register_t;
      alias uint __uint32_t;
      alias ushort __uint16_t;

      struct mcontext_t {
       __register_t    mc_onstack;
       __register_t    mc_rdi;
       __register_t    mc_rsi;
       __register_t    mc_rdx;
       __register_t    mc_rcx;
       __register_t    mc_r8;
       __register_t    mc_r9;
       __register_t    mc_rax;
       __register_t    mc_rbx;
       __register_t    mc_rbp;
       __register_t    mc_r10;
       __register_t    mc_r11;
       __register_t    mc_r12;
       __register_t    mc_r13;
       __register_t    mc_r14;
       __register_t    mc_r15;
       __uint32_t      mc_trapno;
       __uint16_t      mc_fs;
       __uint16_t      mc_gs;
       __register_t    mc_addr;
       __uint32_t      mc_flags;
       __uint16_t      mc_es;
       __uint16_t      mc_ds;
       __register_t    mc_err;
       __register_t    mc_rip;
       __register_t    mc_cs;
       __register_t    mc_rflags;
       __register_t    mc_rsp;
       __register_t    mc_ss;

       long    mc_len;                 /* sizeof(mcontext_t) */

       long    mc_fpformat;
       long    mc_ownedfp;

       align(16)
       long[64]    mc_fpstate;

       __register_t    mc_fsbase;
       __register_t    mc_gsbase;

       long[6]    mc_spare;
      }
    }
    else version (X86)
    {
        alias int __register_t;

        struct mcontext_t
        {
            __register_t    mc_onstack;
            __register_t    mc_gs;
            __register_t    mc_fs;
            __register_t    mc_es;
            __register_t    mc_ds;
            __register_t    mc_edi;
            __register_t    mc_esi;
            __register_t    mc_ebp;
            __register_t    mc_isp;
            __register_t    mc_ebx;
            __register_t    mc_edx;
            __register_t    mc_ecx;
            __register_t    mc_eax;
            __register_t    mc_trapno;
            __register_t    mc_err;
            __register_t    mc_eip;
            __register_t    mc_cs;
            __register_t    mc_eflags;
            __register_t    mc_esp;
            __register_t    mc_ss;

            int             mc_len;
            int             mc_fpformat;
            int             mc_ownedfp;
            int[1]          mc_spare1;

            align(16)
            int[128]        mc_fpstate;

            __register_t    mc_fsbase;
            __register_t    mc_gsbase;

            int[6]          mc_spare2;
        }
    }
    else version (AArch64)
    {
        alias __register_t = long;

        struct gpregs
        {
            __register_t[30] gp_x;
            __register_t     gp_lr;
            __register_t     gp_sp;
            __register_t     gp_elr;
            uint             gp_spsr;
            int              gp_pad;
        }

        struct fpregs
        {
            ulong[2][32]    fp_q; // __uint128_t
            uint            fp_sr;
            uint            fp_cr;
            int             fp_flags;
            int             fp_pad;
        }

        struct mcontext_t
        {
            gpregs          mc_gpregs;
            fpregs          mc_fpregs;
            int             mc_flags;
            int             mc_pad;
            ulong[8]        mc_spare;
        }
    }

    // <ucontext.h>
    enum UCF_SWAPPED = 0x00000001;

    struct ucontext_t
    {
        sigset_t        uc_sigmask;
        mcontext_t      uc_mcontext;

        ucontext_t*     uc_link;
        stack_t         uc_stack;
        int             uc_flags;
        int[4]          __spare__;
    }
}
else version (NetBSD)
{

    version (X86_64)
    {
      enum { NGREG = 26 };
      alias __greg_t = ulong;
      alias __gregset_t = __greg_t[NGREG];
      alias __fpregset_t = align(8)ubyte[512];

      struct mcontext_t {
        __gregset_t     __gregs;
        __greg_t        _mc_tlsbase;
        __fpregset_t    __fpregs;
      }
    }
    else version (X86)
    {
      enum { NGREG = 19 };
      alias __greg_t = ulong;
      alias __gregset_t = __greg_t[_NGREG];
      struct __fpregset_t{
        union __fp_reg_set{
                struct __fpchip_state{
                        int[27]     __fp_state; /* Environment and registers */
                } ;       /* x87 regs in fsave format */
                struct __fp_xmm_state{
                        ubyte[512]    __fp_xmm;
                } ;       /* x87 and xmm regs in fxsave format */
                int[128]     __fp_fpregs;
        };
        __fpregset_t __fp_reg_set;
        int[33]     __fp_pad;                   /* Historic padding */
      };

      struct mcontext_t {
        __gregset_t     __gregs;
        __fpregset_t    __fpregs;
        __greg_t        _mc_tlsbase;
      }
    }

    struct ucontext_t
    {
        uint    uc_flags;       /* properties */
        ucontext_t *    uc_link;        /* context to resume */
        sigset_t        uc_sigmask;     /* signals blocked in this context */
        stack_t         uc_stack;       /* the stack used by this context */
        mcontext_t      uc_mcontext;    /* machine state */
        /+ todo #if defined(_UC_MACHINE_PAD)
                long            __uc_pad[_UC_MACHINE_PAD];
        #endif
        +/

    }
}
else version (DragonFlyBSD)
{
    // <machine/ucontext.h>
    version (X86_64)
    {
      alias long __register_t;
      alias uint __uint32_t;
      alias ushort __uint16_t;

      struct mcontext_t {
        __register_t    mc_onstack;
        __register_t    mc_rdi;
        __register_t    mc_rsi;
        __register_t    mc_rdx;
        __register_t    mc_rcx;
        __register_t    mc_r8;
        __register_t    mc_r9;
        __register_t    mc_rax;
        __register_t    mc_rbx;
        __register_t    mc_rbp;
        __register_t    mc_r10;
        __register_t    mc_r11;
        __register_t    mc_r12;
        __register_t    mc_r13;
        __register_t    mc_r14;
        __register_t    mc_r15;
        __register_t    mc_xflags;
        __register_t    mc_trapno;
        __register_t    mc_addr;
        __register_t    mc_flags;
        __register_t    mc_err;
        __register_t    mc_rip;
        __register_t    mc_cs;
        __register_t    mc_rflags;
        __register_t    mc_rsp;
        __register_t    mc_ss;

        uint            mc_len;
        uint            mc_fpformat;
        uint            mc_ownedfp;
        uint            mc_reserved;
        uint[8]         mc_unused;
        int[256]        mc_fpregs;
      };  // __attribute__((aligned(64)));
    }
    else
    {
        static assert(0, "Only X86_64 support on DragonFlyBSD");
    }

    // <ucontext.h>
    enum UCF_SWAPPED = 0x00000001;

    struct ucontext_t
    {
        sigset_t        uc_sigmask;
        mcontext_t      uc_mcontext;

        ucontext_t*     uc_link;
        stack_t         uc_stack;
        void            function(ucontext_t *, void *) uc_cofunc;
        void*           uc_arg;
        int[4]          __spare__;
    }
}
else version (Solaris)
{
    private import core.stdc.stdint;

    alias uint[4] upad128_t;

    version (SPARC64)
    {
        enum _NGREG = 21;
        alias long greg_t;
    }
    else version (SPARC)
    {
        enum _NGREG = 19;
        alias int greg_t;
    }
    else version (X86_64)
    {
        enum _NGREG = 28;
        alias long greg_t;
    }
    else version (X86)
    {
        enum _NGREG = 19;
        alias int greg_t;
    }
    else
        static assert(0, "unimplemented");

    alias greg_t[_NGREG] gregset_t;

    version (SPARC64)
    {
        private
        {
            struct _fpq
            {
                uint *fpq_addr;
                uint fpq_instr;
            }

            struct fq
            {
                union
                {
                    double whole;
                    _fpq fpq;
                }
            }
        }

        struct fpregset_t
        {
            union
            {
                uint[32]   fpu_regs;
                double[32] fpu_dregs;
                real[16]   fpu_qregs;
            }
            fq    *fpu_q;
            ulong fpu_fsr;
            ubyte fpu_qcnt;
            ubyte fpu_q_entrysize;
            ubyte fpu_en;
        }
    }
    else version (SPARC)
    {
        private
        {
            struct _fpq
            {
                uint *fpq_addr;
                uint fpq_instr;
            }

            struct fq
            {
                union
                {
                    double whole;
                    _fpq fpq;
                }
            }
        }

        struct fpregset_t
        {
            union
            {
                uint[32]   fpu_regs;
                double[16] fpu_dregs;
            };
            fq    *fpu_q;
            uint  fpu_fsr;
            ubyte fpu_qcnt;
            ubyte fpu_q_entrysize;
            ubyte fpu_en;
        }
    }
    else version (X86_64)
    {
        private
        {
            union _u_st
            {
                ushort[5]   fpr_16;
                upad128_t   __fpr_pad;
            }
        }

        struct fpregset_t
        {
            union fp_reg_set
            {
                struct fpchip_state
                {
                    ushort          cw;
                    ushort          sw;
                    ubyte           fctw;
                    ubyte           __fx_rsvd;
                    ushort          fop;
                    ulong           rip;
                    ulong           rdp;
                    uint            mxcsr;
                    uint            mxcsr_mask;
                    _u_st[8]        st;
                    upad128_t[16]   xmm;
                    upad128_t[6]    __fx_ign2;
                    uint            status;
                    uint            xstatus;
                }
                uint[130]   f_fpregs;
            }
        }
    }
    else version (X86)
    {
        struct fpregset_t
        {
            union u_fp_reg_set
            {
                struct s_fpchip_state
                {
                    uint[27]        state;
                    uint            status;
                    uint            mxcsr;
                    uint            xstatus;
                    uint[2]         __pad;
                    upad128_t[8]    xmm;
                }
                s_fpchip_state    fpchip_state;

                struct s_fp_emul_space
                {
                    ubyte[246]  fp_emul;
                    ubyte[2]    fp_epad;
                }
                s_fp_emul_space   fp_emul_space;
                uint[95]        f_fpregs;
            }
        u_fp_reg_set fp_reg_set;
        }
    }
    else
        static assert(0, "unimplemented");

    version (SPARC_Any)
    {
        private
        {
            struct rwindow
            {
                greg_t[8]     rw_local;
                greg_t[8]     rw_in;
            }

            struct gwindows_t
            {
                int         wbcnt;
                greg_t[31] *spbuf;
                rwindow[31] wbuf;
            }

            struct xrs_t
            {
                uint         xrs_id;
                caddr_t      xrs_ptr;
            }

            struct cxrs_t
            {
                uint         cxrs_id;
                caddr_t      cxrs_ptr;
            }

            alias int64_t[16] asrset_t;
        }

        struct mcontext_t
        {
            gregset_t    gregs;
            gwindows_t   *gwins;
            fpregset_t   fpregs;
            xrs_t        xrs;
            version (SPARC64)
            {
                asrset_t asrs;
                cxrs_t   cxrs;
                c_long[2] filler;
            }
            else version (SPARC)
            {
                cxrs_t   cxrs;
                c_long[17] filler;
            }
        }
    }
    else version (X86_Any)
    {
        private
        {
            struct xrs_t
            {
                uint         xrs_id;
                caddr_t      xrs_ptr;
            }
        }

        struct mcontext_t
        {
            gregset_t   gregs;
            fpregset_t  fpregs;
        }
    }

    struct ucontext_t
    {
        version (SPARC_Any)
            uint    uc_flags;
        else version (X86_Any)
            c_ulong uc_flags;
        ucontext_t  *uc_link;
        sigset_t    uc_sigmask;
        stack_t     uc_stack;
        mcontext_t  uc_mcontext;
        version (SPARC64)
            c_long[4]  uc_filler;
        else version (SPARC)
            c_long[23] uc_filler;
        else version (X86_Any)
        {
            xrs_t      uc_xrs;
            c_long[3]  uc_filler;
        }
    }
}
else version (CRuntime_UClibc)
{
    version (X86_64)
    {
        enum
        {
            REG_R8 = 0,
            REG_R9,
            REG_R10,
            REG_R11,
            REG_R12,
            REG_R13,
            REG_R14,
            REG_R15,
            REG_RDI,
            REG_RSI,
            REG_RBP,
            REG_RBX,
            REG_RDX,
            REG_RAX,
            REG_RCX,
            REG_RSP,
            REG_RIP,
            REG_EFL,
            REG_CSGSFS,     /* Actually short cs, gs, fs, __pad0.  */
            REG_ERR,
            REG_TRAPNO,
            REG_OLDMASK,
            REG_CR2
        }

        alias sigcontext mcontext_t;

        struct ucontext_t
        {
            c_ulong         uc_flags;
            ucontext_t*     uc_link;
            stack_t         uc_stack;
            mcontext_t      uc_mcontext;
            sigset_t        uc_sigmask;
        }
    }
    else version (MIPS32)
    {
        alias greg_t    = ulong;
        enum NGREG      = 32;
        enum NFPREG     = 32;
        alias gregset_t = greg_t[NGREG];

        struct fpregset_t
        {
            union fp_r
            {
                double[NFPREG]  fp_dregs;
                struct _fp_fregs
                {
                    float   _fp_fregs;
                    uint    _fp_pad;
                }
                _fp_fregs[NFPREG] fp_fregs;
            }
        }

        version (MIPS_O32)
        {
            struct mcontext_t
            {
                uint regmask;
                uint status;
                greg_t pc;
                gregset_t gregs;
                fpregset_t fpregs;
                uint fp_owned;
                uint fpc_csr;
                uint fpc_eir;
                uint used_math;
                uint dsp;
                greg_t mdhi;
                greg_t mdlo;
                c_ulong hi1;
                c_ulong lo1;
                c_ulong hi2;
                c_ulong lo2;
                c_ulong hi3;
                c_ulong lo3;
            }
        }
        else
        {
            struct mcontext_t
            {
                gregset_t gregs;
                fpregset_t fpregs;
                greg_t mdhi;
                greg_t hi1;
                greg_t hi2;
                greg_t hi3;
                greg_t mdlo;
                greg_t lo1;
                greg_t lo2;
                greg_t lo3;
                greg_t pc;
                uint fpc_csr;
                uint used_math;
                uint dsp;
                uint reserved;
            }
        }

        struct ucontext_t
        {
            c_ulong uc_flags;
            ucontext_t* uc_link;
            stack_t uc_stack;
            mcontext_t uc_mcontext;
            sigset_t uc_sigmask;
        }
    }
    else version (ARM)
    {
        enum
        {
            R0 = 0,
            R1 = 1,
            R2 = 2,
            R3 = 3,
            R4 = 4,
            R5 = 5,
            R6 = 6,
            R7 = 7,
            R8 = 8,
            R9 = 9,
            R10 = 10,
            R11 = 11,
            R12 = 12,
            R13 = 13,
            R14 = 14,
            R15 = 15
        }

        struct sigcontext
        {
            c_ulong trap_no;
            c_ulong error_code;
            c_ulong oldmask;
            c_ulong arm_r0;
            c_ulong arm_r1;
            c_ulong arm_r2;
            c_ulong arm_r3;
            c_ulong arm_r4;
            c_ulong arm_r5;
            c_ulong arm_r6;
            c_ulong arm_r7;
            c_ulong arm_r8;
            c_ulong arm_r9;
            c_ulong arm_r10;
            c_ulong arm_fp;
            c_ulong arm_ip;
            c_ulong arm_sp;
            c_ulong arm_lr;
            c_ulong arm_pc;
            c_ulong arm_cpsr;
            c_ulong fault_address;
        }

        alias sigcontext mcontext_t;

        struct ucontext_t
        {
            c_ulong uc_flags;
            ucontext_t* uc_link;
            stack_t uc_stack;
            mcontext_t uc_mcontext;
            sigset_t uc_sigmask;
            align(8) c_ulong[128] uc_regspace;
        }
    }
    else
        static assert(0, "unimplemented");
}

//
// Obsolescent (OB)
//
/*
int  getcontext(ucontext_t*);
void makecontext(ucontext_t*, void function(), int, ...);
int  setcontext(in ucontext_t*);
int  swapcontext(ucontext_t*, in ucontext_t*);
*/

static if ( is( ucontext_t ) )
{
    int  getcontext(ucontext_t*);

    version (Solaris)
    {
        version (SPARC_Any)
        {
            void __makecontext_v2(ucontext_t*, void function(), int, ...);
            alias makecontext = __makecontext_v2;
        }
        else
            void makecontext(ucontext_t*, void function(), int, ...);
    }
    else
        void makecontext(ucontext_t*, void function(), int, ...);

    int  setcontext(in ucontext_t*);
    int  swapcontext(ucontext_t*, in ucontext_t*);
}

version (Solaris)
{
    int walkcontext(in ucontext_t*, int function(uintptr_t, int, void*), void*);
    int addrtosymstr(uintptr_t, char*, int);
    int printstack(int);
}

