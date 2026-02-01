/**
  * D header file for Solaris sys/regset.h.
  *
  * Copyright: Copyright © 2025, The D Language Foundation
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Iain Buclaw
  */
module core.sys.solaris.sys.regset;

version (Solaris):
extern (C):
nothrow:
@nogc:

import core.stdc.config : c_long, c_ulong;
import core.sys.posix.sys.types : caddr_t;

version (X86)     version = X86_Any;
version (X86_64)  version = X86_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;

version (X86_64)
{
    enum REG_GSBASE    = 27;
    enum REG_FSBASE    = 26;
    enum REG_DS        = 25;
    enum REG_ES        = 24;

    enum REG_GS        = 23;
    enum REG_FS        = 22;
    enum REG_SS        = 21;
    enum REG_RSP       = 20;
    enum REG_RFL       = 19;
    enum REG_CS        = 18;
    enum REG_RIP       = 17;
    enum REG_ERR       = 16;
    enum REG_TRAPNO    = 15;
    enum REG_RAX       = 14;
    enum REG_RCX       = 13;
    enum REG_RDX       = 12;
    enum REG_RBX       = 11;
    enum REG_RBP       = 10;
    enum REG_RSI       = 9;
    enum REG_RDI       = 8;
    enum REG_R8        = 7;
    enum REG_R9        = 6;
    enum REG_R10       = 5;
    enum REG_R11       = 4;
    enum REG_R12       = 3;
    enum REG_R13       = 2;
    enum REG_R14       = 1;
    enum REG_R15       = 0;

    enum REG_PC = REG_RIP;
    enum REG_FP = REG_RBP;
    enum REG_SP = REG_RSP;
    enum REG_PS = REG_RFL;
    enum REG_R0 = REG_RAX;
    enum REG_R1 = REG_RDX;
}
else version (X86)
{
    enum REG_SS        = 18;
    enum REG_UESP      = 17;
    enum REG_EFL       = 16;
    enum REG_CS        = 15;
    enum REG_EIP       = 14;
    enum REG_ERR       = 13;
    enum REG_TRAPNO    = 12;
    enum REG_EAX       = 11;
    enum REG_ECX       = 10;
    enum REG_EDX       = 9;
    enum REG_EBX       = 8;
    enum REG_ESP       = 7;
    enum REG_EBP       = 6;
    enum REG_ESI       = 5;
    enum REG_EDI       = 4;
    enum REG_DS        = 3;
    enum REG_ES        = 2;
    enum REG_FS        = 1;
    enum REG_GS        = 0;

    enum REG_PC = REG_EIP;
    enum REG_FP = REG_EBP;
    enum REG_SP = REG_UESP;
    enum REG_PS = REG_EFL;
    enum REG_R0 = REG_EAX;
    enum REG_R1 = REG_EDX;
}
else version (SPARC_Any)
{
    enum REG_PC  = 1;
    enum REG_nPC = 2;
    enum REG_Y   = 3;
    enum REG_G1  = 4;
    enum REG_G2  = 5;
    enum REG_G3  = 6;
    enum REG_G4  = 7;
    enum REG_G5  = 8;
    enum REG_G6  = 9;
    enum REG_G7  = 10;
    enum REG_O0  = 11;
    enum REG_O1  = 12;
    enum REG_O2  = 13;
    enum REG_O3  = 14;
    enum REG_O4  = 15;
    enum REG_O5  = 16;
    enum REG_O6  = 17;
    enum REG_O7  = 18;

    enum REG_SP = REG_O6;
    enum REG_R0 = REG_O0;
    enum REG_R1 = REG_O1;

    version (SPARC64)
    {
        enum REG_CCR = 0;
        enum REG_ASI = 19;
        enum REG_FPRS = 20;
    }
    else
    {
        enum REG_PSR = 0;
        enum REG_PS  = REG_PSR;
    }
}
else
    static assert(false, "Architecture not supported.");

/*
 * A gregset_t is defined as an array type.
 */
version (X86_64)
{
    enum _NGREG = 28;
    alias greg_t = c_long;
}
else version (X86)
{
    enum _NGREG = 19;
    alias greg_t = int;
}
else version (SPARC64)
{
    enum _NGREG = 21;
    alias greg_t = c_long;
}
else version (SPARC)
{
    enum _NGREG = 19;
    alias greg_t = int;
}
else
    static assert(0, "unimplemented");

alias gregset_t = greg_t[_NGREG];

version (X86_Any)
{
    /*
     * Xregs extension
     */
    struct xrs_t
    {
        c_ulong xrs_id;
        caddr_t xrs_ptr;
    }
}
else version (SPARC_Any)
{
    /*
     * Defines the minimal format of a floating point instruction queue entry.
     */
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

    /*
     * Defines how a register window can appear on the stack
     */
    struct rwindow
    {
        greg_t[8] rw_local;
        greg_t[8] rw_in;
    }

    struct gwindows_t
    {
        int         wbcnt;
        greg_t[31] *spbuf;
        rwindow[31] wbuf;
    }

    /*
     * For associating extra register state with ucontext structure and is kept
     * within the uc_mcontext filler area
     */
    struct xrs_t
    {
        uint    xrs_id;
        caddr_t xrs_ptr;
    }

    struct cxrs_t
    {
        uint    cxrs_id;
        caddr_t cxrs_ptr;
    }

    alias asrset_t = long[16];
}

/*
 * The floating point processor state
 */
version (X86_64)
{
    private
    {
        alias upad128_t = uint[4];
        union _u_st
        {
            ushort[5]   fpr_16;
            upad128_t   __fpr_pad;
        }
    }

    struct fpu
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
    private alias upad128_t = uint[4];

    struct fpu
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
else version (SPARC64)
{
    struct fpu
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
    struct fpu
    {
        union
        {
            uint[32]   fpu_regs;
            double[16] fpu_dregs;
        }
        fq    *fpu_q;
        uint  fpu_fsr;
        ubyte fpu_qcnt;
        ubyte fpu_q_entrysize;
        ubyte fpu_en;
    }
}
else
    static assert(0, "unimplemented");

alias fpregset_t = fpu;
