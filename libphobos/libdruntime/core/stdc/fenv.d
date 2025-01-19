/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_fenv.h.html, _fenv.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/_fenv.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

module core.stdc.fenv;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

extern (C):
nothrow:
@nogc:

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (HPPA)    version = HPPA_Any;
version (MIPS32)  version = MIPS_Any;
version (MIPS64)  version = MIPS_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;
version (S390)    version = IBMZ_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;
version (SystemZ) version = IBMZ_Any;
version (X86)     version = X86_Any;
version (X86_64)  version = X86_Any;

version (MinGW)
    version = GNUFP;
version (CRuntime_Glibc)
    version = GNUFP;

version (GNUFP)
{
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/x86/fpu/bits/fenv.h
    version (X86)
    {
        struct fenv_t
        {
            ushort __control_word;
            ushort __unused1;
            ushort __status_word;
            ushort __unused2;
            ushort __tags;
            ushort __unused3;
            uint   __eip;
            ushort __cs_selector;
            ushort __opcode;
            uint   __data_offset;
            ushort __data_selector;
            ushort __unused5;
        }

        alias fexcept_t = ushort;
    }
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/x86/fpu/bits/fenv.h
    else version (X86_64)
    {
        struct fenv_t
        {
            ushort __control_word;
            ushort __unused1;
            ushort __status_word;
            ushort __unused2;
            ushort __tags;
            ushort __unused3;
            uint   __eip;
            ushort __cs_selector;
            ushort __opcode;
            uint   __data_offset;
            ushort __data_selector;
            ushort __unused5;
            uint   __mxcsr;
        }

        alias fexcept_t = ushort;
    }
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/hppa/bits/fenv.h
    else version (HPPA_Any)
    {
        struct fenv_t
        {
            uint    __status_word;
            uint[7] __exception;
        }

        alias fexcept_t = uint;
    }
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/mips/bits/fenv.h
    else version (MIPS_Any)
    {
        struct fenv_t
        {
            uint   __fp_control_register;
        }

        alias fexcept_t = ushort;
    }
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/aarch64/bits/fenv.h
    else version (AArch64)
    {
        struct fenv_t
        {
            uint __fpcr;
            uint __fpsr;
        }

        alias fexcept_t = uint;
    }
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/arm/bits/fenv.h
    else version (ARM)
    {
        struct fenv_t
        {
            uint __cw;
        }

        alias fexcept_t = uint;
    }
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/powerpc/bits/fenv.h
    else version (PPC_Any)
    {
        alias fenv_t = double;
        alias fexcept_t = uint;
    }
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/riscv/bits/fenv.h
    else version (RISCV_Any)
    {
        alias fenv_t = uint;
        alias fexcept_t = uint;
    }
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/sparc/fpu/bits/fenv.h
    else version (SPARC_Any)
    {
        import core.stdc.config : c_ulong;

        alias fenv_t = c_ulong;
        alias fexcept_t = c_ulong;
    }
    // https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/s390/fpu/bits/fenv.h
    else version (IBMZ_Any)
    {
        struct fenv_t
        {
            fexcept_t __fpc;
            void*     __unused;
        }

        alias fexcept_t = uint;
    }
    else version (LoongArch64)
    {
        struct fenv_t
        {
            uint   __fp_control_register;
        }

        alias fexcept_t = uint;
    }
    else
    {
        static assert(0, "Unimplemented architecture");
    }
}
else version (CRuntime_Microsoft)
{
    struct fenv_t
    {
        uint ctl;
        uint stat;
    }

    alias fexcept_t = uint;
}
else version (Darwin)
{
    version (BigEndian)
    {
        alias uint fenv_t;
        alias uint fexcept_t;
    }
    version (LittleEndian)
    {
        struct fenv_t
        {
            ushort  __control;
            ushort  __status;
            uint    __mxcsr;
            byte[8] __reserved;
        }

        alias ushort fexcept_t;
    }
}
else version (FreeBSD)
{
    struct fenv_t
    {
        ushort __control;
        ushort __mxcsr_hi;
        ushort __status;
        ushort __mxcsr_lo;
        uint __tag;
        byte[16] __other;
    }

    alias ushort fexcept_t;
}
else version (NetBSD)
{
    version (X86_64)
    {
        struct fenv_t
        {
            struct _x87
            {
                    uint control;       /* Control word register */
                    uint status;        /* Status word register */
                    uint tag;           /* Tag word register */
                    uint[4] others;     /* EIP, Pointer Selector, etc */
            }
            _x87 x87;

            uint mxcsr;                 /* Control and status register */
        }
   }
   version (X86)
   {
        struct fenv_t
        {
            struct _x87
            {
                    ushort control;     /* Control word register */
                    ushort unused1;
                    ushort status;      /* Status word register */
                    ushort unused2;
                    ushort tag;         /* Tag word register */
                    ushort unused3;
                    uint[4] others;     /* EIP, Pointer Selector, etc */
            }
            _x87 x87;
            uint mxcsr;                 /* Control and status register */
        }

    }

    alias uint fexcept_t;
}
else version (OpenBSD)
{
    struct fenv_t
    {
        struct __x87
        {
            uint    __control;
            uint    __status;
            uint    __tag;
            uint[4] __others;
        }
    }
    uint __mxcsr;

    alias fexcept_t = uint;
}
else version (DragonFlyBSD)
{
    struct fenv_t
    {
        struct _x87
        {
                uint control;
                uint status;
                uint tag;
                uint[4] others;
        }
        _x87 x87;

        uint mxcsr;
    }

    alias uint fexcept_t;
}
else version (CRuntime_Bionic)
{
    version (X86)
    {
        struct fenv_t
        {
            ushort   __control;
            ushort   __mxcsr_hi;
            ushort   __status;
            ushort   __mxcsr_lo;
            uint     __tag;
            byte[16] __other;
        }

        alias ushort fexcept_t;
    }
    else version (ARM)
    {
        alias uint fenv_t;
        alias uint fexcept_t;
    }
    else version (AArch64)
    {
        struct fenv_t
        {
            uint   __control;
            uint   __status;
        }

        alias uint fexcept_t;
    }
    else version (X86_64)
    {
        struct fenv_t
        {
            struct _x87
            {
                uint    __control;
                uint    __status;
                uint    __tag;
                uint[4] __others;
            }
            _x87 __x87;

            uint __mxcsr;
        }

        alias uint fexcept_t;
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }
}
else version (Solaris)
{
    import core.stdc.config : c_ulong;

    enum FEX_NUM_EXC = 12;

    struct fex_handler_t
    {
        int             __mode;
        void function() __handler;
    }

    struct fenv_t
    {
        fex_handler_t[FEX_NUM_EXC]  __handler;
        c_ulong                     __fsr;
    }

    alias int fexcept_t;
}
else version (CRuntime_Musl)
{
    version (AArch64)
    {
        struct fenv_t
        {
            uint __fpcr;
            uint __fpsr;
        }
        alias uint fexcept_t;
    }
    else version (ARM)
    {
        import core.stdc.config : c_ulong;

        struct fenv_t
        {
            c_ulong __cw;
        }
        alias c_ulong fexcept_t;
    }
    else version (IBMZ_Any)
    {
        alias uint fenv_t;
        alias uint fexcept_t;
    }
    else version (MIPS_Any)
    {
        struct fenv_t
        {
            uint __cw;
        }
        alias ushort fexcept_t;
    }
    else version (PPC_Any)
    {
        alias double fenv_t;
        alias uint fexcept_t;
    }
    else version (X86_Any)
    {
        struct fenv_t
        {
            ushort __control_word;
            ushort __unused1;
            ushort __status_word;
            ushort __unused2;
            ushort __tags;
            ushort __unused3;
            uint   __eip;
            ushort __cs_selector;
            ushort __opcode;
            uint   __data_offset;
            ushort __data_selector;
            ushort __unused5;
            version (X86_64)
                uint __mxcsr;
        }
        alias ushort fexcept_t;
    }
    else version (LoongArch64)
    {
        struct fenv_t
        {
            uint __cw;
        }
        alias uint fexcept_t;
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }
}
else version (CRuntime_Newlib)
{
    version (AArch64)
    {
        alias fenv_t = ulong;
        alias fexcept_t = ulong;
    }
    else version (RISCV_Any)
    {
        alias fenv_t = size_t;
        alias fexcept_t = size_t;
    }
    else version (X86_Any)
    {
        struct fenv_t
        {
            uint _fpu_cw;
            uint _fpu_sw;
            uint _fpu_tagw;
            uint _fpu_ipoff;
            uint _fpu_ipsel;
            uint _fpu_opoff;
            uint _fpu_opsel;
            uint _sse_mxcsr;
        }
        alias fexcept_t = uint;
    }
    else version (SPARC64)
    {
        alias fenv_t = ulong;
        alias fexcept_t = ulong;
    }
    else version (SPARC)
    {
        alias fenv_t = uint;
        alias fexcept_t = uint;
    }
    else
    {
        alias fenv_t = int;
        alias fexcept_t = int;
    }
}
else version (CRuntime_UClibc)
{
    version (X86)
    {
        struct fenv_t
        {
            ushort __control_word;
            ushort __unused1;
            ushort __status_word;
            ushort __unused2;
            ushort __tags;
            ushort __unused3;
            uint   __eip;
            ushort __cs_selector;
            ushort __opcode;
            uint   __data_offset;
            ushort __data_selector;
            ushort __unused5;
        }

        alias fexcept_t = ushort;
    }
    else version (X86_64)
    {
        struct fenv_t
        {
            ushort __control_word;
            ushort __unused1;
            ushort __status_word;
            ushort __unused2;
            ushort __tags;
            ushort __unused3;
            uint   __eip;
            ushort __cs_selector;
            ushort __opcode;
            uint   __data_offset;
            ushort __data_selector;
            ushort __unused5;
            uint   __mxcsr;
        }

        alias fexcept_t = ushort;
    }
    else version (MIPS_Any)
    {
        struct fenv_t
        {
            uint __fp_control_register;
        }

        alias fexcept_t = ushort;
    }
    else version (ARM)
    {
        struct fenv_t
        {
            uint __cw;
        }

        alias fexcept_t = uint;
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }
}
else
{
    static assert( false, "Unsupported platform" );
}

version (CRuntime_Microsoft)
{
    enum
    {
        FE_INEXACT      = 1, ///
        FE_UNDERFLOW    = 2, ///
        FE_OVERFLOW     = 4, ///
        FE_DIVBYZERO    = 8, ///
        FE_INVALID      = 0x10, ///
        FE_ALL_EXCEPT   = 0x1F, ///
        FE_TONEAREST    = 0, ///
        FE_UPWARD       = 0x100, ///
        FE_DOWNWARD     = 0x200, ///
        FE_TOWARDZERO   = 0x300, ///
    }
}
else version (Solaris)
{
    version (SPARC_Any)
    {
        enum
        {
            FE_TONEAREST    = 0,
            FE_TOWARDZERO   = 1,
            FE_UPWARD       = 2,
            FE_DOWNWARD     = 3,
        }

        enum
        {
            FE_INEXACT      = 0x01,
            FE_DIVBYZERO    = 0x02,
            FE_UNDERFLOW    = 0x04,
            FE_OVERFLOW     = 0x08,
            FE_INVALID      = 0x10,
            FE_ALL_EXCEPT   = 0x1f,
        }

    }
    else version (X86_Any)
    {
        enum
        {
            FE_TONEAREST    = 0,
            FE_DOWNWARD     = 1,
            FE_UPWARD       = 2,
            FE_TOWARDZERO   = 3,
        }

        enum
        {
            FE_INVALID      = 0x01,
            FE_DIVBYZERO    = 0x04,
            FE_OVERFLOW     = 0x08,
            FE_UNDERFLOW    = 0x10,
            FE_INEXACT      = 0x20,
            FE_ALL_EXCEPT   = 0x3d,
        }
    }
    else
    {
        static assert(0, "Unimplemented architecture");
    }
}
else
{
    version (X86)
    {
        // Define bits representing the exception.
        enum
        {
            FE_INVALID      = 0x01, ///
            FE_DENORMAL     = 0x02, /// non-standard
            FE_DIVBYZERO    = 0x04, ///
            FE_OVERFLOW     = 0x08, ///
            FE_UNDERFLOW    = 0x10, ///
            FE_INEXACT      = 0x20, ///
            FE_ALL_EXCEPT   = 0x3F, ///
        }

        // The ix87 FPU supports all of the four defined rounding modes.
        enum
        {
            FE_TONEAREST    = 0, ///
            FE_DOWNWARD     = 0x400, ///
            FE_UPWARD       = 0x800, ///
            FE_TOWARDZERO   = 0xC00, ///
        }
    }
    else version (X86_64)
    {
        // Define bits representing the exception.
        enum
        {
            FE_INVALID      = 0x01, ///
            FE_DENORMAL     = 0x02, /// non-standard
            FE_DIVBYZERO    = 0x04, ///
            FE_OVERFLOW     = 0x08, ///
            FE_UNDERFLOW    = 0x10, ///
            FE_INEXACT      = 0x20, ///
            FE_ALL_EXCEPT   = 0x3F, ///
        }

        // The ix87 FPU supports all of the four defined rounding modes.
        enum
        {
            FE_TONEAREST    = 0, ///
            FE_DOWNWARD     = 0x400, ///
            FE_UPWARD       = 0x800, ///
            FE_TOWARDZERO   = 0xC00, ///
        }
    }
    else version (ARM_Any)
    {
        // Define bits representing exceptions in the FPU status word.
        enum
        {
            FE_INVALID      = 1,  ///
            FE_DIVBYZERO    = 2,  ///
            FE_OVERFLOW     = 4,  ///
            FE_UNDERFLOW    = 8,  ///
            FE_INEXACT      = 16, ///
            FE_ALL_EXCEPT   = 31, ///
        }

        // VFP supports all of the four defined rounding modes.
        enum
        {
            FE_TONEAREST    = 0,        ///
            FE_UPWARD       = 0x400000, ///
            FE_DOWNWARD     = 0x800000, ///
            FE_TOWARDZERO   = 0xC00000, ///
        }
    }
    else version (HPPA_Any)
    {
        // Define bits representing the exception.
        enum
        {
            FE_INEXACT      = 0x01, ///
            FE_UNDERFLOW    = 0x02, ///
            FE_OVERFLOW     = 0x04, ///
            FE_DIVBYZERO    = 0x08, ///
            FE_INVALID      = 0x10, ///
            FE_ALL_EXCEPT   = 0x1F, ///
        }

        // The HPPA FPU supports all of the four defined rounding modes.
        enum
        {
            FE_TONEAREST    =   0x0, ///
            FE_TOWARDZERO   = 0x200, ///
            FE_UPWARD       = 0x400, ///
            FE_DOWNWARD     = 0x600, ///
        }
    }
    else version (MIPS_Any)
    {
        // Define bits representing the exception.
        enum
        {
            FE_INEXACT      = 0x04, ///
            FE_UNDERFLOW    = 0x08, ///
            FE_OVERFLOW     = 0x10, ///
            FE_DIVBYZERO    = 0x20, ///
            FE_INVALID      = 0x40, ///
            FE_ALL_EXCEPT   = 0x7C, ///
        }

        // The MIPS FPU supports all of the four defined rounding modes.
        enum
        {
            FE_TONEAREST    = 0x0, ///
            FE_TOWARDZERO   = 0x1, ///
            FE_UPWARD       = 0x2, ///
            FE_DOWNWARD     = 0x3, ///
        }
    }
    else version (PPC_Any)
    {
        // Define bits representing the exception.
        enum
        {
            FE_INEXACT                    = 0x2000000,  ///
            FE_DIVBYZERO                  = 0x4000000,  ///
            FE_UNDERFLOW                  = 0x8000000,  ///
            FE_OVERFLOW                   = 0x10000000, ///
            FE_INVALID                    = 0x20000000, ///
            FE_INVALID_SNAN               = 0x1000000,  /// non-standard
            FE_INVALID_ISI                = 0x800000,   /// non-standard
            FE_INVALID_IDI                = 0x400000,   /// non-standard
            FE_INVALID_ZDZ                = 0x200000,   /// non-standard
            FE_INVALID_IMZ                = 0x100000,   /// non-standard
            FE_INVALID_COMPARE            = 0x80000,    /// non-standard
            FE_INVALID_SOFTWARE           = 0x400,      /// non-standard
            FE_INVALID_SQRT               = 0x200,      /// non-standard
            FE_INVALID_INTEGER_CONVERSION = 0x100,      /// non-standard
            FE_ALL_INVALID                = 0x1F80700,  /// non-standard
            FE_ALL_EXCEPT                 = 0x3E000000, ///
        }

        // PowerPC chips support all of the four defined rounding modes.
        enum
        {
            FE_TONEAREST    = 0, ///
            FE_TOWARDZERO   = 1, ///
            FE_UPWARD       = 2, ///
            FE_DOWNWARD     = 3, ///
        }
    }
    else version (RISCV_Any)
    {
        // Define bits representing exceptions in the FPSR status word.
        enum
        {
            FE_INEXACT      = 0x01, ///
            FE_UNDERFLOW    = 0x02, ///
            FE_OVERFLOW     = 0x04, ///
            FE_DIVBYZERO    = 0x08, ///
            FE_INVALID      = 0x10, ///
            FE_ALL_EXCEPT   = 0x1f, ///
        }

        // Define bits representing rounding modes in the FPCR Rmode field.
        enum
        {
            FE_TONEAREST    = 0x0, ///
            FE_TOWARDZERO   = 0x1, ///
            FE_DOWNWARD     = 0x2, ///
            FE_UPWARD       = 0x3, ///
        }
    }
    else version (SPARC_Any)
    {
        // Define bits representing the exception.
        enum
        {
            FE_INVALID      = 0x200, ///
            FE_OVERFLOW     = 0x100, ///
            FE_UNDERFLOW    = 0x80,  ///
            FE_DIVBYZERO    = 0x40,  ///
            FE_INEXACT      = 0x20,  ///
            FE_ALL_EXCEPT   = 0x3E0, ///
        }

        // The Sparc FPU supports all of the four defined rounding modes.
        enum
        {
            FE_TONEAREST    = 0x0,        ///
            FE_TOWARDZERO   = 0x40000000, ///
            FE_UPWARD       = 0x80000000, ///
            FE_DOWNWARD     = 0xc0000000, ///
        }
    }
    else version (IBMZ_Any)
    {
        // Define bits representing the exception.
        enum
        {
            FE_INVALID      = 0x80, ///
            FE_DIVBYZERO    = 0x40, ///
            FE_OVERFLOW     = 0x20, ///
            FE_UNDERFLOW    = 0x10, ///
            FE_INEXACT      = 0x08, ///
            FE_ALL_EXCEPT   = 0xF8, ///
        }

        // SystemZ supports all of the four defined rounding modes.
        enum
        {
            FE_TONEAREST    = 0x0, ///
            FE_DOWNWARD     = 0x3, ///
            FE_UPWARD       = 0x2, ///
            FE_TOWARDZERO   = 0x1, ///
        }
    }
    else version (LoongArch64)
    {
        // Define bits representing exceptions in the Flags field in FCSR{0,2}.
        enum
        {
            FE_INEXACT      = 0x010000, ///
            FE_UNDERFLOW    = 0x020000, ///
            FE_OVERFLOW     = 0x040000, ///
            FE_DIVBYZERO    = 0x080000, ///
            FE_INVALID      = 0x100000, ///
            FE_ALL_EXCEPT   = 0x1f0000, ///
        }

        // Define bits representing rounding modes in the RM field in FCSR{0,3}.
        enum
        {
            FE_TONEAREST    = 0x000, ///
            FE_TOWARDZERO   = 0x100, ///
            FE_UPWARD       = 0x200, ///
            FE_DOWNWARD     = 0x300, ///
        }
    }
    else
    {
        static assert(0, "Unimplemented architecture");
    }

}

version (GNUFP)
{
    ///
    enum FE_DFL_ENV = cast(fenv_t*)(-1);
}
else version (CRuntime_Microsoft)
{
    private extern __gshared fenv_t _Fenv0;
    ///
    enum FE_DFL_ENV = &_Fenv0;
}
else version (Darwin)
{
    private extern __gshared fenv_t _FE_DFL_ENV;
    ///
    enum FE_DFL_ENV = &_FE_DFL_ENV;
}
else version (FreeBSD)
{
    private extern const fenv_t __fe_dfl_env;
    ///
    enum FE_DFL_ENV = &__fe_dfl_env;
}
else version (NetBSD)
{
    private extern const fenv_t __fe_dfl_env;
    ///
    enum FE_DFL_ENV = &__fe_dfl_env;
}
else version (OpenBSD)
{
    private extern const fenv_t __fe_dfl_env;
    ///
    enum FE_DFL_ENV = &__fe_dfl_env;
}
else version (DragonFlyBSD)
{
    private extern const fenv_t __fe_dfl_env;
    ///
    enum FE_DFL_ENV = &__fe_dfl_env;
}
else version (CRuntime_Bionic)
{
    private extern const fenv_t __fe_dfl_env;
    ///
    enum FE_DFL_ENV = &__fe_dfl_env;
}
else version (Solaris)
{
    private extern const fenv_t __fenv_def_env;
    ///
    enum FE_DFL_ENV = &__fenv_def_env;
}
else version (CRuntime_Musl)
{
    ///
    enum FE_DFL_ENV = cast(fenv_t*)(-1);
}
else version (CRuntime_UClibc)
{
    ///
    enum FE_DFL_ENV = cast(fenv_t*)(-1);
}
else
{
    static assert( false, "Unsupported platform" );
}

///
int feclearexcept(int excepts);

///
int fetestexcept(int excepts);
///
int feholdexcept(fenv_t* envp);

///
int fegetexceptflag(fexcept_t* flagp, int excepts);
///
int fesetexceptflag(const scope fexcept_t* flagp, int excepts);

///
int fegetround();
///
int fesetround(int round);

///
int fegetenv(fenv_t* envp);
///
int fesetenv(const scope fenv_t* envp);

// MS define feraiseexcept() and feupdateenv() inline.
version (CRuntime_Microsoft) // supported since MSVCRT 12 (VS 2013) only
{
    ///
    int feraiseexcept()(int excepts)
    {
        struct Entry
        {
            int    exceptVal;
            double num;
            double denom;
        }
        static immutable Entry[5] table =
        [ // Raise exception by evaluating num / denom:
            { FE_INVALID,   0.0,    0.0    },
            { FE_DIVBYZERO, 1.0,    0.0    },
            { FE_OVERFLOW,  1e+300, 1e-300 },
            { FE_UNDERFLOW, 1e-300, 1e+300 },
            { FE_INEXACT,   2.0,    3.0    }
        ];

        if ((excepts &= FE_ALL_EXCEPT) == 0)
            return 0;

        // Raise the exceptions not masked:
        double ans = void;
        foreach (i; 0 .. table.length)
        {
            if ((excepts & table[i].exceptVal) != 0)
                ans = table[i].num / table[i].denom;
        }

        return 0;
    }

    ///
    int feupdateenv()(const scope fenv_t* envp)
    {
        int excepts = fetestexcept(FE_ALL_EXCEPT);
        return (fesetenv(envp) != 0 || feraiseexcept(excepts) != 0 ? 1 : 0);
    }
}
else
{
    ///
    int feraiseexcept(int excepts);
    ///
    int feupdateenv(const scope fenv_t* envp);
}
