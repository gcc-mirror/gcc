/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_math.h.html, _math.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/_math.d)
 */

module core.stdc.math;

import core.stdc.config;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

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

extern (C):
@trusted: // All functions here operate on floating point and integer values only.
nothrow:
@nogc:

///
alias float  float_t;
///
alias double double_t;

///
enum double HUGE_VAL      = double.infinity;
///
enum double HUGE_VALF     = float.infinity;
///
enum double HUGE_VALL     = real.infinity;

///
enum float INFINITY       = float.infinity;
///
enum float NAN            = float.nan;

version (FreeBSD)
{
    ///
    enum int FP_ILOGB0        = -int.max;
    ///
    enum int FP_ILOGBNAN      = int.max;
}
else version (NetBSD)
{
    ///
    enum int FP_ILOGB0        = -int.max;
    ///
    enum int FP_ILOGBNAN      = int.max;
}
else version (OpenBSD)
{
    ///
    enum int FP_ILOGB0        = -int.max;
    ///
    enum int FP_ILOGBNAN      = int.max;
}
else version (DragonFlyBSD)
{
    ///
    enum int FP_ILOGB0        = -int.max;
    ///
    enum int FP_ILOGBNAN      = int.max;
}
else version (Solaris)
{
    ///
    enum int FP_ILOGB0        = -int.max;
    ///
    enum int FP_ILOGBNAN      = int.max;
}
else version (CRuntime_Bionic)
{
    ///
    enum int FP_ILOGB0        = -int.max;
    ///
    enum int FP_ILOGBNAN      = int.max;
}
else version (CRuntime_UClibc)
{
    version (X86_Any)
    {
        ///
        enum int FP_ILOGB0        = int.min;
        ///
        enum int FP_ILOGBNAN      = int.min;
    }
    else version (MIPS_Any)
    {
        ///
        enum int FP_ILOGB0        = -int.max;
        ///
        enum int FP_ILOGBNAN      = int.max;
    }
    else version (ARM)
    {
        ///
        enum int FP_ILOGB0        = -int.max;
        ///
        enum int FP_ILOGBNAN      = int.max;
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }
}
else version (CRuntime_Glibc)
{
    version (X86_Any)
    {
        ///
        enum int FP_ILOGB0        = int.min;
        ///
        enum int FP_ILOGBNAN      = int.min;
    }
    else version (ARM_Any)
    {
        ///
        enum int FP_ILOGB0        = -int.max;
        ///
        enum int FP_ILOGBNAN      = int.max;
    }
    else version (HPPA_Any)
    {
        ///
        enum int FP_ILOGB0        = -int.max;
        ///
        enum int FP_ILOGBNAN      = int.max;
    }
    else version (MIPS_Any)
    {
        ///
        enum int FP_ILOGB0        = -int.max;
        ///
        enum int FP_ILOGBNAN      = int.max;
    }
    else version (PPC_Any)
    {
        ///
        enum int FP_ILOGB0        = -int.max;
        ///
        enum int FP_ILOGBNAN      = int.max;
    }
    else version (RISCV_Any)
    {
        ///
        enum int FP_ILOGB0        = -int.max;
        ///
        enum int FP_ILOGBNAN      = int.max;
    }
    else version (SPARC_Any)
    {
        ///
        enum int FP_ILOGB0        = -int.max;
        ///
        enum int FP_ILOGBNAN      = int.max;
    }
    else version (IBMZ_Any)
    {
        ///
        enum int FP_ILOGB0        = -int.max;
        ///
        enum int FP_ILOGBNAN      = int.max;
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }
}
else
{
    ///
    enum int FP_ILOGB0        = int.min;
    ///
    enum int FP_ILOGBNAN      = int.min;
}

///
enum int MATH_ERRNO       = 1;
///
enum int MATH_ERREXCEPT   = 2;
///
enum int math_errhandling = MATH_ERRNO | MATH_ERREXCEPT;

version (none)
{
    //
    // these functions are all macros in C
    //

    //int fpclassify(real-floating x);
    pure int fpclassify(float x);
    pure int fpclassify(double x);
    pure int fpclassify(real x);

    //int isfinite(real-floating x);
    pure int isfinite(float x);
    pure int isfinite(double x);
    pure int isfinite(real x);

    //int isinf(real-floating x);
    pure int isinf(float x);
    pure int isinf(double x);
    pure int isinf(real x);

    //int isnan(real-floating x);
    pure int isnan(float x);
    pure int isnan(double x);
    pure int isnan(real x);

    //int isnormal(real-floating x);
    pure int isnormal(float x);
    pure int isnormal(double x);
    pure int isnormal(real x);

    //int signbit(real-floating x);
    pure int signbit(float x);
    pure int signbit(double x);
    pure int signbit(real x);

    //int isgreater(real-floating x, real-floating y);
    pure int isgreater(float x, float y);
    pure int isgreater(double x, double y);
    pure int isgreater(real x, real y);

    //int isgreaterequal(real-floating x, real-floating y);
    pure int isgreaterequal(float x, float y);
    pure int isgreaterequal(double x, double y);
    pure int isgreaterequal(real x, real y);

    //int isless(real-floating x, real-floating y);
    pure int isless(float x, float y);
    pure int isless(double x, double y);
    pure int isless(real x, real y);

    //int islessequal(real-floating x, real-floating y);
    pure int islessequal(float x, float y);
    pure int islessequal(double x, double y);
    pure int islessequal(real x, real y);

    //int islessgreater(real-floating x, real-floating y);
    pure int islessgreater(float x, float y);
    pure int islessgreater(double x, double y);
    pure int islessgreater(real x, real y);

    //int isunordered(real-floating x, real-floating y);
    pure int isunordered(float x, float y);
    pure int isunordered(double x, double y);
    pure int isunordered(real x, real y);
}

version (CRuntime_DigitalMars)
{
    enum
    {
        ///
        FP_NANS        = 0,
        ///
        FP_NANQ        = 1,
        ///
        FP_INFINITE    = 2,
        ///
        FP_NORMAL      = 3,
        ///
        FP_SUBNORMAL   = 4,
        ///
        FP_ZERO        = 5,
        ///
        FP_NAN         = FP_NANQ,
        ///
        FP_EMPTY       = 6,
        ///
        FP_UNSUPPORTED = 7,
    }

    enum
    {
        ///
        FP_FAST_FMA  = 0,
        ///
        FP_FAST_FMAF = 0,
        ///
        FP_FAST_FMAL = 0,
    }

    pure uint __fpclassify_f(float x);
    pure uint __fpclassify_d(double x);
    pure uint __fpclassify_ld(real x);

    //int fpclassify(real-floating x);
    ///
    pragma(mangle, "__fpclassify_f") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassify_d") pure int fpclassify(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__fpclassify_d" : "__fpclassify_ld")
    pure int fpclassify(real x);

  extern (D)
  {
    //int isfinite(real-floating x);
    ///
    pure int isfinite(float x)       { return fpclassify(x) >= FP_NORMAL; }
    ///
    pure int isfinite(double x)      { return fpclassify(x) >= FP_NORMAL; }
    ///
    pure int isfinite(real x)        { return fpclassify(x) >= FP_NORMAL; }

    //int isinf(real-floating x);
    ///
    pure int isinf(float x)          { return fpclassify(x) == FP_INFINITE; }
    ///
    pure int isinf(double x)         { return fpclassify(x) == FP_INFINITE; }
    ///
    pure int isinf(real x)           { return fpclassify(x) == FP_INFINITE; }

    //int isnan(real-floating x);
    ///
    pure int isnan(float x)          { return fpclassify(x) <= FP_NANQ;   }
    ///
    pure int isnan(double x)         { return fpclassify(x) <= FP_NANQ;   }
    ///
    pure int isnan(real x)           { return fpclassify(x) <= FP_NANQ;   }

    //int isnormal(real-floating x);
    ///
    pure int isnormal(float x)       { return fpclassify(x) == FP_NORMAL; }
    ///
    pure int isnormal(double x)      { return fpclassify(x) == FP_NORMAL; }
    ///
    pure int isnormal(real x)        { return fpclassify(x) == FP_NORMAL; }

    //int signbit(real-floating x);
    ///
    pure int signbit(float x)     { return (cast(short*)&(x))[1] & 0x8000; }
    ///
    pure int signbit(double x)    { return (cast(short*)&(x))[3] & 0x8000; }
    ///
    pure int signbit(real x)
    {
        return (real.sizeof == double.sizeof)
            ? (cast(short*)&(x))[3] & 0x8000
            : (cast(short*)&(x))[4] & 0x8000;
    }
  }
}
else version (CRuntime_Microsoft) // fully supported since MSVCRT 12 (VS 2013) only
{
  version (all) // legacy stuff to be removed in the future
  {
    enum
    {
        _FPCLASS_SNAN = 1,
        _FPCLASS_QNAN = 2,
        _FPCLASS_NINF = 4,
        _FPCLASS_NN   = 8,
        _FPCLASS_ND   = 0x10,
        _FPCLASS_NZ   = 0x20,
        _FPCLASS_PZ   = 0x40,
        _FPCLASS_PD   = 0x80,
        _FPCLASS_PN   = 0x100,
        _FPCLASS_PINF = 0x200,
    }

    //deprecated("Please use the standard C99 function copysignf() instead.")
    pure float _copysignf(float x, float s);

    //deprecated("_chgsignf(x) is a non-standard MS extension. Please consider using -x instead.")
    pure float _chgsignf(float x);

    version (Win64) // not available in 32-bit runtimes
    {
        //deprecated("Please use the standard C99 function isfinite() instead.")
        pure int _finitef(float x);

        //deprecated("Please use the standard C99 function isnan() instead.")
        pure int _isnanf(float x);

        //deprecated("Please use the standard C99 function fpclassify() instead.")
        pure int _fpclassf(float x);
    }

    //deprecated("Please use the standard C99 function copysign() instead.")
    pure double _copysign(double x, double s);

    //deprecated("_chgsign(x) is a non-standard MS extension. Please consider using -x instead.")
    pure double _chgsign(double x);

    //deprecated("Please use the standard C99 function isfinite() instead.")
    pure int _finite(double x);

    //deprecated("Please use the standard C99 function isnan() instead.")
    pure int _isnan(double x);

    //deprecated("Please use the standard C99 function fpclassify() instead.")
    pure int _fpclass(double x);
  }

  version (MinGW)
  {
    enum
    {
        ///
        FP_NAN = 0x0100,
        ///
        FP_NORMAL = 0x0400,
        ///
        FP_INFINITE = FP_NAN | FP_NORMAL,
        ///
        FP_ZERO = 0x0400,
        ///
        FP_SUBNORMAL = FP_NORMAL | FP_ZERO
    }

    pure int __fpclassifyf(float x);
    pure int __fpclassify(double x);
    pure int __fpclassifyl(real x);

    pure int __isnanf(float x);
    pure int __isnan(double x);
    pure int __isnanl(real x);

    pure int __signbitf(float x);
    pure int __signbit(double x);
    pure int __signbitl(real x);

    //int fpclassify(real-floating x);
    ///
    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassify")  pure int fpclassify(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__fpclassify" : "__fpclassifyl")
        pure int fpclassify(real x);

    extern (D)
    {
        //int isfinite(real-floating x);
        ///
        pure int isfinite(float x)       { return (fpclassify(x) & FP_NORMAL) == 0; }
        ///
        pure int isfinite(double x)      { return (fpclassify(x) & FP_NORMAL) == 0; }
        ///
        pure int isfinite(real x)        { return (fpclassify(x) & FP_NORMAL) == 0; }

        //int isinf(real-floating x);
        ///
        pure int isinf(float x)          { return fpclassify(x) == FP_INFINITE; }
        ///
        pure int isinf(double x)         { return fpclassify(x) == FP_INFINITE; }
        ///
        pure int isinf(real x)           { return fpclassify(x) == FP_INFINITE; }
    }

    //int isnan(real-floating x);
    ///
    pragma(mangle, "__isnanf") pure int isnan(float x);
    ///
    pragma(mangle, "__isnan")  pure int isnan(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__isnan" : "__isnanl")
        pure int isnan(real x);

    extern (D)
    {
        //int isnormal(real-floating x);
        ///
        int isnormal(float x)       { return fpclassify(x) == FP_NORMAL; }
        ///
        int isnormal(double x)      { return fpclassify(x) == FP_NORMAL; }
        ///
        int isnormal(real x)        { return fpclassify(x) == FP_NORMAL; }
    }

    //int signbit(real-floating x);
    ///
    pragma(mangle, "__signbitf") pure int signbit(float x);
    ///
    pragma(mangle, "__signbit")  pure int signbit(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__signbit" : "__signbitl")
        int signbit(real x);
  }
  else
  {
    enum
    {
        ///
        FP_SUBNORMAL = -2,
        ///
        FP_NORMAL    = -1,
        ///
        FP_ZERO      =  0,
        ///
        FP_INFINITE  =  1,
        ///
        FP_NAN       =  2,
    }

    extern(D)
    {
        //int fpclassify(real-floating x);
        ///
        extern(C) pragma(mangle, "_fdclass") pure int fpclassify(float x);
        ///
        extern(C) pragma(mangle, "_dclass")  pure int fpclassify(double x);
        ///
        pure int fpclassify()(real x)
        {
            static if (real.sizeof == double.sizeof)
                return fpclassify(cast(double) x);
            else
                static assert(false, "fpclassify(real) not supported by MS C runtime");
        }

        //int isfinite(real-floating x);
        ///
        pure int isfinite()(float x)     { return fpclassify(x) <= 0; }
        ///
        pure int isfinite()(double x)    { return fpclassify(x) <= 0; }
        ///
        pure int isfinite()(real x)      { return fpclassify(x) <= 0; }

        //int isinf(real-floating x);
        ///
        pure int isinf()(float x)        { return fpclassify(x) == FP_INFINITE; }
        ///
        pure int isinf()(double x)       { return fpclassify(x) == FP_INFINITE; }
        ///
        pure int isinf()(real x)         { return fpclassify(x) == FP_INFINITE; }

        //int isnan(real-floating x);
        version (none) // requires MSVCRT 12+ (VS 2013)
        {
            ///
            pure int isnan(float x)      { return fpclassify(x) == FP_NAN; }
            ///
            pure int isnan(double x)     { return fpclassify(x) == FP_NAN; }
            ///
            pure int isnan(real x)       { return fpclassify(x) == FP_NAN; }
        }
        else // for backward compatibility with older runtimes
        {
            ///
            pure int isnan(float x)      { version (Win64) return _isnanf(x); else return _isnan(cast(double) x); }
            ///
            extern(C) pragma(mangle, "_isnan") pure int isnan(double x);
            ///
            pure int isnan(real x)       { return _isnan(cast(double) x); }
        }

        //int isnormal(real-floating x);
        ///
        pure int isnormal()(float x)     { return fpclassify(x) == FP_NORMAL; }
        ///
        pure int isnormal()(double x)    { return fpclassify(x) == FP_NORMAL; }
        ///
        pure int isnormal()(real x)      { return fpclassify(x) == FP_NORMAL; }

        //int signbit(real-floating x);
        ///
        extern(C) pragma(mangle, "_fdsign") pure int signbit(float x);
        ///
        extern(C) pragma(mangle, "_dsign")  pure int signbit(double x);
        ///
        pure int signbit()(real x)
        {
            static if (real.sizeof == double.sizeof)
                return signbit(cast(double) x);
            else
                return (cast(short*)&(x))[4] & 0x8000;
        }
    }
  }
}
else version (CRuntime_Glibc)
{
    enum
    {
        ///
        FP_NAN,
        ///
        FP_INFINITE,
        ///
        FP_ZERO,
        ///
        FP_SUBNORMAL,
        ///
        FP_NORMAL,
    }

    enum
    {
        ///
        FP_FAST_FMA  = 0,
        ///
        FP_FAST_FMAF = 0,
        ///
        FP_FAST_FMAL = 0,
    }

    pure int __fpclassifyf(float x);
    pure int __fpclassify(double x);
    pure int __fpclassifyl(real x);

    pure int __finitef(float x);
    pure int __finite(double x);
    pure int __finitel(real x);

    pure int __isinff(float x);
    pure int __isinf(double x);
    pure int __isinfl(real x);

    pure int __isnanf(float x);
    pure int __isnan(double x);
    pure int __isnanl(real x);

    pure int __signbitf(float x);
    pure int __signbit(double x);
    pure int __signbitl(real x);

    //int fpclassify(real-floating x);
      ///
    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassify")  pure int fpclassify(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__fpclassify" : "__fpclassifyl")
    pure int fpclassify(real x);

    //int isfinite(real-floating x);
    ///
    pragma(mangle, "__finitef") pure int isfinite(float x);
    ///
    pragma(mangle, "__finite")  pure int isfinite(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__finite" : "__finitel")
    pure int isfinite(real x);

    //int isinf(real-floating x);
    ///
    pragma(mangle, "__isinff") pure int isinf(float x);
    ///
    pragma(mangle, "__isinf")  pure int isinf(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__isinf" : "__isinfl")
    pure int isinf(real x);

    //int isnan(real-floating x);
    ///
    pragma(mangle, "__isnanf") pure int isnan(float x);
    ///
    pragma(mangle, "__isnan")  pure int isnan(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__isnan" : "__isnanl")
    pure int isnan(real x);

    //int isnormal(real-floating x);
    ///
    extern (D) pure int isnormal(float x)       { return fpclassify(x) == FP_NORMAL; }
    ///
    extern (D) pure int isnormal(double x)      { return fpclassify(x) == FP_NORMAL; }
    ///
    extern (D) pure int isnormal(real x)        { return fpclassify(x) == FP_NORMAL; }

    //int signbit(real-floating x);
    ///
    pragma(mangle, "__signbitf") pure int signbit(float x);
    ///
    pragma(mangle, "__signbit")  pure int signbit(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__signbit" : "__signbitl")
    pure int signbit(real x);
}
else version (CRuntime_Musl)
{
    enum
    {
        ///
        FP_NAN,
        ///
        FP_INFINITE,
        ///
        FP_ZERO,
        ///
        FP_SUBNORMAL,
        ///
        FP_NORMAL,
    }

    enum
    {
        ///
        FP_FAST_FMA  = 0,
        ///
        FP_FAST_FMAF = 0,
        ///
        FP_FAST_FMAL = 0,
    }

  pure {
    int __fpclassifyf(float x);
    int __fpclassify(double x);
    int __fpclassifyl(real x);

    int __signbitf(float x);
    int __signbit(double x);
    int __signbitl(real x);
  }

    //int fpclassify(real-floating x);
      ///
    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassify")  pure int fpclassify(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__fpclassify" : "__fpclassifyl")
    pure int fpclassify(real x);
  extern (D) pure
  {
    private uint __FLOAT_BITS(float __f)
    {
        union __u_t {
            float __f;
            uint __i;
        }
        __u_t __u;
        __u.__f = __f;
        return __u.__i;
    }
    private ulong __DOUBLE_BITS(double __f)
    {
        union __u_t {
            double __f;
            ulong __i;
        }
        __u_t __u;
        __u.__f = __f;
        return __u.__i;
    }

    //int isfinite(real-floating x);
    ///
    int isfinite(float x)       { return (__FLOAT_BITS(x) & 0x7fffffff) < 0x7f800000; }
    ///
    int isfinite(double x)      { return (__DOUBLE_BITS(x) & -1UL>>1) < 0x7ffUL<<52;  }
    ///
    int isfinite(real x)
    {
        return (real.sizeof == double.sizeof)
            ? isfinite(cast(double)x)
            : __fpclassifyl(x) > FP_INFINITE;
    }

    //int isinf(real-floating x);
    ///
    int isinf(float x)          { return (__FLOAT_BITS(x) & 0x7fffffff) == 0x7f800000;  }
    ///
    int isinf(double x)         { return (__DOUBLE_BITS(x) & -1UL>>1) == 0x7ffUL<<52;   }
    ///
    int isinf(real x)
    {
        return (real.sizeof == double.sizeof)
            ? isinf(cast(double)x)
            : __fpclassifyl(x) == FP_INFINITE;
    }

    //int isnan(real-floating x);
    ///
    int isnan(float x)          { return (__FLOAT_BITS(x) & 0x7fffffff) > 0x7f800000;  }
    ///
    int isnan(double x)         { return (__DOUBLE_BITS(x) & -1UL>>1) > 0x7ffUL<<52;   }
    ///
    int isnan(real x)
    {
        return (real.sizeof == double.sizeof)
            ? isnan(cast(double)x)
            : __fpclassifyl(x) == FP_NAN;
    }

    //int isnormal(real-floating x);
    ///
    int isnormal(float x)       { return fpclassify(x) == FP_NORMAL; }
    ///
    int isnormal(double x)      { return fpclassify(x) == FP_NORMAL; }
    ///
    int isnormal(real x)        { return fpclassify(x) == FP_NORMAL; }
  }

    //int signbit(real-floating x);
    ///
    pragma(mangle, "__signbitf") pure int signbit(float x);
    ///
    pragma(mangle, "__signbit")  pure int signbit(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__signbit" : "__signbitl")
    pure int signbit(real x);
}
else version (CRuntime_UClibc)
{
    enum
    {
        ///
        FP_NAN,
        ///
        FP_INFINITE,
        ///
        FP_ZERO,
        ///
        FP_SUBNORMAL,
        ///
        FP_NORMAL,
    }

    enum
    {
        ///
        FP_FAST_FMA  = 0,
        ///
        FP_FAST_FMAF = 0,
        ///
        FP_FAST_FMAL = 0,
    }

    pure int __fpclassifyf(float x);
    pure int __fpclassify(double x);
    pure int __fpclassifyl(real x);

    pure int __finitef(float x);
    pure int __finite(double x);
    pure int __finitel(real x);

    pure int __isinff(float x);
    pure int __isinf(double x);
    pure int __isinfl(real x);

    pure int __isnanf(float x);
    pure int __isnan(double x);
    pure int __isnanl(real x);

    pure int __signbitf(float x);
    pure int __signbit(double x);
    pure int __signbitl(real x);

    ///
    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassify")  pure int fpclassify(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__fpclassify" : "__fpclassifyl")
    pure int fpclassify(real x);

    ///
    pragma(mangle, "__finitef") pure int isfinite(float x);
    ///
    pragma(mangle, "__finite")  pure int isfinite(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__finite" : "__finitel")
    pure int isfinite(real x);

    ///
    pragma(mangle, "__isinff") pure int isinf(float x);
    ///
    pragma(mangle, "__isinf")  pure int isinf(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__isinf" : "__isinfl")
    pure int isinf(real x);

    ///
    pragma(mangle, "__isnanf") pure int isnan(float x);
    ///
    pragma(mangle, "__isnan")  pure int isnan(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__isnan" : "__isnanl")
    pure int isnan(real x);

  extern (D) pure
  {
    ///
    int isnormal(float x)       { return fpclassify(x) == FP_NORMAL; }
    ///
    int isnormal(double x)      { return fpclassify(x) == FP_NORMAL; }
    ///
    int isnormal(real x)        { return fpclassify(x) == FP_NORMAL; }
  }

    ///
    pragma(mangle, "__signbitf") pure int signbit(float x);
    ///
    pragma(mangle, "__signbit")  pure int signbit(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__signbit" : "__signbitl")
    pure int signbit(real x);
}
else version (Darwin)
{
    enum
    {
        ///
        FP_NAN         = 1,
        ///
        FP_INFINITE    = 2,
        ///
        FP_ZERO        = 3,
        ///
        FP_NORMAL      = 4,
        ///
        FP_SUBNORMAL   = 5,
    }

    enum
    {
        ///
        FP_FAST_FMA  = 0,
        ///
        FP_FAST_FMAF = 0,
        ///
        FP_FAST_FMAL = 0,
    }

    pure int __fpclassifyf(float x);
    pure int __fpclassifyd(double x);

    pure int __isfinitef(float x);
    pure int __isfinited(double x);

    pure int __isinff(float x);
    pure int __isinfd(double x);

    pure int __isnanf(float x);
    pure int __isnand(double x);

    // __isnormal family exists, but iOS implementation returns wrong results
    // for subnormals

    pure int __signbitf(float x);
    pure int __signbitd(double x);
    pure int __signbitl(real x);

    // Support of OSX < 10.8 needs legacy function names without "l" suffix
    // with exception of __signbitl.  Otherwise could use else version like
    // other Darwins
    version (OSX)
    {
        version (AArch64)
        {
            // Available in macOS ARM
            pure int __fpclassifyl(real x);
            pure int __isfinitel(real x);
            pure int __isinfl(real x);
            pure int __isnanl(real x);
        }
        else
        {
            pure int __fpclassify(real x);
            pure int __isfinite(real x);
            pure int __isinf(real x);
            pure int __isnan(real x);
            alias __fpclassifyl = __fpclassify;
            alias __isfinitel = __isfinite;
            alias __isinfl = __isinf;
            alias __isnanl = __isnan;
        }
    }
    else
    {
        // Available OSX >= 10.8, iOS >= 6.0, all TVOS and WatchOS
        pure int __fpclassifyl(real x);
        pure int __isfinitel(real x);
        pure int __isinfl(real x);
        pure int __isnanl(real x);
    }

    //int fpclassify(real-floating x);
    ///
    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassifyd") pure int fpclassify(double x);
    ///
    pragma(mangle, __fpclassifyl.mangleof) pure int fpclassify(real x);

    //int isfinite(real-floating x);
    ///
    pragma(mangle, "__isfinitef") pure int isfinite(float x);
    ///
    pragma(mangle, "__isfinited") pure int isfinite(double x);
    ///
    pragma(mangle, __isfinitel.mangleof) pure int isfinite(real x);

    //int isinf(real-floating x);
    ///
    pragma(mangle, "__isinff") pure int isinf(float x);
    ///
    pragma(mangle, "__isinfd") pure int isinf(double x);
    ///
    pragma(mangle, __isinfl.mangleof) pure int isinf(real x);

    //int isnan(real-floating x);
    ///
    pragma(mangle, "__isnanf") pure int isnan(float x);
    ///
    pragma(mangle, "__isnand") pure int isnan(double x);
    ///
    pragma(mangle, __isnanl.mangleof) pure int isnan(real x);

  extern (D)
  {
    //int isnormal(real-floating x);
    ///
    pure int isnormal(float x)       { return fpclassify(x) == FP_NORMAL; }
    ///
    pure int isnormal(double x)      { return fpclassify(x) == FP_NORMAL; }
    ///
    pure int isnormal(real x)        { return fpclassify(x) == FP_NORMAL; }
  }

    //int signbit(real-floating x);
    ///
    pragma(mangle, "__signbitf") pure int signbit(float x);
    ///
    pragma(mangle, "__signbitd") pure int signbit(double x);
    ///
    pragma(mangle, "__signbitl") pure int signbit(real x);
}
else version (FreeBSD)
{
    enum
    {
        ///
        FP_INFINITE  = 0x01,
        ///
        FP_NAN       = 0x02,
        ///
        FP_NORMAL    = 0x04,
        ///
        FP_SUBNORMAL = 0x08,
        ///
        FP_ZERO      = 0x10,
    }

    enum
    {
        ///
        FP_FAST_FMA  = 0,
        ///
        FP_FAST_FMAF = 0,
        ///
        FP_FAST_FMAL = 0,
    }

    pure int __fpclassifyd(double);
    pure int __fpclassifyf(float);
    pure int __fpclassifyl(real);
    pure int __isfinitef(float);
    pure int __isfinite(double);
    pure int __isfinitel(real);
    pure int __isinff(float);
    pure int __isinfl(real);
    pure int __isnanl(real);
    pure int __isnormalf(float);
    pure int __isnormal(double);
    pure int __isnormall(real);
    pure int __signbit(double);
    pure int __signbitf(float);
    pure int __signbitl(real);

    //int fpclassify(real-floating x);
      ///
    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassifyd") pure int fpclassify(double x);
    ///
    pragma(mangle, "__fpclassifyl") pure int fpclassify(real x);

    //int isfinite(real-floating x);
    ///
    pragma(mangle, "__isfinitef") pure int isfinite(float x);
    ///
    pragma(mangle, "__isfinite")  pure int isfinite(double x);
    ///
    pragma(mangle, "__isfinitel") pure int isfinite(real x);

    //int isinf(real-floating x);
    ///
    pragma(mangle, "__isinff") pure int isinf(float x);
    ///
    extern (D) pure int isinf(double x)         { return __isinfl(x); }
    ///
    pragma(mangle, "__isinfl") pure int isinf(real x);

    //int isnan(real-floating x);
    ///
    extern (D) pure int isnan(float x)          { return __isnanl(x); }
    ///
    extern (D) pure int isnan(double x)         { return __isnanl(x); }
    ///
    pragma(mangle, "__isnanl") pure int isnan(real x);

    //int isnormal(real-floating x);
    ///
    pragma(mangle, "__isnormalf") pure int isnormal(float x);
    ///
    pragma(mangle, "__isnormal")  pure int isnormal(double x);
    ///
    pragma(mangle, "__isnormall") pure int isnormal(real x);

    //int signbit(real-floating x);
    ///
    pragma(mangle, "__signbitf") pure int signbit(float x);
    ///
    pragma(mangle, "__signbit")  pure int signbit(double x);
    ///
    extern (D) pure int signbit(real x)         { return __signbit(x); }
}
else version (OpenBSD)
{
    enum
    {
        ///
        FP_INFINITE  = 0x01,
        ///
        FP_NAN       = 0x02,
        ///
        FP_NORMAL    = 0x04,
        ///
        FP_SUBNORMAL = 0x08,
        ///
        FP_ZERO      = 0x10,
    }

    enum
    {
        ///
        FP_FAST_FMA  = 1,
        ///
        FP_FAST_FMAF = 1,
        ///
        FP_FAST_FMAL = 1,
    }

    pure int __fpclassify(double);
    pure int __fpclassifyf(float);
    pure int __fpclassifyl(real);
    pure int __isfinitef(float);
    pure int __isfinite(double);
    pure int __isfinitel(real);
    pure int __isinff(float);
    pure int __isinfl(real);
    pure int __isnanl(real);
    pure int __isnormalf(float);
    pure int __isnormal(double);
    pure int __isnormall(real);
    pure int __signbit(double);
    pure int __signbitf(float);
    pure int __signbitl(real);

    //int fpclassify(real-floating x);
      ///
    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassify") pure int fpclassify(double x);
    ///
    pragma(mangle, "__fpclassifyl") pure int fpclassify(real x);

    //int isfinite(real-floating x);
    ///
    pragma(mangle, "__isfinitef") pure int isfinite(float x);
    ///
    pragma(mangle, "__isfinite")  pure int isfinite(double x);
    ///
    pragma(mangle, "__isfinitel") pure int isfinite(real x);

    //int isinf(real-floating x);
    ///
    pragma(mangle, "__isinff") pure int isinf(float x);
    ///
    extern (D) pure int isinf(double x)         { return __isinfl(x); }
    ///
    pragma(mangle, "__isinfl") pure int isinf(real x);

    //int isnan(real-floating x);
    ///
    extern (D) pure int isnan(float x)          { return __isnanl(x); }
    ///
    extern (D) pure int isnan(double x)         { return __isnanl(x); }
    ///
    pragma(mangle, "__isnanl") pure int isnan(real x);

    //int isnormal(real-floating x);
    ///
    pragma(mangle, "__isnormalf") pure int isnormal(float x);
    ///
    pragma(mangle, "__isnormal")  pure int isnormal(double x);
    ///
    pragma(mangle, "__isnormall") pure int isnormal(real x);

    //int signbit(real-floating x);
    ///
    pragma(mangle, "__signbitf") pure int signbit(float x);
    ///
    pragma(mangle, "__signbit")  pure int signbit(double x);
    ///
    extern (D) pure int signbit(real x)         { return __signbit(x); }
}
else version (NetBSD)
{
    enum
    {
        ///
        FP_INFINITE    = 0,
        ///
        FP_NAN         = 1,
        ///
        FP_NORMAL      = 2,
        ///
        FP_SUBNORMAL   = 3,
        ///
        FP_ZERO        = 4,
    }

    enum
    {
        ///
        FP_FAST_FMA  = 0,
        ///
        FP_FAST_FMAF = 0,
        ///
        FP_FAST_FMAL = 0,
    }

    pure uint __fpclassifyf(float x);
    pure uint __fpclassifyd(double x);
    pure uint __fpclassifyl(real x);

    //int fpclassify(real-floating x);
    ///
    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassifyd") pure int fpclassify(double x);
    ///
    pragma(mangle, real.sizeof == double.sizeof ? "__fpclassifyd" : "__fpclassifyl")
    pure int fpclassify(real x);

  extern (D)
  {
    //int isfinite(real-floating x);
    ///
    pure int isfinite(float x)       { return fpclassify(x) >= FP_NORMAL; }
    ///
    pure int isfinite(double x)      { return fpclassify(x) >= FP_NORMAL; }
    ///
    pure int isfinite(real x)        { return fpclassify(x) >= FP_NORMAL; }

    //int isinf(real-floating x);
    ///
    pure int isinf(float x)          { return fpclassify(x) == FP_INFINITE; }
    ///
    pure int isinf(double x)         { return fpclassify(x) == FP_INFINITE; }
    ///
    pure int isinf(real x)           { return fpclassify(x) == FP_INFINITE; }

    //int isnan(real-floating x);
    ///
    pure int isnan(float x)          { return fpclassify(x) == FP_NAN;   }
    ///
    pure int isnan(double x)         { return fpclassify(x) == FP_NAN;   }
    ///
    pure int isnan(real x)           { return fpclassify(x) == FP_NAN;   }

    //int isnormal(real-floating x);
    ///
    pure int isnormal(float x)       { return fpclassify(x) == FP_NORMAL; }
    ///
    pure int isnormal(double x)      { return fpclassify(x) == FP_NORMAL; }
    ///
    pure int isnormal(real x)        { return fpclassify(x) == FP_NORMAL; }

    //int signbit(real-floating x);
    ///
    pure int signbit(float x)     { return (cast(short*)&(x))[1] & 0x8000; }
    ///
    pure int signbit(double x)    { return (cast(short*)&(x))[3] & 0x8000; }
    ///
    pure int signbit(real x)
    {
        return (real.sizeof == double.sizeof)
            ? (cast(short*)&(x))[3] & 0x8000
            : (cast(short*)&(x))[4] & 0x8000;
    }
  }
}
else version (DragonFlyBSD)
{
    enum
    {
        FP_INFINITE  = 0x01,
        FP_NAN       = 0x02,
        FP_NORMAL    = 0x04,
        FP_SUBNORMAL = 0x08,
        FP_ZERO      = 0x10,
    }

    /*
     * /usr/include/math.h : martynas@openbsd believes only F version is true.
       enum FP_FAST_FMA  = 1;
       enum FP_FAST_FMAL = 1;
     */
    enum  FP_FAST_FMAF = 1;

    pure int __fpclassifyd(double);
    pure int __fpclassifyf(float);
    pure int __fpclassifyl(real);
    pure int __isfinitef(float);
    pure int __isfinite(double);
    pure int __isfinitel(real);
    pure int __isinff(float);
    pure int __isinf(double);
    pure int __isinfl(real);
    pure int __isnanf(float);
    pure int __isnan(double);
    pure int __isnanl(real);
    pure int __isnormalf(float);
    pure int __isnormal(double);
    pure int __isnormall(real);
    pure int __signbit(double);
    pure int __signbitf(float);
    pure int __signbitl(real);

    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    pragma(mangle, "__fpclassifyd") pure int fpclassify(double x);
    pragma(mangle, "__fpclassifyl") pure int fpclassify(real x);

    pragma(mangle, "__isfinitef") pure int isfinite(float x);
    pragma(mangle, "__isfinite")  pure int isfinite(double x);
    pragma(mangle, "__isfinitel") pure int isfinite(real x);

    pragma(mangle, "__isinff") pure int isinf(float x);
    pragma(mangle, "__isinf")  pure int isinf(double x);
    pragma(mangle, "__isinfl") pure int isinf(real x);

    pragma(mangle, "__isnanf") pure int isnan(float x);
    pragma(mangle, "__isnan")  pure int isnan(double x);
    pragma(mangle, "__isnanl") pure int isnan(real x);

    pragma(mangle, "__isnormalf") pure int isnormal(float x);
    pragma(mangle, "__isnormal")  pure int isnormal(double x);
    pragma(mangle, "__isnormall") pure int isnormal(real x);

    pragma(mangle, "__signbitf") pure int signbit(float x);
    pragma(mangle, "__signbit")  pure int signbit(double x);
    pragma(mangle, "__signbitl") pure int signbit(real x);
}
else version (Solaris)
{
    enum
    {
        FP_INFINITE  = 3,
        FP_NAN       = 4,
        FP_NORMAL    = 2,
        FP_SUBNORMAL = 1,
        FP_ZERO      = 0,
    }

    enum
    {
        ///
        FP_FAST_FMA  = 0,
        ///
        FP_FAST_FMAF = 0,
        ///
        FP_FAST_FMAL = 0,
    }

  extern (D)
  {
    //int fpclassify(real-floating x);
    ///
    pure int fpclassify(float x)
    {
        return isnan(x) ? FP_NAN    : isinf(x)  ? FP_INFINITE :
            isnormal(x) ? FP_NORMAL : x == 0.0f ? FP_ZERO :
                          FP_SUBNORMAL;
    }

    ///
    pure int fpclassify(double x)
    {
        return isnan(x) ? FP_NAN    : isinf(x)  ? FP_INFINITE :
            isnormal(x) ? FP_NORMAL : x == 0.0  ? FP_ZERO :
                          FP_SUBNORMAL;
    }

    ///
    pure int fpclassify(real x)
    {
        return isnan(x) ? FP_NAN    : isinf(x)  ? FP_INFINITE :
            isnormal(x) ? FP_NORMAL : x == 0.0L ? FP_ZERO :
                          FP_SUBNORMAL;
    }

    //int isfinite(real-floating x);
    ///
    pure int isfinite(float x)       { return !isnan(x) && !isinf(x); }
    ///
    pure int isfinite(double x)      { return !isnan(x) && !isinf(x); }
    ///
    pure int isfinite(real x)        { return !isnan(x) && !isinf(x); }

    //int isinf(real-floating x);
    ///
    pure int isinf(float x)          { return x == float.infinity || x == -float.infinity; }
    ///
    pure int isinf(double x)         { return x == double.infinity || x == -double.infinity; }
    ///
    pure int isinf(real x)           { return x == real.infinity || x == -real.infinity; }

    //int isnan(real-floating x);
    ///
    pure int isnan(float x)          { return x != x; }
    ///
    pure int isnan(double x)         { return x != x; }
    ///
    pure int isnan(real x)           { return x != x; }

    //int isnormal(real-floating x);
    ///
    pure int isnormal(float x)
    {
        import core.math;
        return isfinite(x) && fabs(x) >= float.min_normal;
    }
    ///
    pure int isnormal(double x)
    {
        import core.math;
        return isfinite(x) && fabs(x) >= double.min_normal;
    }
    ///
    pure int isnormal(real x)
    {
        import core.math;
        return isfinite(x) && fabs(x) >= real.min_normal;
    }

    //int signbit(real-floating x);
    ///
    pure int signbit(float x)
    {
        version (SPARC_Any)
            return cast(int)(*cast(uint*)&x >> 31);
        else version (X86_Any)
            return cast(int)(*cast(uint*)&x >> 31);
        else
            static assert(false, "Architecture not supported.");
    }
    ///
    pure int signbit(double x)
    {
        version (SPARC_Any)
            return cast(int)(*cast(uint*)&x >> 31);
        else version (X86_Any)
            return cast(int)((cast(uint*)&x)[1] >> 31);
        else
            static assert(false, "Architecture not supported.");
    }
    ///
    pure int signbit(real x)
    {
        version (SPARC_Any)
            return cast(int)(*cast(uint*)&x >> 31);
        else version (X86_Any)
            return cast(int)((cast(ushort *)&x)[4] >> 15);
        else
            static assert(false, "Architecture not supported.");
    }
  }
}
else version (CRuntime_Bionic)
{
    enum
    {
        ///
        FP_INFINITE  = 0x01,
        ///
        FP_NAN       = 0x02,
        ///
        FP_NORMAL    = 0x04,
        ///
        FP_SUBNORMAL = 0x08,
        ///
        FP_ZERO      = 0x10,
    }

    ///
    enum FP_FAST_FMAF;

    pure int __fpclassifyd(double);
    pure int __fpclassifyf(float);
    pure int __fpclassifyl(real);

    pure int __isfinitef(float);
    pure int __isfinite(double);
    pure int __isfinitel(real);

    pure int __isinff(float);
    pure int __isinf(double);
    pure int __isinfl(real);

    pure int isnanf(float);
    pure int isnan(double);
    pure int __isnanl(real);

    pure int __isnormalf(float);
    pure int __isnormal(double);
    pure int __isnormall(real);

    pure int __signbit(double);
    pure int __signbitf(float);
    pure int __signbitl(real);

    //int fpclassify(real-floating x);
      ///
    pragma(mangle, "__fpclassifyf") pure int fpclassify(float x);
    ///
    pragma(mangle, "__fpclassifyd") pure int fpclassify(double x);
    ///
    pragma(mangle, "__fpclassifyl") pure int fpclassify(real x);

    //int isfinite(real-floating x);
    ///
    pragma(mangle, "__isfinitef") pure int isfinite(float x);
    ///
    pragma(mangle, "__isfinite")  pure int isfinite(double x);
    ///
    pragma(mangle, "__isfinitel") pure int isfinite(real x);

    //int isinf(real-floating x);
    ///
    pragma(mangle, "__isinff") pure int isinf(float x);
    ///
    pragma(mangle, "__isinf")  pure int isinf(double x);
    ///
    pragma(mangle, "__isinfl") pure int isinf(real x);

    //int isnan(real-floating x);
    ///
    pragma(mangle, "isnanf")   pure int isnan(float x);
    ///
    pragma(mangle, "__isnanl") pure int isnan(real x);

    //int isnormal(real-floating x);
    ///
    pragma(mangle, "__isnormalf") pure int isnormal(float x);
    ///
    pragma(mangle, "__isnormal")  pure int isnormal(double x);
    ///
    pragma(mangle, "__isnormall") pure int isnormal(real x);

    //int signbit(real-floating x);
    ///
    pragma(mangle, "__signbitf") pure int signbit(float x);
    ///
    pragma(mangle, "__signbit")  pure int signbit(double x);
    ///
    pragma(mangle, "__signbitl") pure int signbit(real x);
}

extern (D)
{
    //int isgreater(real-floating x, real-floating y);
    ///
    pure int isgreater(float x, float y)        { return x > y; }
    ///
    pure int isgreater(double x, double y)      { return x > y; }
    ///
    pure int isgreater(real x, real y)          { return x > y; }

    //int isgreaterequal(real-floating x, real-floating y);
    ///
    pure int isgreaterequal(float x, float y)   { return x >= y; }
    ///
    pure int isgreaterequal(double x, double y) { return x >= y; }
    ///
    pure int isgreaterequal(real x, real y)     { return x >= y; }

    //int isless(real-floating x, real-floating y);
    ///
    pure int isless(float x, float y)           { return x < y; }
    ///
    pure int isless(double x, double y)         { return x < y; }
    ///
    pure int isless(real x, real y)             { return x < y; }

    //int islessequal(real-floating x, real-floating y);
    ///
    pure int islessequal(float x, float y)      { return x <= y; }
    ///
    pure int islessequal(double x, double y)    { return x <= y; }
    ///
    pure int islessequal(real x, real y)        { return x <= y; }

    //int islessgreater(real-floating x, real-floating y);
    ///
    pure int islessgreater(float x, float y)    { return x != y && !isunordered(x, y); }
    ///
    pure int islessgreater(double x, double y)  { return x != y && !isunordered(x, y); }
    ///
    pure int islessgreater(real x, real y)      { return x != y && !isunordered(x, y); }

    //int isunordered(real-floating x, real-floating y);
    ///
    pure int isunordered(float x, float y)      { return isnan(x) || isnan(y); }
    ///
    pure int isunordered(double x, double y)    { return isnan(x) || isnan(y); }
    ///
    pure int isunordered(real x, real y)        { return isnan(x) || isnan(y); }
}

/* MS define some functions inline.
 * Additionally, their *l functions work with a 64-bit long double and are thus
 * useless for 80-bit D reals. So we use our own wrapper implementations working
 * internally with reduced 64-bit precision.
 * This also enables relaxing real to 64-bit double.
 */
version (CRuntime_Microsoft) // fully supported since MSVCRT 12 (VS 2013) only
{
    ///
    double  acos(double x);
    ///
    float   acosf(float x);
    ///
    extern(D) real acosl()(real x)   { return acos(cast(double) x); }

    ///
    double  asin(double x);
    ///
    float   asinf(float x);
    ///
    extern(D) real asinl()(real x)   { return asin(cast(double) x); }

    ///
    pure double  atan(double x);
    ///
    pure float   atanf(float x);
    ///
    pure extern(D) real atanl()(real x)   { return atan(cast(double) x); }

    ///
    double  atan2(double y, double x);
    ///
    float   atan2f(float y, float x);
    ///
    extern(D) real atan2l()(real y, real x) { return atan2(cast(double) y, cast(double) x); }

    ///
    pure double  cos(double x);
    ///
    pure float   cosf(float x);
    ///
    extern(D) pure real cosl()(real x)    { return cos(cast(double) x); }

    ///
    pure double  sin(double x);
    ///
    pure float   sinf(float x);
    ///
    extern(D) pure real sinl()(real x)    { return sin(cast(double) x); }

    ///
    pure double  tan(double x);
    ///
    pure float   tanf(float x);
    ///
    extern(D) pure real tanl()(real x)    { return tan(cast(double) x); }

    ///
    double  acosh(double x);
    ///
    float   acoshf(float x);
    ///
    extern(D) real acoshl()(real x)  { return acosh(cast(double) x); }

    ///
    pure double  asinh(double x);
    ///
    pure float   asinhf(float x);
    ///
    pure extern(D) real asinhl()(real x)  { return asinh(cast(double) x); }

    ///
    double  atanh(double x);
    ///
    float   atanhf(float x);
    ///
    extern(D) real atanhl()(real x)  { return atanh(cast(double) x); }

    ///
    double  cosh(double x);
    ///
    float   coshf(float x);
    ///
    extern(D) real coshl()(real x)   { return cosh(cast(double) x); }

    ///
    double  sinh(double x);
    ///
    float   sinhf(float x);
    ///
    extern(D) real sinhl()(real x)   { return sinh(cast(double) x); }

    ///
    pure double  tanh(double x);
    ///
    pure float   tanhf(float x);
    ///
    extern(D) pure real tanhl()(real x)   { return tanh(cast(double) x); }

    ///
    double  exp(double x);
    ///
    float   expf(float x);
    ///
    extern(D) real expl()(real x)    { return exp(cast(double) x); }

    ///
    double  exp2(double x);
    ///
    float   exp2f(float x);
    ///
    extern(D) real exp2l()(real x)   { return exp2(cast(double) x); }

    ///
    double  expm1(double x);
    ///
    float   expm1f(float x);
    ///
    extern(D) real expm1l()(real x)  { return expm1(cast(double) x); }

    ///
    pure double  frexp(double value, int* exp);
    ///
    extern(D) pure float frexpf()(float value, int* exp) { return cast(float) frexp(value, exp); }
    ///
    extern(D) pure real  frexpl()(real value, int* exp)  { return frexp(cast(double) value, exp); }

    ///
    int     ilogb(double x);
    ///
    int     ilogbf(float x);
    ///
    extern(D) int ilogbl()(real x)   { return ilogb(cast(double) x); }

    ///
    double  ldexp(double x, int exp);
    ///
    extern(D) float ldexpf()(float x, int exp) { return cast(float) ldexp(x, exp); }
    ///
    extern(D) real  ldexpl()(real x, int exp)  { return ldexp(cast(double) x, exp); }

    ///
    double  log(double x);
    ///
    float   logf(float x);
    ///
    extern(D) real logl()(real x)    { return log(cast(double) x); }

    ///
    double  log10(double x);
    ///
    float   log10f(float x);
    ///
    extern(D) real log10l()(real x)  { return log10(cast(double) x); }

    ///
    double  log1p(double x);
    ///
    float   log1pf(float x);
    ///
    extern(D) real log1pl()(real x)  { return log1p(cast(double) x); }

    ///
    double  log2(double x);
    ///
    float   log2f(float x);
    ///
    extern(D) real log2l()(real x)   { return log2(cast(double) x); }

    ///
    double  logb(double x);
    ///
    float   logbf(float x);
    ///
    extern(D) real logbl()(real x)   { return logb(cast(double) x); }

    ///
    pure double  modf(double value, double* iptr);
    ///
    pure float   modff(float value, float* iptr);
    ///
    extern(D) pure real modfl()(real value, real* iptr)
    {
        double i;
        double r = modf(cast(double) value, &i);
        *iptr = i;
        return r;
    }

    ///
    double  scalbn(double x, int n);
    ///
    float   scalbnf(float x, int n);
    ///
    extern(D) real scalbnl()(real x, int n) { return scalbn(cast(double) x, n); }

    ///
    double  scalbln(double x, c_long n);
    ///
    float   scalblnf(float x, c_long n);
    ///
    extern(D) real scalblnl()(real x, c_long n) { return scalbln(cast(double) x, n); }

    ///
    pure double  cbrt(double x);
    ///
    pure float   cbrtf(float x);
    ///
    extern(D) pure real cbrtl()(real x)   { return cbrt(cast(double) x); }

    ///
    pure double  fabs(double x);
    ///
    extern(D) pure float fabsf()(float x) { return cast(float) fabs(x); }
    ///
    extern(D) pure real  fabsl()(real x)  { return fabs(cast(double) x); }

    ///
    extern(C) pragma(mangle, "_hypot")  double hypot(double x, double y);
    ///
    extern(C) pragma(mangle, "_hypotf") float  hypotf(float x, float y);
    ///
    extern(D) real   hypotl(real x, real y)    { return hypot(cast(double) x, cast(double) y); }

    ///
    double  pow(double x, double y);
    ///
    float   powf(float x, float y);
    ///
    extern(D) real powl()(real x, real y) { return pow(cast(double) x, cast(double) y); }

    ///
    double  sqrt(double x);
    ///
    float   sqrtf(float x);
    ///
    extern(D) real sqrtl()(real x)   { return sqrt(cast(double) x); }

    ///
    pure double  erf(double x);
    ///
    pure float   erff(float x);
    ///
    extern(D) pure real erfl()(real x)    { return erf(cast(double) x); }

    ///
    double  erfc(double x);
    ///
    float   erfcf(float x);
    ///
    extern(D) real erfcl()(real x)   { return erfc(cast(double) x); }

    ///
    double  lgamma(double x);
    ///
    float   lgammaf(float x);
    ///
    extern(D) real lgammal()(real x) { return lgamma(cast(double) x); }

    ///
    double  tgamma(double x);
    ///
    float   tgammaf(float x);
    ///
    extern(D) real tgammal()(real x) { return tgamma(cast(double) x); }

    ///
    pure double  ceil(double x);
    ///
    pure float   ceilf(float x);
    ///
    extern(D) pure real ceill()(real x)   { return ceil(cast(double) x); }

    ///
    pure double  floor(double x);
    ///
    pure float   floorf(float x);
    ///
    extern(D) pure real floorl()(real x)  { return floor(cast(double) x); }

    ///
    pure double  nearbyint(double x);
    ///
    pure float   nearbyintf(float x);
    ///
    extern(D) pure real nearbyintl()(real x) { return nearbyint(cast(double) x); }

    ///
    pure double  rint(double x);
    ///
    pure float   rintf(float x);
    ///
    extern(D) pure real rintl()(real x)   { return rint(cast(double) x); }

    ///
    c_long  lrint(double x);
    ///
    c_long  lrintf(float x);
    ///
    extern(D) c_long lrintl()(real x) { return lrint(cast(double) x); }

    ///
    long    llrint(double x);
    ///
    long    llrintf(float x);
    ///
    extern(D) long llrintl()(real x) { return llrint(cast(double) x); }

    ///
    pure double  round(double x);
    ///
    pure float   roundf(float x);
    ///
    extern(D) pure real roundl()(real x)  { return round(cast(double) x); }

    ///
    c_long  lround(double x);
    ///
    c_long  lroundf(float x);
    ///
    extern(D) c_long lroundl()(real x) { return lround(cast(double) x); }

    ///
    long    llround(double x);
    ///
    long    llroundf(float x);
    ///
    extern(D) long llroundl()(real x) { return llround(cast(double) x); }

    ///
    pure double  trunc(double x);
    ///
    pure float   truncf(float x);
    ///
    extern(D) pure real truncl()(real x)  { return trunc(cast(double) x); }

    ///
    double  fmod(double x, double y);
    ///
    float   fmodf(float x, float y);
    ///
    extern(D) real fmodl()(real x, real y) { return fmod(cast(double) x, cast(double) y); }

    ///
    double  remainder(double x, double y);
    ///
    float   remainderf(float x, float y);
    ///
    extern(D) real remainderl()(real x, real y) { return remainder(cast(double) x, cast(double) y); }

    ///
    double  remquo(double x, double y, int* quo);
    ///
    float   remquof(float x, float y, int* quo);
    ///
    extern(D) real remquol()(real x, real y, int* quo) { return remquo(cast(double) x, cast(double) y, quo); }

    ///
    pure double  copysign(double x, double y);
    ///
    pure float   copysignf(float x, float y);
    ///
    extern(D) pure real copysignl()(real x, real y) { return copysign(cast(double) x, cast(double) y); }

    ///
    pure double  nan(char* tagp);
    ///
    pure float   nanf(char* tagp);
    ///
    extern(D) pure real nanl()(char* tagp) { return nan(tagp); }

    ///
    double  nextafter(double x, double y);
    ///
    float   nextafterf(float x, float y);
    ///
    extern(D) real nextafterl()(real x, real y) { return nextafter(cast(double) x, cast(double) y); }

    ///
    double  nexttoward(double x, real y);
    ///
    float   nexttowardf(float x, real y);
    ///
    extern(D) real nexttowardl()(real x, real y) { return nexttoward(cast(double) x, cast(double) y); }

    ///
    double  fdim(double x, double y);
    ///
    float   fdimf(float x, float y);
    ///
    extern(D) real fdiml()(real x, real y) { return fdim(cast(double) x, cast(double) y); }

    ///
    pure double  fmax(double x, double y);
    ///
    pure float   fmaxf(float x, float y);
    ///
    extern(D) pure real fmaxl()(real x, real y) { return fmax(cast(double) x, cast(double) y); }

    ///
    pure double  fmin(double x, double y);
    ///
    pure float   fminf(float x, float y);
    ///
    extern(D) pure real fminl()(real x, real y) { return fmin(cast(double) x, cast(double) y); }

    ///
    pure double  fma(double x, double y, double z);
    ///
    pure float   fmaf(float x, float y, float z);
    ///
    extern(D) pure real fmal()(real x, real y, real z) { return fma(cast(double) x, cast(double) y, cast(double) z); }
}
else version (FreeBSD)
{
    ///
    double  acos(double x);
    ///
    float   acosf(float x);
    ///
    real    acosl(real x); // since 8.0

    ///
    double  asin(double x);
    ///
    float   asinf(float x);
    ///
    real    asinl(real x); // since 8.0

    ///
    pure double  atan(double x);
    ///
    pure float   atanf(float x);
    ///
    pure real    atanl(real x); // since 8.0

    ///
    double  atan2(double y, double x);
    ///
    float   atan2f(float y, float x);
    ///
    real    atan2l(real y, real x); // since 8.0

    ///
    pure double  cos(double x);
    ///
    pure float   cosf(float x);
    ///
    pure real    cosl(real x); // since 8.0

    ///
    pure double  sin(double x);
    ///
    pure float   sinf(float x);
    ///
    pure real    sinl(real x); // since 8.0

    ///
    pure double  tan(double x);
    ///
    pure float   tanf(float x);
    ///
    pure real    tanl(real x); // since 8.0

    ///
    double  acosh(double x);
    ///
    float   acoshf(float x);
    ///
    real    acoshl(real x); // since 10.0

    ///
    pure double  asinh(double x);
    ///
    pure float   asinhf(float x);
    ///
    pure real    asinhl(real x); // since 10.0

    ///
    double  atanh(double x);
    ///
    float   atanhf(float x);
    ///
    real    atanhl(real x); // since 10.0

    ///
    double  cosh(double x);
    ///
    float   coshf(float x);
    ///
    real    coshl(real x); // since 10.1

    ///
    double  sinh(double x);
    ///
    float   sinhf(float x);
    ///
    real    sinhl(real x); // since 10.1

    ///
    pure double  tanh(double x);
    ///
    pure float   tanhf(float x);
    ///
    pure real    tanhl(real x); // since 10.1

    ///
    double  exp(double x);
    ///
    float   expf(float x);
    ///
    real    expl(real x); // since 10.0

    ///
    double  exp2(double x);
    ///
    float   exp2f(float x);
    ///
    real    exp2l(real x); // since 8.0

    ///
    double  expm1(double x);
    ///
    float   expm1f(float x);
    ///
    real    expm1l(real x); // since 10.0

    ///
    pure double  frexp(double value, int* exp);
    ///
    pure float   frexpf(float value, int* exp);
    ///
    pure real    frexpl(real value, int* exp); // since 6.0

    ///
    int     ilogb(double x);
    ///
    int     ilogbf(float x);
    ///
    int     ilogbl(real x); // since 5.4

    ///
    double  ldexp(double x, int exp);
    ///
    float   ldexpf(float x, int exp);
    ///
    real    ldexpl(real x, int exp); // since 6.0

    ///
    double  log(double x);
    ///
    float   logf(float x);
    ///
    real    logl(real x); // since 10.0

    ///
    double  log10(double x);
    ///
    float   log10f(float x);
    ///
    real    log10l(real x); // since 10.0

    ///
    double  log1p(double x);
    ///
    float   log1pf(float x);
    ///
    real    log1pl(real x); // since 10.0

    ///
    double log2(double x); // since 8.3
    ///
    float  log2f(float x); // since 8.3
    ///
    real   log2l(real x); // since 10.0

    ///
    double  logb(double x);
    ///
    float   logbf(float x);
    ///
    real    logbl(real x); // since 8.0

    ///
    pure double  modf(double value, double* iptr);
    ///
    pure float   modff(float value, float* iptr);
    ///
    pure real    modfl(real value, real *iptr); // since 8.0

    ///
    double  scalbn(double x, int n);
    ///
    float   scalbnf(float x, int n);
    ///
    real    scalbnl(real x, int n); // since 6.0

    ///
    double  scalbln(double x, c_long n);
    ///
    float   scalblnf(float x, c_long n);
    ///
    real    scalblnl(real x, c_long n); // since 6.0

    ///
    pure double  cbrt(double x);
    ///
    pure float   cbrtf(float x);
    ///
    pure real    cbrtl(real x); // since 9.0

    ///
    pure double  fabs(double x);
    ///
    pure float   fabsf(float x);
    ///
    pure real    fabsl(real x); // since 5.3

    ///
    double  hypot(double x, double y);
    ///
    float   hypotf(float x, float y);
    ///
    real    hypotl(real x, real y); // since 8.0

    ///
    double  pow(double x, double y);
    ///
    float   powf(float x, float y);
    ///
    real    powl(real x, real y); // since 10.4

    ///
    double  sqrt(double x);
    ///
    float   sqrtf(float x);
    ///
    real    sqrtl(real x); // since 8.0

    ///
    pure double  erf(double x);
    ///
    pure float   erff(float x);
    ///
    pure real    erfl(real x); // since 10.1

    ///
    double  erfc(double x);
    ///
    float   erfcf(float x);
    ///
    real    erfcl(real x); // since 10.1

    ///
    double  lgamma(double x);
    ///
    float   lgammaf(float x);
    ///
    real    lgammal(real x); // since 10.2

    ///
    double  tgamma(double x);
    ///
    float   tgammaf(float x);
    ///
    real    tgammal(real x); // since 11.2

    ///
    pure double  ceil(double x);
    ///
    pure float   ceilf(float x);
    ///
    pure real    ceill(real x); // since 5.4

    ///
    pure double  floor(double x);
    ///
    pure float   floorf(float x);
    ///
    pure real    floorl(real x); // since 5.4

    ///
    pure double  nearbyint(double x);
    ///
    pure float   nearbyintf(float x);
    ///
    pure real    nearbyintl(real x); // since 8.0

    ///
    pure double  rint(double x);
    ///
    pure float   rintf(float x);
    ///
    pure real    rintl(real x); // since 8.0

    ///
    c_long  lrint(double x);
    ///
    c_long  lrintf(float x);
    ///
    c_long  lrintl(real x); // since 8.0

    ///
    long    llrint(double x);
    ///
    long    llrintf(float x);
    ///
    long    llrintl(real x); // since 8.0

    ///
    pure double  round(double x);
    ///
    pure float   roundf(float x);
    ///
    pure real    roundl(real x); // since 6.0

    ///
    c_long  lround(double x);
    ///
    c_long  lroundf(float x);
    ///
    c_long  lroundl(real x); // since 6.0

    ///
    long    llround(double x);
    ///
    long    llroundf(float x);
    ///
    long    llroundl(real x); // since 6.0

    ///
    pure double  trunc(double x);
    ///
    pure float   truncf(float x);
    ///
    pure real    truncl(real x); // since 6.0

    ///
    double  fmod(double x, double y);
    ///
    float   fmodf(float x, float y);
    ///
    real    fmodl(real x, real y); // since 8.0

    ///
    double  remainder(double x, double y);
    ///
    float   remainderf(float x, float y);
    ///
    real    remainderl(real x, real y); // since 8.0

    ///
    double  remquo(double x, double y, int* quo);
    ///
    float   remquof(float x, float y, int* quo);
    ///
    real    remquol(real x, real y, int* quo); // since 8.0

    ///
    pure double  copysign(double x, double y);
    ///
    pure float   copysignf(float x, float y);
    ///
    pure real    copysignl(real x, real y); // since 5.3

    ///
    pure double  nan(const char*); // since 8.0
    ///
    pure float   nanf(const char*); // since 8.0
    ///
    pure real    nanl(const char*); // since 8.0

    ///
    double  nextafter(double x, double y);
    ///
    float   nextafterf(float x, float y);
    ///
    real    nextafterl(real x, real y); // since 6.0

    ///
    double  nexttoward(double x, real y);
    ///
    float   nexttowardf(float x, real y);
    ///
    real    nexttowardl(real x, real y); // since 6.0

    ///
    double  fdim(double x, double y);
    ///
    float   fdimf(float x, float y);
    ///
    real    fdiml(real x, real y); // since 5.3

    ///
    pure double  fmax(double x, double y);
    ///
    pure float   fmaxf(float x, float y);
    ///
    pure real    fmaxl(real x, real y); // since 5.3

    ///
    pure double  fmin(double x, double y);
    ///
    pure float   fminf(float x, float y);
    ///
    pure real    fminl(real x, real y); // since 5.3

    ///
    pure double  fma(double x, double y, double z);
    ///
    pure float   fmaf(float x, float y, float z);
    ///
    pure real    fmal(real x, real y, real z); // since 6.0
}
else version (NetBSD)
{

    ///
    real    acosl(real x);
    ///
    real    asinl(real x);
    ///
    pure real    atanl(real x);
    ///
    real    atan2l(real y, real x);
    ///
    pure real    cosl(real x);
    ///
    pure real    sinl(real x);
    ///
    pure real    tanl(real x);
    ///
    real    exp2l(real x);
    ///
    pure real    frexpl(real value, int* exp);
    ///
    int     ilogbl(real x);
    ///
    real    ldexpl(real x, int exp);
    ///
    real    logbl(real x);
    ///
    pure real    modfl(real value, real *iptr);
    ///
    real    scalbnl(real x, int n);
    ///
    real    scalblnl(real x, c_long n);
    ///
    pure real    fabsl(real x);
    ///
    real    hypotl(real x, real y);
    ///
    real    sqrtl(real x);
    ///
    pure real    ceill(real x);
    ///
    pure real    floorl(real x);
    ///
    pure real    nearbyintl(real x);
    ///
    pure real    rintl(real x);
    ///
    extern(D) c_long lrintl(real x) { return cast(c_long)rintl(x); }
    ///
    pure real    roundl(real x);
    ///
    extern(D) c_long lroundl(real x) { return cast(c_long)roundl(x);}
    ///
    extern(D) long llroundl(real x) { return cast(long)roundl(x);}
    ///
    pure real    truncl(real x);
    ///
    real    fmodl(real x, real y);
    ///
    real    remainderl(real x, real y)  { return remainder(x,y); }
    ///
    real    remquol(real x, real y, int* quo){ return remquo(x,y,quo); }
    ///
    pure real    copysignl(real x, real y);
    ///
    pure double  nan(char* tagp);
    ///
    pure float   nanf(char* tagp);
    ///
    pure real    nanl(char* tagp);
    ///
    real    nextafterl(real x, real y);
    ///
    extern(D) real nexttowardl(real x, real y) { return nexttoward(cast(double) x, cast(double) y); }
    ///
    real    fdiml(real x, real y);
    ///
    pure real    fmaxl(real x, real y);
    ///
    pure real    fminl(real x, real y);
    ///
    pure real    fmal(real x, real y, real z);

    ///
    double  acos(double x);
    ///
    float   acosf(float x);

    ///
    double  asin(double x);
    ///
    float   asinf(float x);

    ///
    pure double  atan(double x);
    ///
    pure float   atanf(float x);

    ///
    double  atan2(double y, double x);
    ///
    float   atan2f(float y, float x);

    ///
    pure double  cos(double x);
    ///
    pure float   cosf(float x);

    ///
    pure double  sin(double x);
    ///
    pure float   sinf(float x);

    ///
    pure double  tan(double x);
    ///
    pure float   tanf(float x);

    ///
    double  acosh(double x);
    ///
    float   acoshf(float x);
    ///
    real    acoshl(real x);

    ///
    pure double  asinh(double x);
    ///
    pure float   asinhf(float x);
    ///
    pure real    asinhl(real x);

    ///
    double  atanh(double x);
    ///
    float   atanhf(float x);
    ///
    real    atanhl(real x);

    ///
    double  cosh(double x);
    ///
    float   coshf(float x);
    ///
    real    coshl(real x);

    ///
    double  sinh(double x);
    ///
    float   sinhf(float x);
    ///
    real    sinhl(real x);

    ///
    pure double  tanh(double x);
    ///
    pure float   tanhf(float x);
    ///
    pure real    tanhl(real x);

    ///
    double  exp(double x);
    ///
    float   expf(float x);
    ///
    real    expl(real x);

    ///
    double  exp2(double x);
    ///
    float   exp2f(float x);

    ///
    double  expm1(double x);
    ///
    float   expm1f(float x);
    ///
    real    expm1l(real x)  { return expm1(cast(double) x); }

    ///
    pure double  frexp(double value, int* exp);
    ///
    pure float   frexpf(float value, int* exp);

    ///
    int     ilogb(double x);
    ///
    int     ilogbf(float x);

    ///
    double  ldexp(double x, int exp);
    ///
    float   ldexpf(float x, int exp);

    ///
    double  log(double x);
    ///
    float   logf(float x);
    /// NetBSD has no logl. It is just alias log(double)
    real    logl(real x)
    {
        if (x<0) return real.nan;
        if (x==0) return -real.infinity;
        if (isnan(x) || isinf(x)) return x;
        real rs = 0;
        if (x>double.max)
        {
            immutable MAX = log(double.max);
            for (; x>double.max; x /= double.max)
                rs += MAX;
        }
        else if (x<double.min_normal)
        {
            immutable MIN = log(double.min_normal);
            for (; x<double.min_normal; x /= double.min_normal)
                rs += MIN;
        }
        rs += log(x);
        return rs;
    }

    ///
    double  log10(double x);
    ///
    float   log10f(float x);
    ///NetBSD has no log10l. It is just alias log(double)
    real    log10l(real x)
    {
        if (x<0) return real.nan;
        if (x==0) return -real.infinity;
        if (isnan(x) || isinf(x)) return x;

        real rs = 0;
        if (x>double.max)
        {
            immutable MAX = log10(double.max);
            for (; x>double.max; x /= double.max)
                rs += MAX;
        }
        else if (x<double.min_normal)
        {
            immutable MIN = log10(double.min_normal);
            for (; x<double.min_normal; x /= double.min_normal)
                rs += MIN;
        }
        rs += log10(x);
        return rs;
    }


    ///
    double  log1p(double x);
    ///
    float   log1pf(float x);
    ///
    extern(D) real log1pl(real x) { return log1p(cast(double) x); }

    private enum real ONE_LN2 = 1 / 0x1.62e42fefa39ef358p-1L;
    ///
    extern(D) double log2(double x) { return log(x) * ONE_LN2; }
    ///
    extern(D) float log2f(float x) { return logf(x) * ONE_LN2; }
    ///
    real    log2l(real x)  { return logl(x) * ONE_LN2; }

    ///
    double  logb(double x);
    ///
    float   logbf(float x);

    ///
    pure double  modf(double value, double* iptr);
    ///
    pure float   modff(float value, float* iptr);

    ///
    double  scalbn(double x, int n);
    ///
    float   scalbnf(float x, int n);

    ///
    double  scalbln(double x, c_long n);
    ///
    float   scalblnf(float x, c_long n);

    ///
    pure double  cbrt(double x);
    ///
    pure float   cbrtf(float x);
    ///
    pure real    cbrtl(real x);

    ///
    pure double  fabs(double x);
    ///
    pure float   fabsf(float x);

    ///
    double  hypot(double x, double y);
    ///
    float   hypotf(float x, float y);

    ///
    double  pow(double x, double y);
    ///
    float   powf(float x, float y);
    ///
    real    powl(real x, real y);

    ///
    double  sqrt(double x);
    ///
    float   sqrtf(float x);

    ///
    pure double  erf(double x);
    ///
    pure float   erff(float x);
    ///
    extern(D) pure real erfl(real x) { return erf(cast(double) x); }

    ///
    double  erfc(double x);
    ///
    float   erfcf(float x);
    ///
    real    erfcl(real x)  { return erfc(cast(double) x); }

    ///
    double  lgamma(double x);
    ///
    float   lgammaf(float x);
    ///
    real    lgammal(real x){ return lgamma(x); }

    ///
    double  tgamma(double x);
    ///
    float   tgammaf(float x);
    ///
    real    tgammal(real x){ return tgamma(cast(double) x); }

    ///
    pure double  ceil(double x);
    ///
    pure float   ceilf(float x);

    ///
    pure double  floor(double x);
    ///
    pure float   floorf(float x);

    ///
    pure double  nearbyint(double x);
    ///
    pure float   nearbyintf(float x);

    ///
    pure double  rint(double x);
    ///
    pure float   rintf(float x);

    ///
    c_long  lrint(double x);
    ///
    c_long  lrintf(float x);

    ///
    long    llrint(double x);
    ///
    long    llrintf(float x);
    ///
    extern(D) long llrintl(real x) { return cast(long)rintl(x); }

    ///
    pure double  round(double x);
    ///
    pure float   roundf(float x);

    ///
    c_long  lround(double x);
    ///
    c_long  lroundf(float x);

    ///
    long    llround(double x);
    ///
    long    llroundf(float x);

    ///
    pure double  trunc(double x);
    ///
    pure float   truncf(float x);

    ///
    double  fmod(double x, double y);
    ///
    float   fmodf(float x, float y);

    ///
    double  remainder(double x, double y);
    ///
    float   remainderf(float x, float y);

    ///
    double  remquo(double x, double y, int* quo);
    ///
    float   remquof(float x, float y, int* quo);

    ///
    pure double  copysign(double x, double y);
    ///
    pure float   copysignf(float x, float y);

    ///
    double  nextafter(double x, double y);
    ///
    float   nextafterf(float x, float y);

    ///
    double  nexttoward(double x, real y);
    ///
    float   nexttowardf(float x, real y);

    ///
    double  fdim(double x, double y);
    ///
    float   fdimf(float x, float y);

    ///
    pure double  fmax(double x, double y);
    ///
    pure float   fmaxf(float x, float y);

    ///
    pure double  fmin(double x, double y);
    ///
    pure float   fminf(float x, float y);

    ///
    pure double  fma(double x, double y, double z);
    ///
    pure float   fmaf(float x, float y, float z);
}
else version (OpenBSD)
{
    ///
    double acos(double x);
    ///
    double asin(double x);
    ///
    pure double atan(double x);
    ///
    double atan2(double, double);
    ///
    pure double cos(double x);
    ///
    pure double sin(double x);
    ///
    pure double tan(double x);
    ///
    double cosh(double x);
    ///
    double sinh(double x);
    ///
    pure double tanh(double x);
    ///
    double exp(double x);
    ///
    pure double frexp(double, int *exp);
    ///
    double ldexp(double, int exp);
    ///
    double log(double x);
    ///
    double log10(double x);
    ///
    pure double modf(double x, double *iptr);
    ///
    double pow(double x, double y);
    ///
    double sqrt(double x);
    ///
    pure double ceil(double x);
    ///
    pure double fabs(double x);
    ///
    pure double floor(double x);
    ///
    double fmod(double x, double);
    ///
    double acosh(double x);
    ///
    pure double asinh(double x);
    ///
    double atanh(double x);
    ///
    double exp2(double x);
    ///
    double expm1(double x);
    ///
    int ilogb(double x);
    ///
    double log1p(double x);
    ///
    double log2(double x);
    ///
    double logb(double x);
    ///
    double scalbn(double x, int n);
    ///
    double scalbln(double x, c_long n);
    ///
    pure double cbrt(double x);
    ///
    double hypot(double x, double y);
    ///
    pure double erf(double x);
    ///
    double erfc(double x);
    ///
    double lgamma(double x);
    ///
    double tgamma(double x);
    ///
    pure double nearbyint(double x);
    ///
    pure double rint(double x);
    ///
    c_long lrint(double x);
    ///
    long llrint(double x);
    ///
    pure double round(double x);
    ///
    c_long lround(double x);
    ///
    long  llround(double x);
    ///
    pure double trunc(double x);
    ///
    double remainder(double x , double y);
    ///
    double remquo(double x, double y, int * quo);
    ///
    pure double copysign(double x, double y);
    ///
    pure double nan(const char *);
    ///
    double nextafter(double x, double y);
    ///
    double nexttoward(double x, real y);
    ///
    double fdim(double x, double y);
    ///
    pure double fmax(double x, double y);
    ///
    pure double fmin(double x, double y);
    ///
    pure double fma(double x, double y, double z);
    ///
    double j0(double x);
    ///
    double j1(double x);
    ///
    double jn(int, double);
    ///
    double y0(double x);
    ///
    double y1(double x);
    ///
    double yn(int, double);
    ///
    double gamma(double x);
    ///
    double scalb(double x, double y);
    ///
    double drem(double x, double y);
    ///
    int finite(double x);
    ///
    double gamma_r(double x, int *);
    ///
    double lgamma_r(double x, int *);
    ///
    double significand(double x);

    ///
    float acosf(float x);
    ///
    float asinf(float x);
    ///
    pure float atanf(float x);
    ///
    float atan2f(float x, float y);
    ///
    pure float cosf(float x);
    ///
    pure float sinf(float x);
    ///
    pure float tanf(float x);
    ///
    float acoshf(float x);
    ///
    pure float asinhf(float x);
    ///
    float atanhf(float x);
    ///
    float coshf(float x);
    ///
    float sinhf(float x);
    ///
    pure float tanhf(float x);
    ///
    float expf(float x);
    ///
    float exp2f(float x);
    ///
    float expm1f(float x);
    ///
    pure float frexpf(float x, int *exp);
    ///
    int ilogbf(float x);
    ///
    float ldexpf(float x, int exp);
    ///
    float logf(float x);
    ///
    float log10f(float x);
    ///
    float log1pf(float x);
    ///
    float log2f(float x);
    ///
    float logbf(float x);
    ///
    pure float modff(float x, float *iptr);
    ///
    float scalbnf(float x, int y);
    ///
    float scalblnf(float x, c_long y);
    ///
    pure float cbrtf(float x);
    ///
    pure float fabsf(float x);
    ///
    float hypotf(float x, float y);
    ///
    float powf(float x, float y);
    ///
    float sqrtf(float x);
    ///
    pure float erff(float x);
    ///
    float erfcf(float x);
    ///
    float lgammaf(float x);
    ///
    float tgammaf(float x);
    ///
    pure float ceilf(float x);
    ///
    pure float floorf(float x);
    ///
    pure float nearbyintf(float x);
    ///
    pure float rintf(float x);
    ///
    c_long lrintf(float x);
    ///
    long llrintf(float x);
    ///
    pure float roundf(float x);
    ///
    c_long lroundf(float x);
    ///
    long llroundf(float x);
    ///
    pure float truncf(float x);
    ///
    pure float fmodf(float x, float y);
    ///
    float remainderf(float x, float y);
    ///
    float remquof(float x, float y, int *iptr);
    ///
    pure float copysignf(float x, float y);
    ///
    pure float nanf(const char *);
    ///
    float nextafterf(float x, float y);
    ///
    float nexttowardf(float x, real y);
    ///
    float fdimf(float x, float y);
    ///
    pure float fmaxf(float x, float y);
    ///
    pure float fminf(float x, float y);
    ///
    pure float fmaf(float x, float y, float z);
    ///
    float j0f(float x);
    ///
    float j1f(float x);
    ///
    float jnf(int, float);
    ///
    float scalbf(float x, float);
    ///
    float y0f(float x);
    ///
    float y1f(float x);
    ///
    float ynf(int, float);
    ///
    float gammaf(float x);
    ///
    float dremf(float x, float);
    ///
    pure int finitef(float x);
    ///
    pure int isinff(float x);
    ///
    pure int isnanf(float x);
    ///
    float gammaf_r(float x, int *);
    ///
    float lgammaf_r(float x, int *);
    ///
    float significandf(float x);
    ///

    ///
    pure real acosl(real x);
    ///
    pure real asinl(real x);
    ///
    pure real atanl(real x);
    ///
    real atan2l(real y, real x);
    ///
    pure real cosl(real x);
    ///
    pure real sinl(real x);
    ///
    pure real tanl(real x);
    ///
    real acoshl(real x);
    ///
    pure real asinhl(real x);
    ///
    real atanhl(real x);
    ///
    real coshl(real x);
    ///
    real sinhl(real x);
    ///
    pure real tanhl(real x);
    ///
    real expl(real x);
    ///
    real exp2l(real x);
    ///
    real expm1l(real x);
    ///
    pure real frexpl(real x, int *exp);
    ///
    int ilogbl(real x);
    ///
    real ldexpl(real x, int exp);
    ///
    real logl(real x);
    ///
    real log10l(real x);
    ///
    real log1pl(real x);
    ///
    real log2l(real x);
    ///
    real logbl(real x);
    ///
    pure real modfl(real x, real *iptr);
    ///
    real scalbnl(real x, int y);
    ///
    real scalblnl(real x, c_long y);
    ///
    pure real cbrtl(real x);
    ///
    pure real fabsl(real x);
    ///
    real hypotl(real x, real y);
    ///
    real powl(real x, real y);
    ///
    real sqrtl(real x);
    ///
    pure real erfl(real x);
    ///
    real erfcl(real x);
    ///
    real lgammal(real x);
    ///
    real tgammal(real x);
    ///
    pure real ceill(real x);
    ///
    pure real floorl(real x);
    ///
    pure real nearbyintl(real x);
    ///
    pure real rintl(real x);
    ///
    c_long lrintl(real x);
    ///
    long llrintl(real x);
    ///
    pure real roundl(real x);
    ///
    c_long lroundl(real x);
    ///
    long llroundl(real x);
    ///
    pure real truncl(real x);
    ///
    pure real fmodl(real x, real);
    ///
    pure real remainderl(real x, real);
    ///
    pure real remquol(real x, real y, int *iptr);
    ///
    pure real copysignl(real x, real y);
    ///
    pure real nanl(const char *);
    ///
    real nextafterl(real x, real y);
    ///
    real nexttowardl(real x, real y);
    ///
    real fdiml(real x, real y);
    ///
    pure real fmaxl(real x, real y);
    ///
    pure real fminl(real x, real y);
    ///
    pure real fmal(real x, real, real);
}
else version (DragonFlyBSD)
{
    /* double */
    double acos(double x);
    double asin(double x);
    pure double atan(double x);
    double atan2(double, double);
    pure double cos(double x);
    pure double sin(double x);
    pure double tan(double x);

    double cosh(double x);
    double sinh(double x);
    pure double tanh(double x);

    double exp(double x);
    pure double frexp(double, int *exp);
    double ldexp(double, int exp);
    double log(double x);
    double log10(double x);
    pure double modf(double x, double *iptr);

    double pow(double x, double y);
    double sqrt(double x);

    pure double ceil(double x);
    pure double fabs(double x);
    pure double floor(double x);
    double fmod(double x, double);

    double acosh(double x);
    pure double asinh(double x);
    double atanh(double x);

    double exp2(double x);
    double expm1(double x);
    int ilogb(double x);
    double log1p(double x);
    double log2(double x);
    double logb(double x);
    double scalbn(double x, int n);
    double scalbln(double x, c_long n);

    pure double cbrt(double x);
    double hypot(double x, double y);

    pure double erf(double x);
    double erfc(double x);
    double lgamma(double x);
    double tgamma(double x);

    pure double nearbyint(double x);
    pure double rint(double x);
    c_long lrint(double x);
    long llrint(double x);
    pure double round(double x);
    c_long lround(double x);
    long  llround(double x);
    pure double trunc(double x);

    double remainder(double x , double y);
    double remquo(double x, double y, int * quo);

    pure double copysign(double x, double y);
    pure double nan(const char *);
    double nextafter(double x, double y);
    double nexttoward(double x, real y);

    double fdim(double x, double y);
    pure double fmax(double x, double y);
    pure double fmin(double x, double y);

    pure double fma(double x, double y, double z);

    double j0(double x);
    double j1(double x);
    double jn(int, double);
    double y0(double x);
    double y1(double x);
    double yn(int, double);

    double gamma(double x);
    double scalb(double x, double y);

    double drem(double x, double y);
    int finite(double x);
    double gamma_r(double x, int *);
    double lgamma_r(double x, int *);

    double significand(double x);

    /* float */
    float acosf(float x);
    float asinf(float x);
    pure float atanf(float x);
    float atan2f(float x, float y);
    pure float cosf(float x);
    pure float sinf(float x);
    pure float tanf(float x);

    float acoshf(float x);
    pure float asinhf(float x);
    float atanhf(float x);
    float coshf(float x);
    float sinhf(float x);
    pure float tanhf(float x);

    float expf(float x);
    float exp2f(float x);
    float expm1f(float x);
    pure float frexpf(float x, int *exp);
    int ilogbf(float x);
    float ldexpf(float x, int exp);
    float logf(float x);
    float log10f(float x);
    float log1pf(float x);
    float log2f(float x);
    float logbf(float x);
    pure float modff(float x, float *iptr);
    float scalbnf(float x, int y);
    float scalblnf(float x, c_long y);

    pure float cbrtf(float x);
    pure float fabsf(float x);
    float hypotf(float x, float y);
    float powf(float x, float y);
    float sqrtf(float x);

    pure float erff(float x);
    float erfcf(float x);
    float lgammaf(float x);
    float tgammaf(float x);

    pure float ceilf(float x);
    pure float floorf(float x);
    pure float nearbyintf(float x);
    pure float rintf(float x);
    c_long lrintf(float x);
    long llrintf(float x);
    pure float roundf(float x);
    c_long lroundf(float x);
    long llroundf(float x);
    pure float truncf(float x);

    pure float fmodf(float x, float y);
    float remainderf(float x, float y);
    float remquof(float x, float y, int *iptr);

    pure float copysignf(float x, float y);
    pure float nanf(const char *);
    float nextafterf(float x, float y);
    float nexttowardf(float x, real y);

    float fdimf(float x, float y);
    pure float fmaxf(float x, float y);
    pure float fminf(float x, float y);

    pure float fmaf(float x, float y, float z);

    float j0f(float x);
    float j1f(float x);
    float jnf(int, float);
    float scalbf(float x, float);
    float y0f(float x);
    float y1f(float x);
    float ynf(int, float);
    float gammaf(float x);
    float dremf(float x, float);
    pure int finitef(float x);
    pure int isinff(float x);
    pure int isnanf(float x);

    float gammaf_r(float x, int *);
    float lgammaf_r(float x, int *);
    float significandf(float x);

    /* real */
    pure real acosl(real x);
    pure real asinl(real x);
    pure real atanl(real x);
    real atan2l(real y, real x);
    pure real cosl(real x);
    pure real sinl(real x);
    pure real tanl(real x);

    real acoshl(real x);
    pure real asinhl(real x);
    real atanhl(real x);
    real coshl(real x);
    real sinhl(real x);
    pure real tanhl(real x);

    real expl(real x);
    real exp2l(real x);
    real expm1l(real x);
    pure real frexpl(real x, int *exp);
    int ilogbl(real x);
    real ldexpl(real x, int exp);
    real logl(real x);
    real log10l(real x);
    real log1pl(real x);
    real log2l(real x);
    real logbl(real x);
    pure real modfl(real x, real *iptr);
    real scalbnl(real x, int y);
    real scalblnl(real x, c_long y);

    pure real cbrtl(real x);
    pure real fabsl(real x);
    real hypotl(real x, real y);
    real powl(real x, real y);
    real sqrtl(real x);

    pure real erfl(real x);
    real erfcl(real x);
    real lgammal(real x);
    real tgammal(real x);

    pure real ceill(real x);
    pure real floorl(real x);
    pure real nearbyintl(real x);
    pure real rintl(real x);
    c_long lrintl(real x);
    long llrintl(real x);
    pure real roundl(real x);
    c_long lroundl(real x);
    long llroundl(real x);
    pure real truncl(real x);

    pure real fmodl(real x, real);
    pure real remainderl(real x, real);
    pure real remquol(real x, real y, int *iptr);

    pure real copysignl(real x, real y);
    pure real nanl(const char *);
    real nextafterl(real x, real y);
    real nexttowardl(real x, real y);

    real fdiml(real x, real y);
    pure real fmaxl(real x, real y);
    pure real fminl(real x, real y);

    pure real fmal(real x, real, real);
}
else version (CRuntime_Bionic)
{
    ///
    double  acos(double x);
    ///
    float   acosf(float x);
    /// Added since Lollipop
    real    acosl(real x);

    ///
    double  asin(double x);
    ///
    float   asinf(float x);
    /// Added since Lollipop
    real    asinl(real x);

    ///
    pure double  atan(double x);
    ///
    pure float   atanf(float x);
    /// Added since Lollipop
    pure real    atanl(real x);

    ///
    double  atan2(double y, double x);
    ///
    float   atan2f(float y, float x);
    /// Added since Lollipop
    real    atan2l(real y, real x);

    ///
    pure double  cos(double x);
    ///
    pure float   cosf(float x);
    ///
    pure real    cosl(real x);

    ///
    pure double  sin(double x);
    ///
    pure float   sinf(float x);
    /// Added since Lollipop
    pure real    sinl(real x);

    ///
    pure double  tan(double x);
    ///
    pure float   tanf(float x);
    /// Added since Lollipop
    pure real    tanl(real x);

    ///
    double  acosh(double x);
    ///
    float   acoshf(float x);
    /// Added since Lollipop
    real    acoshl(real x);

    ///
    pure double  asinh(double x);
    ///
    pure float   asinhf(float x);
    /// Added since Lollipop
    pure real    asinhl(real x);

    ///
    double  atanh(double x);
    ///
    float   atanhf(float x);
    /// Added since Lollipop
    real    atanhl(real x);

    ///
    double  cosh(double x);
    ///
    float   coshf(float x);
    /// Added since Lollipop
    real    coshl(real x);

    ///
    double  sinh(double x);
    ///
    float   sinhf(float x);
    /// Added since Lollipop
    real    sinhl(real x);

    ///
    pure double  tanh(double x);
    ///
    pure float   tanhf(float x);
    /// Added since Lollipop
    pure real    tanhl(real x);

    ///
    double  exp(double x);
    ///
    float   expf(float x);
    ///
    real    expl(real x);

    ///
    double  exp2(double x);
    ///
    float   exp2f(float x);
    /// Added since Lollipop
    real    exp2l(real x);

    ///
    double  expm1(double x);
    ///
    float   expm1f(float x);
    /// Added since Lollipop
    real    expm1l(real x);

    ///
    pure double  frexp(double value, int* exp);
    ///
    pure float   frexpf(float value, int* exp);
    /// Added since Lollipop
    pure real    frexpl(real value, int* exp);

    ///
    int     ilogb(double x);
    ///
    int     ilogbf(float x);
    ///
    int     ilogbl(real x);

    ///
    double  ldexp(double x, int exp);
    ///
    float   ldexpf(float x, int exp);
    ///
    real    ldexpl(real x, int exp);

    ///
    double  log(double x);
    ///
    float   logf(float x);
    /// Added since Lollipop
    real    logl(real x);

    ///
    double  log10(double x);
    ///
    float   log10f(float x);
    /// Added since Lollipop
    real    log10l(real x);

    ///
    double  log1p(double x);
    ///
    float   log1pf(float x);
    /// Added since Lollipop
    real    log1pl(real x);

    ///
    double  log2(double x);
    ///
    float   log2f(float x);
    ///
    real    log2l(real x);

    ///
    double  logb(double x);
    ///
    float   logbf(float x);
    ///
    real    logbl(real x);

    ///
    pure double  modf(double value, double* iptr);
    ///
    pure float   modff(float value, float* iptr);
    /// Added since Lollipop
    pure real    modfl(real value, real *iptr);

    ///
    double  scalbn(double x, int n);
    ///
    float   scalbnf(float x, int n);
    ///
    real    scalbnl(real x, int n);

    ///
    double  scalbln(double x, c_long n);
    ///
    float   scalblnf(float x, c_long n);
    ///
    real    scalblnl(real x, c_long n);

    ///
    pure double  cbrt(double x);
    ///
    pure float   cbrtf(float x);
    /// Added since Lollipop
    pure real    cbrtl(real x);

    ///
    pure double  fabs(double x);
    ///
    pure float   fabsf(float x);
    ///
    pure real    fabsl(real x);

    ///
    double  hypot(double x, double y);
    ///
    float   hypotf(float x, float y);
    /// Added since Lollipop
    real    hypotl(real x, real y);

    ///
    double  pow(double x, double y);
    ///
    float   powf(float x, float y);
    /// Added since Lollipop
    real    powl(real x, real y);

    ///
    double  sqrt(double x);
    ///
    float   sqrtf(float x);
    /// Added since Lollipop
    real    sqrtl(real x);

    ///
    pure double  erf(double x);
    ///
    pure float   erff(float x);
    /// Added since Lollipop
    pure real    erfl(real x);

    ///
    double  erfc(double x);
    ///
    float   erfcf(float x);
    /// Added since Lollipop
    real    erfcl(real x);

    ///
    double  lgamma(double x);
    ///
    float   lgammaf(float x);
    /// Added since Lollipop
    real    lgammal(real x);

    ///
    double  tgamma(double x);
    ///
    float   tgammaf(float x);
    /// Added since Lollipop
    real    tgammal(real x);

    ///
    pure double  ceil(double x);
    ///
    pure float   ceilf(float x);
    ///
    pure real    ceill(real x);

    ///
    pure double  floor(double x);
    ///
    pure float   floorf(float x);
    ///
    pure real    floorl(real x);

    ///
    pure double  nearbyint(double x);
    ///
    pure float   nearbyintf(float x);
    /// Added since Lollipop
    pure real    nearbyintl(real x);

    ///
    pure double  rint(double x);
    ///
    pure float   rintf(float x);
    /// Added since Lollipop
    pure real    rintl(real x);

    ///
    c_long  lrint(double x);
    ///
    c_long  lrintf(float x);
    /// Added since Lollipop
    c_long  lrintl(real x);

    ///
    long    llrint(double x);
    ///
    long    llrintf(float x);
    /// Added since Lollipop
    long    llrintl(real x);

    ///
    pure double  round(double x);
    ///
    pure float   roundf(float x);
    ///
    pure real    roundl(real x);

    ///
    c_long  lround(double x);
    ///
    c_long  lroundf(float x);
    ///
    c_long  lroundl(real x);

    ///
    long    llround(double x);
    ///
    long    llroundf(float x);
    ///
    long    llroundl(real x);

    ///
    pure double  trunc(double x);
    ///
    pure float   truncf(float x);
    ///
    pure real    truncl(real x);

    ///
    double  fmod(double x, double y);
    ///
    float   fmodf(float x, float y);
    /// Added since Lollipop
    real    fmodl(real x, real y);

    ///
    double  remainder(double x, double y);
    ///
    float   remainderf(float x, float y);
    /// Added since Lollipop
    real    remainderl(real x, real y);

    ///
    double  remquo(double x, double y, int* quo);
    ///
    float   remquof(float x, float y, int* quo);
    /// Added since Lollipop
    real    remquol(real x, real y, int* quo);

    ///
    pure double  copysign(double x, double y);
    ///
    pure float   copysignf(float x, float y);
    ///
    pure real    copysignl(real x, real y);

    ///
    pure double  nan(char* tagp);
    ///
    pure float   nanf(char* tagp);
    ///
    pure real    nanl(char* tagp);

    ///
    double  nextafter(double x, double y);
    ///
    float   nextafterf(float x, float y);
    /// Added since Lollipop
    real    nextafterl(real x, real y);

    ///
    double  nexttoward(double x, real y);
    ///
    float   nexttowardf(float x, real y);
    ///
    real    nexttowardl(real x, real y);

    ///
    double  fdim(double x, double y);
    ///
    float   fdimf(float x, float y);
    ///
    real    fdiml(real x, real y);

    ///
    pure double  fmax(double x, double y);
    ///
    pure float   fmaxf(float x, float y);
    ///
    pure real    fmaxl(real x, real y);

    ///
    pure double  fmin(double x, double y);
    ///
    pure float   fminf(float x, float y);
    ///
    pure real    fminl(real x, real y);

    ///
    pure double  fma(double x, double y, double z);
    ///
    pure float   fmaf(float x, float y, float z);
    /// Added since Lollipop
    pure real    fmal(real x, real y, real z);
}
else version (CRuntime_UClibc)
{
    // uClibc wraps 'long double' to double, so we do the same for 'real'

    ///
    double  acos(double x);
    ///
    float   acosf(float x);
    ///
    extern(D) real acosl(real x) { return acos(cast(double) x); }

    ///
    double  asin(double x);
    ///
    float   asinf(float x);
    ///
    extern(D) real asinl(real x) { return asin(cast(double) x); }

    ///
    pure double  atan(double x);
    ///
    pure float   atanf(float x);
    ///
    extern(D) pure real atanl(real x) { return atan(cast(double) x); }

    ///
    double  atan2(double y, double x);
    ///
    float   atan2f(float y, float x);
    ///
    extern(D) real atan2l(real y, real x) { return atan2(cast(double) y, cast(double) x); }

    ///
    pure double  cos(double x);
    ///
    pure float   cosf(float x);
    ///
    extern(D) pure real cosl(real x) { return cos(cast(double) x); }

    ///
    pure double  sin(double x);
    ///
    pure float   sinf(float x);
    ///
    extern(D) pure real sinl(real x) { return sin(cast(double) x); }

    ///
    pure double  tan(double x);
    ///
    pure float   tanf(float x);
    ///
    extern(D) pure real tanl(real x) { return tan(cast(double) x); }

    ///
    double  acosh(double x);
    ///
    float   acoshf(float x);
    ///
    extern(D) real acoshl(real x) { return acosh(cast(double) x); }

    ///
    pure double  asinh(double x);
    ///
    pure float   asinhf(float x);
    ///
    extern(D) pure real asinhl(real x) { return asinh(cast(double) x); }

    ///
    double  atanh(double x);
    ///
    float   atanhf(float x);
    ///
    extern(D) real atanhl(real x) { return atanh(cast(double) x); }

    ///
    double  cosh(double x);
    ///
    float   coshf(float x);
    ///
    extern(D) real coshl(real x) { return cosh(cast(double) x); }

    ///
    double  sinh(double x);
    ///
    float   sinhf(float x);
    ///
    extern(D) real sinhl(real x) { return sinh(cast(double) x); }

    ///
    double  tanh(double x);
    ///
    float   tanhf(float x);
    ///
    extern(D) real tanhl(real x) { return tanh(cast(double) x); }

    ///
    double  exp(double x);
    ///
    float   expf(float x);
    ///
    extern(D) real expl(real x) { return exp(cast(double) x); }

    ///
    double  exp2(double x);
    ///
    float   exp2f(float x);
    ///
    extern(D) real exp2l(real x) { return exp2(cast(double) x); }

    ///
    double  expm1(double x);
    ///
    float   expm1f(float x);
    ///
    extern(D) real expm1l(real x) { return expm1(cast(double) x); }

    ///
    pure double  frexp(double value, int* exp);
    ///
    pure float   frexpf(float value, int* exp);
    ///
    extern(D) pure real frexpl(real value, int* exp) { return frexp(cast(double) value, exp); }

    ///
    int     ilogb(double x);
    ///
    int     ilogbf(float x);
    ///
    extern(D) int ilogbl(real x) { return ilogb(cast(double) x); }

    ///
    double  ldexp(double x, int exp);
    ///
    float   ldexpf(float x, int exp);
    ///
    extern(D) real ldexpl(real x, int exp) { return ldexp(cast(double) x, exp); }

    ///
    double  log(double x);
    ///
    float   logf(float x);
    ///
    extern(D) real logl(real x) { return log(cast(double) x); }

    ///
    double  log10(double x);
    ///
    float   log10f(float x);
    ///
    extern(D) real log10l(real x) { return log10(cast(double) x); }

    ///
    double  log1p(double x);
    ///
    float   log1pf(float x);
    ///
    extern(D) real log1pl(real x) { return log1p(cast(double) x); }

    ///
    double  log2(double x);
    ///
    float   log2f(float x);
    ///
    extern(D) real log2l(real x) { return log2(cast(double) x); }

    ///
    double  logb(double x);
    ///
    float   logbf(float x);
    ///
    extern(D) real logbl(real x) { return logb(cast(double) x); }

    ///
    pure double  modf(double value, double* iptr);
    ///
    pure float   modff(float value, float* iptr);
    ///
    extern(D) pure real modfl(real value, real *iptr)
    {
        static if (double.sizeof == real.sizeof)
             return modf(cast(double) value, cast(double*) iptr);
        else
        {
            double i;
            double r = modf(cast(double) value, &i);
            *iptr = i;
            return r;
        }
    }

    ///
    double  scalbn(double x, int n);
    ///
    float   scalbnf(float x, int n);
    ///
    extern(D) real scalbnl(real x, int n) { return scalbln(cast(double) x, n); }

    ///
    double  scalbln(double x, c_long n);
    ///
    float   scalblnf(float x, c_long n);
    ///
    extern(D) real scalblnl(real x, c_long n) { return scalbln(cast(double) x, n); }

    ///
    pure double  cbrt(double x);
    ///
    pure float   cbrtf(float x);
    ///
    extern(D) pure real cbrtl(real x) { return cbrt(cast(double) x); }

    ///
    pure double  fabs(double x);
    ///
    pure float   fabsf(float x);
    ///
    extern(D) pure real fabsl(real x) { return fabs(cast(double) x); }

    ///
    double  hypot(double x, double y);
    ///
    float   hypotf(float x, float y);
    ///
    extern(D) real hypotl(real x, real y) { return hypot(cast(double) x, cast(double) y); }

    ///
    double  pow(double x, double y);
    ///
    float   powf(float x, float y);
    ///
    extern(D) real powl(real x, real y) { return pow(cast(double) x, cast(double) y); }

    ///
    double  sqrt(double x);
    ///
    float   sqrtf(float x);
    ///
    extern(D) real sqrtl(real x) { return sqrt(cast(double) x); }

    ///
    pure double  erf(double x);
    ///
    pure float   erff(float x);
    ///
    extern(D) pure real erfl(real x) { return erf(cast(double) x); }

    ///
    double  erfc(double x);
    ///
    float   erfcf(float x);
    ///
    extern(D) real erfcl(real x) { return erfc(cast(double) x); }

    ///
    double  lgamma(double x);
    ///
    float   lgammaf(float x);
    ///
    extern(D) real lgammal(real x) { return lgamma(cast(double) x); }

    ///
    double  tgamma(double x);
    ///
    float   tgammaf(float x);
    ///
    extern(D) real tgammal(real x) { return tgamma(cast(double) x); }

    ///
    pure double  ceil(double x);
    ///
    pure float   ceilf(float x);
    ///
    extern(D) pure real ceill(real x) { return ceil(cast(double) x); }

    ///
    pure double  floor(double x);
    ///
    pure float   floorf(float x);
    ///
    extern(D) pure real floorl(real x) { return floor(cast(double) x); }

    ///
    pure double  nearbyint(double x);
    ///
    pure float   nearbyintf(float x);
    ///
    extern(D) pure real nearbyintl(real x) { return nearbyint(cast(double) x); }

    ///
    pure double  rint(double x);
    ///
    pure float   rintf(float x);
    ///
    extern(D) pure real rintl(real x) { return rint(cast(double) x); }

    ///
    c_long  lrint(double x);
    ///
    c_long  lrintf(float x);
    ///
    extern(D) c_long lrintl(real x) { return lrint(cast(double) x); }

    ///
    long    llrint(double x);
    ///
    long    llrintf(float x);
    ///
    extern(D) long llrintl(real x) { return llrint(cast(double) x); }

    ///
    pure double  round(double x);
    ///
    pure float   roundf(float x);
    ///
    extern(D) pure real roundl(real x) { return round(cast(double) x); }

    ///
    c_long  lround(double x);
    ///
    c_long  lroundf(float x);
    ///
    extern(D) c_long lroundl(real x) { return lround(cast(double) x); }

    ///
    long    llround(double x);
    ///
    long    llroundf(float x);
    ///
    extern(D) long llroundl(real x) { return llround(cast(double) x); }

    ///
    pure double  trunc(double x);
    ///
    pure float   truncf(float x);
    ///
    extern(D) pure real truncl(real x) { return trunc(cast(double) x); }

    ///
    double  fmod(double x, double y);
    ///
    float   fmodf(float x, float y);
    ///
    extern(D) real fmodl(real x, real y) { return fmod(cast(double) x, cast(double) y); }

    ///
    double  remainder(double x, double y);
    ///
    float   remainderf(float x, float y);
    ///
    extern(D) real remainderl(real x, real y) { return remainder(cast(double) x, cast(double) y); }

    ///
    double  remquo(double x, double y, int* quo);
    ///
    float   remquof(float x, float y, int* quo);
    ///
    extern(D) real remquol(real x, real y, int* quo) { return remquo(cast(double) x, cast(double) y, quo); }

    ///
    pure double  copysign(double x, double y);
    ///
    pure float   copysignf(float x, float y);
    ///
    extern(D) pure real copysignl(real x, real y) { return copysign(cast(double) x, cast(double) y); }

    ///
    pure double  nan(char* tagp);
    ///
    pure float   nanf(char* tagp);
    ///
    extern(D) pure real nanl(char* tagp) { return nan(tagp); }

    ///
    double  nextafter(double x, double y);
    ///
    float   nextafterf(float x, float y);
    ///
    extern(D) real nextafterl(real x, real y) { return nextafter(cast(double) x, cast(double) y); }

    ///
    double  nexttoward(double x, real y);
    ///
    float   nexttowardf(float x, real y);
    ///
    extern(D) real nexttowardl(real x, real y) { return nexttoward(cast(double) x, cast(double) y); }

    ///
    double  fdim(double x, double y);
    ///
    float   fdimf(float x, float y);
    ///
    extern(D) real fdiml(real x, real y) { return fdim(cast(double) x, cast(double) y); }

    ///
    pure double  fmax(double x, double y);
    ///
    pure float   fmaxf(float x, float y);
    ///
    extern(D) pure real fmaxl(real x, real y) { return fmax(cast(double) x, cast(double) y); }

    ///
    pure double  fmin(double x, double y);
    ///
    pure float   fminf(float x, float y);
    ///
    extern(D) pure real fminl(real x, real y) { return fmin(cast(double) x, cast(double) y); }

    ///
    pure double  fma(double x, double y, double z);
    ///
    pure float   fmaf(float x, float y, float z);
    ///
    extern(D) pure real fmal(real x, real y, real z) { return fma(cast(double) x, cast(double) y, cast(double) z); }
}
else
{
    ///
    double  acos(double x);
    ///
    float   acosf(float x);
    ///
    real    acosl(real x);

    ///
    double  asin(double x);
    ///
    float   asinf(float x);
    ///
    real    asinl(real x);

    ///
    pure double  atan(double x);
    ///
    pure float   atanf(float x);
    ///
    pure real    atanl(real x);

    ///
    double  atan2(double y, double x);
    ///
    float   atan2f(float y, float x);
    ///
    real    atan2l(real y, real x);

    ///
    pure double  cos(double x);
    ///
    pure float   cosf(float x);
    ///
    pure real    cosl(real x);

    ///
    pure double  sin(double x);
    ///
    pure float   sinf(float x);
    ///
    pure real    sinl(real x);

    ///
    pure double  tan(double x);
    ///
    pure float   tanf(float x);
    ///
    pure real    tanl(real x);

    ///
    double  acosh(double x);
    ///
    float   acoshf(float x);
    ///
    real    acoshl(real x);

    ///
    pure double  asinh(double x);
    ///
    pure float   asinhf(float x);
    ///
    pure real    asinhl(real x);

    ///
    double  atanh(double x);
    ///
    float   atanhf(float x);
    ///
    real    atanhl(real x);

    ///
    double  cosh(double x);
    ///
    float   coshf(float x);
    ///
    real    coshl(real x);

    ///
    double  sinh(double x);
    ///
    float   sinhf(float x);
    ///
    real    sinhl(real x);

    ///
    pure double  tanh(double x);
    ///
    pure float   tanhf(float x);
    ///
    pure real    tanhl(real x);

    ///
    double  exp(double x);
    ///
    float   expf(float x);
    ///
    real    expl(real x);

    ///
    double  exp2(double x);
    ///
    float   exp2f(float x);
    ///
    real    exp2l(real x);

    ///
    double  expm1(double x);
    ///
    float   expm1f(float x);
    ///
    real    expm1l(real x);

    ///
    pure double  frexp(double value, int* exp);
    ///
    pure float   frexpf(float value, int* exp);
    ///
    pure real    frexpl(real value, int* exp);

    ///
    int     ilogb(double x);
    ///
    int     ilogbf(float x);
    ///
    int     ilogbl(real x);

    ///
    double  ldexp(double x, int exp);
    ///
    float   ldexpf(float x, int exp);
    ///
    real    ldexpl(real x, int exp);

    ///
    double  log(double x);
    ///
    float   logf(float x);
    ///
    real    logl(real x);

    ///
    double  log10(double x);
    ///
    float   log10f(float x);
    ///
    real    log10l(real x);

    ///
    double  log1p(double x);
    ///
    float   log1pf(float x);
    ///
    real    log1pl(real x);

    ///
    double  log2(double x);
    ///
    float   log2f(float x);
    ///
    real    log2l(real x);

    ///
    double  logb(double x);
    ///
    float   logbf(float x);
    ///
    real    logbl(real x);

    ///
    pure double  modf(double value, double* iptr);
    ///
    pure float   modff(float value, float* iptr);
    ///
    pure real    modfl(real value, real *iptr);

    ///
    double  scalbn(double x, int n);
    ///
    float   scalbnf(float x, int n);
    ///
    real    scalbnl(real x, int n);

    ///
    double  scalbln(double x, c_long n);
    ///
    float   scalblnf(float x, c_long n);
    ///
    real    scalblnl(real x, c_long n);

    ///
    pure double  cbrt(double x);
    ///
    pure float   cbrtf(float x);
    ///
    pure real    cbrtl(real x);

    ///
    pure double  fabs(double x);
    version (CRuntime_Microsoft)
    {
    }
    else
    {
        ///
        pure float   fabsf(float x);
        ///
        pure real    fabsl(real x);
    }

    ///
    double  hypot(double x, double y);
    ///
    float   hypotf(float x, float y);
    ///
    real    hypotl(real x, real y);

    ///
    double  pow(double x, double y);
    ///
    float   powf(float x, float y);
    ///
    real    powl(real x, real y);

    ///
    double  sqrt(double x);
    ///
    float   sqrtf(float x);
    ///
    real    sqrtl(real x);

    ///
    pure double  erf(double x);
    ///
    pure float   erff(float x);
    ///
    pure real    erfl(real x);

    ///
    double  erfc(double x);
    ///
    float   erfcf(float x);
    ///
    real    erfcl(real x);

    ///
    double  lgamma(double x);
    ///
    float   lgammaf(float x);
    ///
    real    lgammal(real x);

    ///
    double  tgamma(double x);
    ///
    float   tgammaf(float x);
    ///
    real    tgammal(real x);

    ///
    pure double  ceil(double x);
    ///
    pure float   ceilf(float x);
    ///
    pure real    ceill(real x);

    ///
    pure double  floor(double x);
    ///
    pure float   floorf(float x);
    ///
    pure real    floorl(real x);

    ///
    pure double  nearbyint(double x);
    ///
    pure float   nearbyintf(float x);
    ///
    pure real    nearbyintl(real x);

    ///
    pure double  rint(double x);
    ///
    pure float   rintf(float x);
    ///
    pure real    rintl(real x);

    ///
    c_long  lrint(double x);
    ///
    c_long  lrintf(float x);
    ///
    c_long  lrintl(real x);

    ///
    long    llrint(double x);
    ///
    long    llrintf(float x);
    ///
    long    llrintl(real x);

    ///
    pure double  round(double x);
    ///
    pure float   roundf(float x);
    ///
    pure real    roundl(real x);

    ///
    c_long  lround(double x);
    ///
    c_long  lroundf(float x);
    ///
    c_long  lroundl(real x);

    ///
    long    llround(double x);
    ///
    long    llroundf(float x);
    ///
    long    llroundl(real x);

    ///
    pure double  trunc(double x);
    ///
    pure float   truncf(float x);
    ///
    pure real    truncl(real x);

    ///
    double  fmod(double x, double y);
    ///
    float   fmodf(float x, float y);
    ///
    real    fmodl(real x, real y);

    ///
    double  remainder(double x, double y);
    ///
    float   remainderf(float x, float y);
    ///
    real    remainderl(real x, real y);

    ///
    double  remquo(double x, double y, int* quo);
    ///
    float   remquof(float x, float y, int* quo);
    ///
    real    remquol(real x, real y, int* quo);

    ///
    pure double  copysign(double x, double y);
    ///
    pure float   copysignf(float x, float y);
    ///
    pure real    copysignl(real x, real y);

    ///
    pure double  nan(char* tagp);
    ///
    pure float   nanf(char* tagp);
    ///
    pure real    nanl(char* tagp);

    ///
    double  nextafter(double x, double y);
    ///
    float   nextafterf(float x, float y);
    ///
    real    nextafterl(real x, real y);

    ///
    double  nexttoward(double x, real y);
    ///
    float   nexttowardf(float x, real y);
    ///
    real    nexttowardl(real x, real y);

    ///
    double  fdim(double x, double y);
    ///
    float   fdimf(float x, float y);
    ///
    real    fdiml(real x, real y);

    ///
    pure double  fmax(double x, double y);
    ///
    pure float   fmaxf(float x, float y);
    ///
    pure real    fmaxl(real x, real y);

    ///
    pure double  fmin(double x, double y);
    ///
    pure float   fminf(float x, float y);
    ///
    pure real    fminl(real x, real y);

    ///
    pure double  fma(double x, double y, double z);
    ///
    pure float   fmaf(float x, float y, float z);
    ///
    pure real    fmal(real x, real y, real z);
}
