/* This D file is implicitly imported by all ImportC source files.
 * It provides definitions for C compiler builtin functions and declarations.
 * The purpose is to make it unnecessary to hardwire them into the compiler.
 * As the leading double underscore suggests, this is for internal use only.
 *
 * Copyright: Copyright Digital Mars 2022
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright
 * Source: $(DRUNTIMESRC __builtins.d)
 */


module __builtins;

/* gcc relies on internal __builtin_xxxx functions and templates to
 * accomplish <stdarg.h>. D does the same thing with templates in core.stdc.stdarg.
 * Here, we redirect the gcc builtin declarations to the equivalent
 * ones in core.stdc.stdarg, thereby avoiding having to hardwire them
 * into the D compiler.
 */

alias va_list = imported!"core.stdc.stdarg".va_list;

version (Posix)
{
    version (X86_64)
        alias __va_list_tag = imported!"core.stdc.stdarg".__va_list_tag;
}

alias __builtin_va_start = imported!"core.stdc.stdarg".va_start;

alias __builtin_va_end = imported!"core.stdc.stdarg".va_end;

alias __builtin_va_copy = imported!"core.stdc.stdarg".va_copy;

/* dmd's ImportC rewrites __builtin_va_arg into an instantiation of va_arg
 */
alias va_arg = imported!"core.stdc.stdarg".va_arg;

version (CRuntime_Microsoft)
{
    //https://docs.microsoft.com/en-us/cpp/cpp/int8-int16-int32-int64?view=msvc-170
    alias __int8 = byte;
    alias __int16 = short;
    alias __int32 = int;
    alias __int64 = long;
}

/*********** floating point *************/

/* https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html
 */

version (DigitalMars)
{
    double __builtin_inf()()  { return double.infinity; }
    float  __builtin_inff()() { return float.infinity; }
    real   __builtin_infl()() { return real.infinity; }

    alias __builtin_huge_val  = __builtin_inf;
    alias __builtin_huge_valf = __builtin_inff;
    alias __builtin_huge_vall = __builtin_infl;

    alias __builtin_fabs  = imported!"core.stdc.math".fabs;
    alias __builtin_fabsf = imported!"core.stdc.math".fabsf;
    alias __builtin_fabsl = imported!"core.stdc.math".fabsl;

    ushort __builtin_bswap16()(ushort value)
    {
        return cast(ushort) (((value >> 8) & 0xFF) | ((value << 8) & 0xFF00U));
    }

    uint __builtin_bswap32()(uint value)
    {
        import core.bitop;
        return core.bitop.bswap(value);
    }

    ulong  __builtin_bswap64()(ulong value)
    {
        import core.bitop;
        return core.bitop.bswap(value);
    }

    // Lazily imported on first use
    private alias c_long = imported!"core.stdc.config".c_long;

    // Stub these out to no-ops
    int    __builtin_constant_p(T)(T exp) { return 0; } // should be something like __traits(compiles, enum X = expr)
    c_long __builtin_expect()(c_long exp, c_long c) { return exp; }
    void*  __builtin_assume_aligned()(const void* p, size_t align_, ...) { return cast(void*)p; }

    // https://releases.llvm.org/13.0.0/tools/clang/docs/LanguageExtensions.html#builtin-assume
    void __builtin_assume(T)(lazy T arg) { }

    /* Header on macOS for arm64 references this.
     * Don't need to implement it, it just needs to compile
     */
    align (16) struct __uint128_t
    {
        ulong a, b;
    }
}
