/***
 * D compatible types that correspond to various basic types in associated
 * C and C++ compilers.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/_config.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module core.stdc.config;

version (StdDdoc)
{
    private
    {
        version (Posix)
            enum isPosix = true;
        else
            enum isPosix = false;
        static if (isPosix && (void*).sizeof > int.sizeof)
        {
            alias ddoc_long = long;
            alias ddoc_ulong = ulong;
        }
        else
        {
            alias ddoc_long = int;
            alias ddoc_ulong = uint;
        }
        struct ddoc_complex(T) { T re; T im; }
    }

    /***
     * Used for a signed integer type that corresponds in size to the associated
     * C compiler's `long` type.
     */
    alias c_long = ddoc_long;

    /***
     * Used for an unsigned integer type that corresponds in size to the associated
     * C compiler's `unsigned long` type.
     */
    alias c_ulong = ddoc_ulong;

    /***
     * Used for a signed integer type that corresponds in size and mangling to the associated
     * C++ compiler's `long` type.
     */
    alias cpp_long = c_long;

    /***
     * Used for an unsigned integer type that corresponds in size and mangling to the associated
     * C++ compiler's `unsigned long` type.
     */
    alias cpp_ulong = c_ulong;

    /***
     * Used for a signed integer type that corresponds in size and mangling to the associated
     * C++ compiler's `long long` type.
     */
    alias cpp_longlong = long;

    /***
     * Used for an unsigned integer type that corresponds in size and mangling to the associated
     * C++ compiler's `unsigned long long` type.
     */
    alias cpp_ulonglong = ulong;

    /***
     * Used for a floating point type that corresponds in size and mangling to the associated
     * C++ compiler's `long double` type.
     */
    alias c_long_double = real;

    /***
     * Used for an unsigned integer type that corresponds in size and mangling to the associated
     * C++ compiler's `size_t` type.
     */
    alias cpp_size_t = size_t;

    /***
     * Used for a signed integer type that corresponds in size and mangling to the associated
     * C++ compiler's `ptrdiff_t` type.
     */
    alias cpp_ptrdiff_t = ptrdiff_t;

    /***
     * Used for a complex floating point type that corresponds in size and ABI to the associated
     * C compiler's `_Complex float` type.
     */
    alias c_complex_float = ddoc_complex!float;

    /***
     * Used for a complex floating point type that corresponds in size and ABI to the associated
     * C compiler's `_Complex double` type.
     */
    alias c_complex_double = ddoc_complex!double;

    /***
     * Used for a complex floating point type that corresponds in size and ABI to the associated
     * C compiler's `_Complex long double` type.
     */
    alias c_complex_real = ddoc_complex!real;
}
else
{

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (GNU)
{
    import gcc.builtins;

    alias __builtin_clong  c_long;
    alias __builtin_culong c_ulong;

    enum __c_long  : __builtin_clong;
    enum __c_ulong : __builtin_culong;

    alias __c_long  cpp_long;
    alias __c_ulong cpp_ulong;

    enum __c_longlong  : __builtin_clonglong;
    enum __c_ulonglong : __builtin_culonglong;

    alias __c_longlong  cpp_longlong;
    alias __c_ulonglong cpp_ulonglong;
}
else version (Windows)
{
    enum __c_long  : int;
    enum __c_ulong : uint;

    alias int   c_long;
    alias uint  c_ulong;

    alias __c_long   cpp_long;
    alias __c_ulong  cpp_ulong;

    alias long  cpp_longlong;
    alias ulong cpp_ulonglong;
}
else version (Posix)
{
  static if ( (void*).sizeof > int.sizeof )
  {
    enum __c_longlong  : long;
    enum __c_ulonglong : ulong;

    alias long  c_long;
    alias ulong c_ulong;

    alias long   cpp_long;
    alias ulong  cpp_ulong;

    alias __c_longlong  cpp_longlong;
    alias __c_ulonglong cpp_ulonglong;
  }
  else
  {
    enum __c_long  : int;
    enum __c_ulong : uint;

    alias int   c_long;
    alias uint  c_ulong;

    alias __c_long   cpp_long;
    alias __c_ulong  cpp_ulong;

    alias long  cpp_longlong;
    alias ulong cpp_ulonglong;
  }
}
else version (WASI)
{
    static if ( (void*).sizeof > int.sizeof )
    {
        enum __c_longlong  : long;
        enum __c_ulonglong : ulong;

        alias long  c_long;
        alias ulong c_ulong;

        alias long   cpp_long;
        alias ulong  cpp_ulong;

        alias __c_longlong  cpp_longlong;
        alias __c_ulonglong cpp_ulonglong;
    }
    else
    {
        enum __c_long  : int;
        enum __c_ulong : uint;

        alias int   c_long;
        alias uint  c_ulong;

        alias __c_long   cpp_long;
        alias __c_ulong  cpp_ulong;

        alias long  cpp_longlong;
        alias ulong cpp_ulonglong;
    }
}

version (GNU)
    alias c_long_double = real;
else version (LDC)
    alias c_long_double = real; // 64-bit real for MSVC targets
else version (SDC)
{
    version (X86)
        alias c_long_double = real;
    else version (X86_64)
        alias c_long_double = real;
}
else version (CRuntime_Microsoft)
{
    /* long double is 64 bits, not 80 bits, but is mangled differently
     * than double. To distinguish double from long double, create a wrapper to represent
     * long double, then recognize that wrapper specially in the compiler
     * to generate the correct name mangling and correct function call/return
     * ABI conformance.
     */
    enum __c_long_double : double;

    alias __c_long_double c_long_double;
}
else version (DigitalMars)
{
    version (X86)
    {
        alias real c_long_double;
    }
    else version (X86_64)
    {
        version (linux)
            alias real c_long_double;
        else version (FreeBSD)
            alias real c_long_double;
        else version (OpenBSD)
            alias real c_long_double;
        else version (NetBSD)
            alias real c_long_double;
        else version (DragonFlyBSD)
            alias real c_long_double;
        else version (Solaris)
            alias real c_long_double;
        else version (Darwin)
            alias real c_long_double;
    }
}

static assert(is(c_long_double), "c_long_double needs to be declared for this platform/architecture.");

version (Darwin)
{
    alias cpp_size_t = cpp_ulong;
    version (D_LP64)
        alias cpp_ptrdiff_t = cpp_long;
    else
        alias cpp_ptrdiff_t = ptrdiff_t;
}
else
{
    alias cpp_size_t = size_t;
    alias cpp_ptrdiff_t = ptrdiff_t;
}

// ABI layout of native complex types.
private struct _Complex(T)
{
    T re;
    T im;

    // Helper properties.
    pragma(inline, true)
    {
        static @property epsilon()()    { return _Complex(T.epsilon, T.epsilon); }
        static @property infinity()()   { return _Complex(T.infinity, T.infinity); }
        static @property max()()        { return _Complex(T.max, T.max); }
        static @property min_normal()() { return _Complex(T.min_normal, T.min_normal); }
        static @property nan()()        { return _Complex(T.nan, T.nan); }
        static @property dig()()        { return T.dig; }
        static @property mant_dig()()   { return T.mant_dig; }
        static @property max_10_exp()() { return T.max_10_exp; }
        static @property max_exp()()    { return T.max_exp; }
        static @property min_10_exp()() { return T.min_10_exp; }
        static @property min_exp()()    { return T.min_exp; }
    }
}

enum __c_complex_float  : _Complex!float;
enum __c_complex_double : _Complex!double;
enum __c_complex_real   : _Complex!c_long_double;

alias c_complex_float = __c_complex_float;
alias c_complex_double = __c_complex_double;
alias c_complex_real = __c_complex_real;
}


// Returns the mangled name for the 64-bit time_t versions of
// functions affected by musl's transition to 64-bit time_t.
// https://musl.libc.org/time64.html
version (CRuntime_Musl)
{
    version (CRuntime_Musl_Pre_Time64)
        enum muslRedirTime64 = false;
    else
    {
        // time_t was defined as a C long in older Musl versions.
        enum muslRedirTime64 = (c_long.sizeof == 4);
    }
}
else
    enum muslRedirTime64 = false;

package(core) template muslRedirTime64Mangle(string name, string redirectedName)
{
    static if (muslRedirTime64)
        enum muslRedirTime64Mangle = redirectedName;
    else
        enum muslRedirTime64Mangle = name;
}
