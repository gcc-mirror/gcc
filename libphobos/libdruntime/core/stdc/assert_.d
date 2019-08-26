/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_assert.h.html, _assert.h)
 *
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Source:    $(DRUNTIMESRC core/stdc/_assert_.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

/****************************
 * These are the various functions called by the assert() macro.
 * They are all noreturn functions, although D doesn't have a specific attribute for that.
 */

module core.stdc.assert_;

extern (C):
@trusted:
nothrow:
@nogc:

version (CRuntime_DigitalMars)
{
    /***
     * Assert failure function in the Digital Mars C library.
     */
    void _assert(const(void)* exp, const(void)* file, uint line);
}
else version (CRuntime_Microsoft)
{
    /***
     * Assert failure function in the Microsoft C library.
     * `_assert` is not in assert.h, but it is in the library.
     */
    void _wassert(const(wchar)* exp, const(wchar)* file, uint line);
    ///
    void _assert(const(char)* exp, const(char)* file, uint line);
}
else version (OSX)
{
    /***
     * Assert failure function in the OSX C library.
     */
    void __assert_rtn(const(char)* func, const(char)* file, uint line, const(char)* exp);
}
else version (FreeBSD)
{
    /***
     * Assert failure function in the FreeBSD C library.
     */
    void __assert(const(char)* exp, const(char)* file, uint line);
}
else version (NetBSD)
{
    /***
     * Assert failure function in the NetBSD C library.
     */
    void __assert(const(char)* file, int line, const(char)* exp);
}
else version (OpenBSD)
{
    /***
     * Assert failure function in the OpenBSD C library.
     */
    void __assert(const(char)* file, int line, const(char)* exp);
    ///
    void __assert2(const(char)* file, int line, const(char)* func, const(char)* exp);
}
else version (DragonFlyBSD)
{
    /***
     * Assert failure function in the DragonFlyBSD C library.
     */
    void __assert(const(char)* exp, const(char)* file, uint line);
}
else version (CRuntime_Glibc)
{
    /***
     * Assert failure functions in the GLIBC library.
     */
    void __assert(const(char)* exp, const(char)* file, uint line);
    ///
    void __assert_fail(const(char)* exp, const(char)* file, uint line, const(char)* func);
    ///
    void __assert_perror_fail(int errnum, const(char)* file, uint line, const(char)* func);
}
else version (CRuntime_Bionic)
{
    void __assert(const(char)* __file, int __line, const(char)* __msg);
}
else version (CRuntime_Musl)
{
     /***
     * Assert failure function in the Musl C library.
     */
    void __assert_fail(const(char)* exp, const(char)* file, uint line, const(char)* func);
}
else version (CRuntime_UClibc)
{
    void __assert(const(char)* exp, const(char)* file, uint line, const(char)* func);
}
else version (Solaris)
{
    void __assert_c99(const(char)* exp, const(char)* file, uint line, const(char)* func);
}
else
{
    static assert(0);
}
