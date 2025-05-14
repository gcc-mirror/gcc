/**
 * Entry point for exception handling support routines.
 *
 * There are three style of exception handling being supported by DMD:
 * DWARF, Win32, and Win64. The Win64 code also supports POSIX.
 * Support for those scheme is in `rt.dwarfeh`, `rt.deh_win32`, and
 * `rt.deh_win64_posix`, respectively, and publicly imported here.
 *
 * When an exception is thrown by the user, the compiler translates
 * code like `throw e;` into either `_d_throwdwarf` (for DWARF exceptions)
 * or `_d_throwc` (Win32 / Win64), with the `Exception` object as argument.
 *
 * During those functions' handling, they eventually call `_d_createTrace`,
 * which will store inside the `Exception` object the return of
 * `_d_traceContext`, which is an object implementing  `Throwable.TraceInfo`.
 * `_d_traceContext` is a configurable hook, and by default will call
 * `core.runtime : defaultTraceHandler`, which itself will call `backtrace`
 * or something similar to store an array of stack frames (`void*` pointers)
 * in the object it returns.
 * Note that `defaultTraceHandler` returns a GC-allocated instance,
 * hence a GC allocation can happen in the middle of throwing an `Exception`.
 *
 * The `Throwable.TraceInfo`-implementing should not resolves function names,
 * file and line number until its `opApply` function is called, avoiding the
 * overhead of reading the debug infos until the user call `toString`.
 * If the user only calls `Throwable.message` (or use `Throwable.msg` directly),
 * only the overhead of `backtrace` will be paid, which is minimal enouh.
 *
 * Copyright: Copyright Digital Mars 1999 - 2020.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Walter Bright
 * Source: $(DRUNTIMESRC rt/deh.d)
 */
module rt.deh;

extern (C)
{
    Throwable.TraceInfo _d_traceContext(void* ptr = null);
    Throwable.TraceDeallocator rt_getTraceDeallocator();
    void _d_createTrace(Throwable t, void* context)
    {
        if (t !is null && t.info is null &&
            cast(byte*) t !is typeid(t).initializer.ptr)
        {
            t.info = _d_traceContext(context);
            t.infoDeallocator = rt_getTraceDeallocator();
        }
    }
}

version (GNU)
    public import gcc.deh;
else version (Win32)
    public import rt.deh_win32;
else version (Win64)
    public import rt.deh_win64_posix;
else version (Posix)
    public import rt.deh_win64_posix;
else
    static assert (0, "Unsupported architecture");
