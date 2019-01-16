/**
 * Implementation of exception handling support routines.
 *
 * Copyright: Copyright Digital Mars 1999 - 2013.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Walter Bright
 * Source: $(DRUNTIMESRC src/rt/deh.d)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module rt.deh;

extern (C)
{
    Throwable.TraceInfo _d_traceContext(void* ptr = null);
    void _d_createTrace(Object o, void* context)
    {
        auto t = cast(Throwable) o;

        if (t !is null && t.info is null &&
            cast(byte*) t !is typeid(t).initializer.ptr)
        {
            t.info = _d_traceContext(context);
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

