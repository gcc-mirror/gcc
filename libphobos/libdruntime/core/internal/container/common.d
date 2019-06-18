/**
 * Common code for writing containers.
 *
 * Copyright: Copyright Martin Nowak 2013.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 */
module core.internal.container.common;

import core.stdc.stdlib : malloc, realloc;
public import core.stdc.stdlib : free;
import core.internal.traits : dtorIsNothrow;
nothrow:

void* xrealloc(void* ptr, size_t sz) nothrow @nogc
{
    import core.exception;

    if (!sz) { .free(ptr); return null; }
    if (auto nptr = .realloc(ptr, sz)) return nptr;
    .free(ptr); onOutOfMemoryErrorNoGC();
    assert(0);
}

void* xmalloc(size_t sz) nothrow @nogc
{
    import core.exception;
    if (auto nptr = .malloc(sz))
        return nptr;
    onOutOfMemoryErrorNoGC();
    assert(0);
}

void destroy(T)(ref T t) if (is(T == struct) && dtorIsNothrow!T)
{
    scope (failure) assert(0); // nothrow hack
    object.destroy(t);
}

void destroy(T)(ref T t) if (!is(T == struct))
{
    t = T.init;
}

void initialize(T)(ref T t) if (is(T == struct))
{
    import core.internal.lifetime : emplaceInitializer;
    emplaceInitializer(t);
}

void initialize(T)(ref T t) if (!is(T == struct))
{
    t = T.init;
}

version (CoreUnittest) struct RC()
{
nothrow:
    this(size_t* cnt) { ++*(_cnt = cnt); }
    ~this() { if (_cnt) --*_cnt; }
    this(this) { if (_cnt) ++*_cnt; }
    size_t* _cnt;
}
