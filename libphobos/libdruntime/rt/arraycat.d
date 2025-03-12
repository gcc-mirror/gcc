/**
 * Implementation of array copy support routines.
 *
 * Copyright: Copyright Digital Mars 2004 - 2016.
 * License:   Distributed under the
 *            $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 * Authors:   Walter Bright, Sean Kelly
 * Source:    $(DRUNTIMESRC rt/_arraycat.d)
 */

module rt.arraycat;

// debug = PRINTF;

import core.internal.util.array;
import core.stdc.string : memcpy;

debug(PRINTF) import core.stdc.stdio : printf;

extern (C) @trusted nothrow:

void[] _d_arraycopy(size_t size, void[] from, void[] to)
{
    debug(PRINTF) printf("f = %p,%zd, t = %p,%zd, size = %zd\n",
                 from.ptr, from.length, to.ptr, to.length, size);

    enforceRawArraysConformable("copy", size, from, to);
    memcpy(to.ptr, from.ptr, to.length * size);
    return to;
}
