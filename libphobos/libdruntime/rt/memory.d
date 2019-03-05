/**
 * This module tells the garbage collector about the static data and bss segments,
 * so the GC can scan them for roots. It does not deal with thread local static data.
 *
 * Copyright: Copyright Digital Mars 2000 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Walter Bright, Sean Kelly
 * Source: $(DRUNTIMESRC src/rt/_memory.d)
 */

module rt.memory;


import core.memory;
import rt.sections;

void initStaticDataGC()
{
    foreach (ref sg; SectionGroup)
    {
        foreach (rng; sg.gcRanges)
            GC.addRange(rng.ptr, rng.length);
    }
}
