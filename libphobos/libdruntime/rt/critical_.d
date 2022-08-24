/**
 * Implementation of support routines for synchronized blocks.
 *
 * Copyright: Copyright Digital Mars 2000 - 2011.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Sean Kelly
 * Source: $(DRUNTIMESRC rt/_critical_.d)
 */

/*          Copyright Digital Mars 2000 - 2011.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module rt.critical_;

nothrow:

import rt.monitor_, core.atomic;

extern (C) void _d_critical_init() @nogc nothrow
{
    initMutex(cast(Mutex*)&gcs.mtx);
    head = &gcs;
}

extern (C) void _d_critical_term() @nogc nothrow
{
    // This function is only ever called by the runtime shutdown code
    // and therefore is single threaded so the following cast is fine.
    auto h = cast()head;
    for (auto p = h; p; p = p.next)
        destroyMutex(cast(Mutex*)&p.mtx);
}

extern (C) void _d_criticalenter(D_CRITICAL_SECTION* cs)
{
    assert(cs !is null);
    ensureMutex(cast(shared(D_CRITICAL_SECTION*)) cs);
    lockMutex(&cs.mtx);
}

extern (C) void _d_criticalenter2(D_CRITICAL_SECTION** pcs)
{
    if (atomicLoad!(MemoryOrder.acq)(*cast(shared) pcs) is null)
    {
        lockMutex(cast(Mutex*)&gcs.mtx);
        if (atomicLoad!(MemoryOrder.raw)(*cast(shared) pcs) is null)
        {
            auto cs = new shared D_CRITICAL_SECTION;
            initMutex(cast(Mutex*)&cs.mtx);
            atomicStore!(MemoryOrder.rel)(*cast(shared) pcs, cs);
        }
        unlockMutex(cast(Mutex*)&gcs.mtx);
    }
    lockMutex(&(*pcs).mtx);
}

extern (C) void _d_criticalexit(D_CRITICAL_SECTION* cs)
{
    assert(cs !is null);
    unlockMutex(&cs.mtx);
}

private:

shared D_CRITICAL_SECTION* head;
shared D_CRITICAL_SECTION gcs;

struct D_CRITICAL_SECTION
{
    D_CRITICAL_SECTION* next;
    Mutex mtx;
}

void ensureMutex(shared(D_CRITICAL_SECTION)* cs)
{
    if (atomicLoad!(MemoryOrder.acq)(cs.next) is null)
    {
        lockMutex(cast(Mutex*)&gcs.mtx);
        if (atomicLoad!(MemoryOrder.raw)(cs.next) is null)
        {
            initMutex(cast(Mutex*)&cs.mtx);
            auto ohead = head;
            head = cs;
            atomicStore!(MemoryOrder.rel)(cs.next, ohead);
        }
        unlockMutex(cast(Mutex*)&gcs.mtx);
    }
}
