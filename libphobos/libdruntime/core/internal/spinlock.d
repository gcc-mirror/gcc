/**
 * SpinLock for runtime internal usage.
 *
 * Copyright: Copyright Digital Mars 2015 -.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 * Source: $(DRUNTIMESRC core/internal/_spinlock.d)
 */
module core.internal.spinlock;

import core.atomic, core.thread;

shared struct SpinLock
{
    /// for how long is the lock usually contended
    enum Contention : ubyte
    {
        brief,
        medium,
        lengthy,
    }

@trusted @nogc nothrow:
    this(Contention contention)
    {
        this.contention = contention;
    }

    void lock()
    {
        if (cas(&val, size_t(0), size_t(1)))
            return;
        // Try to reduce the chance of another cas failure
        // TTAS lock (https://en.wikipedia.org/wiki/Test_and_test-and-set)
        immutable step = 1 << contention;
        while (true)
        {
            for (size_t n; atomicLoad!(MemoryOrder.raw)(val); n += step)
                yield(n);
            if (cas(&val, size_t(0), size_t(1)))
                return;
        }
    }

    void unlock()
    {
        atomicStore!(MemoryOrder.rel)(val, size_t(0));
    }

    /// yield with backoff
    void yield(size_t k)
    {
        if (k < pauseThresh)
            return pause();
        else if (k < 32)
            return Thread.yield();
        Thread.sleep(1.msecs);
    }

private:
    version (D_InlineAsm_X86)
        enum X86 = true;
    else version (D_InlineAsm_X86_64)
        enum X86 = true;
    else
        enum X86 = false;

    static if (X86)
    {
        enum pauseThresh = 16;
        void pause()
        {
            asm @trusted @nogc nothrow
            {
                // pause instruction
                rep;
                nop;
            }
        }
    }
    else
    {
        enum pauseThresh = 4;
        void pause()
        {
        }
    }

    size_t val;
    Contention contention;
}

// aligned to cacheline to avoid false sharing
shared align(64) struct AlignedSpinLock
{
    this(SpinLock.Contention contention)
    {
        impl = shared(SpinLock)(contention);
    }

    SpinLock impl;
    alias impl this;
}
