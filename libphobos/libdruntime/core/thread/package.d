/**
 * The thread module provides support for thread creation and management.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Walter Bright, Alex Rønne Petersen, Martin Nowak
 * Source:    $(DRUNTIMESRC core/thread/package.d)
 */

module core.thread;

public import core.time;
public import core.thread.fiber;
public import core.thread.osthread;
public import core.thread.threadbase;
public import core.thread.threadgroup;
public import core.thread.types;
public import core.thread.context;


// this test is here to avoid a cyclic dependency between
// core.thread and core.atomic
unittest
{
    import core.atomic;

    // Use heap memory to ensure an optimizing
    // compiler doesn't put things in registers.
    uint* x = new uint();
    bool* f = new bool();
    uint* r = new uint();

    auto thr = new Thread(()
    {
        while (!*f)
        {
        }

        atomicFence();

        *r = *x;
    });

    thr.start();

    *x = 42;

    atomicFence();

    *f = true;

    atomicFence();

    thr.join();

    assert(*r == 42);
}
