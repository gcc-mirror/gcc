/**
 * Exception allocation, cloning, and release compiler support routines.
 *
 * Copyright: Copyright (c) 2017 by D Language Foundation
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors: Walter Bright
 * Source: $(DRUNTIMESRC rt/_ehalloc.d)
 */

module rt.ehalloc;

//debug = PRINTF;

debug (PRINTF) import core.stdc.stdio : printf;


/********************************************
 * Delete exception instance `t` from the exception pool.
 * Must have been allocated with `_d_newThrowable()`.
 * This is meant to be called at the close of a catch block.
 * It's nothrow because otherwise any function with a catch block could
 * not be nothrow.
 * Input:
 *      t = Throwable
 */

nothrow extern (C) void _d_delThrowable(Throwable t)
{
    if (t)
    {
        debug(PRINTF) printf("_d_delThrowable(%p)\n", t);

        /* If allocated by the GC, don't free it.
         * Let the GC handle it.
         * Supporting this is necessary while transitioning
         * to this new scheme for allocating exceptions.
         */
        auto refcount = t.refcount();
        if (refcount == 0)
            return;     // it was allocated by the GC

        if (refcount == 1)
            assert(0);  // no zombie objects

        t.refcount() = --refcount;
        if (refcount > 1)
            return;

        TypeInfo_Class **pc = cast(TypeInfo_Class **)t;
        if (*pc)
        {
            TypeInfo_Class ci = **pc;

            if (!(ci.m_flags & TypeInfo_Class.ClassFlags.noPointers))
            {
                // Inform the GC about the pointers in the object instance
                import core.memory : GC;
                GC.removeRange(cast(void*) t);
            }
        }

        try
        {
            import rt.lifetime : rt_finalize;
            rt_finalize(cast(void*) t);
        }
        catch (Throwable t)
        {
            assert(0);  // should never happen since Throwable.~this() is nothrow
        }
        import core.stdc.stdlib : free;
        debug(PRINTF) printf("free(%p)\n", t);
        free(cast(void*) t);
    }
}
