/**
 * Contains the external GC interface.
 *
 * Copyright: Copyright Digital Mars 2005 - 2016.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Sean Kelly
 */

/*          Copyright Digital Mars 2005 - 2016.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module gc.proxy;

import gc.impl.conservative.gc;
import gc.impl.manual.gc;
import gc.config;
import gc.gcinterface;

static import core.memory;

private
{
    static import core.memory;
    alias BlkInfo = core.memory.GC.BlkInfo;

    extern (C) void thread_init();
    extern (C) void thread_term();

    __gshared GC instance;
    __gshared GC proxiedGC; // used to iterate roots of Windows DLLs

}


extern (C)
{

    void gc_init()
    {
        config.initialize();
        ManualGC.initialize(instance);
        ConservativeGC.initialize(instance);
        if (instance is null)
        {
            import core.stdc.stdio : fprintf, stderr;
            import core.stdc.stdlib : exit;

            fprintf(stderr, "No GC was initialized, please recheck the name of the selected GC ('%.*s').\n", cast(int)config.gc.length, config.gc.ptr);
            exit(1);
        }

        // NOTE: The GC must initialize the thread library
        //       before its first collection.
        thread_init();
    }

    void gc_term()
    {
        // NOTE: There may be daemons threads still running when this routine is
        //       called.  If so, cleaning memory out from under then is a good
        //       way to make them crash horribly.  This probably doesn't matter
        //       much since the app is supposed to be shutting down anyway, but
        //       I'm disabling cleanup for now until I can think about it some
        //       more.
        //
        // NOTE: Due to popular demand, this has been re-enabled.  It still has
        //       the problems mentioned above though, so I guess we'll see.

        instance.collectNoStack(); // not really a 'collect all' -- still scans
                                    // static data area, roots, and ranges.

        thread_term();

        ManualGC.finalize(instance);
        ConservativeGC.finalize(instance);
    }

    void gc_enable()
    {
        instance.enable();
    }

    void gc_disable()
    {
        instance.disable();
    }

    void gc_collect() nothrow
    {
        instance.collect();
    }

    void gc_minimize() nothrow
    {
        instance.minimize();
    }

    uint gc_getAttr( void* p ) nothrow
    {
        return instance.getAttr(p);
    }

    uint gc_setAttr( void* p, uint a ) nothrow
    {
        return instance.setAttr(p, a);
    }

    uint gc_clrAttr( void* p, uint a ) nothrow
    {
        return instance.clrAttr(p, a);
    }

    void* gc_malloc( size_t sz, uint ba = 0, const TypeInfo ti = null ) nothrow
    {
        return instance.malloc(sz, ba, ti);
    }

    BlkInfo gc_qalloc( size_t sz, uint ba = 0, const TypeInfo ti = null ) nothrow
    {
        return instance.qalloc( sz, ba, ti );
    }

    void* gc_calloc( size_t sz, uint ba = 0, const TypeInfo ti = null ) nothrow
    {
        return instance.calloc( sz, ba, ti );
    }

    void* gc_realloc( void* p, size_t sz, uint ba = 0, const TypeInfo ti = null ) nothrow
    {
        return instance.realloc( p, sz, ba, ti );
    }

    size_t gc_extend( void* p, size_t mx, size_t sz, const TypeInfo ti = null ) nothrow
    {
        return instance.extend( p, mx, sz,ti );
    }

    size_t gc_reserve( size_t sz ) nothrow
    {
        return instance.reserve( sz );
    }

    void gc_free( void* p ) nothrow
    {
        return instance.free( p );
    }

    void* gc_addrOf( void* p ) nothrow
    {
        return instance.addrOf( p );
    }

    size_t gc_sizeOf( void* p ) nothrow
    {
        return instance.sizeOf( p );
    }

    BlkInfo gc_query( void* p ) nothrow
    {
        return instance.query( p );
    }

    core.memory.GC.Stats gc_stats() nothrow
    {
        return instance.stats();
    }

    void gc_addRoot( void* p ) nothrow
    {
        return instance.addRoot( p );
    }

    void gc_addRange( void* p, size_t sz, const TypeInfo ti = null ) nothrow
    {
        return instance.addRange( p, sz, ti );
    }

    void gc_removeRoot( void* p ) nothrow
    {
        return instance.removeRoot( p );
    }

    void gc_removeRange( void* p ) nothrow
    {
        return instance.removeRange( p );
    }

    void gc_runFinalizers( in void[] segment ) nothrow
    {
        return instance.runFinalizers( segment );
    }

    bool gc_inFinalizer() nothrow
    {
        return instance.inFinalizer();
    }

    GC gc_getProxy() nothrow
    {
        return instance;
    }

    export
    {
        void gc_setProxy( GC proxy )
        {
            foreach (root; instance.rootIter)
            {
                proxy.addRoot(root);
            }

            foreach (range; instance.rangeIter)
            {
                proxy.addRange(range.pbot, range.ptop - range.pbot, range.ti);
            }

            proxiedGC = instance; // remember initial GC to later remove roots
            instance = proxy;
        }

        void gc_clrProxy()
        {
            foreach (root; proxiedGC.rootIter)
            {
                instance.removeRoot(root);
            }

            foreach (range; proxiedGC.rangeIter)
            {
                instance.removeRange(range);
            }

            instance = proxiedGC;
            proxiedGC = null;
        }
    }
}
