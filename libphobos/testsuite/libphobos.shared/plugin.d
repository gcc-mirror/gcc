import core.thread, core.memory, core.atomic;

// test init
shared uint gctor, gdtor, tctor, tdtor;
shared static this() { if (atomicOp!"+="(gctor, 1) != 1) assert(0); }
shared static ~this() { if (atomicOp!"+="(gdtor, 1) != 1) assert(0); }
static this() { atomicOp!"+="(tctor, 1); }
static ~this() { atomicOp!"+="(tdtor, 1); }

// test GC
__gshared Object root;
void alloc() { root = new Object(); }
void access() { assert(root.toString() !is null); } // vtbl call will fail if finalized
void free() { root = null; }

Object tls_root;
void tls_alloc() { tls_root = new Object(); }
void tls_access() { assert(tls_root.toString() !is null); } // vtbl call will fail if finalized
void tls_free() { tls_root = null; }

void stack(alias func)()
{
    // allocate some extra stack space to not keep references to GC memory on the scanned stack
    ubyte[1024] buf = void;
    func();
}

void testGC()
{
    import core.memory;

    stack!alloc();
    stack!tls_alloc();
    stack!access();
    stack!tls_access();
    GC.collect();
    stack!tls_access();
    stack!access();
    stack!tls_free();
    stack!free();
}

extern(C) int runTests()
{
    try
    {
        assert(atomicLoad!(MemoryOrder.acq)(gctor) == 1);
        assert(atomicLoad!(MemoryOrder.acq)(gdtor) == 0);
        assert(atomicLoad!(MemoryOrder.acq)(tctor) >= 1);
        assert(atomicLoad!(MemoryOrder.acq)(tdtor) >= 0);
        // test some runtime functionality
        testGC();
        new Thread(&testGC).start.join;
    }
    catch (Throwable)
    {
        return false;
    }
    return true;
}

// Provide a way to initialize D from C programs that are D agnostic.
import core.runtime : rt_init, rt_term;

extern(C) int plugin_init()
{
    return rt_init();
}

extern(C) int plugin_term()
{
    return rt_term();
}
