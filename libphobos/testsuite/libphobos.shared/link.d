import lib;

void testEH()
{
    bool passed;
    try
        lib.throwException();
    catch (Exception e)
        passed = true;
    assert(passed); passed = false;

    assert(lib.collectException({throw new Exception(null);}) !is null);
    assert(lib.collectException({lib.throwException();}) !is null);
}

void testGC()
{
    import core.memory;
    lib.alloc();
    lib.tls_alloc();
    lib.access();
    lib.tls_access();
    GC.collect();
    lib.tls_access();
    lib.access();
    lib.tls_free();
    lib.free();
}

import core.atomic : atomicOp;
shared static this() { assert(lib.shared_static_ctor == 1); }
shared static ~this() { assert(lib.shared_static_dtor == 0); }
shared uint static_ctor, static_dtor;
static this() { assert(lib.static_ctor == atomicOp!"+="(static_ctor, 1)); }
static ~this() { assert(lib.static_dtor + 1 == atomicOp!"+="(static_dtor, 1)); }

void testInit()
{
    import core.thread;

    assert(lib.static_ctor == 1);
    assert(lib.static_dtor == 0);
    static void foo()
    {
        assert(lib.shared_static_ctor == 1);
        assert(lib.shared_static_dtor == 0);
        assert(lib.static_ctor == 2);
        assert(lib.static_dtor == 0);
    }
    auto thr = new Thread(&foo);
    thr.start();
    assert(thr.join() is null);
    assert(lib.shared_static_ctor == 1);
    assert(lib.shared_static_dtor == 0);
    assert(lib.static_ctor == 2);
    assert(lib.static_dtor == 1);
}

void main()
{
    testEH();
    testGC();
    testInit();
}
