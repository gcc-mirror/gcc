import core.runtime;
import core.stdc.stdio;
import core.stdc.string;
import core.thread;
import core.sys.posix.dlfcn;

void runTest()
{
    Object obj;
    obj = Object.factory("lib.MyFinalizer");
    assert(obj.toString() == "lib.MyFinalizer");
    obj = Object.factory("lib.MyFinalizerBig");
    assert(obj.toString() == "lib.MyFinalizerBig");
}

class NoFinalize
{
    size_t _finalizeCounter;

    ~this()
    {
        ++_finalizeCounter;
    }
}

class NoFinalizeBig : NoFinalize
{
    ubyte[4096] _big = void;
}

extern (C) alias SetFinalizeCounter = void function(shared(size_t*));

void main(string[] args)
{
    auto name = args[0] ~ '\0';
    const pathlen = strrchr(name.ptr, '/') - name.ptr + 1;
    name = name[0 .. pathlen] ~ "lib.so";

    auto h = Runtime.loadLibrary(name);
    assert(h !is null);

    auto nf1 = new NoFinalize;
    auto nf2 = new NoFinalizeBig;

    shared size_t finalizeCounter;
    auto setFinalizeCounter = cast(SetFinalizeCounter)dlsym(h, "setFinalizeCounter");
    setFinalizeCounter(&finalizeCounter);

    runTest();
    auto thr = new Thread(&runTest);
    thr.start();
    thr.join();

    auto r = Runtime.unloadLibrary(h);
    if (!r)
        assert(0);
    if (finalizeCounter != 4)
        assert(0);
    if (nf1._finalizeCounter)
        assert(0);
    if (nf2._finalizeCounter)
        assert(0);
}
