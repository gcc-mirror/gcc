import core.runtime, core.sys.posix.dlfcn;

extern(C) alias RunTests = int function();

extern(C) int runDepTests(const char* name)
{
    auto h = rt_loadLibrary(name);
    if (h is null) return false;
    auto runTests = cast(RunTests).dlsym(h, "runTests");
    assert(runTests !is null);
    if (!runTests()) return false;
    return rt_unloadLibrary(h);
}
