import core.runtime;
import core.stdc.string;
import core.sys.posix.dlfcn;

extern(C) alias RunDepTests = int function();

void main(string[] args)
{
    auto name = args[0] ~ '\0';
    const pathlen = strrchr(name.ptr, '/') - name.ptr + 1;
    name = name[0 .. pathlen] ~ "liblinkdep.so";

    auto h = Runtime.loadLibrary(name);
    assert(h);
    auto runDepTests = cast(RunDepTests)dlsym(h, "runDepTests");
    assert(runDepTests());
    assert(Runtime.unloadLibrary(h));
}
