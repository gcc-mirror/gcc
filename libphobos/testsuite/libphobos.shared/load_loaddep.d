import core.runtime;
import core.stdc.string;
import core.sys.posix.dlfcn;

extern(C) alias RunDepTests = int function(const char*);

void main(string[] args)
{
    auto name = args[0] ~ '\0';
    const pathlen = strrchr(name.ptr, '/') - name.ptr + 1;
    auto root = name[0 .. pathlen];
    auto libloaddep = root ~ "libloaddep.so";
    auto h = Runtime.loadLibrary(libloaddep);
    auto runDepTests = cast(RunDepTests)dlsym(h, "runDepTests");
    assert(runDepTests((root ~ "lib.so\0").ptr));
    assert(Runtime.unloadLibrary(h));
}
