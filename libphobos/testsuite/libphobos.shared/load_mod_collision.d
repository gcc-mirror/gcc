module lib; // module collides with lib.so

import core.runtime;
import core.stdc.stdio;
import core.stdc.string;
import core.sys.posix.dlfcn;

void main(string[] args)
{
    auto name = args[0] ~ '\0';
    const pathlen = strrchr(name.ptr, '/') - name.ptr + 1;
    name = name[0 .. pathlen] ~ "lib.so";
    auto lib = Runtime.loadLibrary(name);
}
