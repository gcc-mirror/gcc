module core.sys.bionic.unistd;

version (CRuntime_Bionic) extern(C) nothrow @nogc:

int flock(int, int) @trusted;
