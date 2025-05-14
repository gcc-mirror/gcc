@safe unittest
{
    import std.internal.cstring;

    version (Posix)
    {
        import core.stdc.stdlib : free;
        import core.sys.posix.stdlib : setenv;
        import std.exception : enforce;

        void setEnvironment(scope const(char)[] name, scope const(char)[] value)
        { enforce(setenv(name.tempCString(), value.tempCString(), 1) != -1); }
    }

    version (Windows)
    {
        import core.sys.windows.winbase : SetEnvironmentVariableW;
        import std.exception : enforce;

        void setEnvironment(scope const(char)[] name, scope const(char)[] value)
        { enforce(SetEnvironmentVariableW(name.tempCStringW(), value.tempCStringW())); }
    }
}

nothrow @nogc @system unittest
{
    import std.internal.cstring;

    import core.stdc.string;

    string str = "abc";

    // Intended usage
    assert(strlen(str.tempCString()) == 3);

    // Correct usage
    auto tmp = str.tempCString();
    assert(strlen(tmp) == 3); // or `tmp.ptr`, or `tmp.buffPtr`

    // $(RED WARNING): $(RED Incorrect usage)
    auto pInvalid1 = str.tempCString().ptr;
    const char* pInvalid2 = str.tempCString();
    // Both pointers refer to invalid memory here as
    // returned values aren't assigned to a variable and
    // both primary expressions are ended.
}

