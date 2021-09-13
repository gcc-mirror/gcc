module core.internal.abort;

/*
 * Use instead of assert(0, msg), since this does not print a message for -release compiled
 * code, and druntime is -release compiled.
 */
void abort(scope string msg, scope string filename = __FILE__, size_t line = __LINE__) @nogc nothrow @safe
{
    import core.stdc.stdlib: c_abort = abort;
    // use available OS system calls to print the message to stderr
    version (Posix)
    {
        import core.sys.posix.unistd: write;
        static void writeStr(const(char)[][] m...) @nogc nothrow @trusted
        {
            foreach (s; m)
                write(2, s.ptr, s.length);
        }
    }
    else version (Windows)
    {
        import core.sys.windows.windows: GetStdHandle, STD_ERROR_HANDLE, WriteFile, INVALID_HANDLE_VALUE;
        auto h = (() @trusted => GetStdHandle(STD_ERROR_HANDLE))();
        if (h == INVALID_HANDLE_VALUE)
            // attempt best we can to print the message
            assert(0, msg);
        void writeStr(const(char)[][] m...) @nogc nothrow @trusted
        {
            foreach (s; m)
            {
                assert(s.length <= uint.max);
                WriteFile(h, s.ptr, cast(uint)s.length, null, null);
            }
        }
    }
    else
        static assert(0, "Unsupported OS");

    import core.internal.string;
    UnsignedStringBuf strbuff;

    // write an appropriate message, then abort the program
    writeStr("Aborting from ", filename, "(", line.unsignedToTempString(strbuff, 10), ") ", msg);
    c_abort();
}
