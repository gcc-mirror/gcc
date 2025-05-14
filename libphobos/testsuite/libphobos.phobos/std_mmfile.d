@system unittest
{
    import std.mmfile;

    import std.file;
    std.file.write(deleteme, "hello"); // deleteme is a temporary filename
    scope(exit) remove(deleteme);

    // Use a scope class so the file will be closed at the end of this function
    scope mmfile = new MmFile(deleteme);

    assert(mmfile.length == "hello".length);

    // Access file contents with the slice operator
    // This is typed as `void[]`, so cast to `char[]` or `ubyte[]` to use it
    const data = cast(const(char)[]) mmfile[];

    // At this point, the file content may not have been read yet.
    // In that case, the following memory access will intentionally
    // trigger a page fault, causing the kernel to load the file contents
    assert(data[0 .. 5] == "hello");
}

@system unittest
{
    import std.mmfile;

    import std.file;
    scope(exit) remove(deleteme);

    scope mmfile = new MmFile(deleteme, MmFile.Mode.readWriteNew, 5, null);
    assert(mmfile.length == 5);

    auto data = cast(ubyte[]) mmfile[];

    // This write to memory will be reflected in the file contents
    data[] = '\n';

    mmfile.flush();

    assert(std.file.read(deleteme) == "\n\n\n\n\n");
}

