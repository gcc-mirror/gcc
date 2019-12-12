final class Class
{
    // This gets triggered although the instance always stays referenced.
    ~this()
    {
        import core.stdc.stdlib;
        abort();
    }
}

Class obj;

static this()
{
    obj = new Class;
}

static ~this()
{
    // Free without destruction to avoid triggering abort()
    import core.memory;
    GC.free(cast(void*)obj);
}

void doit()
{
    foreach (i; 0 .. 10_000)
        new ubyte[](100_000);
}

void main()
{
    import core.thread;
    auto t = new Thread(&doit);
    t.start();

    // This triggers the GC that frees the still referenced Class instance.
    doit();
}
