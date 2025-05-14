@system unittest
{
    import std.internal.scopebuffer;

    ubyte[10] tmpbuf = void;
    auto sb = scopeBuffer(tmpbuf);
    scope(exit) sb.free();
}

