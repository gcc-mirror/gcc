@safe unittest
{
    import std.outbuffer;

        OutBuffer buf = new OutBuffer();
        buf.write(cast(ubyte) 1);
        buf.align2();
        assert(buf.toBytes() == "\x01\x00");
        buf.write(cast(ubyte) 2);
        buf.align4();
        assert(buf.toBytes() == "\x01\x00\x02\x00");
        buf.write(cast(ubyte) 3);
        buf.alignSize(8);
        assert(buf.toBytes() == "\x01\x00\x02\x00\x03\x00\x00\x00");
    
}

@safe unittest
{
    import std.outbuffer;

        OutBuffer buf = new OutBuffer();
        buf.write(cast(ubyte) 1);
        buf.align2(0x55);
        assert(buf.toBytes() == "\x01\x55");
        buf.write(cast(ubyte) 2);
        buf.align4(0x55);
        assert(buf.toBytes() == "\x01\x55\x02\x55");
        buf.write(cast(ubyte) 3);
        buf.alignSize(8, 0x55);
        assert(buf.toBytes() == "\x01\x55\x02\x55\x03\x55\x55\x55");
    
}

@safe unittest
{
    import std.outbuffer;

        OutBuffer b = new OutBuffer();
        b.writef("a%sb", 16);
        assert(b.toString() == "a16b");
    
}

@safe unittest
{
    import std.outbuffer;

        OutBuffer b = new OutBuffer();
        b.writef!"a%sb"(16);
        assert(b.toString() == "a16b");
    
}

@safe unittest
{
    import std.outbuffer;

        OutBuffer b = new OutBuffer();
        b.writefln("a%sb", 16);
        assert(b.toString() == "a16b\n");
    
}

@safe unittest
{
    import std.outbuffer;

        OutBuffer b = new OutBuffer();
        b.writefln!"a%sb"(16);
        assert(b.toString() == "a16b\n");
    
}

@safe unittest
{
    import std.outbuffer;

    import std.string : cmp;

    OutBuffer buf = new OutBuffer();

    assert(buf.offset == 0);
    buf.write("hello");
    buf.write(cast(byte) 0x20);
    buf.write("world");
    buf.writef(" %d", 62665);
    assert(cmp(buf.toString(), "hello world 62665") == 0);

    buf.clear();
    assert(cmp(buf.toString(), "") == 0);
    buf.write("New data");
    assert(cmp(buf.toString(),"New data") == 0);
}

