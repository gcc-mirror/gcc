@safe unittest
{
    import std.socket;

    InternetHost ih = new InternetHost;

    ih.getHostByAddr(0x7F_00_00_01);
    assert(ih.addrList[0] == 0x7F_00_00_01);
    ih.getHostByAddr("127.0.0.1");
    assert(ih.addrList[0] == 0x7F_00_00_01);

    if (!ih.getHostByName("www.digitalmars.com"))
        return;             // don't fail if not connected to internet

    assert(ih.addrList.length);
    InternetAddress ia = new InternetAddress(ih.addrList[0], InternetAddress.PORT_ANY);
    assert(ih.name == "www.digitalmars.com" || ih.name == "digitalmars.com",
            ih.name);

    /* The following assert randomly fails in the test suite.
     * https://issues.dlang.org/show_bug.cgi?id=22791
     * So just ignore it when it fails.
     */
    //assert(ih.getHostByAddr(ih.addrList[0]));
    if (ih.getHostByAddr(ih.addrList[0]))
    {
        string getHostNameFromInt = ih.name.dup;

        // This randomly fails in the compiler test suite
        //assert(ih.getHostByAddr(ia.toAddrString()));

        if (ih.getHostByAddr(ia.toAddrString()))
        {
            string getHostNameFromStr = ih.name.dup;
            assert(getHostNameFromInt == getHostNameFromStr);
        }
    }
}

@system unittest
{
    import std.socket;

        auto addr1 = new InternetAddress("127.0.0.1", 80);
        auto addr2 = new InternetAddress("127.0.0.2", 80);

        assert(addr1 == addr1);
        assert(addr1 != addr2);
    
}

@safe unittest
{
    import std.socket;

    immutable ubyte[4] data = [1, 2, 3, 4];
    auto pair = socketPair();
    scope(exit) foreach (s; pair) s.close();

    pair[0].send(data[]);

    auto buf = new ubyte[data.length];
    pair[1].receive(buf);
    assert(buf == data);
}

