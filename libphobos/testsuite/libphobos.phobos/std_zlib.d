@system unittest
{
    import std.zlib;

    static ubyte[] data = [1,2,3,4,5,6,7,8,9,10];

    uint adler = adler32(0u, data);
    assert(adler == 0xdc0037);
}

@system unittest
{
    import std.zlib;

        // some random data
        ubyte[1024] originalData = void;

        // append garbage data (or don't, this works in both cases)
        auto compressedData = cast(ubyte[]) compress(originalData) ~ cast(ubyte[]) "whatever";

        auto decompressor = new UnCompress();
        auto uncompressedData = decompressor.uncompress(compressedData);

        assert(uncompressedData[] == originalData[],
                "The uncompressed and the original data differ");
        assert(decompressor.empty, "The UnCompressor reports not being done");
    
}

