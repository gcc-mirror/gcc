@safe unittest
{
    import std.digest.murmurhash;

    // MurmurHash3!32, MurmurHash3!(128, 32) and MurmurHash3!(128, 64) implement
    // the std.digest Template API.
    static assert(isDigest!(MurmurHash3!32));
    // The convenient digest template allows for quick hashing of any data.
    ubyte[4] hashed = digest!(MurmurHash3!32)([1, 2, 3, 4]);
    assert(hashed == [0, 173, 69, 68]);
}

@safe unittest
{
    import std.digest.murmurhash;

    // One can also hash ubyte data piecewise by instanciating a hasher and call
    // the 'put' method.
    const(ubyte)[] data1 = [1, 2, 3];
    const(ubyte)[] data2 = [4, 5, 6, 7];
    // The incoming data will be buffered and hashed element by element.
    MurmurHash3!32 hasher;
    hasher.put(data1);
    hasher.put(data2);
    // The call to 'finish' ensures:
    // - the remaining bits are processed
    // - the hash gets finalized
    auto hashed = hasher.finish();
    assert(hashed == [181, 151, 88, 252]);
}

@safe unittest
{
    import std.digest.murmurhash;

    // Using `putElements`, `putRemainder` and `finalize` you gain full
    // control over which part of the algorithm to run.
    // This allows for maximum throughput but needs extra care.

    // Data type must be the same as the hasher's element type:
    // - uint for MurmurHash3!32
    // - uint[4] for MurmurHash3!(128, 32)
    // - ulong[2] for MurmurHash3!(128, 64)
    const(uint)[] data = [1, 2, 3, 4];
    // Note the hasher starts with 'Fast'.
    MurmurHash3!32 hasher;
    // Push as many array of elements as you need. The less calls the better.
    hasher.putElements(data);
    // Put remainder bytes if needed. This method can be called only once.
    hasher.putRemainder(ubyte(1), ubyte(1), ubyte(1));
    // Call finalize to incorporate data length in the hash.
    hasher.finalize();
    // Finally get the hashed value.
    auto hashed = hasher.getBytes();
    assert(hashed == [188, 165, 108, 2]);
}

@safe unittest
{
    import std.digest.murmurhash;

    ubyte[4] hashed = digest!(MurmurHash3!32)([1, 2, 3, 4]);
    assert(hashed == [0, 173, 69, 68]);
}

@safe unittest
{
    import std.digest.murmurhash;

    const(ubyte)[] data1 = [1, 2, 3];
    const(ubyte)[] data2 = [4, 5, 6, 7];
    // The incoming data will be buffered and hashed element by element.
    MurmurHash3!32 hasher;
    hasher.put(data1);
    hasher.put(data2);
    // The call to 'finish' ensures:
    // - the remaining bits are processed
    // - the hash gets finalized
    auto hashed = hasher.finish();
    assert(hashed == [181, 151, 88, 252]);
}

