/**
 * The default hash implementation.
 *
 * Copyright: Copyright Sean Kelly 2009 - 2016.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 * Source: $(DRUNTIMESRC src/rt/util/_hash.d)
 */
module rt.util.hash;


version (X86)
    version = AnyX86;
version (X86_64)
    version = AnyX86;
version (AnyX86)
    version = HasUnalignedOps;


@trusted pure nothrow @nogc
size_t hashOf( const(void)[] buf, size_t seed )
{
    /*
     * This is Paul Hsieh's SuperFastHash algorithm, described here:
     *   http://www.azillionmonkeys.com/qed/hash.html
     * It is protected by the following open source license:
     *   http://www.azillionmonkeys.com/qed/weblicense.html
     */
    static uint get16bits( const (ubyte)* x ) pure nothrow @nogc
    {
        // CTFE doesn't support casting ubyte* -> ushort*, so revert to
        // per-byte access when in CTFE.
        version (HasUnalignedOps)
        {
            if (!__ctfe)
                return *cast(ushort*) x;
        }

        return ((cast(uint) x[1]) << 8) + (cast(uint) x[0]);
    }

    // NOTE: SuperFastHash normally starts with a zero hash value.  The seed
    //       value was incorporated to allow chaining.
    auto data = cast(const(ubyte)*) buf.ptr;
    auto len = buf.length;
    auto hash = seed;

    if ( len == 0 || data is null )
        return 0;

    int rem = len & 3;
    len >>= 2;

    for ( ; len > 0; len-- )
    {
        hash += get16bits( data );
        auto tmp = (get16bits( data + 2 ) << 11) ^ hash;
        hash  = (hash << 16) ^ tmp;
        data += 2 * ushort.sizeof;
        hash += hash >> 11;
    }

    switch ( rem )
    {
    case 3: hash += get16bits( data );
            hash ^= hash << 16;
            hash ^= data[ushort.sizeof] << 18;
            hash += hash >> 11;
            break;
    case 2: hash += get16bits( data );
            hash ^= hash << 11;
            hash += hash >> 17;
            break;
    case 1: hash += *data;
            hash ^= hash << 10;
            hash += hash >> 1;
            break;
     default:
            break;
    }

    /* Force "avalanching" of final 127 bits */
    hash ^= hash << 3;
    hash += hash >> 5;
    hash ^= hash << 4;
    hash += hash >> 17;
    hash ^= hash << 25;
    hash += hash >> 6;

    return hash;
}

unittest
{
    enum test_str = "Sample string";
    size_t hashval = hashOf(test_str, 5);

    //import core.stdc.stdio;
    //printf("hashval = %lld\n", cast(long)hashval);

    if (hashval.sizeof == 4)
        assert(hashval == 528740845);
    else if (hashval.sizeof == 8)
        assert(hashval == 8106800467257150594L);
    else
        assert(0);
}
