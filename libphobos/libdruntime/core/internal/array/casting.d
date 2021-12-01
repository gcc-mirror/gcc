/**
 This module contains compiler support for casting dynamic arrays

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/internal/_array/_casting.d)
*/
module core.internal.array.casting;

/**
Used by `__ArrayCast` to emit a descriptive error message.

It is a template so it can be used by `__ArrayCast` in -betterC
builds.  It is separate from `__ArrayCast` to minimize code
bloat.

Params:
    fromType = name of the type being cast from
    fromSize = total size in bytes of the array being cast from
    toType   = name of the type being cast o
    toSize   = total size in bytes of the array being cast to
 */
private void onArrayCastError()(string fromType, size_t fromSize, string toType, size_t toSize) @trusted
{
    import core.internal.string : unsignedToTempString;
    import core.memory : pureMalloc;

    // convert discontiguous `msgComponents` to contiguous string on the C heap
    enum msgLength = 2048;
    // note: never freed!
    char* msg = cast(char *)pureMalloc(msgLength);

    size_t index = 0;
    void add(const(char)[] m)
    {
        import core.stdc.string : memcpy;

        auto N = msgLength - 1 - index;
        if (N > m.length)
            N = m.length;
        // prevent superfluous and betterC-unfriendly checks via direct memcpy
        memcpy(msg + index, m.ptr, N);
        index += N;
    }

    add("An array of size ");
    auto s = unsignedToTempString(fromSize);
    add(s[]);
    add(" does not align on an array of size ");
    s = unsignedToTempString(toSize);
    add(s[]);
    add(", so `");
    add(fromType);
    add("` cannot be cast to `");
    add(toType);
    add("`");
    msg[index] = '\0'; // null-termination

    // first argument must evaluate to `false` at compile-time to maintain memory safety in release builds
    assert(false, msg[0 .. index]);
}

/**
The compiler lowers expressions of `cast(TTo[])TFrom[]` to
this implementation.

Params:
    from = the array to reinterpret-cast

Returns:
    `from` reinterpreted as `TTo[]`
 */
TTo[] __ArrayCast(TFrom, TTo)(return scope TFrom[] from) @nogc pure @trusted
{
    const fromSize = from.length * TFrom.sizeof;
    const toLength = fromSize / TTo.sizeof;

    if ((fromSize % TTo.sizeof) != 0)
    {
        onArrayCastError(TFrom.stringof, fromSize, TTo.stringof, toLength * TTo.sizeof);
    }

    struct Array
    {
        size_t length;
        void* ptr;
    }
    auto a = cast(Array*)&from;
    a.length = toLength; // jam new length
    return *cast(TTo[]*)a;
}

@safe @nogc pure nothrow unittest
{
    byte[int.sizeof * 3] b = cast(byte) 0xab;
    int[] i;
    short[] s;

    i = __ArrayCast!(byte, int)(b);
    assert(i.length == 3);
    foreach (v; i)
        assert(v == cast(int) 0xabab_abab);

    s = __ArrayCast!(byte, short)(b);
    assert(s.length == 6);
    foreach (v; s)
        assert(v == cast(short) 0xabab);

    s = __ArrayCast!(int, short)(i);
    assert(s.length == 6);
    foreach (v; s)
        assert(v == cast(short) 0xabab);
}
