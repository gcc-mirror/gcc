/*
 * Copyright: 2014 by Digital Mars
 * License: $(LINK2 http://boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors: Walter Bright
 * Source: $(PHOBOSSRC std/internal/_scopebuffer.d)
 */

module std.internal.scopebuffer;


//debug=ScopeBuffer;

import core.stdc.stdlib : realloc;
import std.traits;

/**************************************
 * ScopeBuffer encapsulates using a local array as a temporary buffer.
 * It is initialized with a local array that should be large enough for
 * most uses. If the need exceeds that size, ScopeBuffer will reallocate
 * the data using its `realloc` function.
 *
 * ScopeBuffer cannot contain more than `(uint.max-16)/2` elements.
 *
 * ScopeBuffer is an Output Range.
 *
 * Since ScopeBuffer may store elements of type `T` in `malloc`'d memory,
 * those elements are not scanned when the GC collects. This can cause
 * memory corruption. Do not use ScopeBuffer when elements of type `T` point
 * to the GC heap, except when a `realloc` function is provided which supports this.
 *
 * Example:
---
import core.stdc.stdio;
import std.internal.scopebuffer;
void main()
{
    char[2] buf = void;
    auto textbuf = ScopeBuffer!char(buf);
    scope(exit) textbuf.free(); // necessary for cleanup

    // Put characters and strings into textbuf, verify they got there
    textbuf.put('a');
    textbuf.put('x');
    textbuf.put("abc");
    assert(textbuf.length == 5);
    assert(textbuf[1 .. 3] == "xa");
    assert(textbuf[3] == 'b');

    // Can shrink it
    textbuf.length = 3;
    assert(textbuf[0 .. textbuf.length] == "axa");
    assert(textbuf[textbuf.length - 1] == 'a');
    assert(textbuf[1 .. 3] == "xa");

    textbuf.put('z');
    assert(textbuf[] == "axaz");

    // Can shrink it to 0 size, and reuse same memory
    textbuf.length = 0;
}
---
 * It is invalid to access ScopeBuffer's contents when ScopeBuffer goes out of scope.
 * Hence, copying the contents are necessary to keep them around:
---
import std.internal.scopebuffer;
string cat(string s1, string s2)
{
    char[10] tmpbuf = void;
    auto textbuf = ScopeBuffer!char(tmpbuf);
    scope(exit) textbuf.free();
    textbuf.put(s1);
    textbuf.put(s2);
    textbuf.put("even more");
    return textbuf[].idup;
}
---
 * ScopeBuffer is intended for high performance usages in $(D @system) and $(D @trusted) code.
 * It is designed to fit into two 64 bit registers, again for high performance use.
 * If used incorrectly, memory leaks and corruption can result. Be sure to use
 * $(D scope(exit) textbuf.free();) for proper cleanup, and do not refer to a ScopeBuffer
 * instance's contents after $(D ScopeBuffer.free()) has been called.
 *
 * The `realloc` parameter defaults to C's `realloc()`. Another can be supplied to override it.
 *
 * ScopeBuffer instances may be copied, as in:
---
textbuf = doSomething(textbuf, args);
---
 * which can be very efficent, but these must be regarded as a move rather than a copy.
 * Additionally, the code between passing and returning the instance must not throw
 * exceptions, otherwise when `ScopeBuffer.free()` is called, memory may get corrupted.
 */

@system
struct ScopeBuffer(T, alias realloc = /*core.stdc.stdlib*/.realloc)
if (isAssignable!T &&
    !hasElaborateDestructor!T &&
    !hasElaborateCopyConstructor!T &&
    !hasElaborateAssign!T)
{
    import core.exception : onOutOfMemoryError;
    import core.stdc.string : memcpy;


    /**************************
     * Initialize with buf to use as scratch buffer space.
     * Params:
     *  buf = Scratch buffer space, must have length that is even
     * Example:
     * ---
     * ubyte[10] tmpbuf = void;
     * auto sbuf = ScopeBuffer!ubyte(tmpbuf);
     * ---
     * Note:
     * If buf was created by the same `realloc` passed as a parameter
     * to `ScopeBuffer`, then the contents of `ScopeBuffer` can be extracted without needing
     * to copy them, and `ScopeBuffer.free()` will not need to be called.
     */
    this(T[] buf)
        in
        {
            assert(!(buf.length & wasResized));    // assure even length of scratch buffer space
            assert(buf.length <= uint.max);     // because we cast to uint later
        }
    body
    {
        this.buf = buf.ptr;
        this.bufLen = cast(uint) buf.length;
    }

    @system unittest
    {
        ubyte[10] tmpbuf = void;
        auto sbuf = ScopeBuffer!ubyte(tmpbuf);
    }

    /**************************
     * Releases any memory used.
     * This will invalidate any references returned by the `[]` operator.
     * A destructor is not used, because that would make it not POD
     * (Plain Old Data) and it could not be placed in registers.
     */
    void free()
    {
        debug(ScopeBuffer) buf[0 .. bufLen] = 0;
        if (bufLen & wasResized)
            realloc(buf, 0);
        buf = null;
        bufLen = 0;
        used = 0;
    }

    /************************
     * Append element c to the buffer.
     * This member function makes `ScopeBuffer` an Output Range.
     */
    void put(T c)
    {
        /* j will get enregistered, while used will not because resize() may change used
         */
        const j = used;
        if (j == bufLen)
        {
            assert(j <= (uint.max - 16) / 2);
            resize(j * 2 + 16);
        }
        buf[j] = c;
        used = j + 1;
    }

    /************************
     * Append array s to the buffer.
     *
     * If $(D const(T)) can be converted to $(D T), then put will accept
     * $(D const(T)[]) as input. It will accept a $(D T[]) otherwise.
     */
    package alias CT = Select!(is(const(T) : T), const(T), T);
    /// ditto
    void put(CT[] s)
    {
        const newlen = used + s.length;
        assert((cast(ulong) used + s.length) <= uint.max);
        const len = bufLen;
        if (newlen > len)
        {
            assert(len <= uint.max / 2);
            resize(newlen <= len * 2 ? len * 2 : newlen);
        }
        buf[used .. newlen] = s[];
        used = cast(uint) newlen;
    }

    /******
     * Returns:
     *  A slice into the temporary buffer.
     * Warning:
     *  The result is only valid until the next `put()` or `ScopeBuffer` goes out of scope.
     */
    @system inout(T)[] opSlice(size_t lower, size_t upper) inout
        in
        {
            assert(lower <= bufLen);
            assert(upper <= bufLen);
            assert(lower <= upper);
        }
    body
    {
        return buf[lower .. upper];
    }

    /// ditto
    @system inout(T)[] opSlice() inout
    {
        assert(used <= bufLen);
        return buf[0 .. used];
    }

    /*******
     * Returns:
     *  The element at index i.
     */
    ref inout(T) opIndex(size_t i) inout
    {
        assert(i < bufLen);
        return buf[i];
    }

    /***
     * Returns:
     *  The number of elements in the `ScopeBuffer`.
     */
    @property size_t length() const
    {
        return used;
    }

    /***
     * Used to shrink the length of the buffer,
     * typically to `0` so the buffer can be reused.
     * Cannot be used to extend the length of the buffer.
     */
    @property void length(size_t i)
        in
        {
            assert(i <= this.used);
        }
    body
    {
        this.used = cast(uint) i;
    }

    alias opDollar = length;

  private:
    T* buf;
    // Using uint instead of size_t so the struct fits in 2 registers in 64 bit code
    uint bufLen;
    enum wasResized = 1;         // this bit is set in bufLen if we control the memory
    uint used;

    void resize(size_t newsize)
        in
        {
            assert(newsize <= uint.max);
        }
    body
    {
        //writefln("%s: oldsize %s newsize %s", id, buf.length, newsize);
        newsize |= wasResized;
        void *newBuf = realloc((bufLen & wasResized) ? buf : null, newsize * T.sizeof);
        if (!newBuf)
            onOutOfMemoryError();
        if (!(bufLen & wasResized))
        {
            memcpy(newBuf, buf, used * T.sizeof);
            debug(ScopeBuffer) buf[0 .. bufLen] = 0;
        }
        buf = cast(T*) newBuf;
        bufLen = cast(uint) newsize;

        /* This function is called only rarely,
         * inlining results in poorer register allocation.
         */
        version (DigitalMars)
            /* With dmd, a fake loop will prevent inlining.
             * Using a hack until a language enhancement is implemented.
             */
            while (1) { break; }
    }
}

@system unittest
{
    import core.stdc.stdio;
    import std.range;

    char[2] tmpbuf = void;
    {
    // Exercise all the lines of code except for assert(0)'s
    auto textbuf = ScopeBuffer!char(tmpbuf);
    scope(exit) textbuf.free();

    static assert(isOutputRange!(ScopeBuffer!char, char));

    textbuf.put('a');
    textbuf.put('x');
    textbuf.put("abc");         // tickle put([])'s resize
    assert(textbuf.length == 5);
    assert(textbuf[1 .. 3] == "xa");
    assert(textbuf[3] == 'b');

    textbuf.length = textbuf.length - 1;
    assert(textbuf[0 .. textbuf.length] == "axab");

    textbuf.length = 3;
    assert(textbuf[0 .. textbuf.length] == "axa");
    assert(textbuf[textbuf.length - 1] == 'a');
    assert(textbuf[1 .. 3] == "xa");

    textbuf.put(cast(dchar)'z');
    assert(textbuf[] == "axaz");

    textbuf.length = 0;                 // reset for reuse
    assert(textbuf.length == 0);

    foreach (char c; "asdf;lasdlfaklsdjfalksdjfa;lksdjflkajsfdasdfkja;sdlfj")
    {
        textbuf.put(c); // tickle put(c)'s resize
    }
    assert(textbuf[] == "asdf;lasdlfaklsdjfalksdjfa;lksdjflkajsfdasdfkja;sdlfj");
    } // run destructor on textbuf here

}

@system unittest
{
    string cat(string s1, string s2)
    {
        char[10] tmpbuf = void;
        auto textbuf = ScopeBuffer!char(tmpbuf);
        scope(exit) textbuf.free();
        textbuf.put(s1);
        textbuf.put(s2);
        textbuf.put("even more");
        return textbuf[].idup;
    }

    auto s = cat("hello", "betty");
    assert(s == "hellobettyeven more");
}

// const
@system unittest
{
    char[10] tmpbuf = void;
    auto textbuf = ScopeBuffer!char(tmpbuf);
    scope(exit) textbuf.free();
    foreach (i; 0 .. 10) textbuf.put('w');
    const csb = textbuf;
    const elem = csb[3];
    const slice0 = csb[0 .. 5];
    const slice1 = csb[];
}

/*********************************
 * Creates a `ScopeBuffer` instance using type deduction - see
 * $(LREF .ScopeBuffer.this) for details.
 * Params:
 *      tmpbuf = the initial buffer to use
 * Returns:
 *      An instance of `ScopeBuffer`.
 */

auto scopeBuffer(T)(T[] tmpbuf)
{
    return ScopeBuffer!T(tmpbuf);
}

///
@system unittest
{
    ubyte[10] tmpbuf = void;
    auto sb = scopeBuffer(tmpbuf);
    scope(exit) sb.free();
}

@system unittest
{
    ScopeBuffer!(int*) b;
    int*[] s;
    b.put(s);

    ScopeBuffer!char c;
    string s1;
    char[] s2;
    c.put(s1);
    c.put(s2);
}
