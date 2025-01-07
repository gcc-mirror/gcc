/**
 * This module describes the digest APIs used in Phobos. All digests follow
 * these APIs. Additionally, this module contains useful helper methods which
 * can be used with every digest type.
 *
$(SCRIPT inhibitQuickIndex = 1;)

$(DIVC quickindex,
$(BOOKTABLE ,
$(TR $(TH Category) $(TH Functions)
)
$(TR $(TDNW Template API) $(TD $(MYREF isDigest) $(MYREF DigestType) $(MYREF hasPeek)
  $(MYREF hasBlockSize)
  $(MYREF ExampleDigest) $(MYREF digest) $(MYREF hexDigest) $(MYREF makeDigest)
)
)
$(TR $(TDNW OOP API) $(TD $(MYREF Digest)
)
)
$(TR $(TDNW Helper functions) $(TD $(MYREF toHexString) $(MYREF secureEqual))
)
$(TR $(TDNW Implementation helpers) $(TD $(MYREF digestLength) $(MYREF WrapperDigest))
)
)
)

 * APIs:
 * There are two APIs for digests: The template API and the OOP API. The template API uses structs
 * and template helpers like $(LREF isDigest). The OOP API implements digests as classes inheriting
 * the $(LREF Digest) interface. All digests are named so that the template API struct is called "$(B x)"
 * and the OOP API class is called "$(B x)Digest". For example we have `MD5` <--> `MD5Digest`,
 * `CRC32` <--> `CRC32Digest`, etc.
 *
 * The template API is slightly more efficient. It does not have to allocate memory dynamically,
 * all memory is allocated on the stack. The OOP API has to allocate in the finish method if no
 * buffer was provided. If you provide a buffer to the OOP APIs finish function, it doesn't allocate,
 * but the $(LREF Digest) classes still have to be created using `new` which allocates them using the GC.
 *
 * The OOP API is useful to change the digest function and/or digest backend at 'runtime'. The benefit here
 * is that switching e.g. Phobos MD5Digest and an OpenSSLMD5Digest implementation is ABI compatible.
 *
 * If just one specific digest type and backend is needed, the template API is usually a good fit.
 * In this simplest case, the template API can even be used without templates: Just use the "$(B x)" structs
 * directly.
 *
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:
 * Johannes Pfau
 *
 * Source:    $(PHOBOSSRC std/digest/package.d)
 *
 * CTFE:
 * Digests do not work in CTFE
 *
 * TODO:
 * Digesting single bits (as opposed to bytes) is not implemented. This will be done as another
 * template constraint helper (hasBitDigesting!T) and an additional interface (BitDigest)
 */
/*          Copyright Johannes Pfau 2012.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.digest;

public import std.ascii : LetterCase;
import std.meta : allSatisfy;
import std.range.primitives;
import std.traits;


///
@system unittest
{
    import std.digest.crc;

    //Simple example
    char[8] hexHash = hexDigest!CRC32("The quick brown fox jumps over the lazy dog");
    assert(hexHash == "39A34F41");

    //Simple example, using the API manually
    CRC32 context = makeDigest!CRC32();
    context.put(cast(ubyte[])"The quick brown fox jumps over the lazy dog");
    ubyte[4] hash = context.finish();
    assert(toHexString(hash) == "39A34F41");
}

///
@system unittest
{
    //Generating the hashes of a file, idiomatic D way
    import std.digest.crc, std.digest.md, std.digest.sha;
    import std.stdio;

    // Digests a file and prints the result.
    void digestFile(Hash)(string filename)
    if (isDigest!Hash)
    {
        auto file = File(filename);
        auto result = digest!Hash(file.byChunk(4096 * 1024));
        writefln("%s (%s) = %s", Hash.stringof, filename, toHexString(result));
    }

    void main(string[] args)
    {
        foreach (name; args[1 .. $])
        {
            digestFile!MD5(name);
            digestFile!SHA1(name);
            digestFile!CRC32(name);
        }
    }
}
///
@system unittest
{
    //Generating the hashes of a file using the template API
    import std.digest.crc, std.digest.md, std.digest.sha;
    import std.stdio;
    // Digests a file and prints the result.
    void digestFile(Hash)(ref Hash hash, string filename)
    if (isDigest!Hash)
    {
        File file = File(filename);

        //As digests imlement OutputRange, we could use std.algorithm.copy
        //Let's do it manually for now
        foreach (buffer; file.byChunk(4096 * 1024))
            hash.put(buffer);

        auto result = hash.finish();
        writefln("%s (%s) = %s", Hash.stringof, filename, toHexString(result));
    }

    void uMain(string[] args)
    {
        MD5 md5;
        SHA1 sha1;
        CRC32 crc32;

        md5.start();
        sha1.start();
        crc32.start();

        foreach (arg; args[1 .. $])
        {
            digestFile(md5, arg);
            digestFile(sha1, arg);
            digestFile(crc32, arg);
        }
    }
}

///
@system unittest
{
    import std.digest.crc, std.digest.md, std.digest.sha;
    import std.stdio;

    // Digests a file and prints the result.
    void digestFile(Digest hash, string filename)
    {
        File file = File(filename);

        //As digests implement OutputRange, we could use std.algorithm.copy
        //Let's do it manually for now
        foreach (buffer; file.byChunk(4096 * 1024))
          hash.put(buffer);

        ubyte[] result = hash.finish();
        writefln("%s (%s) = %s", typeid(hash).toString(), filename, toHexString(result));
    }

    void umain(string[] args)
    {
        auto md5 = new MD5Digest();
        auto sha1 = new SHA1Digest();
        auto crc32 = new CRC32Digest();

        foreach (arg; args[1 .. $])
        {
          digestFile(md5, arg);
          digestFile(sha1, arg);
          digestFile(crc32, arg);
        }
    }
}

version (StdDdoc)
    version = ExampleDigest;

version (ExampleDigest)
{
    /**
     * This documents the general structure of a Digest in the template API.
     * All digest implementations should implement the following members and therefore pass
     * the $(LREF isDigest) test.
     *
     * Note:
     * $(UL
     * $(LI A digest must be a struct (value type) to pass the $(LREF isDigest) test.)
     * $(LI A digest passing the $(LREF isDigest) test is always an `OutputRange`)
     * )
     */
    struct ExampleDigest
    {
        public:
            /**
             * Use this to feed the digest with data.
             * Also implements the $(REF isOutputRange, std,range,primitives)
             * interface for `ubyte` and `const(ubyte)[]`.
             * The following usages of `put` must work for any type which
             * passes $(LREF isDigest):
             * Example:
             * ----
             * ExampleDigest dig;
             * dig.put(cast(ubyte) 0); //single ubyte
             * dig.put(cast(ubyte) 0, cast(ubyte) 0); //variadic
             * ubyte[10] buf;
             * dig.put(buf); //buffer
             * ----
             */
            @trusted void put(scope const(ubyte)[] data...)
            {

            }

            /**
             * This function is used to (re)initialize the digest.
             * It must be called before using the digest and it also works as a 'reset' function
             * if the digest has already processed data.
             */
            @trusted void start()
            {

            }

            /**
             * The finish function returns the final hash sum and resets the Digest.
             *
             * Note:
             * The actual type returned by finish depends on the digest implementation.
             * `ubyte[16]` is just used as an example. It is guaranteed that the type is a
             * static array of ubytes.
             *
             * $(UL
             * $(LI Use $(LREF DigestType) to obtain the actual return type.)
             * $(LI Use $(LREF digestLength) to obtain the length of the ubyte array.)
             * )
             */
            @trusted ubyte[16] finish()
            {
                return (ubyte[16]).init;
            }
    }
}

///
@system unittest
{
    //Using the OutputRange feature
    import std.algorithm.mutation : copy;
    import std.digest.md;
    import std.range : repeat;

    auto oneMillionRange = repeat!ubyte(cast(ubyte)'a', 1000000);
    auto ctx = makeDigest!MD5();
    copy(oneMillionRange, &ctx); //Note: You must pass a pointer to copy!
    assert(ctx.finish().toHexString() == "7707D6AE4E027C70EEA2A935C2296F21");
}

/**
 * Use this to check if a type is a digest. See $(LREF ExampleDigest) to see what
 * a type must provide to pass this check.
 *
 * Note:
 * This is very useful as a template constraint (see examples)
 *
 * BUGS:
 * $(UL
 * $(LI Does not yet verify that put takes scope parameters.)
 * $(LI Should check that finish() returns a ubyte[num] array)
 * )
 */
template isDigest(T)
{
    import std.range : isOutputRange;
    enum bool isDigest = isOutputRange!(T, const(ubyte)[]) && isOutputRange!(T, ubyte) &&
        is(T == struct) &&
        is(typeof(
        {
            T dig = void; //Can define
            dig.put(cast(ubyte) 0, cast(ubyte) 0); //varags
            dig.start(); //has start
            auto value = dig.finish(); //has finish
        }));
}

///
@system unittest
{
    import std.digest.crc;
    static assert(isDigest!CRC32);
}
///
@system unittest
{
    import std.digest.crc;
    void myFunction(T)()
    if (isDigest!T)
    {
        T dig;
        dig.start();
        auto result = dig.finish();
    }
    myFunction!CRC32();
}

/**
 * Use this template to get the type which is returned by a digest's $(LREF finish) method.
 */
template DigestType(T)
{
    static if (isDigest!T)
    {
        alias DigestType =
            ReturnType!(typeof(
            {
                T dig = void;
                return dig.finish();
            }));
    }
    else
        static assert(false, T.stringof ~ " is not a digest! (fails isDigest!T)");
}

///
@system unittest
{
    import std.digest.crc;
    assert(is(DigestType!(CRC32) == ubyte[4]));
}
///
@system unittest
{
    import std.digest.crc;
    CRC32 dig;
    dig.start();
    DigestType!CRC32 result = dig.finish();
}

/**
 * Used to check if a digest supports the `peek` method.
 * Peek has exactly the same function signatures as finish, but it doesn't reset
 * the digest's internal state.
 *
 * Note:
 * $(UL
 * $(LI This is very useful as a template constraint (see examples))
 * $(LI This also checks if T passes $(LREF isDigest))
 * )
 */
template hasPeek(T)
{
    enum bool hasPeek = isDigest!T &&
        is(typeof(
        {
            T dig = void; //Can define
            DigestType!T val = dig.peek();
        }));
}

///
@system unittest
{
    import std.digest.crc, std.digest.md;
    assert(!hasPeek!(MD5));
    assert(hasPeek!CRC32);
}
///
@system unittest
{
    import std.digest.crc;
    void myFunction(T)()
    if (hasPeek!T)
    {
        T dig;
        dig.start();
        auto result = dig.peek();
    }
    myFunction!CRC32();
}

/**
 * Checks whether the digest has a `blockSize` member, which contains the
 * digest's internal block size in bits. It is primarily used by $(REF HMAC, std,digest,hmac).
 */

template hasBlockSize(T)
if (isDigest!T)
{
    enum bool hasBlockSize = __traits(compiles, { size_t blockSize = T.blockSize; });
}

///
@system unittest
{
    import std.digest.hmac, std.digest.md;
    static assert(hasBlockSize!MD5        && MD5.blockSize      == 512);
    static assert(hasBlockSize!(HMAC!MD5) && HMAC!MD5.blockSize == 512);
}

package template isDigestibleRange(Range)
{
    import std.digest.md;
    import std.range : isInputRange, ElementType;
    enum bool isDigestibleRange = isInputRange!Range && is(typeof(
          {
          MD5 ha; //Could use any conformant hash
          ElementType!Range val;
          ha.put(val);
          }));
}

/**
 * This is a convenience function to calculate a hash using the template API.
 * Every digest passing the $(LREF isDigest) test can be used with this function.
 *
 * Params:
 *  range= an `InputRange` with `ElementType` `ubyte`, `ubyte[]` or `ubyte[num]`
 */
DigestType!Hash digest(Hash, Range)(auto ref Range range)
if (!isArray!Range
    && isDigestibleRange!Range)
{
    Hash hash;
    hash.start();
    alias E = ElementType!Range; // Not necessarily ubyte. Could be ubyte[N] or ubyte[] or something w/alias this.
    static if (!(__traits(isScalar, E) && E.sizeof == 1))
    {
        foreach (e; range)
            hash.put(e);
        return hash.finish();
    }
    else
    {
        static if (hasBlockSize!Hash)
            enum bufferBytes = Hash.blockSize >= (8192 * 8) ? 8192 : Hash.blockSize <= 64 ? 8 : (Hash.blockSize / 8);
        else
            enum bufferBytes = 8;
        ubyte[bufferBytes] buffer = void;
        static if (isRandomAccessRange!Range && hasLength!Range)
        {
            const end = range.length;
            size_t i = 0;
            while (end - i >= buffer.length)
            {
                foreach (ref e; buffer)
                    e = range[i++];
                hash.put(buffer);
            }
            if (const remaining = end - i)
            {
                foreach (ref e; buffer[0 .. remaining])
                    e = range[i++];
                hash.put(buffer[0 .. remaining]);
            }
            return hash.finish();
        }
        else
        {
            for (;;)
            {
                size_t n = buffer.length;
                foreach (i, ref e; buffer)
                {
                    if (range.empty)
                    {
                        n = i;
                        break;
                    }
                    e = range.front;
                    range.popFront();
                }
                if (n)
                    hash.put(buffer[0 .. n]);
                if (n != buffer.length)
                    return hash.finish();
            }
        }
    }
}

///
@system unittest
{
    import std.digest.md;
    import std.range : repeat;
    auto testRange = repeat!ubyte(cast(ubyte)'a', 100);
    auto md5 = digest!MD5(testRange);
}

/**
 * This overload of the digest function handles arrays.
 *
 * Params:
 *  data= one or more arrays of any type
 */
DigestType!Hash digest(Hash, T...)(scope const T data)
if (allSatisfy!(isArray, typeof(data)))
{
    Hash hash;
    hash.start();
    foreach (datum; data)
        hash.put(cast(const(ubyte[]))datum);
    return hash.finish();
}

///
@system unittest
{
    import std.digest.crc, std.digest.md, std.digest.sha;
    auto md5   = digest!MD5(  "The quick brown fox jumps over the lazy dog");
    auto sha1  = digest!SHA1( "The quick brown fox jumps over the lazy dog");
    auto crc32 = digest!CRC32("The quick brown fox jumps over the lazy dog");
    assert(toHexString(crc32) == "39A34F41");
}

///
@system unittest
{
    import std.digest.crc;
    auto crc32 = digest!CRC32("The quick ", "brown ", "fox jumps over the lazy dog");
    assert(toHexString(crc32) == "39A34F41");
}

/**
 * This is a convenience function similar to $(LREF digest), but it returns the string
 * representation of the hash. Every digest passing the $(LREF isDigest) test can be used with this
 * function.
 *
 * Params:
 *  order= the order in which the bytes are processed (see $(LREF toHexString))
 *  range= an `InputRange` with `ElementType` `ubyte`, `ubyte[]` or `ubyte[num]`
 */
char[digestLength!(Hash)*2] hexDigest(Hash, Order order = Order.increasing, Range)(ref Range range)
if (!isArray!Range && isDigestibleRange!Range)
{
    return toHexString!order(digest!Hash(range));
}

///
@system unittest
{
    import std.digest.md;
    import std.range : repeat;
    auto testRange = repeat!ubyte(cast(ubyte)'a', 100);
    assert(hexDigest!MD5(testRange) == "36A92CC94A9E0FA21F625F8BFB007ADF");
}

/**
 * This overload of the hexDigest function handles arrays.
 *
 * Params:
 *  order= the order in which the bytes are processed (see $(LREF toHexString))
 *  data= one or more arrays of any type
 */
char[digestLength!(Hash)*2] hexDigest(Hash, Order order = Order.increasing, T...)(scope const T data)
if (allSatisfy!(isArray, typeof(data)))
{
    return toHexString!order(digest!Hash(data));
}

///
@system unittest
{
    import std.digest.crc;
    assert(hexDigest!(CRC32, Order.decreasing)("The quick brown fox jumps over the lazy dog") == "414FA339");
}
///
@system unittest
{
    import std.digest.crc;
    assert(hexDigest!(CRC32, Order.decreasing)("The quick ", "brown ", "fox jumps over the lazy dog") == "414FA339");
}

/**
 * This is a convenience function which returns an initialized digest, so it's not necessary to call
 * start manually.
 */
Hash makeDigest(Hash)()
{
    Hash hash;
    hash.start();
    return hash;
}

///
@system unittest
{
    import std.digest.md;
    auto md5 = makeDigest!MD5();
    md5.put(0);
    assert(toHexString(md5.finish()) == "93B885ADFE0DA089CDF634904FD59F71");
}

/*+*************************** End of template part, welcome to OOP land **************************/

/**
 * This describes the OOP API. To understand when to use the template API and when to use the OOP API,
 * see the module documentation at the top of this page.
 *
 * The Digest interface is the base interface which is implemented by all digests.
 *
 * Note:
 * A Digest implementation is always an `OutputRange`
 */
interface Digest
{
    public:
        /**
         * Use this to feed the digest with data.
         * Also implements the $(REF isOutputRange, std,range,primitives)
         * interface for `ubyte` and `const(ubyte)[]`.
         *
         * Example:
         * ----
         * void test(Digest dig)
         * {
         *     dig.put(cast(ubyte) 0); //single ubyte
         *     dig.put(cast(ubyte) 0, cast(ubyte) 0); //variadic
         *     ubyte[10] buf;
         *     dig.put(buf); //buffer
         * }
         * ----
         */
        @trusted nothrow void put(scope const(ubyte)[] data...);

        /**
         * Resets the internal state of the digest.
         * Note:
         * $(LREF finish) calls this internally, so it's not necessary to call
         * `reset` manually after a call to $(LREF finish).
         */
        @trusted nothrow void reset();

        /**
         * This is the length in bytes of the hash value which is returned by $(LREF finish).
         * It's also the required size of a buffer passed to $(LREF finish).
         */
        @trusted nothrow @property size_t length() const;

        /**
         * The finish function returns the hash value. It takes an optional buffer to copy the data
         * into. If a buffer is passed, it must be at least $(LREF length) bytes big.
         */
        @trusted nothrow ubyte[] finish();
        ///ditto
        nothrow ubyte[] finish(ubyte[] buf);
        // https://issues.dlang.org/show_bug.cgi?id=6549
        /*in
        {
            assert(buf.length >= this.length);
        }*/

        /**
         * This is a convenience function to calculate the hash of a value using the OOP API.
         */
        final @trusted nothrow ubyte[] digest(scope const(void[])[] data...)
        {
            this.reset();
            foreach (datum; data)
                this.put(cast(ubyte[]) datum);
            return this.finish();
        }
}

///
@system unittest
{
    //Using the OutputRange feature
    import std.algorithm.mutation : copy;
    import std.digest.md;
    import std.range : repeat;

    auto oneMillionRange = repeat!ubyte(cast(ubyte)'a', 1000000);
    auto ctx = new MD5Digest();
    copy(oneMillionRange, ctx);
    assert(ctx.finish().toHexString() == "7707D6AE4E027C70EEA2A935C2296F21");
}

///
@system unittest
{
    import std.digest.crc, std.digest.md, std.digest.sha;
    ubyte[] md5   = (new MD5Digest()).digest("The quick brown fox jumps over the lazy dog");
    ubyte[] sha1  = (new SHA1Digest()).digest("The quick brown fox jumps over the lazy dog");
    ubyte[] crc32 = (new CRC32Digest()).digest("The quick brown fox jumps over the lazy dog");
    assert(crcHexString(crc32) == "414FA339");
}

///
@system unittest
{
    import std.digest.crc;
    ubyte[] crc32 = (new CRC32Digest()).digest("The quick ", "brown ", "fox jumps over the lazy dog");
    assert(crcHexString(crc32) == "414FA339");
}

@system unittest
{
    import std.range : isOutputRange;
    assert(!isDigest!(Digest));
    assert(isOutputRange!(Digest, ubyte));
}

///
@system unittest
{
    void test(Digest dig)
    {
        dig.put(cast(ubyte) 0); //single ubyte
        dig.put(cast(ubyte) 0, cast(ubyte) 0); //variadic
        ubyte[10] buf;
        dig.put(buf); //buffer
    }
}

/*+*************************** End of OOP part, helper functions follow ***************************/

/**
 * See $(LREF toHexString)
 */
enum Order : bool
{
    increasing, ///
    decreasing ///
}

///
@safe unittest
{
    import std.digest.crc : CRC32;

    auto crc32 = digest!CRC32("The quick ", "brown ", "fox jumps over the lazy dog");
    assert(crc32.toHexString!(Order.decreasing) == "414FA339");
    assert(crc32.toHexString!(LetterCase.lower, Order.decreasing) == "414fa339");
}


/**
 * Used to convert a hash value (a static or dynamic array of ubytes) to a string.
 * Can be used with the OOP and with the template API.
 *
 * The additional order parameter can be used to specify the order of the input data.
 * By default the data is processed in increasing order, starting at index 0. To process it in the
 * opposite order, pass Order.decreasing as a parameter.
 *
 * The additional letterCase parameter can be used to specify the case of the output data.
 * By default the output is in upper case. To change it to the lower case
 * pass LetterCase.lower as a parameter.
 *
 * Note:
 * The function overloads returning a string allocate their return values
 * using the GC. The versions returning static arrays use pass-by-value for
 * the return value, effectively avoiding dynamic allocation.
 */
char[num*2] toHexString(Order order = Order.increasing, size_t num, LetterCase letterCase = LetterCase.upper)
(const ubyte[num] digest)
{

    char[num*2] result;
    size_t i;
    toHexStringImpl!(order, letterCase)(digest, result);
    return result;
}

///ditto
char[num*2] toHexString(LetterCase letterCase, Order order = Order.increasing, size_t num)(in ubyte[num] digest)
{
    return toHexString!(order, num, letterCase)(digest);
}

///ditto
string toHexString(Order order = Order.increasing, LetterCase letterCase = LetterCase.upper)
(in ubyte[] digest)
{
    auto result = new char[digest.length*2];
    toHexStringImpl!(order, letterCase)(digest, result);
    import std.exception : assumeUnique;
    // memory was just created, so casting to immutable is safe
    return () @trusted { return assumeUnique(result); }();
}

///ditto
string toHexString(LetterCase letterCase, Order order = Order.increasing)(in ubyte[] digest)
{
    return toHexString!(order, letterCase)(digest);
}

//For more example unittests, see Digest.digest, digest

///
@safe unittest
{
    import std.digest.crc;
    //Test with template API:
    auto crc32 = digest!CRC32("The quick ", "brown ", "fox jumps over the lazy dog");
    //Lower case variant:
    assert(toHexString!(LetterCase.lower)(crc32) == "39a34f41");
    //Usually CRCs are printed in this order, though:
    assert(toHexString!(Order.decreasing)(crc32) == "414FA339");
    assert(toHexString!(LetterCase.lower, Order.decreasing)(crc32) == "414fa339");
}

///
@safe unittest
{
    import std.digest.crc;
    // With OOP API
    auto crc32 = (new CRC32Digest()).digest("The quick ", "brown ", "fox jumps over the lazy dog");
    //Usually CRCs are printed in this order, though:
    assert(toHexString!(Order.decreasing)(crc32) == "414FA339");
}

@safe unittest
{
    ubyte[16] data;
    assert(toHexString(data) == "00000000000000000000000000000000");

    assert(toHexString(cast(ubyte[4])[42, 43, 44, 45]) == "2A2B2C2D");
    assert(toHexString(cast(ubyte[])[42, 43, 44, 45]) == "2A2B2C2D");
    assert(toHexString!(Order.decreasing)(cast(ubyte[4])[42, 43, 44, 45]) == "2D2C2B2A");
    assert(toHexString!(Order.decreasing, LetterCase.lower)(cast(ubyte[4])[42, 43, 44, 45]) == "2d2c2b2a");
    assert(toHexString!(Order.decreasing)(cast(ubyte[])[42, 43, 44, 45]) == "2D2C2B2A");
}

/*+*********************** End of public helper part, private helpers follow ***********************/

/*
 * Used to convert from a ubyte[] slice to a ref ubyte[N].
 * This helper is used internally in the WrapperDigest template to wrap the template API's
 * finish function.
 */
ref T[N] asArray(size_t N, T)(ref T[] source, string errorMsg = "")
{
     assert(source.length >= N, errorMsg);
     return *cast(T[N]*) source.ptr;
}

/*
 * Fill in a preallocated buffer with the ASCII hex representation from a byte buffer
 */
private void toHexStringImpl(Order order, LetterCase letterCase, BB, HB)
(scope const ref BB byteBuffer, ref HB hexBuffer){
    static if (letterCase == LetterCase.upper)
    {
        import std.ascii : hexDigits = hexDigits;
    }
    else
    {
        import std.ascii : hexDigits = lowerHexDigits;
    }

    size_t i;
    static if (order == Order.increasing)
    {
        foreach (u; byteBuffer)
        {
            hexBuffer[i++] = hexDigits[u >> 4];
            hexBuffer[i++] = hexDigits[u & 15];
        }
    }
    else
    {
        size_t j = byteBuffer.length -1;
        while (i < byteBuffer.length*2)
        {
            hexBuffer[i++] = hexDigits[byteBuffer[j] >> 4];
            hexBuffer[i++] = hexDigits[byteBuffer[j] & 15];
            j--;
        }
    }
}


/*
 * Returns the length (in bytes) of the hash value produced by T.
 */
template digestLength(T)
if (isDigest!T)
{
    enum size_t digestLength = (ReturnType!(T.finish)).length;
}

@safe pure nothrow @nogc
unittest
{
    import std.digest.md : MD5;
    import std.digest.sha : SHA1, SHA256, SHA512;
    assert(digestLength!MD5 == 16);
    assert(digestLength!SHA1 == 20);
    assert(digestLength!SHA256 == 32);
    assert(digestLength!SHA512 == 64);
}

/**
 * Wraps a template API hash struct into a Digest interface.
 * Modules providing digest implementations will usually provide
 * an alias for this template (e.g. MD5Digest, SHA1Digest, ...).
 */
class WrapperDigest(T)
if (isDigest!T) : Digest
{
    protected:
        T _digest;

    public final:
        /**
         * Initializes the digest.
         */
        this()
        {
            _digest.start();
        }

        /**
         * Use this to feed the digest with data.
         * Also implements the $(REF isOutputRange, std,range,primitives)
         * interface for `ubyte` and `const(ubyte)[]`.
         */
        @trusted nothrow void put(scope const(ubyte)[] data...)
        {
            _digest.put(data);
        }

        /**
         * Resets the internal state of the digest.
         * Note:
         * $(LREF finish) calls this internally, so it's not necessary to call
         * `reset` manually after a call to $(LREF finish).
         */
        @trusted nothrow void reset()
        {
            _digest.start();
        }

        /**
         * This is the length in bytes of the hash value which is returned by $(LREF finish).
         * It's also the required size of a buffer passed to $(LREF finish).
         */
        @trusted nothrow @property size_t length() const pure
        {
            return digestLength!T;
        }

        /**
         * The finish function returns the hash value. It takes an optional buffer to copy the data
         * into. If a buffer is passed, it must have a length at least $(LREF length) bytes.
         *
         * Example:
         * --------
         *
         * import std.digest.md;
         * ubyte[16] buf;
         * auto hash = new WrapperDigest!MD5();
         * hash.put(cast(ubyte) 0);
         * auto result = hash.finish(buf[]);
         * //The result is now in result (and in buf). If you pass a buffer which is bigger than
         * //necessary, result will have the correct length, but buf will still have it's original
         * //length
         * --------
         */
        nothrow ubyte[] finish(ubyte[] buf)
        in
        {
            assert(buf.length >= this.length, "Given buffer is smaller than the local buffer.");
        }
        do
        {
            enum string msg = "Buffer needs to be at least " ~ digestLength!(T).stringof ~ " bytes " ~
                "big, check " ~ typeof(this).stringof ~ ".length!";
            asArray!(digestLength!T)(buf, msg) = _digest.finish();
            return buf[0 .. digestLength!T];
        }

        ///ditto
        @trusted nothrow ubyte[] finish()
        {
            enum len = digestLength!T;
            auto buf = new ubyte[len];
            asArray!(digestLength!T)(buf) = _digest.finish();
            return buf;
        }

        version (StdDdoc)
        {
            /**
             * Works like `finish` but does not reset the internal state, so it's possible
             * to continue putting data into this WrapperDigest after a call to peek.
             *
             * These functions are only available if `hasPeek!T` is true.
             */
            @trusted ubyte[] peek(ubyte[] buf) const;
            ///ditto
            @trusted ubyte[] peek() const;
        }
        else static if (hasPeek!T)
        {
            @trusted ubyte[] peek(ubyte[] buf) const
            in
            {
                assert(buf.length >= this.length, "Given buffer is smaller than the local buffer.");
            }
            do
            {
                enum string msg = "Buffer needs to be at least " ~ digestLength!(T).stringof ~ " bytes " ~
                    "big, check " ~ typeof(this).stringof ~ ".length!";
                asArray!(digestLength!T)(buf, msg) = _digest.peek();
                return buf[0 .. digestLength!T];
            }

            @trusted ubyte[] peek() const
            {
                enum len = digestLength!T;
                auto buf = new ubyte[len];
                asArray!(digestLength!T)(buf) = _digest.peek();
                return buf;
            }
        }
}

///
@system unittest
{
    import std.digest.md;
    //Simple example
    auto hash = new WrapperDigest!MD5();
    hash.put(cast(ubyte) 0);
    auto result = hash.finish();
}

///
@system unittest
{
    //using a supplied buffer
    import std.digest.md;
    ubyte[16] buf;
    auto hash = new WrapperDigest!MD5();
    hash.put(cast(ubyte) 0);
    auto result = hash.finish(buf[]);
    //The result is now in result (and in buf). If you pass a buffer which is bigger than
    //necessary, result will have the correct length, but buf will still have it's original
    //length
}

@safe unittest
{
    // Test peek & length
    import std.digest.crc;
    auto hash = new WrapperDigest!CRC32();
    assert(hash.length == 4);
    hash.put(cast(const(ubyte[]))"The quick brown fox jumps over the lazy dog");
    assert(hash.peek().toHexString() == "39A34F41");
    ubyte[5] buf;
    assert(hash.peek(buf).toHexString() == "39A34F41");
}

/**
 * Securely compares two digest representations while protecting against timing
 * attacks. Do not use `==` to compare digest representations.
 *
 * The attack happens as follows:
 *
 * $(OL
 *     $(LI An attacker wants to send harmful data to your server, which
 *     requires a integrity HMAC SHA1 token signed with a secret.)
 *     $(LI The length of the token is known to be 40 characters long due to its format,
 *     so the attacker first sends `"0000000000000000000000000000000000000000"`,
 *     then `"1000000000000000000000000000000000000000"`, and so on.)
 *     $(LI The given HMAC token is compared with the expected token using the
 *     `==` string comparison, which returns `false` as soon as the first wrong
 *     element is found. If a wrong element is found, then a rejection is sent
 *     back to the sender.)
 *     $(LI Eventually, the attacker is able to determine the first character in
 *     the correct token because the sever takes slightly longer to return a
 *     rejection. This is due to the comparison moving on to second item in
 *     the two arrays, seeing they are different, and then sending the rejection.)
 *     $(LI It may seem like too small of a difference in time for the attacker
 *     to notice, but security researchers have shown that differences as
 *     small as $(LINK2 http://www.cs.rice.edu/~dwallach/pub/crosby-timing2009.pdf,
 *     20µs can be reliably distinguished) even with network inconsistencies.)
 *     $(LI Repeat the process for each character until the attacker has the whole
 *     correct token and the server accepts the harmful data. This can be done
 *     in a week with the attacker pacing the attack to 10 requests per second
 *     with only one client.)
 * )
 *
 * This function defends against this attack by always comparing every single
 * item in the array if the two arrays are the same length. Therefore, this
 * function is always $(BIGOH n) for ranges of the same length.
 *
 * This attack can also be mitigated via rate limiting and banning IPs which have too
 * many rejected requests. However, this does not completely solve the problem,
 * as the attacker could be in control of a bot net. To fully defend against
 * the timing attack, rate limiting, banning IPs, and using this function
 * should be used together.
 *
 * Params:
 *     r1 = A digest representation
 *     r2 = A digest representation
 * Returns:
 *     `true` if both representations are equal, `false` otherwise
 * See_Also:
 *     $(LINK2 https://en.wikipedia.org/wiki/Timing_attack, The Wikipedia article
 *     on timing attacks).
 */
bool secureEqual(R1, R2)(R1 r1, R2 r2)
if (isInputRange!R1 && isInputRange!R2 && !isInfinite!R1 && !isInfinite!R2 &&
    (isIntegral!(ElementEncodingType!R1) || isSomeChar!(ElementEncodingType!R1)) &&
    !is(CommonType!(ElementEncodingType!R1, ElementEncodingType!R2) == void))
{
    static if (hasLength!R1 && hasLength!R2)
        if (r1.length != r2.length)
            return false;

    int result;

    static if (isRandomAccessRange!R1 && isRandomAccessRange!R2 &&
               hasLength!R1 && hasLength!R2)
    {
        foreach (i; 0 .. r1.length)
            result |= r1[i] ^ r2[i];
    }
    else static if (hasLength!R1 && hasLength!R2)
    {
        // Lengths are the same so we can squeeze out a bit of performance
        // by not checking if r2 is empty
        for (; !r1.empty; r1.popFront(), r2.popFront())
        {
            result |= r1.front ^ r2.front;
        }
    }
    else
    {
        // Generic case, walk both ranges
        for (; !r1.empty; r1.popFront(), r2.popFront())
        {
            if (r2.empty) return false;
            result |= r1.front ^ r2.front;
        }
        if (!r2.empty) return false;
    }

    return result == 0;
}

///
@system pure unittest
{
    import std.digest.hmac : hmac;
    import std.digest.sha : SHA1;
    import std.string : representation;

    // a typical HMAC data integrity verification
    auto secret = "A7GZIP6TAQA6OHM7KZ42KB9303CEY0MOV5DD6NTV".representation;
    auto data = "data".representation;

    auto hex1 = data.hmac!SHA1(secret).toHexString;
    auto hex2 = data.hmac!SHA1(secret).toHexString;
    auto hex3 = "data1".representation.hmac!SHA1(secret).toHexString;

    assert( secureEqual(hex1[], hex2[]));
    assert(!secureEqual(hex1[], hex3[]));
}

@system pure unittest
{
    import std.internal.test.dummyrange : ReferenceInputRange;
    import std.range : takeExactly;
    import std.string : representation;
    import std.utf : byWchar, byDchar;

    {
        auto hex1 = "02CA3484C375EDD3C0F08D3F50D119E61077".representation;
        auto hex2 = "02CA3484C375EDD3C0F08D3F50D119E610779018".representation;
        assert(!secureEqual(hex1, hex2));
    }
    {
        auto hex1 = "02CA3484C375EDD3C0F08D3F50D119E610779018"w.representation;
        auto hex2 = "02CA3484C375EDD3C0F08D3F50D119E610779018"d.representation;
        assert(secureEqual(hex1, hex2));
    }
    {
        auto hex1 = "02CA3484C375EDD3C0F08D3F50D119E610779018".byWchar;
        auto hex2 = "02CA3484C375EDD3C0F08D3F50D119E610779018".byDchar;
        assert(secureEqual(hex1, hex2));
    }
    {
        auto hex1 = "02CA3484C375EDD3C0F08D3F50D119E61077".byWchar;
        auto hex2 = "02CA3484C375EDD3C0F08D3F50D119E610779018".byDchar;
        assert(!secureEqual(hex1, hex2));
    }
    {
        auto hex1 = new ReferenceInputRange!int([0, 1, 2, 3, 4, 5, 6, 7, 8]).takeExactly(9);
        auto hex2 = new ReferenceInputRange!int([0, 1, 2, 3, 4, 5, 6, 7, 8]).takeExactly(9);
        assert(secureEqual(hex1, hex2));
    }
    {
        auto hex1 = new ReferenceInputRange!int([0, 1, 2, 3, 4, 5, 6, 7, 8]).takeExactly(9);
        auto hex2 = new ReferenceInputRange!int([0, 1, 2, 3, 4, 5, 6, 7, 9]).takeExactly(9);
        assert(!secureEqual(hex1, hex2));
    }
}

/**
 * Validates a hex string.
 *
 * Checks whether all characters following an optional "0x" suffix
 * are valid hexadecimal digits.
 *
 * Params:
 *     hex = hexdecimal encoded byte array
 * Returns:
 *     true = if valid
 */
bool isHexString(String)(String hex) @safe pure nothrow @nogc
if (isSomeString!String)
{
    import std.ascii : isHexDigit;

    if ((hex.length >= 2) && (hex[0 .. 2] == "0x"))
    {
        hex = hex[2 .. $];
    }

    foreach (digit; hex)
    {
        if (!digit.isHexDigit)
        {
            return false;
        }
    }

    return true;
}

///
@safe unittest
{
    assert(isHexString("0x0123456789ABCDEFabcdef"));
    assert(isHexString("0123456789ABCDEFabcdef"));
    assert(!isHexString("g"));
    assert(!isHexString("#"));
}

/**
 * Converts a hex text string to a range of bytes.
 *
 * The input to this function MUST be valid.
 * $(REF isHexString, std, digest) can be used to check for this if needed.
 *
 * Params:
 *     hex = String representation of a hexdecimal-encoded byte array.
 * Returns:
 *     A forward range of bytes.
 */
auto fromHexStringAsRange(String)(String hex) @safe pure nothrow @nogc
if (isSomeString!String)
{
    return HexStringDecoder!String(hex);
}

///
@safe unittest
{
    import std.range.primitives : ElementType, isForwardRange;
    import std.traits : ReturnType;

    // The decoder implements a forward range.
    static assert(isForwardRange!(ReturnType!(fromHexStringAsRange!string)));
    static assert(isForwardRange!(ReturnType!(fromHexStringAsRange!wstring)));
    static assert(isForwardRange!(ReturnType!(fromHexStringAsRange!dstring)));

    // The element type of the range is always `ubyte`.
    static assert(
        is(ElementType!(ReturnType!(fromHexStringAsRange!string)) == ubyte)
    );
    static assert(
        is(ElementType!(ReturnType!(fromHexStringAsRange!wstring)) == ubyte)
    );
    static assert(
        is(ElementType!(ReturnType!(fromHexStringAsRange!dstring)) == ubyte)
    );
}

@safe unittest
{
    import std.array : staticArray;

    // `staticArray` consumes the range returned by `fromHexStringAsRange`.
    assert("0x0000ff".fromHexStringAsRange.staticArray!3  == [0, 0, 0xFF]);
    assert("0x0000ff"w.fromHexStringAsRange.staticArray!3 == [0, 0, 0xFF]);
    assert("0x0000ff"d.fromHexStringAsRange.staticArray!3 == [0, 0, 0xFF]);
    assert("0xff12ff".fromHexStringAsRange.staticArray!1  == [0xFF]);
    assert("0x12ff".fromHexStringAsRange.staticArray!2    == [0x12, 255]);
    assert(
        "0x3AaAA".fromHexStringAsRange.staticArray!4 == [0x3, 0xAA, 0xAA, 0x00]
    );
}

/**
 * Converts a hex text string to a range of bytes.
 *
 * Params:
 *     hex = String representation of a hexdecimal-encoded byte array.
 * Returns:
 *     An newly allocated array of bytes.
 * Throws:
 *     Exception on invalid input.
 * Example:
 * ---
 * ubyte[] dby  = "0xBA".fromHexString;
 * ---
 * See_Also:
 *     $(REF fromHexString, std, digest) for a range version of the function.
 */
ubyte[] fromHexString(String)(String hex) @safe pure
if (isSomeString!String)
{
    // This function is trivial, yet necessary for consistency.
    // It provides a similar API to its `toHexString` counterpart.

    if (!hex.isHexString)
    {
        import std.conv : text;

        throw new Exception(
            "The provided character sequence `"
                ~ hex.text
                ~ "` is not a valid hex string."
        );
    }

    if ((hex.length >= 2) && (hex[0 .. 2] == "0x"))
    {
        hex = hex[2 .. $];
    }

    auto decoder = HexStringDecoder!String(hex);
    auto result = new ubyte[](decoder.length);

    size_t idx = 0;
    foreach (b; decoder)
    {
        result[idx++] = b;
    }
    return result;
}

///
@safe unittest
{
    // Single byte
    assert("0xff".fromHexString  == [255]);
    assert("0xff"w.fromHexString == [255]);
    assert("0xff"d.fromHexString == [255]);
    assert("0xC0".fromHexString  == [192]);
    assert("0x00".fromHexString  == [0]);

    // Nothing
    assert("".fromHexString  == []);
    assert(""w.fromHexString == []);
    assert(""d.fromHexString == []);

    // Nothing but a prefix
    assert("0x".fromHexString  == []);
    assert("0x"w.fromHexString == []);
    assert("0x"d.fromHexString == []);

    // Half a byte
    assert("0x1".fromHexString  == [0x01]);
    assert("0x1"w.fromHexString == [0x01]);
    assert("0x1"d.fromHexString == [0x01]);

    // Mixed case is fine.
    assert("0xAf".fromHexString == [0xAF]);
    assert("0xaF".fromHexString == [0xAF]);

    // Multiple bytes
    assert("0xfff".fromHexString     == [0x0F, 0xFF]);
    assert("0x123AaAa".fromHexString == [0x01, 0x23, 0xAA, 0xAA]);
    assert("EBBBBF".fromHexString    == [0xEB, 0xBB, 0xBF]);

    // md5 sum
    assert("d41d8cd98f00b204e9800998ecf8427e".fromHexString == [
        0xD4, 0x1D, 0x8C, 0xD9, 0x8F, 0x00, 0xB2, 0x04,
        0xE9, 0x80, 0x09, 0x98, 0xEC, 0xF8, 0x42, 0x7E,
    ]);
}

///
@safe unittest
{
    // Cycle self-test
    const ubyte[] initial = [0x00, 0x12, 0x34, 0xEB];
    assert(initial == initial.toHexString().fromHexString());
}

private ubyte hexDigitToByte(dchar hexDigit) @safe pure nothrow @nogc
{
    static int hexDigitToByteImpl(dchar hexDigit)
    {
        if (hexDigit >= '0' && hexDigit <= '9')
        {
            return hexDigit - '0';
        }
        else if (hexDigit >= 'A' && hexDigit <= 'F')
        {
            return hexDigit - 'A' + 10;
        }
        else if (hexDigit >= 'a' && hexDigit <= 'f')
        {
            return hexDigit - 'a' + 10;
        }

        assert(false, "Cannot convert invalid hex digit.");
    }

    return hexDigitToByteImpl(hexDigit) & 0xFF;
}

@safe unittest
{
    assert(hexDigitToByte('0') == 0x0);
    assert(hexDigitToByte('9') == 0x9);
    assert(hexDigitToByte('a') == 0xA);
    assert(hexDigitToByte('b') == 0xB);
    assert(hexDigitToByte('A') == 0xA);
    assert(hexDigitToByte('C') == 0xC);
}

private struct HexStringDecoder(String)
if (isSomeString!String)
{
    String hex;
    ubyte front;
    bool empty;

    this(String hex)
    {
        if ((hex.length >= 2) && (hex[0 .. 2] == "0x"))
        {
            hex = hex[2 .. $];
        }

        if (hex.length == 0)
        {
            empty = true;
            return;
        }

        const oddInputLength = (hex.length % 2 == 1);

        if (oddInputLength)
        {
            front = hexDigitToByte(hex[0]);
            hex = hex[1 .. $];
        }
        else
        {
            front = cast(ubyte)(hexDigitToByte(hex[0]) << 4 | hexDigitToByte(hex[1]));
            hex = hex[2 .. $];
        }

        this.hex = hex;
    }

    void popFront()
    {
        if (hex.length == 0)
        {
            empty = true;
            return;
        }

        front = cast(ubyte)(hexDigitToByte(hex[0]) << 4 | hexDigitToByte(hex[1]));
        hex = hex[2 .. $];
    }

    typeof(this) save()
    {
        return this;
    }

    size_t length() const
    {
        if (this.empty)
        {
            return 0;
        }

        // current front + remainder
        return 1 + (hex.length >> 1);
    }
}

@safe unittest
{
    auto decoder = HexStringDecoder!string("");
    assert(decoder.empty);
    assert(decoder.length == 0);

    decoder = HexStringDecoder!string("0x");
    assert(decoder.empty);
    assert(decoder.length == 0);
}

@safe unittest
{
    auto decoder = HexStringDecoder!string("0x0077FF");
    assert(!decoder.empty);
    assert(decoder.length == 3);
    assert(decoder.front == 0x00);

    decoder.popFront();
    assert(!decoder.empty);
    assert(decoder.length == 2);
    assert(decoder.front == 0x77);

    decoder.popFront();
    assert(!decoder.empty);
    assert(decoder.length == 1);
    assert(decoder.front == 0xFF);

    decoder.popFront();
    assert(decoder.length == 0);
    assert(decoder.empty);
}

@safe unittest
{
    auto decoder = HexStringDecoder!string("0x7FF");
    assert(!decoder.empty);
    assert(decoder.length == 2);
    assert(decoder.front == 0x07);

    decoder.popFront();
    assert(!decoder.empty);
    assert(decoder.length == 1);
    assert(decoder.front == 0xFF);

    decoder.popFront();
    assert(decoder.length == 0);
    assert(decoder.empty);
}
