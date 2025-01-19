// Written in the D programming language.

/**
 * Support for Base64 encoding and decoding.
 *
 * This module provides two default implementations of Base64 encoding,
 * $(LREF Base64) with a standard encoding alphabet, and a variant
 * $(LREF Base64URL) that has a modified encoding alphabet designed to be
 * safe for embedding in URLs and filenames.
 *
 * Both variants are implemented as instantiations of the template
 * $(LREF Base64Impl). Most users will not need to use this template
 * directly; however, it can be used to create customized Base64 encodings,
 * such as one that omits padding characters, or one that is safe to embed
 * inside a regular expression.
 *
 * Example:
 * -----
 * ubyte[] data = [0x14, 0xfb, 0x9c, 0x03, 0xd9, 0x7e];
 *
 * const(char)[] encoded = Base64.encode(data);
 * assert(encoded == "FPucA9l+");
 *
 * ubyte[] decoded = Base64.decode("FPucA9l+");
 * assert(decoded == [0x14, 0xfb, 0x9c, 0x03, 0xd9, 0x7e]);
 * -----
 *
 * The range API is supported for both encoding and decoding:
 *
 * Example:
 * -----
 * // Create MIME Base64 with CRLF, per line 76.
 * File f = File("./text.txt", "r");
 * scope(exit) f.close();
 *
 * Appender!string mime64 = appender!string;
 *
 * foreach (encoded; Base64.encoder(f.byChunk(57)))
 * {
 *     mime64.put(encoded);
 *     mime64.put("\r\n");
 * }
 *
 * writeln(mime64.data);
 * -----
 *
 * References:
 * $(LINK2 https://tools.ietf.org/html/rfc4648, RFC 4648 - The Base16, Base32, and Base64
 * Data Encodings)
 *
 * Copyright: Masahiro Nakagawa 2010-.
 * License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Masahiro Nakagawa, Daniel Murphy (Single value Encoder and Decoder)
 * Source:    $(PHOBOSSRC std/base64.d)
 * Macros:
 *      LREF2=<a href="#$1">`$2`</a>
 */
module std.base64;

import std.exception : enforce;
import std.range.primitives : empty, front, isInputRange, isOutputRange,
    isForwardRange, ElementType, hasLength, popFront, put, save;
import std.traits : isArray;

// Make sure module header code examples work correctly.
pure @safe unittest
{
    ubyte[] data = [0x14, 0xfb, 0x9c, 0x03, 0xd9, 0x7e];

    const(char)[] encoded = Base64.encode(data);
    assert(encoded == "FPucA9l+");

    ubyte[] decoded = Base64.decode("FPucA9l+");
    assert(decoded == [0x14, 0xfb, 0x9c, 0x03, 0xd9, 0x7e]);
}

/**
 * Implementation of standard _Base64 encoding.
 *
 * See $(LREF Base64Impl) for a description of available methods.
 */
alias Base64 = Base64Impl!('+', '/');

///
pure @safe unittest
{
    ubyte[] data = [0x83, 0xd7, 0x30, 0x7a, 0x01, 0x3f];
    assert(Base64.encode(data) == "g9cwegE/");
    assert(Base64.decode("g9cwegE/") == data);
}


/**
 * Variation of Base64 encoding that is safe for use in URLs and filenames.
 *
 * See $(LREF Base64Impl) for a description of available methods.
 */
alias Base64URL = Base64Impl!('-', '_');

///
pure @safe unittest
{
    ubyte[] data = [0x83, 0xd7, 0x30, 0x7a, 0x01, 0x3f];
    assert(Base64URL.encode(data) == "g9cwegE_");
    assert(Base64URL.decode("g9cwegE_") == data);
}

/**
 * Unpadded variation of Base64 encoding that is safe for use in URLs and
 * filenames, as used in RFCs 4648 and 7515 (JWS/JWT/JWE).
 *
 * See $(LREF Base64Impl) for a description of available methods.
 */
alias Base64URLNoPadding = Base64Impl!('-', '_', Base64.NoPadding);

///
pure @safe unittest
{
    ubyte[] data = [0x83, 0xd7, 0x30, 0x7b, 0xef];
    assert(Base64URLNoPadding.encode(data) == "g9cwe-8");
    assert(Base64URLNoPadding.decode("g9cwe-8") == data);
}

/**
 * Template for implementing Base64 encoding and decoding.
 *
 * For most purposes, direct usage of this template is not necessary; instead,
 * this module provides default implementations: $(LREF Base64), implementing
 * basic Base64 encoding, and $(LREF Base64URL) and $(LREF Base64URLNoPadding),
 * that implement the Base64 variant for use in URLs and filenames, with
 * and without padding, respectively.
 *
 * Customized Base64 encoding schemes can be implemented by instantiating this
 * template with the appropriate arguments. For example:
 *
 * -----
 * // Non-standard Base64 format for embedding in regular expressions.
 * alias Base64Re = Base64Impl!('!', '=', Base64.NoPadding);
 * -----
 *
 * NOTE:
 * Encoded strings will not have any padding if the `Padding` parameter is
 * set to `NoPadding`.
 */
template Base64Impl(char Map62th, char Map63th, char Padding = '=')
{
    enum NoPadding = '\0';  /// represents no-padding encoding


    // Verify Base64 characters
    static assert(Map62th < 'A' || Map62th > 'Z', "Character '" ~ Map62th ~ "' cannot be used twice");
    static assert(Map63th < 'A' || Map63th > 'Z', "Character '" ~ Map63th ~ "' cannot be used twice");
    static assert(Padding < 'A' || Padding > 'Z', "Character '" ~ Padding ~ "' cannot be used twice");
    static assert(Map62th < 'a' || Map62th > 'z', "Character '" ~ Map62th ~ "' cannot be used twice");
    static assert(Map63th < 'a' || Map63th > 'z', "Character '" ~ Map63th ~ "' cannot be used twice");
    static assert(Padding < 'a' || Padding > 'z', "Character '" ~ Padding ~ "' cannot be used twice");
    static assert(Map62th < '0' || Map62th > '9', "Character '" ~ Map62th ~ "' cannot be used twice");
    static assert(Map63th < '0' || Map63th > '9', "Character '" ~ Map63th ~ "' cannot be used twice");
    static assert(Padding < '0' || Padding > '9', "Character '" ~ Padding ~ "' cannot be used twice");
    static assert(Map62th != Map63th, "Character '" ~ Map63th ~ "' cannot be used twice");
    static assert(Map62th != Padding, "Character '" ~ Padding ~ "' cannot be used twice");
    static assert(Map63th != Padding, "Character '" ~ Padding ~ "' cannot be used twice");
    static assert(Map62th != NoPadding, "'\\0' is not a valid Base64character");
    static assert(Map63th != NoPadding, "'\\0' is not a valid Base64character");


    /* Encode functions */


    private immutable EncodeMap = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" ~ Map62th ~ Map63th;


    /**
     * Calculates the length needed to store the encoded string corresponding
     * to an input of the given length.
     *
     * Params:
     *  sourceLength = Length of the source array.
     *
     * Returns:
     *  The length of a Base64 encoding of an array of the given length.
     */
    @safe @nogc
    pure nothrow size_t encodeLength(in size_t sourceLength)
    {
        static if (Padding == NoPadding)
            return (sourceLength / 3) * 4 + (sourceLength % 3 == 0 ? 0 : sourceLength % 3 == 1 ? 2 : 3);
        else
            return (sourceLength / 3 + (sourceLength % 3 ? 1 : 0)) * 4;
    }

    ///
    @safe unittest
    {
        ubyte[] data = [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e];

        // Allocate a buffer large enough to hold the encoded string.
        auto buf = new char[Base64.encodeLength(data.length)];

        Base64.encode(data, buf);
        assert(buf == "Gis8TV1u");
    }


    // ubyte[] to char[]


    /**
     * Encode $(D_PARAM source) into a `char[]` buffer using Base64
     * encoding.
     *
     * Params:
     *  source = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
     *           to _encode.
     *  buffer = The `char[]` buffer to store the encoded result.
     *
     * Returns:
     *  The slice of $(D_PARAM buffer) that contains the encoded string.
     */
    @trusted
    pure char[] encode(R1, R2)(const scope R1 source, return scope R2 buffer)
    if (isArray!R1 && is(ElementType!R1 : ubyte) && is(R2 == char[]))
    in
    {
        assert(buffer.length >= encodeLength(source.length), "Insufficient buffer for encoding");
    }
    out(result)
    {
        assert(result.length == encodeLength(source.length), "The length of result is different from Base64");
    }
    do
    {
        immutable srcLen = source.length;
        if (srcLen == 0)
            return [];

        immutable blocks = srcLen / 3;
        immutable remain = srcLen % 3;
        auto      bufptr = buffer.ptr;
        auto      srcptr = source.ptr;

        foreach (Unused; 0 .. blocks)
        {
            immutable val = srcptr[0] << 16 | srcptr[1] << 8 | srcptr[2];
            *bufptr++ = EncodeMap[val >> 18       ];
            *bufptr++ = EncodeMap[val >> 12 & 0x3f];
            *bufptr++ = EncodeMap[val >>  6 & 0x3f];
            *bufptr++ = EncodeMap[val       & 0x3f];
            srcptr += 3;
        }

        if (remain)
        {
            immutable val = srcptr[0] << 16 | (remain == 2 ? srcptr[1] << 8 : 0);
            *bufptr++ = EncodeMap[val >> 18       ];
            *bufptr++ = EncodeMap[val >> 12 & 0x3f];

            final switch (remain)
            {
            case 2:
                *bufptr++ = EncodeMap[val >> 6 & 0x3f];
                static if (Padding != NoPadding)
                    *bufptr++ = Padding;
                break;
            case 1:
                static if (Padding != NoPadding)
                {
                    *bufptr++ = Padding;
                    *bufptr++ = Padding;
                }
                break;
            }
        }

        // encode method can't assume buffer length. So, slice needed.
        return buffer[0 .. bufptr - buffer.ptr];
    }

    ///
    @nogc nothrow @safe unittest
    {
        ubyte[6] data = [0x83, 0xd7, 0x30, 0x7a, 0x01, 0x3f];
        char[32] buffer;    // much bigger than necessary

        // Just to be sure...
        auto encodedLength = Base64.encodeLength(data.length);
        assert(buffer.length >= encodedLength);

        // encode() returns a slice to the provided buffer.
        auto encoded = Base64.encode(data[], buffer[]);
        assert(encoded is buffer[0 .. encodedLength]);
        assert(encoded == "g9cwegE/");
    }


    // InputRange to char[]


    /**
     * ditto
     */
    char[] encode(R1, R2)(R1 source, R2 buffer)
    if (!isArray!R1 && isInputRange!R1 &&
        is(ElementType!R1 : ubyte) && hasLength!R1 &&
        is(R2 == char[]))
    in
    {
        assert(buffer.length >= encodeLength(source.length), "Insufficient buffer for encoding");
    }
    out(result)
    {
        // @@@BUG@@@ D's DbC can't caputre an argument of function and store the result of precondition.
        //assert(result.length == encodeLength(source.length), "The length of result is different from Base64");
    }
    do
    {
        immutable srcLen = source.length;
        if (srcLen == 0)
            return [];

        immutable blocks = srcLen / 3;
        immutable remain = srcLen % 3;
        auto      bufptr = buffer.ptr;

        foreach (Unused; 0 .. blocks)
        {
            immutable v1 = source.front; source.popFront();
            immutable v2 = source.front; source.popFront();
            immutable v3 = source.front; source.popFront();
            immutable val = v1 << 16 | v2 << 8 | v3;
            *bufptr++ = EncodeMap[val >> 18       ];
            *bufptr++ = EncodeMap[val >> 12 & 0x3f];
            *bufptr++ = EncodeMap[val >>  6 & 0x3f];
            *bufptr++ = EncodeMap[val       & 0x3f];
        }

        if (remain)
        {
            size_t val = source.front << 16;
            if (remain == 2)
            {
                source.popFront();
                val |= source.front << 8;
            }

            *bufptr++ = EncodeMap[val >> 18       ];
            *bufptr++ = EncodeMap[val >> 12 & 0x3f];

            final switch (remain)
            {
            case 2:
                *bufptr++ = EncodeMap[val >> 6 & 0x3f];
                static if (Padding != NoPadding)
                    *bufptr++ = Padding;
                break;
            case 1:
                static if (Padding != NoPadding)
                {
                    *bufptr++ = Padding;
                    *bufptr++ = Padding;
                }
                break;
            }
        }

        // @@@BUG@@@ Workaround for DbC problem. See comment on 'out'.
        version (StdUnittest)
            assert(
                bufptr - buffer.ptr == encodeLength(srcLen),
                "The length of result is different from Base64"
            );

        // encode method can't assume buffer length. So, slice needed.
        return buffer[0 .. bufptr - buffer.ptr];
    }


    // ubyte[] to OutputRange


    /**
     * Encodes $(D_PARAM source) into an
     * $(REF_ALTTEXT output range, isOutputRange, std,range,primitives) using
     * Base64 encoding.
     *
     * Params:
     *  source = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
     *           to _encode.
     *  range  = The $(REF_ALTTEXT output range, isOutputRange, std,range,primitives)
     *           to store the encoded result.
     *
     * Returns:
     *  The number of times the output range's `put` method was invoked.
     */
    size_t encode(E, R)(scope const(E)[] source, auto ref R range)
    if (is(E : ubyte) && isOutputRange!(R, char) && !is(R == char[]))
    out(result)
    {
        assert(result == encodeLength(source.length), "The number of put is different from the length of Base64");
    }
    do
    {
        immutable srcLen = source.length;
        if (srcLen == 0)
            return 0;

        immutable blocks = srcLen / 3;
        immutable remain = srcLen % 3;
        auto s = source; // copy for out contract length check
        size_t pcount;

        foreach (Unused; 0 .. blocks)
        {
            immutable val = s[0] << 16 | s[1] << 8 | s[2];
            put(range, EncodeMap[val >> 18       ]);
            put(range, EncodeMap[val >> 12 & 0x3f]);
            put(range, EncodeMap[val >>  6 & 0x3f]);
            put(range, EncodeMap[val       & 0x3f]);
            s = s[3 .. $];
            pcount += 4;
        }

        if (remain)
        {
            immutable val = s[0] << 16 | (remain == 2 ? s[1] << 8 : 0);
            put(range, EncodeMap[val >> 18       ]);
            put(range, EncodeMap[val >> 12 & 0x3f]);
            pcount += 2;

            final switch (remain)
            {
            case 2:
                put(range, EncodeMap[val >> 6 & 0x3f]);
                pcount++;

                static if (Padding != NoPadding)
                {
                    put(range, Padding);
                    pcount++;
                }
                break;
            case 1:
                static if (Padding != NoPadding)
                {
                    put(range, Padding);
                    put(range, Padding);
                    pcount += 2;
                }
                break;
            }
        }

        return pcount;
    }

    ///
    @safe pure nothrow unittest
    {
        import std.array : appender;

        auto output = appender!string();
        ubyte[] data = [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e];

        // This overload of encode() returns the number of calls to the output
        // range's put method.
        assert(Base64.encode(data, output) == 8);
        assert(output.data == "Gis8TV1u");
    }


    // InputRange to OutputRange


    /**
     * ditto
     */
    size_t encode(R1, R2)(R1 source, auto ref R2 range)
    if (!isArray!R1 && isInputRange!R1 && is(ElementType!R1 : ubyte) &&
        hasLength!R1 && !is(R2 == char[]) && isOutputRange!(R2, char))
    {
        immutable srcLen = source.length;
        if (srcLen == 0)
            return 0;

        immutable blocks = srcLen / 3;
        immutable remain = srcLen % 3;
        size_t    pcount;

        foreach (Unused; 0 .. blocks)
        {
            immutable v1 = source.front; source.popFront();
            immutable v2 = source.front; source.popFront();
            immutable v3 = source.front; source.popFront();
            immutable val = v1 << 16 | v2 << 8 | v3;
            put(range, EncodeMap[val >> 18       ]);
            put(range, EncodeMap[val >> 12 & 0x3f]);
            put(range, EncodeMap[val >>  6 & 0x3f]);
            put(range, EncodeMap[val       & 0x3f]);
            pcount += 4;
        }

        if (remain)
        {
            size_t val = source.front << 16;
            if (remain == 2)
            {
                source.popFront();
                val |= source.front << 8;
            }

            put(range, EncodeMap[val >> 18       ]);
            put(range, EncodeMap[val >> 12 & 0x3f]);
            pcount += 2;

            final switch (remain)
            {
            case 2:
                put(range, EncodeMap[val >> 6 & 0x3f]);
                pcount++;

                static if (Padding != NoPadding)
                {
                    put(range, Padding);
                    pcount++;
                }
                break;
            case 1:
                static if (Padding != NoPadding)
                {
                    put(range, Padding);
                    put(range, Padding);
                    pcount += 2;
                }
                break;
            }
        }

        // @@@BUG@@@ Workaround for DbC problem.
        version (StdUnittest)
            assert(
                pcount == encodeLength(srcLen),
                "The number of put is different from the length of Base64"
            );

        return pcount;
    }


    /**
     * Encodes $(D_PARAM source) to newly-allocated buffer.
     *
     * This convenience method alleviates the need to manually manage output
     * buffers.
     *
     * Params:
     *  source = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
     *           to _encode.
     *
     * Returns:
     *  A newly-allocated `char[]` buffer containing the encoded string.
     */
    @safe
    pure char[] encode(Range)(Range source)
    if (isArray!Range && is(ElementType!Range : ubyte))
    {
        return encode(source, new char[encodeLength(source.length)]);
    }

    ///
    @safe unittest
    {
        ubyte[] data = [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e];
        assert(Base64.encode(data) == "Gis8TV1u");
    }


    /**
     * ditto
     */
    char[] encode(Range)(Range source)
    if (!isArray!Range && isInputRange!Range &&
        is(ElementType!Range : ubyte) && hasLength!Range)
    {
        return encode(source, new char[encodeLength(source.length)]);
    }


    /**
     * An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) that
     * iterates over the respective Base64 encodings of a range of data items.
     *
     * This range will be a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
     * if the underlying data source is at least a forward range.
     *
     * Note: This struct is not intended to be created in user code directly;
     * use the $(LREF encoder) function instead.
     */
    struct Encoder(Range)
    if (isInputRange!Range && (is(ElementType!Range : const(ubyte)[]) ||
        is(ElementType!Range : const(char)[])))
    {
      private:
        Range  range_;
        char[] buffer_, encoded_;


      public:
        this(Range range)
        {
            range_ = range;
            if (!empty)
                doEncoding();
        }


        /**
         * Returns:
         *  true if there is no more encoded data left.
         */
        @property @trusted
        bool empty()
        {
            return range_.empty;
        }


        /**
         * Returns: The current chunk of encoded data.
         */
        @property @safe
        nothrow char[] front()
        {
            return encoded_;
        }


        /**
         * Advance the range to the next chunk of encoded data.
         *
         * Throws:
         *  `Base64Exception` If invoked when
         *  $(LREF2 .Base64Impl.Encoder.empty, empty) returns `true`.
         */
        void popFront()
        {
            assert(!empty, "Cannot call popFront on Encoder with no data remaining");

            range_.popFront();

            /*
             * This check is very ugly. I think this is a Range's flaw.
             * I very strongly want the Range guideline for unified implementation.
             *
             * In this case, Encoder becomes a beautiful implementation if 'front' performs Base64 encoding.
             */
            if (!empty)
                doEncoding();
        }


        static if (isForwardRange!Range)
        {
            /**
             * Save the current iteration state of the range.
             *
             * This method is only available if the underlying range is a
             * $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives).
             *
             * Returns:
             *  A copy of `this`.
             */
            @property
            typeof(this) save()
            {
                typeof(return) encoder;

                encoder.range_   = range_.save;
                encoder.buffer_  = buffer_.dup;
                encoder.encoded_ = encoder.buffer_[0 .. encoded_.length];

                return encoder;
            }
        }


      private:
        void doEncoding()
        {
            auto data = cast(const(ubyte)[])range_.front;
            auto size = encodeLength(data.length);
            if (size > buffer_.length)
                buffer_.length = size;

            encoded_ = encode(data, buffer_);
        }
    }


    /**
     * An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) that
     * iterates over the encoded bytes of the given source data.
     *
     * It will be a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
     * if the underlying data source is at least a forward range.
     *
     * Note: This struct is not intended to be created in user code directly;
     * use the $(LREF encoder) function instead.
     */
    struct Encoder(Range)
    if (isInputRange!Range && is(ElementType!Range : ubyte))
    {
      private:
        Range range_;
        ubyte first;
        int   pos, padding;


      public:
        this(Range range)
        {
            range_ = range;
            static if (isForwardRange!Range)
                range_ = range_.save;

            if (range_.empty)
                pos = -1;
            else
                popFront();
        }


        /**
         * Returns:
         *  true if there are no more encoded characters to be iterated.
         */
        @property @safe
        nothrow bool empty() const
        {
            static if (Padding == NoPadding)
                return pos < 0;
            else
                return pos < 0 && !padding;
        }


        /**
         * Returns: The current encoded character.
         */
        @property @safe
        nothrow ubyte front()
        {
            return first;
        }


        /**
         * Advance to the next encoded character.
         *
         * Throws:
         *  `Base64Exception` If invoked when $(LREF2 .Base64Impl.Encoder.empty.2,
         *  empty) returns `true`.
         */
        void popFront()
        {
            assert(!empty, "Cannot call popFront on Encoder with no data remaining");

            static if (Padding != NoPadding)
                if (padding)
                {
                    first = Padding;
                    pos   = -1;
                    padding--;
                    return;
                }

            if (range_.empty)
            {
                pos = -1;
                return;
            }

            final switch (pos)
            {
            case 0:
                first = EncodeMap[range_.front >> 2];
                break;
            case 1:
                immutable t = (range_.front & 0b11) << 4;
                range_.popFront();

                if (range_.empty)
                {
                    first   = EncodeMap[t];
                    padding = 3;
                }
                else
                {
                    first = EncodeMap[t | (range_.front >> 4)];
                }
                break;
            case 2:
                immutable t = (range_.front & 0b1111) << 2;
                range_.popFront();

                if (range_.empty)
                {
                    first   = EncodeMap[t];
                    padding = 2;
                }
                else
                {
                    first = EncodeMap[t | (range_.front >> 6)];
                }
                break;
            case 3:
                first = EncodeMap[range_.front & 0b111111];
                range_.popFront();
                break;
            }

            ++pos %= 4;
        }


        static if (isForwardRange!Range)
        {
            /**
             * Save the current iteration state of the range.
             *
             * This method is only available if the underlying range is a
             * $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives).
             *
             * Returns:
             *  A copy of `this`.
             */
            @property
            typeof(this) save()
            {
                auto encoder = this;
                encoder.range_ = encoder.range_.save;
                return encoder;
            }
        }
    }


    /**
     * Construct an `Encoder` that iterates over the Base64 encoding of the
     * given $(REF_ALTTEXT input range, isInputRange, std,range,primitives).
     *
     * Params:
     *  range = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
     *          over the data to be encoded.
     *
     * Returns:
     *  If $(D_PARAM range) is a range of bytes, an `Encoder` that iterates
     *  over the bytes of the corresponding Base64 encoding.
     *
     *  If $(D_PARAM range) is a range of ranges of bytes, an `Encoder` that
     *  iterates over the Base64 encoded strings of each element of the range.
     *
     *  In both cases, the returned `Encoder` will be a
     *  $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives) if the
     *  given `range` is at least a forward range, otherwise it will be only
     *  an input range.
     *
     * Example:
     * This example encodes the input one line at a time.
     * -----
     * File f = File("text.txt", "r");
     * scope(exit) f.close();
     *
     * uint line = 0;
     * foreach (encoded; Base64.encoder(f.byLine()))
     * {
     *     writeln(++line, ". ", encoded);
     * }
     * -----
     *
     * Example:
     * This example encodes the input data one byte at a time.
     * -----
     * ubyte[] data = cast(ubyte[]) "0123456789";
     *
     * // The ElementType of data is not aggregation type
     * foreach (encoded; Base64.encoder(data))
     * {
     *     writeln(encoded);
     * }
     * -----
     */
    Encoder!(Range) encoder(Range)(Range range)
    if (isInputRange!Range)
    {
        return typeof(return)(range);
    }


    /* Decode functions */


    private immutable int[char.max + 1] DecodeMap = [
        'A':0b000000, 'B':0b000001, 'C':0b000010, 'D':0b000011, 'E':0b000100,
        'F':0b000101, 'G':0b000110, 'H':0b000111, 'I':0b001000, 'J':0b001001,
        'K':0b001010, 'L':0b001011, 'M':0b001100, 'N':0b001101, 'O':0b001110,
        'P':0b001111, 'Q':0b010000, 'R':0b010001, 'S':0b010010, 'T':0b010011,
        'U':0b010100, 'V':0b010101, 'W':0b010110, 'X':0b010111, 'Y':0b011000,
        'Z':0b011001, 'a':0b011010, 'b':0b011011, 'c':0b011100, 'd':0b011101,
        'e':0b011110, 'f':0b011111, 'g':0b100000, 'h':0b100001, 'i':0b100010,
        'j':0b100011, 'k':0b100100, 'l':0b100101, 'm':0b100110, 'n':0b100111,
        'o':0b101000, 'p':0b101001, 'q':0b101010, 'r':0b101011, 's':0b101100,
        't':0b101101, 'u':0b101110, 'v':0b101111, 'w':0b110000, 'x':0b110001,
        'y':0b110010, 'z':0b110011, '0':0b110100, '1':0b110101, '2':0b110110,
        '3':0b110111, '4':0b111000, '5':0b111001, '6':0b111010, '7':0b111011,
        '8':0b111100, '9':0b111101, Map62th:0b111110, Map63th:0b111111, Padding:-1
    ];


    /**
     * Given a Base64 encoded string, calculates the length of the decoded
     * string.
     *
     * Params:
     *  sourceLength = The length of the Base64 encoding.
     *
     * Returns:
     *  The length of the decoded string corresponding to a Base64 encoding of
     *  length $(D_PARAM sourceLength).
     */
    @safe
    pure @nogc nothrow size_t decodeLength(in size_t sourceLength)
    {
        static if (Padding == NoPadding)
            return (sourceLength / 4) * 3 + (sourceLength % 4 < 2 ? 0 : sourceLength % 4 == 2 ? 1 : 2);
        else
            return (sourceLength / 4) * 3;
    }

    ///
    @safe unittest
    {
        auto encoded = "Gis8TV1u";

        // Allocate a sufficiently large buffer to hold to decoded result.
        auto buffer = new ubyte[Base64.decodeLength(encoded.length)];

        Base64.decode(encoded, buffer);
        assert(buffer == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);
    }


    // Used in decode contracts. Calculates the actual size the decoded
    // result should have, taking into account trailing padding.
    @safe
    pure @nogc nothrow private size_t realDecodeLength(R)(R source)
    {
        auto expect = decodeLength(source.length);
        static if (Padding != NoPadding)
        {
            if (source.length % 4 == 0)
            {
                expect -= source.length == 0       ? 0 :
                          source[$ - 2] == Padding ? 2 :
                          source[$ - 1] == Padding ? 1 : 0;
            }
        }
        return expect;
    }


    // char[] to ubyte[]


    /**
     * Decodes $(D_PARAM source) into the given buffer.
     *
     * Params:
     *  source = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
     *           to _decode.
     *  buffer = The buffer to store decoded result.
     *
     * Returns:
     *  The slice of $(D_PARAM buffer) containing the decoded result.
     *
     * Throws:
     *  `Base64Exception` if $(D_PARAM source) contains characters outside the
     *  base alphabet of the current Base64 encoding scheme.
     */
    @trusted
    pure ubyte[] decode(R1, R2)(in R1 source, return scope R2 buffer)
    if (isArray!R1 && is(ElementType!R1 : dchar) &&
        is(R2 == ubyte[]) && isOutputRange!(R2, ubyte))
    in
    {
        assert(buffer.length >= realDecodeLength(source), "Insufficient buffer for decoding");
    }
    out(result)
    {
        immutable expect = realDecodeLength(source);
        assert(result.length == expect, "The length of result is different from the expected length");
    }
    do
    {
        immutable srcLen = source.length;
        if (srcLen == 0)
            return [];
        static if (Padding != NoPadding)
            enforce(srcLen % 4 == 0, new Base64Exception("Invalid length of encoded data"));

        immutable blocks = srcLen / 4;
        auto      srcptr = source.ptr;
        auto      bufptr = buffer.ptr;

        foreach (Unused; 0 .. blocks)
        {
            immutable v1 = decodeChar(*srcptr++);
            immutable v2 = decodeChar(*srcptr++);

            *bufptr++ = cast(ubyte)(v1 << 2 | v2 >> 4);

            immutable v3 = decodeChar(*srcptr++);
            if (v3 == -1)
                break;

            *bufptr++ = cast(ubyte)((v2 << 4 | v3 >> 2) & 0xff);

            immutable v4 = decodeChar(*srcptr++);
            if (v4 == -1)
                break;

            *bufptr++ = cast(ubyte)((v3 << 6 | v4) & 0xff);
        }

        static if (Padding == NoPadding)
        {
            immutable remain = srcLen % 4;

            if (remain)
            {
                immutable v1 = decodeChar(*srcptr++);
                immutable v2 = decodeChar(*srcptr++);

                *bufptr++ = cast(ubyte)(v1 << 2 | v2 >> 4);

                if (remain == 3)
                    *bufptr++ = cast(ubyte)((v2 << 4 | decodeChar(*srcptr++) >> 2) & 0xff);
            }
        }

        return buffer[0 .. bufptr - buffer.ptr];
    }

    ///
    @safe unittest
    {
        auto encoded = "Gis8TV1u";
        ubyte[32] buffer;   // much bigger than necessary

        // Just to be sure...
        auto decodedLength = Base64.decodeLength(encoded.length);
        assert(buffer.length >= decodedLength);

        // decode() returns a slice of the given buffer.
        auto decoded = Base64.decode(encoded, buffer[]);
        assert(decoded is buffer[0 .. decodedLength]);
        assert(decoded == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);
    }

    // InputRange to ubyte[]


    /**
     * ditto
     */
    ubyte[] decode(R1, R2)(R1 source, R2 buffer)
    if (!isArray!R1 && isInputRange!R1 &&
        is(ElementType!R1 : dchar) && hasLength!R1 &&
        is(R2 == ubyte[]) && isOutputRange!(R2, ubyte))
    in
    {
        assert(buffer.length >= decodeLength(source.length), "Insufficient buffer for decoding");
    }
    do
    {
        immutable srcLen = source.length;
        if (srcLen == 0)
            return [];
        static if (Padding != NoPadding)
            enforce(srcLen % 4 == 0, new Base64Exception("Invalid length of encoded data"));

        immutable blocks = srcLen / 4;
        auto      bufptr = buffer.ptr;

        foreach (Unused; 0 .. blocks)
        {
            immutable v1 = decodeChar(source.front); source.popFront();
            immutable v2 = decodeChar(source.front); source.popFront();

            *bufptr++ = cast(ubyte)(v1 << 2 | v2 >> 4);

            immutable v3 = decodeChar(source.front);
            if (v3 == -1)
                break;

            *bufptr++ = cast(ubyte)((v2 << 4 | v3 >> 2) & 0xff);
            source.popFront();

            immutable v4 = decodeChar(source.front);
            if (v4 == -1)
                break;

            *bufptr++ = cast(ubyte)((v3 << 6 | v4) & 0xff);
            source.popFront();
        }

        static if (Padding == NoPadding)
        {
            immutable remain = srcLen % 4;

            if (remain)
            {
                immutable v1 = decodeChar(source.front); source.popFront();
                immutable v2 = decodeChar(source.front);

                *bufptr++ = cast(ubyte)(v1 << 2 | v2 >> 4);

                if (remain == 3)
                {
                    source.popFront();
                    *bufptr++ = cast(ubyte)((v2 << 4 | decodeChar(source.front) >> 2) & 0xff);
                }
            }
        }

        // We need to do the check here because we have consumed the length
        version (StdUnittest)
            assert(
                (bufptr - buffer.ptr) >= (decodeLength(srcLen) - 2),
                "The length of result is smaller than expected length"
            );

        return buffer[0 .. bufptr - buffer.ptr];
    }


    // char[] to OutputRange


    /**
     * Decodes $(D_PARAM source) into a given
     * $(REF_ALTTEXT output range, isOutputRange, std,range,primitives).
     *
     * Params:
     *  source = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
     *           to _decode.
     *  range  = The $(REF_ALTTEXT output range, isOutputRange, std,range,primitives)
     *           to store the decoded result.
     *
     * Returns:
     *  The number of times the output range's `put` method was invoked.
     *
     * Throws:
     *  `Base64Exception` if $(D_PARAM source) contains characters outside the
     *  base alphabet of the current Base64 encoding scheme.
     */
    size_t decode(R1, R2)(in R1 source, auto ref R2 range)
    if (isArray!R1 && is(ElementType!R1 : dchar) &&
        !is(R2 == ubyte[]) && isOutputRange!(R2, ubyte))
    out(result)
    {
        immutable expect = realDecodeLength(source);
        assert(result == expect, "The result of decode is different from the expected");
    }
    do
    {
        immutable srcLen = source.length;
        if (srcLen == 0)
            return 0;
        static if (Padding != NoPadding)
            enforce(srcLen % 4 == 0, new Base64Exception("Invalid length of encoded data"));

        immutable blocks = srcLen / 4;
        auto      srcptr = source.ptr;
        size_t    pcount;

        foreach (Unused; 0 .. blocks)
        {
            immutable v1 = decodeChar(*srcptr++);
            immutable v2 = decodeChar(*srcptr++);

            put(range, cast(ubyte)(v1 << 2 | v2 >> 4));
            pcount++;

            immutable v3 = decodeChar(*srcptr++);
            if (v3 == -1)
                break;

            put(range, cast(ubyte)((v2 << 4 | v3 >> 2) & 0xff));
            pcount++;

            immutable v4 = decodeChar(*srcptr++);
            if (v4 == -1)
                break;

            put(range, cast(ubyte)((v3 << 6 | v4) & 0xff));
            pcount++;
        }

        static if (Padding == NoPadding)
        {
            immutable remain = srcLen % 4;

            if (remain)
            {
                immutable v1 = decodeChar(*srcptr++);
                immutable v2 = decodeChar(*srcptr++);

                put(range, cast(ubyte)(v1 << 2 | v2 >> 4));
                pcount++;

                if (remain == 3)
                {
                    put(range, cast(ubyte)((v2 << 4 | decodeChar(*srcptr++) >> 2) & 0xff));
                    pcount++;
                }
            }
        }

        return pcount;
    }

    ///
    @system unittest
    {
        struct OutputRange
        {
            ubyte[] result;
            void put(ubyte b) { result ~= b; }
        }
        OutputRange output;

        // This overload of decode() returns the number of calls to put().
        assert(Base64.decode("Gis8TV1u", output) == 6);
        assert(output.result == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);
    }


    // InputRange to OutputRange


    /**
     * ditto
     */
    size_t decode(R1, R2)(R1 source, auto ref R2 range)
    if (!isArray!R1 && isInputRange!R1 && is(ElementType!R1 : dchar) &&
        hasLength!R1 && !is(R2 == ubyte[]) && isOutputRange!(R2, ubyte))
    out(result)
    {
        // @@@BUG@@@ Workaround for DbC problem.
        //immutable expect = decodeLength(source.length) - 2;
        //assert(result >= expect, "The length of result is smaller than expected length");
    }
    do
    {
        immutable srcLen = source.length;
        if (srcLen == 0)
            return 0;
        static if (Padding != NoPadding)
            enforce(srcLen % 4 == 0, new Base64Exception("Invalid length of encoded data"));

        immutable blocks = srcLen / 4;
        size_t    pcount;

        foreach (Unused; 0 .. blocks)
        {
            immutable v1 = decodeChar(source.front); source.popFront();
            immutable v2 = decodeChar(source.front); source.popFront();

            put(range, cast(ubyte)(v1 << 2 | v2 >> 4));
            pcount++;

            immutable v3 = decodeChar(source.front);
            if (v3 == -1)
                break;

            put(range, cast(ubyte)((v2 << 4 | v3 >> 2) & 0xff));
            source.popFront();
            pcount++;

            immutable v4 = decodeChar(source.front);
            if (v4 == -1)
                break;

            put(range, cast(ubyte)((v3 << 6 | v4) & 0xff));
            source.popFront();
            pcount++;
        }

        static if (Padding == NoPadding)
        {
            immutable remain = srcLen % 4;

            if (remain)
            {
                immutable v1 = decodeChar(source.front); source.popFront();
                immutable v2 = decodeChar(source.front);

                put(range, cast(ubyte)(v1 << 2 | v2 >> 4));
                pcount++;

                if (remain == 3)
                {
                    source.popFront();
                    put(range, cast(ubyte)((v2 << 4 | decodeChar(source.front) >> 2) & 0xff));
                    pcount++;
                }
            }
        }

        // @@@BUG@@@ Workaround for DbC problem.
        version (StdUnittest)
            assert(
                pcount >= (decodeLength(srcLen) - 2),
                "The length of result is smaller than expected length"
            );

        return pcount;
    }


    /**
     * Decodes $(D_PARAM source) into newly-allocated buffer.
     *
     * This convenience method alleviates the need to manually manage decoding
     * buffers.
     *
     * Params:
     *  source = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
     *           to _decode.
     *
     * Returns:
     *  A newly-allocated `ubyte[]` buffer containing the decoded string.
     */
    @safe
    pure ubyte[] decode(Range)(Range source)
    if (isArray!Range && is(ElementType!Range : dchar))
    {
        return decode(source, new ubyte[decodeLength(source.length)]);
    }

    ///
    @safe unittest
    {
        auto data = "Gis8TV1u";
        assert(Base64.decode(data) == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);
    }


    /**
     * ditto
     */
    ubyte[] decode(Range)(Range source)
    if (!isArray!Range && isInputRange!Range &&
        is(ElementType!Range : dchar) && hasLength!Range)
    {
        return decode(source, new ubyte[decodeLength(source.length)]);
    }


    /**
     * An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) that
     * iterates over the decoded data of a range of Base64 encodings.
     *
     * This range will be a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
     * if the underlying data source is at least a forward range.
     *
     * Note: This struct is not intended to be created in user code directly;
     * use the $(LREF decoder) function instead.
     */
    struct Decoder(Range)
    if (isInputRange!Range && (is(ElementType!Range : const(char)[]) ||
        is(ElementType!Range : const(ubyte)[])))
    {
      private:
        Range   range_;
        ubyte[] buffer_, decoded_;


      public:
        this(Range range)
        {
            range_ = range;
            if (!empty)
                doDecoding();
        }


        /**
         * Returns:
         *  true if there are no more elements to be iterated.
         */
        @property @trusted
        bool empty()
        {
            return range_.empty;
        }


        /**
         * Returns: The decoding of the current element in the input.
         */
        @property @safe
        nothrow ubyte[] front()
        {
            return decoded_;
        }


        /**
         * Advance to the next element in the input to be decoded.
         *
         * Throws:
         *  `Base64Exception` if invoked when $(LREF2 .Base64Impl.Decoder.empty,
         *  empty) returns `true`.
         */
        void popFront()
        {
            assert(!empty, "Cannot call popFront on Decoder with no data remaining.");

            range_.popFront();

            /*
             * I mentioned Encoder's popFront.
             */
            if (!empty)
                doDecoding();
        }


        static if (isForwardRange!Range)
        {
            /**
             * Saves the current iteration state.
             *
             * This method is only available if the underlying range is a
             * $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
             *
             * Returns: A copy of `this`.
             */
            @property
            typeof(this) save()
            {
                typeof(return) decoder;

                decoder.range_   = range_.save;
                decoder.buffer_  = buffer_.dup;
                decoder.decoded_ = decoder.buffer_[0 .. decoded_.length];

                return decoder;
            }
        }


      private:
        void doDecoding()
        {
            auto data = cast(const(char)[])range_.front;

            static if (Padding == NoPadding)
            {
                while (data.length % 4 == 1)
                {
                    range_.popFront();
                    data ~= cast(const(char)[])range_.front;
                }
            }
            else
            {
                while (data.length % 4 != 0)
                {
                    range_.popFront();
                    enforce(!range_.empty, new Base64Exception("Invalid length of encoded data"));
                    data ~= cast(const(char)[])range_.front;
                }
            }

            auto size = decodeLength(data.length);
            if (size > buffer_.length)
                buffer_.length = size;

            decoded_ = decode(data, buffer_);
        }
    }


    /**
     * An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) that
     * iterates over the bytes of data decoded from a Base64 encoded string.
     *
     * This range will be a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
     * if the underlying data source is at least a forward range.
     *
     * Note: This struct is not intended to be created in user code directly;
     * use the $(LREF decoder) function instead.
     */
    struct Decoder(Range)
    if (isInputRange!Range && is(ElementType!Range : char))
    {
      private:
        Range range_;
        ubyte first;
        int   pos;


      public:
        this(Range range)
        {
            range_ = range;
            static if (isForwardRange!Range)
                range_ = range_.save;

            static if (Padding != NoPadding && hasLength!Range)
                enforce(range_.length % 4 == 0, new Base64Exception("Invalid length of encoded data"));

            if (range_.empty)
                pos = -1;
            else
                popFront();
        }


        /**
         * Returns:
         *  true if there are no more elements to be iterated.
         */
        @property @safe
        nothrow bool empty() const
        {
            return pos < 0;
        }


        /**
         * Returns: The current decoded byte.
         */
        @property @safe
        nothrow ubyte front()
        {
            return first;
        }


        /**
         * Advance to the next decoded byte.
         *
         * Throws:
         *  `Base64Exception` if invoked when $(LREF2 .Base64Impl.Decoder.empty,
         *  empty) returns `true`.
         */
        void popFront()
        {
            enforce(!empty, new Base64Exception("Cannot call popFront on Decoder with no data remaining"));

            static if (Padding == NoPadding)
            {
                bool endCondition()
                {
                    return range_.empty;
                }
            }
            else
            {
                bool endCondition()
                {
                    enforce(!range_.empty, new Base64Exception("Missing padding"));
                    return range_.front == Padding;
                }
            }

            if (range_.empty || range_.front == Padding)
            {
                pos = -1;
                return;
            }

            final switch (pos)
            {
            case 0:
                enforce(!endCondition(), new Base64Exception("Premature end of data found"));

                immutable t = DecodeMap[range_.front] << 2;
                range_.popFront();

                enforce(!endCondition(), new Base64Exception("Premature end of data found"));
                first = cast(ubyte)(t | (DecodeMap[range_.front] >> 4));
                break;
            case 1:
                immutable t = (DecodeMap[range_.front] & 0b1111) << 4;
                range_.popFront();

                if (endCondition())
                {
                    pos = -1;
                    return;
                }
                else
                {
                    first = cast(ubyte)(t | (DecodeMap[range_.front] >> 2));
                }
                break;
            case 2:
                immutable t = (DecodeMap[range_.front] & 0b11) << 6;
                range_.popFront();

                if (endCondition())
                {
                    pos = -1;
                    return;
                }
                else
                {
                    first = cast(ubyte)(t | DecodeMap[range_.front]);
                }

                range_.popFront();
                break;
            }

            ++pos %= 3;
        }


        static if (isForwardRange!Range)
        {
            /**
             * Saves the current iteration state.
             *
             * This method is only available if the underlying range is a
             * $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
             *
             * Returns: A copy of `this`.
             */
            @property
            typeof(this) save()
            {
                auto decoder = this;
                decoder.range_ = decoder.range_.save;
                return decoder;
            }
        }
    }


    /**
     * Construct a `Decoder` that iterates over the decoding of the given
     * Base64 encoded data.
     *
     * Params:
     *  range = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
     *      over the data to be decoded, or a `char` array. Will not accept
     *      `wchar[]` nor `dchar[]`.
     *
     * Returns:
     *  If $(D_PARAM range) is a range or array of `char`, a `Decoder` that
     *  iterates over the bytes of the corresponding Base64 decoding.
     *
     *  If $(D_PARAM range) is a range of ranges of characters, a `Decoder`
     *  that iterates over the decoded strings corresponding to each element of
     *  the range.
     *
     *  In both cases, the returned `Decoder` will be a
     *  $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives) if the
     *  given `range` is at least a forward range, otherwise it will be only
     *  an input range.
     *
     * If the input data contains characters not found in the base alphabet of
     * the current Base64 encoding scheme, the returned range may throw a
     * `Base64Exception`.
     *
     * Example:
     * This example shows decoding over a range of input data lines.
     * -----
     * foreach (decoded; Base64.decoder(stdin.byLine()))
     * {
     *     writeln(decoded);
     * }
     * -----
     *
     * This example shows decoding one byte at a time.
     * -----
     * auto encoded = Base64.encoder(cast(ubyte[])"0123456789");
     * foreach (n; map!q{a - '0'}(Base64.decoder(encoded)))
     * {
     *     writeln(n);
     * }
     * -----
     */
    Decoder!(Range) decoder(Range)(Range range)
    if (isInputRange!Range)
    {
        return typeof(return)(range);
    }

    /// ditto
    Decoder!(const(ubyte)[]) decoder()(const(char)[] range)
    {
        import std.string : representation;
        return typeof(return)(range.representation);
    }

    ///
    @safe pure unittest
    {
        import std.algorithm.comparison : equal;
        string encoded =
            "VGhvdSBzaGFsdCBuZXZlciBjb250aW51ZSBhZnRlciBhc3NlcnRpbmcgbnVsbA==";

        assert(Base64.decoder(encoded)
            .equal("Thou shalt never continue after asserting null"));
    }


  private:
    @safe
    pure int decodeChar()(char chr)
    {
        immutable val = DecodeMap[chr];

        // enforce can't be a pure function, so I use trivial check.
        if (val == 0 && chr != 'A')
            throw new Base64Exception("Invalid character: " ~ chr);

        return val;
    }


    @safe
    pure int decodeChar()(dchar chr)
    {
        // See above comment.
        if (chr > 0x7f)
            throw new Base64Exception("Base64-encoded character must be a single byte");

        return decodeChar(cast(char) chr);
    }
}

///
@safe unittest
{
    import std.string : representation;

    // pre-defined: alias Base64 = Base64Impl!('+', '/');
    ubyte[] emptyArr;
    assert(Base64.encode(emptyArr) == "");
    assert(Base64.encode("f".representation) == "Zg==");
    assert(Base64.encode("foo".representation) == "Zm9v");

    alias Base64Re = Base64Impl!('!', '=', Base64.NoPadding);
    assert(Base64Re.encode("f".representation) == "Zg");
    assert(Base64Re.encode("foo".representation) == "Zm9v");
}

/**
 * Exception thrown upon encountering Base64 encoding or decoding errors.
 */
class Base64Exception : Exception
{
    @safe pure nothrow
    this(string s, string fn = __FILE__, size_t ln = __LINE__)
    {
        super(s, fn, ln);
    }
}

///
@safe unittest
{
    import std.exception : assertThrown;
    assertThrown!Base64Exception(Base64.decode("ab|c"));
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.sorting : sort;
    import std.conv;
    import std.exception : assertThrown;
    import std.file;
    import std.stdio;

    alias Base64Re = Base64Impl!('!', '=', Base64.NoPadding);

    // Test vectors from RFC 4648
    ubyte[][string] tv = [
         ""      :cast(ubyte[])"",
         "f"     :cast(ubyte[])"f",
         "fo"    :cast(ubyte[])"fo",
         "foo"   :cast(ubyte[])"foo",
         "foob"  :cast(ubyte[])"foob",
         "fooba" :cast(ubyte[])"fooba",
         "foobar":cast(ubyte[])"foobar"
    ];

    { // Base64
        // encode
        assert(Base64.encodeLength(tv[""].length)       == 0);
        assert(Base64.encodeLength(tv["f"].length)      == 4);
        assert(Base64.encodeLength(tv["fo"].length)     == 4);
        assert(Base64.encodeLength(tv["foo"].length)    == 4);
        assert(Base64.encodeLength(tv["foob"].length)   == 8);
        assert(Base64.encodeLength(tv["fooba"].length)  == 8);
        assert(Base64.encodeLength(tv["foobar"].length) == 8);

        assert(Base64.encode(tv[""])       == "");
        assert(Base64.encode(tv["f"])      == "Zg==");
        assert(Base64.encode(tv["fo"])     == "Zm8=");
        assert(Base64.encode(tv["foo"])    == "Zm9v");
        assert(Base64.encode(tv["foob"])   == "Zm9vYg==");
        assert(Base64.encode(tv["fooba"])  == "Zm9vYmE=");
        assert(Base64.encode(tv["foobar"]) == "Zm9vYmFy");

        // decode
        assert(Base64.decodeLength(Base64.encode(tv[""]).length)       == 0);
        assert(Base64.decodeLength(Base64.encode(tv["f"]).length)      == 3);
        assert(Base64.decodeLength(Base64.encode(tv["fo"]).length)     == 3);
        assert(Base64.decodeLength(Base64.encode(tv["foo"]).length)    == 3);
        assert(Base64.decodeLength(Base64.encode(tv["foob"]).length)   == 6);
        assert(Base64.decodeLength(Base64.encode(tv["fooba"]).length)  == 6);
        assert(Base64.decodeLength(Base64.encode(tv["foobar"]).length) == 6);

        assert(Base64.decode(Base64.encode(tv[""]))       == tv[""]);
        assert(Base64.decode(Base64.encode(tv["f"]))      == tv["f"]);
        assert(Base64.decode(Base64.encode(tv["fo"]))     == tv["fo"]);
        assert(Base64.decode(Base64.encode(tv["foo"]))    == tv["foo"]);
        assert(Base64.decode(Base64.encode(tv["foob"]))   == tv["foob"]);
        assert(Base64.decode(Base64.encode(tv["fooba"]))  == tv["fooba"]);
        assert(Base64.decode(Base64.encode(tv["foobar"])) == tv["foobar"]);

        assertThrown!Base64Exception(Base64.decode("ab|c"));

        // Test decoding incomplete strings. RFC does not specify the correct
        // behavior, but the code should never throw Errors on invalid input.

        // decodeLength is nothrow
        assert(Base64.decodeLength(1) == 0);
        assert(Base64.decodeLength(2) <= 1);
        assert(Base64.decodeLength(3) <= 2);

        // may throw Exceptions, may not throw Errors
        assertThrown!Base64Exception(Base64.decode("Zg"));
        assertThrown!Base64Exception(Base64.decode("Zg="));
        assertThrown!Base64Exception(Base64.decode("Zm8"));
        assertThrown!Base64Exception(Base64.decode("Zg==;"));
    }

    { // No padding
        // encode
        assert(Base64Re.encodeLength(tv[""].length)       == 0);
        assert(Base64Re.encodeLength(tv["f"].length)      == 2);
        assert(Base64Re.encodeLength(tv["fo"].length)     == 3);
        assert(Base64Re.encodeLength(tv["foo"].length)    == 4);
        assert(Base64Re.encodeLength(tv["foob"].length)   == 6);
        assert(Base64Re.encodeLength(tv["fooba"].length)  == 7);
        assert(Base64Re.encodeLength(tv["foobar"].length) == 8);

        assert(Base64Re.encode(tv[""])       == "");
        assert(Base64Re.encode(tv["f"])      == "Zg");
        assert(Base64Re.encode(tv["fo"])     == "Zm8");
        assert(Base64Re.encode(tv["foo"])    == "Zm9v");
        assert(Base64Re.encode(tv["foob"])   == "Zm9vYg");
        assert(Base64Re.encode(tv["fooba"])  == "Zm9vYmE");
        assert(Base64Re.encode(tv["foobar"]) == "Zm9vYmFy");

        // decode
        assert(Base64Re.decodeLength(Base64Re.encode(tv[""]).length)       == 0);
        assert(Base64Re.decodeLength(Base64Re.encode(tv["f"]).length)      == 1);
        assert(Base64Re.decodeLength(Base64Re.encode(tv["fo"]).length)     == 2);
        assert(Base64Re.decodeLength(Base64Re.encode(tv["foo"]).length)    == 3);
        assert(Base64Re.decodeLength(Base64Re.encode(tv["foob"]).length)   == 4);
        assert(Base64Re.decodeLength(Base64Re.encode(tv["fooba"]).length)  == 5);
        assert(Base64Re.decodeLength(Base64Re.encode(tv["foobar"]).length) == 6);

        assert(Base64Re.decode(Base64Re.encode(tv[""]))       == tv[""]);
        assert(Base64Re.decode(Base64Re.encode(tv["f"]))      == tv["f"]);
        assert(Base64Re.decode(Base64Re.encode(tv["fo"]))     == tv["fo"]);
        assert(Base64Re.decode(Base64Re.encode(tv["foo"]))    == tv["foo"]);
        assert(Base64Re.decode(Base64Re.encode(tv["foob"]))   == tv["foob"]);
        assert(Base64Re.decode(Base64Re.encode(tv["fooba"]))  == tv["fooba"]);
        assert(Base64Re.decode(Base64Re.encode(tv["foobar"])) == tv["foobar"]);

        // decodeLength is nothrow
        assert(Base64.decodeLength(1) == 0);
    }

    { // with OutputRange
        import std.array;

        auto a = Appender!(char[])([]);
        auto b = Appender!(ubyte[])([]);

        assert(Base64.encode(tv[""], a) == 0);
        assert(Base64.decode(a.data, b) == 0);
        assert(tv[""] == b.data); a.clear(); b.clear();

        assert(Base64.encode(tv["f"], a) == 4);
        assert(Base64.decode(a.data,  b) == 1);
        assert(tv["f"] == b.data); a.clear(); b.clear();

        assert(Base64.encode(tv["fo"], a) == 4);
        assert(Base64.decode(a.data,   b) == 2);
        assert(tv["fo"] == b.data); a.clear(); b.clear();

        assert(Base64.encode(tv["foo"], a) == 4);
        assert(Base64.decode(a.data,    b) == 3);
        assert(tv["foo"] == b.data); a.clear(); b.clear();

        assert(Base64.encode(tv["foob"], a) == 8);
        assert(Base64.decode(a.data,     b) == 4);
        assert(tv["foob"] == b.data); a.clear(); b.clear();

        assert(Base64.encode(tv["fooba"], a) == 8);
        assert(Base64.decode(a.data, b)      == 5);
        assert(tv["fooba"] == b.data); a.clear(); b.clear();

        assert(Base64.encode(tv["foobar"], a) == 8);
        assert(Base64.decode(a.data, b)       == 6);
        assert(tv["foobar"] == b.data); a.clear(); b.clear();
    }

    // https://issues.dlang.org/show_bug.cgi?id=9543
    // These tests were disabled because they actually relied on the input range having length.
    // The implementation (currently) doesn't support encoding/decoding from a length-less source.
    version (none)
    { // with InputRange
        // InputRange to ubyte[] or char[]
        auto encoded = Base64.encode(map!(to!(ubyte))(["20", "251", "156", "3", "217", "126"]));
        assert(encoded == "FPucA9l+");
        assert(Base64.decode(map!q{a}(encoded)) == [0x14, 0xfb, 0x9c, 0x03, 0xd9, 0x7e]);

        // InputRange to OutputRange
        auto a = Appender!(char[])([]);
        auto b = Appender!(ubyte[])([]);
        assert(Base64.encode(map!(to!(ubyte))(["20", "251", "156", "3", "217", "126"]), a) == 8);
        assert(a.data == "FPucA9l+");
        assert(Base64.decode(map!q{a}(a.data), b) == 6);
        assert(b.data == [0x14, 0xfb, 0x9c, 0x03, 0xd9, 0x7e]);
    }

    { // Encoder and Decoder
        {
            string encode_file = std.file.deleteme ~ "-testingEncoder";
            std.file.write(encode_file, "\nf\nfo\nfoo\nfoob\nfooba\nfoobar");

            auto witness = ["", "Zg==", "Zm8=", "Zm9v", "Zm9vYg==", "Zm9vYmE=", "Zm9vYmFy"];
            auto f = File(encode_file);
            scope(exit)
            {
                f.close();
                assert(!f.isOpen);
                std.file.remove(encode_file);
            }

            size_t i;
            foreach (encoded; Base64.encoder(f.byLine()))
                assert(encoded == witness[i++]);

            assert(i == witness.length);
        }

        {
            string decode_file = std.file.deleteme ~ "-testingDecoder";
            std.file.write(decode_file, "\nZg==\nZm8=\nZm9v\nZm9vYg==\nZm9vYmE=\nZm9vYmFy");

            auto witness = sort(tv.keys);
            auto f = File(decode_file);
            scope(exit)
            {
                f.close();
                assert(!f.isOpen);
                std.file.remove(decode_file);
            }

            size_t i;
            foreach (decoded; Base64.decoder(f.byLine()))
                assert(decoded == witness[i++]);

            assert(i == witness.length);
        }

        { // ForwardRange
            {
                auto encoder = Base64.encoder(sort(tv.values));
                auto witness = ["", "Zg==", "Zm8=", "Zm9v", "Zm9vYg==", "Zm9vYmE=", "Zm9vYmFy"];
                size_t i;

                assert(encoder.front == witness[i++]); encoder.popFront();
                assert(encoder.front == witness[i++]); encoder.popFront();
                assert(encoder.front == witness[i++]); encoder.popFront();

                foreach (encoded; encoder.save)
                    assert(encoded == witness[i++]);
            }

            {
                auto decoder = Base64.decoder(["", "Zg==", "Zm8=", "Zm9v", "Zm9vYg==", "Zm9vYmE=", "Zm9vYmFy"]);
                auto witness = sort(tv.values);
                size_t i;

                assert(decoder.front == witness[i++]); decoder.popFront();
                assert(decoder.front == witness[i++]); decoder.popFront();
                assert(decoder.front == witness[i++]); decoder.popFront();

                foreach (decoded; decoder.save)
                    assert(decoded == witness[i++]);
            }
        }
    }

    { // Encoder and Decoder for single character encoding and decoding
        alias Base64NoPadding = Base64Impl!('+', '/', Base64.NoPadding);

        auto tests = [
            ""       : ["", "", "", ""],
            "f"      : ["Zg==", "Zg==", "Zg", "Zg"],
            "fo"     : ["Zm8=", "Zm8=", "Zm8", "Zm8"],
            "foo"    : ["Zm9v", "Zm9v", "Zm9v", "Zm9v"],
            "foob"   : ["Zm9vYg==", "Zm9vYg==", "Zm9vYg", "Zm9vYg"],
            "fooba"  : ["Zm9vYmE=", "Zm9vYmE=", "Zm9vYmE", "Zm9vYmE"],
            "foobar" : ["Zm9vYmFy", "Zm9vYmFy", "Zm9vYmFy", "Zm9vYmFy"],
        ];

        foreach (u, e; tests)
        {
            assert(equal(Base64.encoder(cast(ubyte[]) u), e[0]));
            assert(equal(Base64.decoder(Base64.encoder(cast(ubyte[]) u)), u));

            assert(equal(Base64URL.encoder(cast(ubyte[]) u), e[1]));
            assert(equal(Base64URL.decoder(Base64URL.encoder(cast(ubyte[]) u)), u));

            assert(equal(Base64NoPadding.encoder(cast(ubyte[]) u), e[2]));
            assert(equal(Base64NoPadding.decoder(Base64NoPadding.encoder(cast(ubyte[]) u)), u));

            assert(equal(Base64Re.encoder(cast(ubyte[]) u), e[3]));
            assert(equal(Base64Re.decoder(Base64Re.encoder(cast(ubyte[]) u)), u));
        }
    }
}

// Regression control for the output range ref bug in encode.
@safe unittest
{
    struct InputRange
    {
        ubyte[] impl = [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e];
        @property bool empty() { return impl.length == 0; }
        @property ubyte front() { return impl[0]; }
        void popFront() { impl = impl[1 .. $]; }
        @property size_t length() { return impl.length; }
    }

    struct OutputRange
    {
        char[] result;
        void put(char b) { result ~= b; }
    }

    InputRange ir;
    OutputRange or;
    assert(Base64.encode(ir, or) == 8);
    assert(or.result == "Gis8TV1u");

    // Verify that any existing workaround that uses & still works.
    InputRange ir2;
    OutputRange or2;
    () @trusted {
        assert(Base64.encode(ir2, &or2) == 8);
    }();
    assert(or2.result == "Gis8TV1u");
}

// Regression control for the output range ref bug in decode.
@safe unittest
{
    struct InputRange
    {
        const(char)[] impl = "Gis8TV1u";
        @property bool empty() { return impl.length == 0; }
        @property dchar front() { return impl[0]; }
        void popFront() { impl = impl[1 .. $]; }
        @property size_t length() { return impl.length; }
    }

    struct OutputRange
    {
        ubyte[] result;
        void put(ubyte b) { result ~= b; }
    }

    InputRange ir;
    OutputRange or;
    assert(Base64.decode(ir, or) == 6);
    assert(or.result == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);

    // Verify that any existing workaround that uses & still works.
    InputRange ir2;
    OutputRange or2;
    () @trusted {
        assert(Base64.decode(ir2, &or2) == 6);
    }();
    assert(or2.result == [0x1a, 0x2b, 0x3c, 0x4d, 0x5d, 0x6e]);
}

// https://issues.dlang.org/show_bug.cgi?id=21679
// https://issues.dlang.org/show_bug.cgi?id=21706
@safe unittest
{
    ubyte[][] input;
    assert(Base64.encoder(input).empty);
    assert(Base64.decoder(input).empty);
}

@safe unittest
{
    struct InputRange(ubyte[] data)
    {
        ubyte[] impl = data;
        bool empty() { return impl.length == 0; }
        ubyte front() { return impl[0]; }
        void popFront() { impl = impl[1 .. $]; }
        size_t length() { return impl.length; }
    }

    struct OutputRange
    {
        ubyte[] result;
        void put(ubyte b) { result ~= b; }
    }

    void test_encode(ubyte[] data, string result)()
    {
        InputRange!data ir;
        OutputRange or;
        assert(Base64.encode(ir, or) == result.length);
        assert(or.result == result);
    }

    void test_decode(ubyte[] data, string result)()
    {
        InputRange!data ir;
        OutputRange or;
        assert(Base64.decode(ir, or) == result.length);
        assert(or.result == result);
    }

    test_encode!([], "");
    test_encode!(['x'], "eA==");
    test_encode!([123, 45], "ey0=");

    test_decode!([], "");
    test_decode!(['e', 'A', '=', '='], "x");
    test_decode!(['e', 'y', '0', '='], "{-");
}

@system unittest
{
    // checking forward range
    auto item = Base64.decoder(Base64.encoder(cast(ubyte[]) "foobar"));
    auto copy = item.save();
    item.popFront();
    assert(item.front == 'o');
    assert(copy.front == 'f');
}

@system unittest
{
    // checking invalid dchar
    dchar[] c = cast(dchar[]) "ääää";

    import std.exception : assertThrown;
    assertThrown!Base64Exception(Base64.decode(c));
}

@safe unittest
{
    import std.array : array;

    char[][] input = [['e', 'y'], ['0', '=']];
    assert(Base64.decoder(input).array == [[123, 45]]);
}

// https://issues.dlang.org/show_bug.cgi?id=21707
@safe unittest
{
    import std.exception : assertThrown;

    char[][] t1 = [[ 'Z', 'g', '=' ]];
    assertThrown!Base64Exception(Base64.decoder(t1));

    char[][] t2 = [[ 'e', 'y', '0' ], ['=', '=']];
    assertThrown!Base64Exception(Base64.decoder(t2));
}
