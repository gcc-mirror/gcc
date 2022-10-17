/** Fundamental operations for arbitrary-precision arithmetic
 *
 * These functions are for internal use only.
 */
/*          Copyright Don Clugston 2008 - 2010.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
/* References:
   "Modern Computer Arithmetic" (MCA) is the primary reference for all
    algorithms used in this library.
  - R.P. Brent and P. Zimmermann, "Modern Computer Arithmetic",
    Version 0.5.9, (Oct 2010).
  - C. Burkinel and J. Ziegler, "Fast Recursive Division", MPI-I-98-1-022,
    Max-Planck Institute fuer Informatik, (Oct 1998).
  - G. Hanrot, M. Quercia, and P. Zimmermann, "The Middle Product Algorithm, I.",
    INRIA 4664, (Dec 2002).
  - M. Bodrato and A. Zanoni, "What about Toom-Cook Matrices Optimality?",
    http://bodrato.it/papers (2006).
  - A. Fog, "Optimizing subroutines in assembly language",
    www.agner.org/optimize (2008).
  - A. Fog, "The microarchitecture of Intel and AMD CPU's",
    www.agner.org/optimize (2008).
  - A. Fog, "Instruction tables: Lists of instruction latencies, throughputs
    and micro-operation breakdowns for Intel and AMD CPU's.", www.agner.org/optimize (2008).

Idioms:
  Many functions in this module use
  'func(Tulong)(Tulong x) if (is(Tulong == ulong))' rather than 'func(ulong x)'
  in order to disable implicit conversion.

*/
module std.internal.math.biguintcore;

version (D_InlineAsm_X86)
{
    static import std.internal.math.biguintx86;
}
static import std.internal.math.biguintnoasm;

import std.internal.math.biguintnoasm : BigDigit, KARATSUBALIMIT,
    KARATSUBASQUARELIMIT;

alias multibyteAdd = multibyteAddSub!('+');
alias multibyteSub = multibyteAddSub!('-');

private import std.traits;
private import std.range.primitives;
public import std.ascii : LetterCase;
import std.range.primitives;
import std.traits;

private:

// dipatchers to the right low-level primitives. Added to allow BigInt CTFE for
// 32 bit systems (https://issues.dlang.org/show_bug.cgi?id=14767) although it's
// used by the other architectures too.
// See comments below in case it has to be refactored.
version (X86)
uint multibyteAddSub(char op)(uint[] dest, const(uint)[] src1, const (uint)[] src2, uint carry)
{
    // must be checked before, otherwise D_InlineAsm_X86 is true.
    if (__ctfe)
        return std.internal.math.biguintnoasm.multibyteAddSub!op(dest, src1, src2, carry);
    // Runtime.
    else version (D_InlineAsm_X86)
        return std.internal.math.biguintx86.multibyteAddSub!op(dest, src1, src2, carry);
    // Runtime if no asm available.
    else
        return std.internal.math.biguintnoasm.multibyteAddSub!op(dest, src1, src2, carry);
}
// Any other architecture
else alias multibyteAddSub = std.internal.math.biguintnoasm.multibyteAddSub;

version (X86)
uint multibyteIncrementAssign(char op)(uint[] dest, uint carry)
{
    if (__ctfe)
        return std.internal.math.biguintnoasm.multibyteIncrementAssign!op(dest, carry);
    else version (D_InlineAsm_X86)
        return std.internal.math.biguintx86.multibyteIncrementAssign!op(dest, carry);
    else
        return std.internal.math.biguintnoasm.multibyteIncrementAssign!op(dest, carry);
}
else alias multibyteIncrementAssign = std.internal.math.biguintnoasm.multibyteIncrementAssign;

version (X86)
uint multibyteShl()(uint[] dest, const(uint)[] src, uint numbits)
{
    if (__ctfe)
        return std.internal.math.biguintnoasm.multibyteShl(dest, src, numbits);
    else version (D_InlineAsm_X86)
        return std.internal.math.biguintx86.multibyteShl(dest, src, numbits);
    else
        return std.internal.math.biguintnoasm.multibyteShl(dest, src, numbits);
}
else alias multibyteShl = std.internal.math.biguintnoasm.multibyteShl;

version (X86)
void multibyteShr()(uint[] dest, const(uint)[] src, uint numbits)
{
    if (__ctfe)
        std.internal.math.biguintnoasm.multibyteShr(dest, src, numbits);
    else version (D_InlineAsm_X86)
        std.internal.math.biguintx86.multibyteShr(dest, src, numbits);
    else
        std.internal.math.biguintnoasm.multibyteShr(dest, src, numbits);
}
else alias multibyteShr = std.internal.math.biguintnoasm.multibyteShr;

version (X86)
uint multibyteMul()(uint[] dest, const(uint)[] src, uint multiplier, uint carry)
{
    if (__ctfe)
        return std.internal.math.biguintnoasm.multibyteMul(dest, src, multiplier, carry);
    else version (D_InlineAsm_X86)
        return std.internal.math.biguintx86.multibyteMul(dest, src, multiplier, carry);
    else
        return std.internal.math.biguintnoasm.multibyteMul(dest, src, multiplier, carry);
}
else alias multibyteMul = std.internal.math.biguintnoasm.multibyteMul;

version (X86)
uint multibyteMulAdd(char op)(uint[] dest, const(uint)[] src, uint multiplier, uint carry)
{
    if (__ctfe)
        return std.internal.math.biguintnoasm.multibyteMulAdd!op(dest, src, multiplier, carry);
    else version (D_InlineAsm_X86)
        return std.internal.math.biguintx86.multibyteMulAdd!op(dest, src, multiplier, carry);
    else
        return std.internal.math.biguintnoasm.multibyteMulAdd!op(dest, src, multiplier, carry);
}
else alias multibyteMulAdd = std.internal.math.biguintnoasm.multibyteMulAdd;

version (X86)
void multibyteMultiplyAccumulate()(uint[] dest, const(uint)[] left, const(uint)[] right)
{
    if (__ctfe)
        std.internal.math.biguintnoasm.multibyteMultiplyAccumulate(dest, left, right);
    else version (D_InlineAsm_X86)
        std.internal.math.biguintx86.multibyteMultiplyAccumulate(dest, left, right);
    else
        std.internal.math.biguintnoasm.multibyteMultiplyAccumulate(dest, left, right);
}
else alias multibyteMultiplyAccumulate = std.internal.math.biguintnoasm.multibyteMultiplyAccumulate;

version (X86)
uint multibyteDivAssign()(uint[] dest, uint divisor, uint overflow)
{
    if (__ctfe)
        return std.internal.math.biguintnoasm.multibyteDivAssign(dest, divisor, overflow);
    else version (D_InlineAsm_X86)
        return std.internal.math.biguintx86.multibyteDivAssign(dest, divisor, overflow);
    else
        return std.internal.math.biguintnoasm.multibyteDivAssign(dest, divisor, overflow);
}
else alias multibyteDivAssign = std.internal.math.biguintnoasm.multibyteDivAssign;

version (X86)
void multibyteAddDiagonalSquares()(uint[] dest, const(uint)[] src)
{
    if (__ctfe)
        std.internal.math.biguintnoasm.multibyteAddDiagonalSquares(dest, src);
    else version (D_InlineAsm_X86)
        std.internal.math.biguintx86.multibyteAddDiagonalSquares(dest, src);
    else
        std.internal.math.biguintnoasm.multibyteAddDiagonalSquares(dest, src);
}
else alias multibyteAddDiagonalSquares = std.internal.math.biguintnoasm.multibyteAddDiagonalSquares;

version (X86)
void multibyteTriangleAccumulate()(uint[] dest, const(uint)[] x)
{
    if (__ctfe)
        std.internal.math.biguintnoasm.multibyteTriangleAccumulate(dest, x);
    else version (D_InlineAsm_X86)
        std.internal.math.biguintx86.multibyteTriangleAccumulate(dest, x);
    else
        std.internal.math.biguintnoasm.multibyteTriangleAccumulate(dest, x);
}
else alias multibyteTriangleAccumulate = std.internal.math.biguintnoasm.multibyteTriangleAccumulate;

version (X86)
void multibyteSquare()(BigDigit[] result, const(BigDigit)[] x)
{
    if (__ctfe)
        std.internal.math.biguintnoasm.multibyteSquare(result, x);
    else version (D_InlineAsm_X86)
        std.internal.math.biguintx86.multibyteSquare(result, x);
    else
        std.internal.math.biguintnoasm.multibyteSquare(result, x);
}
else alias multibyteSquare = std.internal.math.biguintnoasm.multibyteSquare;

// Limits for when to switch between algorithms.
// Half the size of the data cache.
@nogc nothrow pure @safe size_t getCacheLimit()
{
    import core.cpuid : dataCaches;
    return dataCaches[0].size * 1024 / 2;
}
enum size_t FASTDIVLIMIT = 100; // crossover to recursive division


// These constants are used by shift operations
static if (BigDigit.sizeof == int.sizeof)
{
    enum { LG2BIGDIGITBITS = 5, BIGDIGITSHIFTMASK = 31 }
    alias BIGHALFDIGIT = ushort;
}
else static if (BigDigit.sizeof == long.sizeof)
{
    alias BIGHALFDIGIT = uint;
    enum { LG2BIGDIGITBITS = 6, BIGDIGITSHIFTMASK = 63 }
}
else static assert(0, "Unsupported BigDigit size");

import std.exception : assumeUnique;
import std.traits : isIntegral;
enum BigDigitBits = BigDigit.sizeof*8;
template maxBigDigits(T)
if (isIntegral!T)
{
    enum maxBigDigits = (T.sizeof+BigDigit.sizeof-1)/BigDigit.sizeof;
}

static immutable BigDigit[] ZERO = [0];
static immutable BigDigit[] ONE = [1];
static immutable BigDigit[] TWO = [2];
static immutable BigDigit[] TEN = [10];


public:

/// BigUint performs memory management and wraps the low-level calls.
struct BigUint
{
private:
    pure invariant()
    {
        assert( data.length >= 1 && (data.length == 1 || data[$-1] != 0 ),
                "Invariant requires data to not empty or zero");
    }

    immutable(BigDigit) [] data = ZERO;

    this(return scope immutable(BigDigit) [] x) pure nothrow @nogc @safe
    {
       data = x;
    }
  package(std)  // used from: std.bigint
    this(T)(T x) pure nothrow @safe scope if (isIntegral!T)
    {
        opAssign(x);
    }

    enum trustedAssumeUnique = function(BigDigit[] input) pure @trusted @nogc {
        return assumeUnique(input);
    };
public:
    // Length in uints
    @property size_t uintLength() pure nothrow const @safe @nogc scope
    {
        static if (BigDigit.sizeof == uint.sizeof)
        {
            return data.length;
        }
        else static if (BigDigit.sizeof == ulong.sizeof)
        {
            return data.length * 2 -
            ((data[$-1] & 0xFFFF_FFFF_0000_0000L) ? 1 : 0);
        }
    }
    @property size_t ulongLength() pure nothrow const @safe @nogc scope
    {
        static if (BigDigit.sizeof == uint.sizeof)
        {
            return (data.length + 1) >> 1;
        }
        else static if (BigDigit.sizeof == ulong.sizeof)
        {
            return data.length;
        }
    }

    // The value at (cast(ulong[]) data)[n]
    ulong peekUlong(size_t n) pure nothrow const @safe @nogc scope
    {
        static if (BigDigit.sizeof == int.sizeof)
        {
            if (data.length == n*2 + 1) return data[n*2];
            return data[n*2] + ((cast(ulong) data[n*2 + 1]) << 32 );
        }
        else static if (BigDigit.sizeof == long.sizeof)
        {
            return data[n];
        }
    }

    uint peekUint(size_t n) pure nothrow const @safe @nogc scope
    {
        static if (BigDigit.sizeof == int.sizeof)
        {
            return data[n];
        }
        else
        {
            immutable x = data[n >> 1];
            return (n & 1) ? cast(uint)(x >> 32) : cast(uint) x;
        }
    }

    ///
    void opAssign(Tulong)(Tulong u) pure nothrow @safe scope if (is (Tulong == ulong))
    {
        if (u == 0) data = ZERO;
        else if (u == 1) data = ONE;
        else if (u == 2) data = TWO;
        else if (u == 10) data = TEN;
        else
        {
            static if (BigDigit.sizeof == int.sizeof)
            {
                uint ulo = cast(uint)(u & 0xFFFF_FFFF);
                uint uhi = cast(uint)(u >> 32);
                if (uhi == 0)
                {
                    data = [ulo];
                }
                else
                {
                    data = [ulo, uhi];
                }
            }
            else static if (BigDigit.sizeof == long.sizeof)
            {
                data = [u];
            }
        }
    }
    void opAssign(Tdummy = void)(BigUint y) pure nothrow @nogc @safe scope
    {
        this.data = y.data;
    }

    ///
    int opCmp(Tdummy = void)(const BigUint y) pure nothrow @nogc const @safe scope
    {
        if (data.length != y.data.length)
            return (data.length > y.data.length) ?  1 : -1;
        size_t k = highestDifferentDigit(data, y.data);
        if (data[k] == y.data[k])
            return 0;
        return data[k] > y.data[k] ? 1 : -1;
    }

    ///
    int opCmp(Tulong)(Tulong y) pure nothrow @nogc const @safe scope if (is (Tulong == ulong))
    {
        if (data.length > maxBigDigits!Tulong)
            return 1;

        foreach_reverse (i; 0 .. maxBigDigits!Tulong)
        {
            BigDigit tmp = cast(BigDigit)(y>>(i*BigDigitBits));
            if (tmp == 0)
                if (data.length >= i+1)
                {
                    // Since ZERO is [0], so we cannot simply return 1 here, as
                    // data[i] would be 0 for i == 0 in that case.
                    return (data[i] > 0) ? 1 : 0;
                }
                else
                    continue;
            else
                if (i+1 > data.length)
                    return -1;
                else if (tmp != data[i])
                    return data[i] > tmp ? 1 : -1;
        }
        return 0;
    }

    bool opEquals(Tdummy = void)(ref const BigUint y) pure nothrow @nogc const @safe scope
    {
           return y.data[] == data[];
    }

    bool opEquals(Tdummy = void)(ulong y) pure nothrow @nogc const @safe scope
    {
        if (data.length > 2)
            return false;
        uint ylo = cast(uint)(y & 0xFFFF_FFFF);
        uint yhi = cast(uint)(y >> 32);
        if (data.length == 2 && data[1]!=yhi)
            return false;
        if (data.length == 1 && yhi != 0)
            return false;
        return (data[0] == ylo);
    }

    bool isZero() pure const nothrow @safe @nogc scope
    {
        return data.length == 1 && data[0] == 0;
    }

    size_t numBytes() pure nothrow const @safe @nogc scope
    {
        return data.length * BigDigit.sizeof;
    }

    // the extra bytes are added to the start of the string
    char [] toDecimalString(int frontExtraBytes) const pure nothrow @safe scope
    {
        immutable predictlength = 20+20*(data.length/2); // just over 19
        char [] buff = new char[frontExtraBytes + predictlength];
        ptrdiff_t sofar = biguintToDecimal(buff, data.dup);
        return buff[sofar-frontExtraBytes..$];
    }

    /** Convert to a hex string, printing a minimum number of digits 'minPadding',
     *  allocating an additional 'frontExtraBytes' at the start of the string.
     *  Padding is done with padChar, which may be '0' or ' '.
     *  'separator' is a digit separation character. If non-zero, it is inserted
     *  between every 8 digits.
     *  Separator characters do not contribute to the minPadding.
     */
    char [] toHexString(int frontExtraBytes, char separator = 0,
            int minPadding=0, char padChar = '0',
            LetterCase letterCase = LetterCase.upper) const pure nothrow @safe scope
    {
        // Calculate number of extra padding bytes
        size_t extraPad = (minPadding > data.length * 2 * BigDigit.sizeof)
            ? minPadding - data.length * 2 * BigDigit.sizeof : 0;

        // Length not including separator bytes
        size_t lenBytes = data.length * 2 * BigDigit.sizeof;

        // Calculate number of separator bytes
        size_t mainSeparatorBytes = separator ? (lenBytes  / 8) - 1 : 0;
        immutable totalSeparatorBytes = separator ? ((extraPad + lenBytes + 7) / 8) - 1: 0;

        char [] buff = new char[lenBytes + extraPad + totalSeparatorBytes + frontExtraBytes];
        biguintToHex(buff[$ - lenBytes - mainSeparatorBytes .. $], data, separator, letterCase);
        if (extraPad > 0)
        {
            if (separator)
            {
                size_t start = frontExtraBytes; // first index to pad
                if (extraPad &7)
                {
                    // Do 1 to 7 extra zeros.
                    buff[frontExtraBytes .. frontExtraBytes + (extraPad & 7)] = padChar;
                    buff[frontExtraBytes + (extraPad & 7)] = (padChar == ' ' ? ' ' : separator);
                    start += (extraPad & 7) + 1;
                }
                for (int i=0; i< (extraPad >> 3); ++i)
                {
                    buff[start .. start + 8] = padChar;
                    buff[start + 8] = (padChar == ' ' ? ' ' : separator);
                    start += 9;
                }
            }
            else
            {
                buff[frontExtraBytes .. frontExtraBytes + extraPad]=padChar;
            }
        }
        int z = frontExtraBytes;
        if (lenBytes > minPadding)
        {
            // Strip leading zeros.
            ptrdiff_t maxStrip = lenBytes - minPadding;
            while (z< buff.length-1 && (buff[z]=='0' || buff[z]==padChar) && maxStrip>0)
            {
                ++z;
                --maxStrip;
            }
        }
        if (padChar!='0')
        {
            // Convert leading zeros into padChars.
            for (size_t k= z; k< buff.length-1 && (buff[k]=='0' || buff[k]==padChar); ++k)
            {
                if (buff[k]=='0') buff[k]=padChar;
            }
        }
        return buff[z-frontExtraBytes..$];
    }

    /**
     * Convert to an octal string.
     */
    char[] toOctalString() pure nothrow @safe const scope
    {
        auto predictLength = 1 + data.length*BigDigitBits / 3;
        char[] buff = new char[predictLength];
        size_t firstNonZero = biguintToOctal(buff, data);
        return buff[firstNonZero .. $];
    }

    // return false if invalid character found
    bool fromHexString(Range)(Range s) scope if (
        isBidirectionalRange!Range && isSomeChar!(ElementType!Range))
    {
        import std.range : walkLength;

        //Strip leading zeros
        while (!s.empty && s.front == '0')
            s.popFront;

        if (s.empty)
        {
            data = ZERO;
            return true;
        }

        immutable len = (s.save.walkLength + 15) / 4;
        auto tmp = new BigDigit[len + 1];
        uint part, sofar, partcount;

        foreach_reverse (character; s)
        {
            if (character == '_')
                continue;

            uint x;
            if (character >= '0' && character <= '9')
            {
                x = character - '0';
            }
            else if (character >= 'A' && character <= 'F')
            {
                x = character - 'A' + 10;
            }
            else if (character >= 'a' && character <= 'f')
            {
                x = character - 'a' + 10;
            }
            else
            {
                return false;
            }

            part >>= 4;
            part |= (x << (32 - 4));
            ++partcount;

            if (partcount == 8)
            {
                tmp[sofar] = part;
                ++sofar;
                partcount = 0;
                part = 0;
            }
        }
        if (part)
        {
            for ( ; partcount != 8; ++partcount) part >>= 4;
            tmp[sofar] = part;
            ++sofar;
        }
        if (sofar == 0)
            data = ZERO;
        else
            data = trustedAssumeUnique(tmp[0 .. sofar]);

        return true;
    }

    // return true if OK; false if erroneous characters found
    bool fromDecimalString(Range)(Range s) scope if (
        isForwardRange!Range && isSomeChar!(ElementType!Range))
    {
        import std.range : walkLength;

        while (!s.empty && s.front == '0')
        {
            s.popFront;
        }

        if (s.empty)
        {
            data = ZERO;
            return true;
        }

        auto predict_length = (18 * 2 + 2 * s.save.walkLength) / 19;
        auto tmp = new BigDigit[predict_length];

        tmp.length = biguintFromDecimal(tmp, s);

        data = trustedAssumeUnique(tmp);
        return true;
    }

    void fromMagnitude(Range)(Range magnitude) scope
        if (isInputRange!Range
            && (isForwardRange!Range || hasLength!Range)
            && isUnsigned!(ElementType!Range))
    {
        while (!magnitude.empty && magnitude.front == 0)
            magnitude.popFront;
        static if (hasLength!Range)
            immutable inputLen = magnitude.length;
        else
            immutable inputLen = magnitude.save.walkLength;
        if (!inputLen)
        {
            this.data = ZERO;
            return;
        }
        // `magnitude` has its most significant element first but BigUint.data
        // stores the most significant last.
        BigDigit[] newDigits;
        alias E = ElementType!Range;
        static if (E.sizeof == BigDigit.sizeof)
        {
            newDigits = new BigDigit[inputLen];
            foreach_reverse (ref digit; newDigits)
            {
                digit = magnitude.front;
                magnitude.popFront();
            }
        }
        else static if (E.sizeof < BigDigit.sizeof)
        {
            enum elementsPerDigit = BigDigit.sizeof / E.sizeof;
            newDigits = new BigDigit[(inputLen + elementsPerDigit - 1) / elementsPerDigit];
            immutable remainder = inputLen % elementsPerDigit;
            // If there is a remainder specially assemble the most significant digit.
            if (remainder)
            {
                BigDigit tmp = magnitude.front;
                magnitude.popFront();
                foreach (_; 1 .. remainder)
                {
                    tmp = (tmp << (E.sizeof * 8)) | magnitude.front;
                    magnitude.popFront();
                }
                newDigits[$-1] = tmp;
            }
            // Assemble full digits from most to least significant.
            foreach_reverse (ref digit; newDigits[0 .. $ - int(remainder != 0)])
            {
                BigDigit tmp;
                static foreach (n; 0 .. elementsPerDigit)
                {
                    tmp |= cast(BigDigit) magnitude.front <<
                        ((BigDigit.sizeof - (E.sizeof * (n + 1))) * 8);
                    magnitude.popFront();
                }
                digit = tmp;
            }
        }
        else static if (E.sizeof > BigDigit.sizeof)
        {
            enum digitsPerElement = E.sizeof / BigDigit.sizeof;
            newDigits = new BigDigit[inputLen * digitsPerElement];
            size_t i = newDigits.length - 1;
            foreach (element; magnitude)
            {
                static foreach (n; 0 .. digitsPerElement)
                    newDigits[i - n] =
                        cast(BigDigit) (element >> ((E.sizeof - (BigDigit.sizeof * (n + 1))) * 8));
                i -= digitsPerElement;
            }
            while (newDigits[$-1] == 0)
                newDigits = newDigits[0 .. $-1];
        }
        else
            static assert(0);
        this.data = trustedAssumeUnique(newDigits);
        return;
    }

    nothrow pure @safe unittest
    {
        immutable BigDigit[] referenceData = [BigDigit(0x2003_4005), 0x6007_8009, 0xABCD];
        // Internal representation is most-significant-last but `fromMagnitude`
        // argument is most-significant-first.
        immutable BigDigit[] referenceMagnitude = [BigDigit(0xABCD), 0x6007_8009, 0x2003_4005];
        BigUint b;
        // Test with reference magnitude.
        b.fromMagnitude(referenceMagnitude);
        assert(b.data == referenceData);
        // Test ubyte array.
        import std.bitmanip : nativeToBigEndian;
        ubyte[] ubyteMagnitude = nativeToBigEndian(referenceMagnitude[0]) ~
            nativeToBigEndian(referenceMagnitude[1]) ~
            nativeToBigEndian(referenceMagnitude[2]);
        b.data = ZERO;
        b.fromMagnitude(ubyteMagnitude);
        assert(b.data == referenceData);
        // Test ulong array.
        static if (BigDigit.sizeof == uint.sizeof)
            immutable(ulong)[] ulongMagnitude = [ulong(referenceMagnitude[0]),
                ((cast(ulong) referenceMagnitude[1]) << 32) | referenceMagnitude[2],
            ];
        else static if (BigDigit.sizeof == ulong.sizeof)
            alias ulongMagnitude = referenceMagnitude;
        b.data = ZERO;
        b.fromMagnitude(ulongMagnitude);
        assert(b.data == referenceData);
    }

    ////////////////////////
    //
    // All of these member functions create a new BigUint.

    // return x >> y
    BigUint opBinary(string op, Tulong)(Tulong y) pure nothrow @safe const return scope
        if (op == ">>" && is (Tulong == ulong))
    {
        assert(y > 0, "Can not right shift BigUint by 0");
        uint bits = cast(uint) y & BIGDIGITSHIFTMASK;
        if ((y >> LG2BIGDIGITBITS) >= data.length) return BigUint(ZERO);
        uint words = cast(uint)(y >> LG2BIGDIGITBITS);
        if (bits == 0)
        {
            return BigUint(data[words..$]);
        }
        else
        {
            uint [] result = new BigDigit[data.length - words];
            multibyteShr(result, data[words..$], bits);

            if (result.length > 1 && result[$-1] == 0)
                return BigUint(trustedAssumeUnique(result[0 .. $-1]));
            else
                return BigUint(trustedAssumeUnique(result));
        }
    }

    // return x << y
    BigUint opBinary(string op, Tulong)(Tulong y) pure nothrow @safe const scope
        if (op == "<<" && is (Tulong == ulong))
    {
        assert(y > 0, "Can not left shift BigUint by 0");
        if (isZero()) return this;
        uint bits = cast(uint) y & BIGDIGITSHIFTMASK;
        assert((y >> LG2BIGDIGITBITS) < cast(ulong)(uint.max),
                "Shift result exceeds temporary store");
        uint words = cast(uint)(y >> LG2BIGDIGITBITS);
        BigDigit [] result = new BigDigit[data.length + words+1];
        result[0 .. words] = 0;
        if (bits == 0)
        {
            result[words .. words+data.length] = data[];
            return BigUint(trustedAssumeUnique(result[0 .. words+data.length]));
        }
        else
        {
            immutable c = multibyteShl(result[words .. words+data.length], data, bits);
            if (c == 0) return BigUint(trustedAssumeUnique(result[0 .. words+data.length]));
            result[$-1] = c;
            return BigUint(trustedAssumeUnique(result));
        }
    }

    // If wantSub is false, return x + y, leaving sign unchanged
    // If wantSub is true, return abs(x - y), negating sign if x < y
    static BigUint addOrSubInt(Tulong)(const scope BigUint x, Tulong y,
            bool wantSub, ref bool sign) pure nothrow @safe if (is(Tulong == ulong))
    {
        BigUint r;
        if (wantSub)
        {   // perform a subtraction
            if (x.data.length > 2)
            {
                // subInt returns GC allocated array, can be safely cast to immutable
                r.data = (() @trusted => cast(immutable) subInt(x.data, y))();
            }
            else
            {   // could change sign!
                ulong xx = x.data[0];
                if (x.data.length > 1)
                    xx += (cast(ulong) x.data[1]) << 32;
                ulong d;
                if (xx <= y)
                {
                    d = y - xx;
                    sign = !sign;
                }
                else
                {
                    d = xx - y;
                }
                if (d == 0)
                {
                    r = 0UL;
                    sign = false;
                    return r;
                }
                if (d > uint.max)
                {
                    r.data = [cast(uint)(d & 0xFFFF_FFFF), cast(uint)(d >> 32)];
                }
                else
                {
                    r.data = [cast(uint)(d & 0xFFFF_FFFF)];
                }
            }
        }
        else
        {
            // addInt returns GC allocated array, can be safely cast to immutable
            r.data = (() @trusted => cast(immutable) addInt(x.data, y))();
        }
        return r;
    }

    // If wantSub is false, return x + y, leaving sign unchanged.
    // If wantSub is true, return abs(x - y), negating sign if x<y
    static BigUint addOrSub(scope BigUint x, scope BigUint y, bool wantSub, ref bool sign)
        pure nothrow @safe
    {
        BigUint r;
        if (wantSub)
        {   // perform a subtraction
            bool negative;
            // sub returns GC allocated array, can be safely cast to immutable
            r.data = (() @trusted => cast(immutable) sub(x.data, y.data, &negative))();
            sign ^= negative;
            if (r.isZero())
            {
                sign = false;
            }
        }
        else
        {
            // add returns GC allocated array, can be safely cast to immutable
            r.data = (() @trusted => cast(immutable) add(x.data, y.data))();
        }
        return r;
    }


    //  return x*y.
    static BigUint mulInt(T = ulong)(BigUint x, T y) pure nothrow @safe
    {
        if (y == 0 || x == 0) return BigUint(ZERO);
        static if (T.sizeof * 8 <= 32)
            uint hi = 0;
        else
            uint hi = cast(uint) (y >>> 32);
        uint lo = cast(uint) (y & 0xFFFF_FFFF);
        uint [] result = new BigDigit[x.data.length+1+(hi != 0)];
        result[x.data.length] = multibyteMul(result[0 .. x.data.length], x.data, lo, 0);
        if (hi != 0)
        {
            result[x.data.length+1] = multibyteMulAdd!('+')(result[1 .. x.data.length+1],
                x.data, hi, 0);
        }
        return BigUint(removeLeadingZeros(trustedAssumeUnique(result)));
    }

    /*  return x * y.
     */
    static BigUint mul(scope BigUint x, scope BigUint y) pure nothrow @safe
    {
        if (y == 0 || x == 0)
            return BigUint(ZERO);
        auto len = x.data.length + y.data.length;
        BigDigit [] result = new BigDigit[len];
        if (y.data.length > x.data.length)
        {
            mulInternal(result, y.data, x.data);
        }
        else
        {
            if (x.data[]==y.data[]) squareInternal(result, x.data);
            else mulInternal(result, x.data, y.data);
        }
        // the highest element could be zero,
        // in which case we need to reduce the length
        return BigUint(removeLeadingZeros(trustedAssumeUnique(result)));
    }

    // return x / y
    static BigUint divInt(T)(return scope BigUint x, T y_) pure nothrow @safe
    if ( is(immutable T == immutable uint) )
    {
        uint y = y_;
        if (y == 1)
            return x;
        uint [] result = new BigDigit[x.data.length];
        if ((y&(-y))==y)
        {
            assert(y != 0, "BigUint division by zero");
            // perfect power of 2
            uint b = 0;
            for (;y != 1; y>>=1)
            {
                ++b;
            }
            multibyteShr(result, x.data, b);
        }
        else
        {
            result[] = x.data[];
            cast(void) multibyteDivAssign(result, y, 0);
        }
        return BigUint(removeLeadingZeros(trustedAssumeUnique(result)));
    }

    static BigUint divInt(T)(scope BigUint x, T y) pure nothrow @safe
    if ( is(immutable T == immutable ulong) )
    {
        if (y <= uint.max)
            return divInt!uint(x, cast(uint) y);
        if (x.data.length < 2)
            return BigUint(ZERO);
        uint hi = cast(uint)(y >>> 32);
        uint lo = cast(uint)(y & 0xFFFF_FFFF);
        immutable uint[2] z = [lo, hi];
        BigDigit[] result = new BigDigit[x.data.length - z.length + 1];
        divModInternal(result, null, x.data, z[]);
        return BigUint(removeLeadingZeros(trustedAssumeUnique(result)));
    }

    // return x % y
    static uint modInt(T)(scope BigUint x, T y_) pure if ( is(immutable T == immutable uint) )
    {
        import core.memory : GC;
        uint y = y_;
        assert(y != 0, "% 0 not allowed");
        if ((y&(-y)) == y)
        {   // perfect power of 2
            return x.data[0] & (y-1);
        }
        else
        {
            // horribly inefficient - malloc, copy, & store are unnecessary.
            uint [] wasteful = new BigDigit[x.data.length];
            wasteful[] = x.data[];
            immutable rem = multibyteDivAssign(wasteful, y, 0);
            () @trusted { GC.free(wasteful.ptr); } ();
            return rem;
        }
    }

    // return x / y
    static BigUint div(return scope BigUint x, scope BigUint y) pure nothrow @safe
    {
        if (y.data.length > x.data.length)
            return BigUint(ZERO);
        if (y.data.length == 1)
            return divInt(x, y.data[0]);
        BigDigit [] result = new BigDigit[x.data.length - y.data.length + 1];
           divModInternal(result, null, x.data, y.data);
        return BigUint(removeLeadingZeros(trustedAssumeUnique(result)));
    }

    // return x % y
    static BigUint mod(return scope BigUint x, scope BigUint y) pure nothrow @safe
    {
        if (y.data.length > x.data.length) return x;
        if (y.data.length == 1)
        {
            return BigUint([modInt(x, y.data[0])]);
        }
        BigDigit [] result = new BigDigit[x.data.length - y.data.length + 1];
        BigDigit [] rem = new BigDigit[y.data.length];
        divModInternal(result, rem, x.data, y.data);
        return BigUint(removeLeadingZeros(trustedAssumeUnique(rem)));
    }

    // Return x / y in quotient, x % y in remainder
    static void divMod(BigUint x, scope BigUint y,
                       out BigUint quotient, out BigUint remainder) pure nothrow @safe
    {
        /* TODO Qualify parameter `x` as `return` when it applies to `out` parameters */
        if (y.data.length > x.data.length)
        {
            quotient = 0uL;
            remainder = x;
        }
        else if (y.data.length == 1)
        {
            quotient = divInt(x, y.data[0]);
            remainder = BigUint([modInt(x, y.data[0])]);
        }
        else
        {
            BigDigit[] quot = new BigDigit[x.data.length - y.data.length + 1];
            BigDigit[] rem = new BigDigit[y.data.length];
            divModInternal(quot, rem, x.data, y.data);
            quotient = BigUint(removeLeadingZeros(trustedAssumeUnique(quot)));
            remainder = BigUint(removeLeadingZeros(trustedAssumeUnique(rem)));
        }
    }

    // return x op y
    static BigUint bitwiseOp(string op)(scope BigUint x, scope BigUint y, bool xSign, bool ySign, ref bool resultSign)
    pure nothrow @safe if (op == "|" || op == "^" || op == "&")
    {
        auto d1 = includeSign(x.data, y.uintLength, xSign);
        auto d2 = includeSign(y.data, x.uintLength, ySign);

        foreach (i; 0 .. d1.length)
        {
            mixin("d1[i] " ~ op ~ "= d2[i];");
        }

        mixin("resultSign = xSign " ~ op ~ " ySign;");

        if (resultSign)
        {
            twosComplement(d1, d1);
        }

        return BigUint(removeLeadingZeros(trustedAssumeUnique(d1)));
    }

    /**
     * Return a BigUint which is x raised to the power of y.
     * Method: Powers of 2 are removed from x, then left-to-right binary
     * exponentiation is used.
     * Memory allocation is minimized: at most one temporary BigUint is used.
     */
    static BigUint pow(return scope BigUint x, ulong y) pure nothrow @safe
    {
        // Deal with the degenerate cases first.
        if (y == 0) return BigUint(ONE);
        if (y == 1) return x;
        if (x == 0 || x == 1) return x;

        BigUint result;

        // Simplify, step 1: Remove all powers of 2.
        uint firstnonzero = firstNonZeroDigit(x.data);
        // Now we know x = x[firstnonzero..$] * (2^^(firstnonzero*BigDigitBits))
        // where BigDigitBits = BigDigit.sizeof * 8

        // See if x[firstnonzero..$] can now fit into a single digit.
        bool singledigit = ((x.data.length - firstnonzero) == 1);
        // If true, then x0 is that digit
        // and the result will be (x0 ^^ y) * (2^^(firstnonzero*y*BigDigitBits))
        BigDigit x0 = x.data[firstnonzero];
        assert(x0 != 0, "pow(0, y) not allowed");
        // Length of the non-zero portion
        size_t nonzerolength = x.data.length - firstnonzero;
        ulong y0;
        uint evenbits = 0; // number of even bits in the bottom of x
        while (!(x0 & 1))
        {
            x0 >>= 1;
            ++evenbits;
        }

        if (x.data.length- firstnonzero == 2)
        {
            // Check for a single digit straddling a digit boundary
            const BigDigit x1 = x.data[firstnonzero+1];
            if ((x1 >> evenbits) == 0)
            {
                x0 |= (x1 << (BigDigit.sizeof * 8 - evenbits));
                singledigit = true;
            }
        }
        // Now if (singledigit), x^^y  = (x0 ^^ y) * 2^^(evenbits * y) * 2^^(firstnonzero*y*BigDigitBits))

        uint evenshiftbits = 0; // Total powers of 2 to shift by, at the end

        // Simplify, step 2: For singledigits, see if we can trivially reduce y

        BigDigit finalMultiplier = 1UL;

        if (singledigit)
        {
            // x fits into a single digit. Raise it to the highest power we can
            // that still fits into a single digit, then reduce the exponent accordingly.
            // We're quite likely to have a residual multiply at the end.
            // For example, 10^^100 = (((5^^13)^^7) * 5^^9) * 2^^100.
            // and 5^^13 still fits into a uint.
            evenshiftbits  = cast(uint)( (evenbits * y) & BIGDIGITSHIFTMASK);
            if (x0 == 1)
            {   // Perfect power of 2
                result = 1UL;
                return result << (evenbits + firstnonzero * 8 * BigDigit.sizeof) * y;
            }
            immutable p = highestPowerBelowUintMax(x0);
            if (y <= p)
            {   // Just do it with pow
                result = cast(ulong) intpow(x0, y);
                if (evenbits + firstnonzero == 0)
                    return result;
                return result << (evenbits + firstnonzero * 8 * BigDigit.sizeof) * y;
            }
            y0 = y / p;
            finalMultiplier = intpow(x0, y - y0*p);
            x0 = intpow(x0, p);
            // Result is x0
            nonzerolength = 1;
        }
        // Now if (singledigit), x^^y  = finalMultiplier * (x0 ^^ y0) * 2^^(evenbits * y) * 2^^(firstnonzero*y*BigDigitBits))

        // Perform a crude check for overflow and allocate result buffer.
        // The length required is y * lg2(x) bits.
        // which will always fit into y*x.length digits. But this is
        // a gross overestimate if x is small (length 1 or 2) and the highest
        // digit is nearly empty.
        // A better estimate is:
        //   y * lg2(x[$-1]/BigDigit.max) + y * (x.length - 1) digits,
        //  and the first term is always between
        //  y * (bsr(x.data[$-1]) + 1) / BIGDIGITBITS and
        //  y * (bsr(x.data[$-1]) + 2) / BIGDIGITBITS
        // For single digit payloads, we already have
        //   x^^y  = finalMultiplier * (x0 ^^ y0) * 2^^(evenbits * y) * 2^^(firstnonzero*y*BigDigitBits))
        // and x0 is almost a full digit, so it's a tight estimate.
        // Number of digits is therefore 1 + x0.length*y0 + (evenbits*y)/BIGDIGIT + firstnonzero*y
        // Note that the divisions must be rounded up.

        // Estimated length in BigDigits
        immutable estimatelength = singledigit
            ? 1 + y0 + ((evenbits*y  + BigDigit.sizeof * 8 - 1) / (BigDigit.sizeof *8)) + firstnonzero*y
            :  x.data.length * y;
        // Imprecise check for overflow. Makes the extreme cases easier to debug
        // (less extreme overflow will result in an out of memory error).
        if (estimatelength > uint.max/(4*BigDigit.sizeof))
            assert(0, "Overflow in BigInt.pow");

        // The result buffer includes space for all the trailing zeros
        BigDigit [] resultBuffer = new BigDigit[cast(size_t) estimatelength];

        // Do all the powers of 2!
        size_t result_start = cast(size_t)( firstnonzero * y
            + (singledigit ? ((evenbits * y) >> LG2BIGDIGITBITS) : 0));

        resultBuffer[0 .. result_start] = 0;
        BigDigit [] t1 = resultBuffer[result_start..$];
        BigDigit [] r1;

        if (singledigit)
        {
            r1 = t1[0 .. 1];
            r1[0] = x0;
            y = y0;
        }
        else
        {
            // It's not worth right shifting by evenbits unless we also shrink the length after each
            // multiply or squaring operation. That might still be worthwhile for large y.
            r1 = t1[0 .. x.data.length - firstnonzero];
            r1[0..$] = x.data[firstnonzero..$];
        }

        if (y>1)
        {   // Set r1 = r1 ^^ y.
            // The secondary buffer only needs space for the multiplication results
            BigDigit [] t2 = new BigDigit[resultBuffer.length - result_start];
            BigDigit [] r2;

            int shifts = 63; // num bits in a long
            while (!(y & 0x8000_0000_0000_0000L))
            {
                y <<= 1;
                --shifts;
            }
            y <<=1;

            while (y != 0)
            {
                // For each bit of y: Set r1 =  r1 * r1
                // If the bit is 1, set r1 = r1 * x
                // Eg, if y is 0b101, result = ((x^^2)^^2)*x == x^^5.
                // Optimization opportunity: if more than 2 bits in y are set,
                // it's usually possible to reduce the number of multiplies
                // by caching odd powers of x. eg for y = 54,
                // (0b110110), set u = x^^3, and result is ((u^^8)*u)^^2
                r2 = t2[0 .. r1.length*2];
                squareInternal(r2, r1);
                if (y & 0x8000_0000_0000_0000L)
                {
                    r1 = t1[0 .. r2.length + nonzerolength];
                    if (singledigit)
                    {
                        r1[$-1] = multibyteMul(r1[0 .. $-1], r2, x0, 0);
                    }
                    else
                    {
                        mulInternal(r1, r2, x.data[firstnonzero..$]);
                    }
                }
                else
                {
                    r1 = t1[0 .. r2.length];
                    r1[] = r2[];
                }
                y <<=1;
                shifts--;
            }
            while (shifts>0)
            {
                r2 = t2[0 .. r1.length * 2];
                squareInternal(r2, r1);
                r1 = t1[0 .. r2.length];
                r1[] = r2[];
                --shifts;
            }
        }

        if (finalMultiplier != 1)
        {
            const BigDigit carry = multibyteMul(r1, r1, finalMultiplier, 0);
            if (carry)
            {
                r1 = t1[0 .. r1.length + 1];
                r1[$-1] = carry;
            }
        }
        if (evenshiftbits)
        {
            const BigDigit carry = multibyteShl(r1, r1, evenshiftbits);
            if (carry != 0)
            {
                r1 = t1[0 .. r1.length + 1];
                r1[$ - 1] = carry;
            }
        }
        while (r1[$ - 1]==0)
        {
            r1=r1[0 .. $ - 1];
        }
        return BigUint(trustedAssumeUnique(resultBuffer[0 .. result_start + r1.length]));
    }

    // Implement toHash so that BigUint works properly as an AA key.
    size_t toHash() const @nogc nothrow pure @safe scope
    {
        return .hashOf(data);
    }

} // end BigUint

@safe pure nothrow unittest
{
    // ulong comparison test
    BigUint a = [1];
    assert(a == 1);
    // https://issues.dlang.org/show_bug.cgi?id=9548
    assert(a < 0x8000_0000_0000_0000UL);

    // https://issues.dlang.org/show_bug.cgi?id=12234
    BigUint z = [0];
    assert(z == 0UL);
    assert(!(z > 0UL));
    assert(!(z < 0UL));
}

// https://issues.dlang.org/show_bug.cgi?id=16223
@system pure nothrow unittest
{
    BigUint a = [3];
    int b = 5;
    assert(BigUint.mulInt(a,b) == 15);
}

// Remove leading zeros from x, to restore the BigUint invariant
inout(BigDigit) [] removeLeadingZeros(return scope inout(BigDigit) [] x) pure nothrow @safe
{
    size_t k = x.length;
    while (k>1 && x[k - 1]==0) --k;
    return x[0 .. k];
}

pure @safe unittest
{
   BigUint r = BigUint([5]);
   BigUint t = BigUint([7]);
   BigUint s = BigUint.mod(r, t);
   assert(s == 5);
}


@safe pure unittest
{
    BigUint r;
    r = 5UL;
    assert(r.peekUlong(0) == 5UL);
    assert(r.peekUint(0) == 5U);
    r = 0x1234_5678_9ABC_DEF0UL;
    assert(r.peekUlong(0) == 0x1234_5678_9ABC_DEF0UL);
    assert(r.peekUint(0) == 0x9ABC_DEF0U);
}


// Pow tests
pure @safe unittest
{
    BigUint r, s;
    r.fromHexString("80000000_00000001");
    s = BigUint.pow(r, 5);
    r.fromHexString("08000000_00000000_50000000_00000001_40000000_00000002_80000000"
      ~ "_00000002_80000000_00000001");
    assert(s == r);
    s = 10UL;
    s = BigUint.pow(s, 39);
    r.fromDecimalString("1000000000000000000000000000000000000000");
    assert(s == r);
    r.fromHexString("1_E1178E81_00000000");
    s = BigUint.pow(r, 15); // Regression test: this used to overflow array bounds

    r.fromDecimalString("000_000_00");
    assert(r == 0);
    r.fromDecimalString("0007");
    assert(r == 7);
    r.fromDecimalString("0");
    assert(r == 0);
}

// Radix conversion tests
@safe pure unittest
{
    BigUint r;
    r.fromHexString("1_E1178E81_00000000");
    assert(r.toHexString(0, '_', 0) == "1_E1178E81_00000000");
    assert(r.toHexString(0, '_', 20) == "0001_E1178E81_00000000");
    assert(r.toHexString(0, '_', 16+8) == "00000001_E1178E81_00000000");
    assert(r.toHexString(0, '_', 16+9) == "0_00000001_E1178E81_00000000");
    assert(r.toHexString(0, '_', 16+8+8) ==   "00000000_00000001_E1178E81_00000000");
    assert(r.toHexString(0, '_', 16+8+8+1) ==      "0_00000000_00000001_E1178E81_00000000");
    assert(r.toHexString(0, '_', 16+8+8+1, ' ') == "                  1_E1178E81_00000000");
    assert(r.toHexString(0, 0, 16+8+8+1) == "00000000000000001E1178E8100000000");
    r = 0UL;
    assert(r.toHexString(0, '_', 0) == "0");
    assert(r.toHexString(0, '_', 7) == "0000000");
    assert(r.toHexString(0, '_', 7, ' ') == "      0");
    assert(r.toHexString(0, '#', 9) == "0#00000000");
    assert(r.toHexString(0, 0, 9) == "000000000");
}

//
@safe pure unittest
{
    BigUint r;
    r.fromHexString("1_E1178E81_00000000");
    assert(r.toHexString(0, '_', 0, '0', LetterCase.lower) == "1_e1178e81_00000000");
    assert(r.toHexString(0, '_', 20, '0', LetterCase.lower) == "0001_e1178e81_00000000");
    assert(r.toHexString(0, '_', 16+8, '0', LetterCase.lower) == "00000001_e1178e81_00000000");
    assert(r.toHexString(0, '_', 16+9, '0', LetterCase.lower) == "0_00000001_e1178e81_00000000");
    assert(r.toHexString(0, '_', 16+8+8, '0', LetterCase.lower) ==   "00000000_00000001_e1178e81_00000000");
    assert(r.toHexString(0, '_', 16+8+8+1, '0', LetterCase.lower) == "0_00000000_00000001_e1178e81_00000000");
    assert(r.toHexString(0, '_', 16+8+8+1, ' ', LetterCase.lower) == "                  1_e1178e81_00000000");
    assert(r.toHexString(0, 0, 16+8+8+1, '0', LetterCase.lower) == "00000000000000001e1178e8100000000");
    r = 0UL;
    assert(r.toHexString(0, '_', 0, '0', LetterCase.lower) == "0");
    assert(r.toHexString(0, '_', 7, '0', LetterCase.lower) == "0000000");
    assert(r.toHexString(0, '_', 7, ' ', LetterCase.lower) == "      0");
    assert(r.toHexString(0, '#', 9, '0', LetterCase.lower) == "0#00000000");
    assert(r.toHexString(0, 'Z', 9, '0', LetterCase.lower) == "0Z00000000");
    assert(r.toHexString(0, 0, 9, '0', LetterCase.lower) == "000000000");
}


private:
void twosComplement(const(BigDigit) [] x, BigDigit[] result)
pure nothrow @safe
{
    foreach (i; 0 .. x.length)
    {
        result[i] = ~x[i];
    }
    result[x.length..$] = BigDigit.max;

    foreach (i; 0 .. result.length)
    {
        if (result[i] == BigDigit.max)
        {
            result[i] = 0;
        }
        else
        {
            result[i] += 1;
            break;
        }
    }
}

// Encode BigInt as BigDigit array (sign and 2's complement)
BigDigit[] includeSign(scope const(BigDigit) [] x, size_t minSize, bool sign)
pure nothrow @safe
{
    size_t length = (x.length > minSize) ? x.length : minSize;
    BigDigit [] result = new BigDigit[length];
    if (sign)
    {
        twosComplement(x, result);
    }
    else
    {
        result[0 .. x.length] = x;
    }
    return result;
}

// works for any type
T intpow(T)(T x, ulong n) pure nothrow @safe
{
    T p;

    switch (n)
    {
    case 0:
        p = 1;
        break;

    case 1:
        p = x;
        break;

    case 2:
        p = x * x;
        break;

    default:
        p = 1;
        while (1)
        {
            if (n & 1)
                p *= x;
            n >>= 1;
            if (!n)
                break;
            x *= x;
        }
        break;
    }
    return p;
}


//  returns the maximum power of x that will fit in a uint.
int highestPowerBelowUintMax(uint x) pure nothrow @safe
{
     assert(x > 1, "x must be greater than 1");
     static immutable ubyte [22] maxpwr = [ 31, 20, 15, 13, 12, 11, 10, 10, 9, 9,
                                          8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7];
     if (x<24) return maxpwr[x-2];
     if (x<41) return 6;
     if (x<85) return 5;
     if (x<256) return 4;
     if (x<1626) return 3;
     if (x<65_536) return 2;
     return 1;
}

//  returns the maximum power of x that will fit in a ulong.
int highestPowerBelowUlongMax(uint x) pure nothrow @safe
{
     assert(x > 1, "x must be greater than 1");
     static immutable ubyte [39] maxpwr = [ 63, 40, 31, 27, 24, 22, 21, 20, 19, 18,
                                         17, 17, 16, 16, 15, 15, 15, 15, 14, 14,
                                         14, 14, 13, 13, 13, 13, 13, 13, 13, 12,
                                         12, 12, 12, 12, 12, 12, 12, 12, 12];
     if (x<41) return maxpwr[x-2];
     if (x<57) return 11;
     if (x<85) return 10;
     if (x<139) return 9;
     if (x<256) return 8;
     if (x<566) return 7;
     if (x<1626) return 6;
     if (x<7132) return 5;
     if (x<65_536) return 4;
     if (x<2_642_246) return 3;
     return 2;
}

version (StdUnittest)
{

private int slowHighestPowerBelowUintMax(uint x) pure nothrow @safe
{
     int pwr = 1;
     for (ulong q = x;x*q < cast(ulong) uint.max; )
     {
         q*=x; ++pwr;
     }
     return pwr;
}

@safe pure unittest
{
    assert(highestPowerBelowUintMax(10)==9);
    for (int k=82; k<88; ++k)
    {
        assert(highestPowerBelowUintMax(k)== slowHighestPowerBelowUintMax(k));
    }
}

}


/*  General unsigned subtraction routine for bigints.
 *  Sets result = x - y. If the result is negative, negative will be true.
 * Returns:
 *    unique memory
 */
BigDigit [] sub(const scope BigDigit [] x, const scope BigDigit [] y, bool *negative)
pure nothrow @safe
{
    if (x.length == y.length)
    {
        // There's a possibility of cancellation, if x and y are almost equal.
        ptrdiff_t last = highestDifferentDigit(x, y);
        BigDigit [] result = new BigDigit[last+1];
        if (x[last] < y[last])
        {   // we know result is negative
            multibyteSub(result[0 .. last+1], y[0 .. last+1], x[0 .. last+1], 0);
            *negative = true;
        }
        else
        {   // positive or zero result
            multibyteSub(result[0 .. last+1], x[0 .. last+1], y[0 .. last+1], 0);
            *negative = false;
        }
        while (result.length > 1 && result[$-1] == 0)
        {
            result = result[0..$-1];
        }
//        if (result.length >1 && result[$-1]==0) return result[0..$-1];
        return result;
    }
    // Lengths are different
    const(BigDigit) [] large, small;
    if (x.length < y.length)
    {
        *negative = true;
        large = y; small = x;
    }
    else
    {
        *negative = false;
        large = x; small = y;
    }
    // result.length will be equal to larger length, or could decrease by 1.


    BigDigit [] result = new BigDigit[large.length];
    BigDigit carry = multibyteSub(result[0 .. small.length], large[0 .. small.length], small, 0);
    result[small.length..$] = large[small.length..$];
    if (carry)
    {
        multibyteIncrementAssign!('-')(result[small.length..$], carry);
    }
    while (result.length > 1 && result[$-1] == 0)
    {
        result = result[0..$-1];
    }
    return result;
}


/*
 * return a + b
 * Returns:
 *    unique memory
 */
BigDigit [] add(const scope BigDigit [] a, const scope BigDigit [] b) pure nothrow @safe
{
    const(BigDigit) [] x, y;
    if (a.length < b.length)
    {
        x = b; y = a;
    }
    else
    {
        x = a; y = b;
    }
    // now we know x.length > y.length
    // create result. add 1 in case it overflows
    BigDigit [] result = new BigDigit[x.length + 1];

    BigDigit carry = multibyteAdd(result[0 .. y.length], x[0 .. y.length], y, 0);
    if (x.length != y.length)
    {
        result[y.length..$-1]= x[y.length..$];
        carry  = multibyteIncrementAssign!('+')(result[y.length..$-1], carry);
    }
    if (carry)
    {
        result[$-1] = carry;
        return result;
    }
    else
        return result[0..$-1];
}

/**  return x + y
 */
BigDigit [] addInt(const BigDigit[] x, ulong y) @safe pure nothrow
{
    uint hi = cast(uint)(y >>> 32);
    uint lo = cast(uint)(y& 0xFFFF_FFFF);
    auto len = x.length;
    if (x.length < 2 && hi != 0) ++len;
    BigDigit [] result = new BigDigit[len+1];
    result[0 .. x.length] = x[];
    if (x.length < 2 && hi != 0)
    {
        result[1]=hi;
        hi=0;
    }
    uint carry = multibyteIncrementAssign!('+')(result[0..$-1], lo);
    if (hi != 0) carry += multibyteIncrementAssign!('+')(result[1..$-1], hi);
    if (carry)
    {
        result[$-1] = carry;
        return result;
    }
    else
        return result[0..$-1];
}

/** Return x - y.
 *  x must be greater than y.
 */
BigDigit [] subInt(const BigDigit[] x, ulong y) pure nothrow @safe
{
    uint hi = cast(uint)(y >>> 32);
    uint lo = cast(uint)(y & 0xFFFF_FFFF);
    BigDigit [] result = new BigDigit[x.length];
    result[] = x[];
    multibyteIncrementAssign!('-')(result[], lo);
    if (hi)
        multibyteIncrementAssign!('-')(result[1..$], hi);
    if (result[$-1] == 0)
        return result[0..$-1];
    else
        return result;
}

/**  General unsigned multiply routine for bigints.
 *  Sets result = x * y.
 *
 *  The length of y must not be larger than the length of x.
 *  Different algorithms are used, depending on the lengths of x and y.
 *  TODO: "Modern Computer Arithmetic" suggests the OddEvenKaratsuba algorithm for the
 *  unbalanced case. (But I doubt it would be faster in practice).
 *
 */
void mulInternal(BigDigit[] result, const(BigDigit)[] x, const(BigDigit)[] y)
    pure nothrow @safe
{
    import core.memory : GC;
    assert( result.length == x.length + y.length,
            "result array must have enough space to store computed result");
    assert( y.length > 0, "y must not be empty");
    assert( x.length >= y.length, "x must be greater or equal than y");
    if (y.length <= KARATSUBALIMIT)
    {
        // Small multiplier, we'll just use the asm classic multiply.
        if (y.length == 1)
        {   // Trivial case, no cache effects to worry about
            result[x.length] = multibyteMul(result[0 .. x.length], x, y[0], 0);
            return;
        }

        immutable CACHELIMIT = getCacheLimit;
        if (x.length + y.length < CACHELIMIT)
            return mulSimple(result, x, y);

        // If x is so big that it won't fit into the cache, we divide it into chunks
        // Every chunk must be greater than y.length.
        // We make the first chunk shorter, if necessary, to ensure this.

        auto chunksize = CACHELIMIT / y.length;
        immutable residual  =  x.length % chunksize;
        if (residual < y.length)
        {
            chunksize -= y.length;
        }

        // Use schoolbook multiply.
        mulSimple(result[0 .. chunksize + y.length], x[0 .. chunksize], y);
        auto done = chunksize;

        while (done < x.length)
        {
            // result[done .. done+ylength] already has a value.
            chunksize = (done + (CACHELIMIT / y.length) < x.length) ? (CACHELIMIT / y.length) :  x.length - done;
            BigDigit [KARATSUBALIMIT] partial;
            partial[0 .. y.length] = result[done .. done+y.length];
            mulSimple(result[done .. done+chunksize+y.length], x[done .. done+chunksize], y);
            addAssignSimple(result[done .. done+chunksize + y.length], partial[0 .. y.length]);
            done += chunksize;
        }
        return;
    }

    immutable half = (x.length >> 1) + (x.length & 1);
    if (2*y.length*y.length <= x.length*x.length)
    {
        // UNBALANCED MULTIPLY
        // Use school multiply to cut into quasi-squares of Karatsuba-size
        // or larger. The ratio of the two sides of the 'square' must be
        // between 1.414:1 and 1:1. Use Karatsuba on each chunk.
        //
        // For maximum performance, we want the ratio to be as close to
        // 1:1 as possible. To achieve this, we can either pad x or y.
        // The best choice depends on the modulus x%y.
        auto numchunks = x.length / y.length;
        auto chunksize = y.length;
        auto extra =  x.length % y.length;
        auto maxchunk = chunksize + extra;
        bool paddingY; // true = we're padding Y, false = we're padding X.
        bool isExtraSmall = extra * extra * 2 < y.length * y.length;
        if (numchunks == 1 && isExtraSmall)
        {
            // We divide (x_first_half * y) and (x_last_half * y)
            // between 1.414:1 and 1.707:1 (1.707 = 1+1/sqrt(2)).
            // (1.414 ~ 1.707)/2:1 is balanced.
            BigDigit [] scratchbuff = new BigDigit[karatsubaRequiredBuffSize(y.length) + y.length];
            BigDigit [] partial = scratchbuff[$ - y.length .. $];
            scratchbuff = scratchbuff[0 .. $ - y.length];
            mulKaratsuba(result[0 .. half + y.length], y, x[0 .. half], scratchbuff);
            partial[] = result[half .. half + y.length];
            mulKaratsuba(result[half .. $], y, x[half .. $], scratchbuff);
            BigDigit c = addAssignSimple(result[half .. half + y.length], partial);
            if (c) multibyteIncrementAssign!('+')(result[half + y.length..$], c);
            () @trusted { GC.free(scratchbuff.ptr); } ();
        }
        else
        {
            if (isExtraSmall)
            {
                // The leftover bit is small enough that it should be incorporated
                // in the existing chunks.
                // Make all the chunks a tiny bit bigger
                // (We're padding y with zeros)
                chunksize += extra / numchunks;
                extra = x.length - chunksize*numchunks;
                // there will probably be a few left over.
                // Every chunk will either have size chunksize, or chunksize+1.
                maxchunk = chunksize + 1;
                paddingY = true;
                assert(chunksize + extra + chunksize *(numchunks-1) == x.length,
                    "Unexpected size");
            }
            else
            {
                // the extra bit is large enough that it's worth making a new chunk.
                // (This means we're padding x with zeros, when doing the first one).
                maxchunk = chunksize;
                ++numchunks;
                paddingY = false;
                assert(extra + chunksize *(numchunks-1) == x.length,
                    "Unexpected size");
            }
            // We make the buffer a bit bigger so we have space for the partial sums.
            BigDigit [] scratchbuff = new BigDigit[karatsubaRequiredBuffSize(maxchunk) + y.length];
            BigDigit [] partial = scratchbuff[$ - y.length .. $];
            scratchbuff = scratchbuff[0 .. $ - y.length];
            size_t done; // how much of X have we done so far?
            if (paddingY)
            {
                // If the first chunk is bigger, do it first. We're padding y.
                mulKaratsuba(result[0 .. y.length + chunksize + (extra > 0 ? 1 : 0 )],
                    x[0 .. chunksize + (extra>0?1:0)], y, scratchbuff);
                done = chunksize + (extra > 0 ? 1 : 0);
                if (extra) --extra;
            }
            else
            {   // We're padding X. Begin with the extra bit.
                mulKaratsuba(result[0 .. y.length + extra], y, x[0 .. extra], scratchbuff);
                done = extra;
                extra = 0;
            }
            immutable basechunksize = chunksize;
            while (done < x.length)
            {
                chunksize = basechunksize + (extra > 0 ? 1 : 0);
                if (extra) --extra;
                partial[] = result[done .. done+y.length];
                mulKaratsuba(result[done .. done + y.length + chunksize],
                        x[done .. done+chunksize], y, scratchbuff);
                addAssignSimple(result[done .. done + y.length + chunksize], partial);
                done += chunksize;
            }
            () @trusted { GC.free(scratchbuff.ptr); } ();
        }
    }
    else
    {
        // Balanced. Use Karatsuba directly.
        BigDigit [] scratchbuff = new BigDigit[karatsubaRequiredBuffSize(x.length)];
        mulKaratsuba(result, x, y, scratchbuff);
        () @trusted { GC.free(scratchbuff.ptr); } ();
    }
}

// https://issues.dlang.org/show_bug.cgi?id=20493
@safe unittest
{
    // the bug report has a testcase with very large numbers (~10^3800 and ~10^2300)
    // the number itself isn't important, only the amount of digits, so we do a simpler
    // multiplication of the same size, analogous to:
    // 11111111 * 11111111 = 0123456787654321
    // but instead of base 10, it's in base `BigDigit`

    BigDigit[398] x = 1;
    BigDigit[236] y = 1;
    BigDigit[x.length + y.length] result;
    mulInternal(result[], x[], y[]);

    // create an array of the form [1, 2, ..., y.length, ..., y.length, y.length-1, ..., 1, 0]
    BigDigit[x.length + y.length] expected = y.length;
    foreach (BigDigit i; 0 .. y.length)
    {
        expected[i] = i+1;
        expected[$-1-i] = i;
    }

    assert(result == expected);
}

/**  General unsigned squaring routine for BigInts.
 *   Sets result = x*x.
 *   NOTE: If the highest half-digit of x is zero, the highest digit of result will
 *   also be zero.
 */
void squareInternal(BigDigit[] result, const BigDigit[] x) pure nothrow @safe
{
  import core.memory : GC;
  // Squaring is potentially half a multiply, plus add the squares of
  // the diagonal elements.
  assert(result.length == 2*x.length,
     "result needs to have twice the capacity of x");
  if (x.length <= KARATSUBASQUARELIMIT)
  {
      if (x.length == 1)
      {
         result[1] = multibyteMul(result[0 .. 1], x, x[0], 0);
         return;
      }
      return squareSimple(result, x);
  }
  // The nice thing about squaring is that it always stays balanced
  BigDigit [] scratchbuff = new BigDigit[karatsubaRequiredBuffSize(x.length)];
  squareKaratsuba(result, x, scratchbuff);
  () @trusted { GC.free(scratchbuff.ptr); } ();
}


import core.bitop : bsr;

/// if remainder is null, only calculate quotient.
void divModInternal(BigDigit [] quotient, BigDigit[] remainder, const BigDigit [] u,
        const BigDigit [] v) pure nothrow @safe
{
    import core.memory : GC;
    assert(quotient.length == u.length - v.length + 1,
        "Invalid quotient length");
    assert(remainder == null || remainder.length == v.length,
        "Invalid remainder");
    assert(v.length > 1, "v must have more than 1 element");
    assert(u.length >= v.length, "u must be as longer or longer than v");

    // Normalize by shifting v left just enough so that
    // its high-order bit is on, and shift u left the
    // same amount. The highest bit of u will never be set.

    BigDigit [] vn = new BigDigit[v.length];
    BigDigit [] un = new BigDigit[u.length + 1];
    // How much to left shift v, so that its MSB is set.
    uint s = BIGDIGITSHIFTMASK - bsr(v[$-1]);
    if (s != 0)
    {
        multibyteShl(vn, v, s);
        un[$-1] = multibyteShl(un[0..$-1], u, s);
    }
    else
    {
        vn[] = v[];
        un[0..$-1] = u[];
        un[$-1] = 0;
    }
    if (quotient.length<FASTDIVLIMIT)
    {
        schoolbookDivMod(quotient, un, vn);
    }
    else
    {
        blockDivMod(quotient, un, vn);
    }

    // Unnormalize remainder, if required.
    if (remainder != null)
    {
        if (s == 0) remainder[] = un[0 .. vn.length];
        else multibyteShr(remainder, un[0 .. vn.length+1], s);
    }
    () @trusted { GC.free(un.ptr); GC.free(vn.ptr); } ();
}

pure @safe unittest
{
    immutable(uint) [] u = [0, 0xFFFF_FFFE, 0x8000_0000];
    immutable(uint) [] v = [0xFFFF_FFFF, 0x8000_0000];
    uint [] q = new uint[u.length - v.length + 1];
    uint [] r = new uint[2];
    divModInternal(q, r, u, v);
    assert(q[]==[0xFFFF_FFFFu, 0]);
    assert(r[]==[0xFFFF_FFFFu, 0x7FFF_FFFF]);
    u = [0, 0xFFFF_FFFE, 0x8000_0001];
    v = [0xFFFF_FFFF, 0x8000_0000];
    divModInternal(q, r, u, v);
}


// Converts a big uint to a hexadecimal string.
//
// Optionally, a separator character (eg, an underscore) may be added between
// every 8 digits.
// buff.length must be data.length*8 if separator is zero,
// or data.length*9 if separator is non-zero. It will be completely filled.
char [] biguintToHex(return scope char [] buff, const scope BigDigit [] data, char separator=0,
        LetterCase letterCase = LetterCase.upper) pure nothrow @safe
{
    int x=0;
    for (ptrdiff_t i=data.length - 1; i >= 0; --i)
    {
        toHexZeroPadded(buff[x .. x+8], data[i], letterCase);
        x+=8;
        if (separator)
        {
            if (i>0) buff[x] = separator;
            ++x;
        }
    }
    return buff;
}

/**
 * Convert a big uint into an octal string.
 *
 * Params:
 *  buff = The destination buffer for the octal string. Must be large enough to
 *      store the result, including leading zeroes, which is
 *      ceil(data.length * BigDigitBits / 3) characters.
 *      The buffer is filled from back to front, starting from `buff[$-1]`.
 *  data = The biguint to be converted.
 *
 * Returns: The index of the leading non-zero digit in `buff`. Will be
 * `buff.length - 1` if the entire big uint is zero.
 */
size_t biguintToOctal(char[] buff, const(BigDigit)[] data)
    pure nothrow @safe @nogc
{
    ubyte carry = 0;
    int shift = 0;
    size_t penPos = buff.length - 1;
    size_t lastNonZero = buff.length - 1;

    pragma(inline) void output(uint digit) @nogc nothrow
    {
        if (digit != 0)
            lastNonZero = penPos;
        buff[penPos--] = cast(char)('0' + digit);
    }

    foreach (bigdigit; data)
    {
        if (shift < 0)
        {
            // Some bits were carried over from previous word.
            assert(shift > -3, "shift must be greater than -3");
            output(((bigdigit << -shift) | carry) & 0b111);
            shift += 3;
            assert(shift > 0, "shift must be 1 or greater");
        }

        while (shift <= BigDigitBits - 3)
        {
            output((bigdigit >>> shift) & 0b111);
            shift += 3;
        }

        if (shift < BigDigitBits)
        {
            // Some bits need to be carried forward.
            carry = (bigdigit >>> shift) & 0b11;
        }
        shift -= BigDigitBits;
        assert(shift >= -2 && shift <= 0, "shift must in [-2,0]");
    }

    if (shift < 0)
    {
        // Last word had bits that haven't been output yet.
        assert(shift > -3, "Shift must be greater than -3");
        output(carry);
    }

    return lastNonZero;
}

/** Convert a big uint into a decimal string.
 *
 * Params:
 *  data    The biguint to be converted. Will be destroyed.
 *  buff    The destination buffer for the decimal string. Must be
 *          large enough to store the result, including leading zeros.
 *          Will be filled backwards, starting from buff[$-1].
 *
 * buff.length must be >= (data.length*32)/log2(10) = 9.63296 * data.length.
 * Returns:
 *    the lowest index of buff which was used.
 */
size_t biguintToDecimal(char [] buff, BigDigit [] data) pure nothrow @safe
{
    ptrdiff_t sofar = buff.length;
    // Might be better to divide by (10^38/2^32) since that gives 38 digits for
    // the price of 3 divisions and a shr; this version only gives 27 digits
    // for 3 divisions.
    while (data.length>1)
    {
        uint rem = multibyteDivAssign(data, 10_0000_0000, 0);
        itoaZeroPadded(buff[sofar-9 .. sofar], rem);
        sofar -= 9;
        if (data[$-1] == 0 && data.length > 1)
        {
            data.length = data.length - 1;
        }
    }
    itoaZeroPadded(buff[sofar-10 .. sofar], data[0]);
    sofar -= 10;
    // and strip off the leading zeros
    while (sofar != buff.length-1 && buff[sofar] == '0')
        sofar++;
    return sofar;
}

/** Convert a decimal string into a big uint.
 *
 * Params:
 *  data    The biguint to be receive the result. Must be large enough to
 *          store the result.
 *  s       The decimal string. May contain _ or 0 .. 9
 *
 * The required length for the destination buffer is slightly less than
 *  1 + s.length/log2(10) = 1 + s.length/3.3219.
 *
 * Returns:
 *    the highest index of data which was used.
 */
int biguintFromDecimal(Range)(BigDigit[] data, Range s)
if (
    isInputRange!Range &&
    isSomeChar!(ElementType!Range) &&
    !isInfinite!Range)
in
{
    static if (hasLength!Range)
        assert((data.length >= 2) || (data.length == 1 && s.length == 1),
            "data has a invalid length");
}
do
{
    import std.conv : ConvException;

    // Convert to base 1e19 = 10_000_000_000_000_000_000.
    // (this is the largest power of 10 that will fit into a long).
    // The length will be less than 1 + s.length/log2(10) = 1 + s.length/3.3219.
    // 485 bits will only just fit into 146 decimal digits.
    // As we convert the string, we record the number of digits we've seen in base 19:
    // hi is the number of digits/19, lo is the extra digits (0 to 18).
    // TODO: This is inefficient for very large strings (it is O(n^^2)).
    // We should take advantage of fast multiplication once the numbers exceed
    // Karatsuba size.
    uint lo = 0; // number of powers of digits, 0 .. 18
    uint x = 0;
    ulong y = 0;
    uint hi = 0; // number of base 1e19 digits
    data[0] = 0; // initially number is 0.
    if (data.length > 1)
        data[1] = 0;

    foreach (character; s)
    {
        if (character == '_')
            continue;

        if (character < '0' || character > '9')
            throw new ConvException("invalid digit");
        x *= 10;
        x += character - '0';
        ++lo;
        if (lo == 9)
        {
            y = x;
            x = 0;
        }
        if (lo == 18)
        {
            y *= 10_0000_0000;
            y += x;
            x = 0;
        }
        if (lo == 19)
        {
            y *= 10;
            y += x;
            x = 0;
            // Multiply existing number by 10^19, then add y1.
            if (hi>0)
            {
                data[hi] = multibyteMul(data[0 .. hi], data[0 .. hi], 1_220_703_125*2u, 0); // 5^13*2 = 0x9184_E72A
                ++hi;
                data[hi] = multibyteMul(data[0 .. hi], data[0 .. hi], 15_625*262_144u, 0); // 5^6*2^18 = 0xF424_0000
                ++hi;
            }
            else
                hi = 2;
            uint c = multibyteIncrementAssign!('+')(data[0 .. hi], cast(uint)(y&0xFFFF_FFFF));
            c += multibyteIncrementAssign!('+')(data[1 .. hi], cast(uint)(y >> 32));
            if (c != 0)
            {
                data[hi]=c;
                ++hi;
            }
            y = 0;
            lo = 0;
        }
    }
    // Now set y = all remaining digits.
    if (lo >= 18)
    {
    }
    else if (lo >= 9)
    {
        for (int k=9; k<lo; ++k) y*=10;
        y+=x;
    }
    else
    {
        for (int k=0; k<lo; ++k) y*=10;
        y+=x;
    }
    if (lo != 0)
    {
        if (hi == 0)
        {
            data[0] = cast(uint) y;
            if (data.length == 1)
            {
                hi = 1;
            }
            else
            {
                data[1] = cast(uint)(y >>> 32);
                hi=2;
            }
        }
        else
        {
            while (lo>0)
            {
                immutable c = multibyteMul(data[0 .. hi], data[0 .. hi], 10, 0);
                if (c != 0)
                {
                    data[hi]=c;
                    ++hi;
                }
                --lo;
            }
            uint c = multibyteIncrementAssign!('+')(data[0 .. hi], cast(uint)(y&0xFFFF_FFFF));
            if (y > 0xFFFF_FFFFL)
            {
                c += multibyteIncrementAssign!('+')(data[1 .. hi], cast(uint)(y >> 32));
            }
            if (c != 0)
            {
                data[hi]=c;
                ++hi;
            }
        }
    }
    while (hi>1 && data[hi-1]==0)
        --hi;
    return hi;
}


// ------------------------
// These in-place functions are only for internal use; they are incompatible
// with COW.

// Classic 'schoolbook' multiplication.
void mulSimple(BigDigit[] result, const(BigDigit) [] left,
        const(BigDigit)[] right) pure nothrow @safe
in
{
    assert(result.length == left.length + right.length,
        "Result must be able to store left + right");
    assert(right.length>1, "right must not be empty");
}
do
{
    result[left.length] = multibyteMul(result[0 .. left.length], left, right[0], 0);
    multibyteMultiplyAccumulate(result[1..$], left, right[1..$]);
}

// Classic 'schoolbook' squaring
void squareSimple(BigDigit[] result, const(BigDigit) [] x) pure nothrow @safe
in
{
    assert(result.length == 2*x.length, "result must be twice as long as x");
    assert(x.length>1, "x must not be empty");
}
do
{
    multibyteSquare(result, x);
}


// add two uints of possibly different lengths. Result must be as long
// as the larger length.
// Returns carry (0 or 1).
uint addSimple(BigDigit[] result, const BigDigit [] left, const BigDigit [] right)
pure nothrow @safe
in
{
    assert(result.length == left.length,
        "result and left must be of the same length");
    assert(left.length >= right.length,
        "left must be longer or of equal length to right");
    assert(right.length > 0, "right must not be empty");
}
do
{
    uint carry = multibyteAdd(result[0 .. right.length],
            left[0 .. right.length], right, 0);
    if (right.length < left.length)
    {
        result[right.length .. left.length] = left[right.length .. $];
        carry = multibyteIncrementAssign!('+')(result[right.length..$], carry);
    }
    return carry;
}

//  result = left - right
// returns carry (0 or 1)
BigDigit subSimple(BigDigit [] result,const(BigDigit) [] left,
        const(BigDigit) [] right) pure nothrow
in
{
    assert(result.length == left.length,
        "result and left must be of the same length");
    assert(left.length >= right.length,
        "left must be longer or of equal length to right");
    assert(right.length > 0, "right must not be empty");
}
do
{
    BigDigit carry = multibyteSub(result[0 .. right.length],
            left[0 .. right.length], right, 0);
    if (right.length < left.length)
    {
        result[right.length .. left.length] = left[right.length .. $];
        carry = multibyteIncrementAssign!('-')(result[right.length..$], carry);
    } //else if (result.length == left.length+1) { result[$-1] = carry; carry=0; }
    return carry;
}


/* result = result - right
 * Returns carry = 1 if result was less than right.
*/
BigDigit subAssignSimple(BigDigit [] result, const(BigDigit) [] right)
pure nothrow @safe
{
    assert(result.length >= right.length,
       "result must be longer or of equal length to right");
    uint c = multibyteSub(result[0 .. right.length], result[0 .. right.length], right, 0);
    if (c && result.length > right.length)
        c = multibyteIncrementAssign!('-')(result[right.length .. $], c);
    return c;
}

/* result = result + right
*/
BigDigit addAssignSimple(BigDigit [] result, const(BigDigit) [] right)
pure nothrow @safe
{
    assert(result.length >= right.length,
       "result must be longer or of equal length to right");
    uint c = multibyteAdd(result[0 .. right.length], result[0 .. right.length], right, 0);
    if (c && result.length > right.length)
       c = multibyteIncrementAssign!('+')(result[right.length .. $], c);
    return c;
}

/* performs result += wantSub? - right : right;
*/
BigDigit addOrSubAssignSimple(BigDigit [] result, const(BigDigit) [] right,
        bool wantSub) pure nothrow @safe
{
    if (wantSub)
        return subAssignSimple(result, right);
    else
        return addAssignSimple(result, right);
}


// return true if x<y, considering leading zeros
bool less(const(BigDigit)[] x, const(BigDigit)[] y) pure nothrow @safe
{
    assert(x.length >= y.length,
       "x must be longer or of equal length to y");
    auto k = x.length-1;
    while (x[k]==0 && k >= y.length)
        --k;
    if (k >= y.length)
        return false;
    while (k>0 && x[k]==y[k])
        --k;
    return x[k] < y[k];
}

// Set result = abs(x-y), return true if result is negative(x<y), false if x <= y.
bool inplaceSub(BigDigit[] result, const(BigDigit)[] x, const(BigDigit)[] y)
    pure nothrow @safe
{
    assert(result.length == ((x.length >= y.length) ? x.length : y.length),
        "result must capable to store the maximum of x and y");

    size_t minlen;
    bool negative;
    if (x.length >= y.length)
    {
        minlen = y.length;
        negative = less(x, y);
    }
    else
    {
       minlen = x.length;
       negative = !less(y, x);
    }
    const (BigDigit)[] large, small;
    if (negative)
    {
        large = y; small = x;
    }
    else
    {
        large = x; small = y;
    }

    BigDigit carry = multibyteSub(result[0 .. minlen], large[0 .. minlen], small[0 .. minlen], 0);
    if (x.length != y.length)
    {
        result[minlen .. large.length]= large[minlen..$];
        result[large.length..$] = 0;
        if (carry)
            multibyteIncrementAssign!('-')(result[minlen..$], carry);
    }
    return negative;
}

/* Determine how much space is required for the temporaries
 * when performing a Karatsuba multiplication.
 * TODO: determining a tight bound is non-trivial and depends on KARATSUBALIMIT, see:
 * https://issues.dlang.org/show_bug.cgi?id=20493
 */
size_t karatsubaRequiredBuffSize(size_t xlen) pure nothrow @safe
{
    return xlen <= KARATSUBALIMIT ? 0 : (xlen * 9) / 4;
}

/* Sets result = x*y, using Karatsuba multiplication.
* x must be longer or equal to y.
* Valid only for balanced multiplies, where x is not shorter than y.
* It is superior to schoolbook multiplication if and only if
*    sqrt(2)*y.length > x.length > y.length.
* Karatsuba multiplication is O(n^1.59), whereas schoolbook is O(n^2)
* The maximum allowable length of x and y is uint.max; but better algorithms
* should be used far before that length is reached.
* Params:
* scratchbuff      An array long enough to store all the temporaries. Will be destroyed.
*/
void mulKaratsuba(BigDigit [] result, const(BigDigit) [] x,
        const(BigDigit)[] y, BigDigit [] scratchbuff) pure nothrow @safe
{
    assert(x.length >= y.length, "x must be greater or equal to y");
    assert(result.length < uint.max, "Operands too large");
    assert(result.length == x.length + y.length,
        "result must be as large as x + y");
    if (x.length <= KARATSUBALIMIT)
    {
        return mulSimple(result, x, y);
    }
    // Must be almost square (otherwise, a schoolbook iteration is better)
    assert(2L * y.length * y.length > (x.length-1) * (x.length-1),
        "Bigint Internal Error: Asymmetric Karatsuba");

    // The subtractive version of Karatsuba multiply uses the following result:
    // (Nx1 + x0)*(Ny1 + y0) = (N*N)*x1y1 + x0y0 + N * (x0y0 + x1y1 - mid)
    // where mid = (x0-x1)*(y0-y1)
    // requiring 3 multiplies of length N, instead of 4.
    // The advantage of the subtractive over the additive version is that
    // the mid multiply cannot exceed length N. But there are subtleties:
    // (x0-x1),(y0-y1) may be negative or zero. To keep it simple, we
    // retain all of the leading zeros in the subtractions

    // half length, round up.
    auto half = (x.length >> 1) + (x.length & 1);

    const(BigDigit) [] x0 = x[0 .. half];
    const(BigDigit) [] x1 = x[half .. $];
    const(BigDigit) [] y0 = y[0 .. half];
    const(BigDigit) [] y1 = y[half .. $];
    BigDigit [] mid = scratchbuff[0 .. half*2];
    BigDigit [] newscratchbuff = scratchbuff[half*2 .. $];
    BigDigit [] resultLow = result[0 .. 2*half];
    BigDigit [] resultHigh = result[2*half .. $];
     // initially use result to store temporaries
    BigDigit [] xdiff= result[0 .. half];
    BigDigit [] ydiff = result[half .. half*2];

    // First, we calculate mid, and sign of mid
    immutable bool midNegative = inplaceSub(xdiff, x0, x1)
                      ^ inplaceSub(ydiff, y0, y1);
    mulKaratsuba(mid, xdiff, ydiff, newscratchbuff);

    // Low half of result gets x0 * y0. High half gets x1 * y1

    mulKaratsuba(resultLow, x0, y0, newscratchbuff);

    if (2L * y1.length * y1.length < x1.length * x1.length)
    {
        // an asymmetric situation has been created.
        // Worst case is if x:y = 1.414 : 1, then x1:y1 = 2.41 : 1.
        // Applying one schoolbook multiply gives us two pieces each 1.2:1
        if (y1.length <= KARATSUBALIMIT)
            mulSimple(resultHigh, x1, y1);
        else
        {
            // divide x1 in two, then use schoolbook multiply on the two pieces.
            auto quarter = (x1.length >> 1) + (x1.length & 1);
            immutable ysmaller = (quarter >= y1.length);
            mulKaratsuba(resultHigh[0 .. quarter+y1.length], ysmaller ? x1[0 .. quarter] : y1,
                ysmaller ? y1 : x1[0 .. quarter], newscratchbuff);
            // Save the part which will be overwritten.
            immutable ysmaller2 = ((x1.length - quarter) >= y1.length);
            newscratchbuff[0 .. y1.length] = resultHigh[quarter .. quarter + y1.length];
            mulKaratsuba(resultHigh[quarter..$], ysmaller2 ? x1[quarter..$] : y1,
                ysmaller2 ? y1 : x1[quarter..$], newscratchbuff[y1.length..$]);

            resultHigh[quarter..$].addAssignSimple(newscratchbuff[0 .. y1.length]);
        }
    }
    else
        mulKaratsuba(resultHigh, x1, y1, newscratchbuff);

    /* We now have result = x0y0 + (N*N)*x1y1
       Before adding or subtracting mid, we must calculate
       result += N * (x0y0 + x1y1)
       We can do this with three half-length additions. With a = x0y0, b = x1y1:
                      aHI aLO
        +       aHI   aLO
        +       bHI   bLO
        +  bHI  bLO
        =  R3   R2    R1   R0
        R1 = aHI + bLO + aLO
        R2 = aHI + bLO + aHI + carry_from_R1
        R3 = bHi + carry_from_R2

     It might actually be quicker to do it in two full-length additions:
     newscratchbuff[2*half] = addSimple(newscratchbuff[0 .. 2*half], result[0 .. 2*half], result[2*half..$]);
     addAssignSimple(result[half..$], newscratchbuff[0 .. 2*half+1]);
   */
    BigDigit[] R1 = result[half .. half*2];
    BigDigit[] R2 = result[half*2 .. half*3];
    BigDigit[] R3 = result[half*3..$];
    BigDigit c1 = multibyteAdd(R2, R2, R1, 0); // c1:R2 = R2 + R1
    BigDigit c2 = multibyteAdd(R1, R2, result[0 .. half], 0); // c2:R1 = R2 + R1 + R0
    BigDigit c3 = addAssignSimple(R2, R3); // R2 = R2 + R1 + R3
    if (c1+c2)
        multibyteIncrementAssign!('+')(result[half*2..$], c1+c2);
    if (c1+c3)
        multibyteIncrementAssign!('+')(R3, c1+c3);

    // And finally we subtract mid
    addOrSubAssignSimple(result[half..$], mid, !midNegative);
}

void squareKaratsuba(BigDigit [] result, const BigDigit [] x,
        BigDigit [] scratchbuff) pure nothrow @safe
{
    // See mulKaratsuba for implementation comments.
    // Squaring is simpler, since it never gets asymmetric.
    assert(result.length < uint.max, "Operands too large");
    assert(result.length == 2*x.length,
        "result must be twice the length of x");
    if (x.length <= KARATSUBASQUARELIMIT)
    {
        return squareSimple(result, x);
    }
    // half length, round up.
    auto half = (x.length >> 1) + (x.length & 1);

    const(BigDigit)[] x0 = x[0 .. half];
    const(BigDigit)[] x1 = x[half .. $];
    BigDigit [] mid = scratchbuff[0 .. half*2];
    BigDigit [] newscratchbuff = scratchbuff[half*2 .. $];
     // initially use result to store temporaries
    BigDigit [] xdiff= result[0 .. half];
    const BigDigit [] ydiff = result[half .. half*2];

    // First, we calculate mid. We don't need its sign
    inplaceSub(xdiff, x0, x1);
    squareKaratsuba(mid, xdiff, newscratchbuff);

    // Set result = x0x0 + (N*N)*x1x1
    squareKaratsuba(result[0 .. 2*half], x0, newscratchbuff);
    squareKaratsuba(result[2*half .. $], x1, newscratchbuff);

    /* result += N * (x0x0 + x1x1)
       Do this with three half-length additions. With a = x0x0, b = x1x1:
        R1 = aHI + bLO + aLO
        R2 = aHI + bLO + aHI + carry_from_R1
        R3 = bHi + carry_from_R2
    */
    BigDigit[] R1 = result[half .. half*2];
    BigDigit[] R2 = result[half*2 .. half*3];
    BigDigit[] R3 = result[half*3..$];
    BigDigit c1 = multibyteAdd(R2, R2, R1, 0); // c1:R2 = R2 + R1
    BigDigit c2 = multibyteAdd(R1, R2, result[0 .. half], 0); // c2:R1 = R2 + R1 + R0
    BigDigit c3 = addAssignSimple(R2, R3); // R2 = R2 + R1 + R3
    if (c1+c2) multibyteIncrementAssign!('+')(result[half*2..$], c1+c2);
    if (c1+c3) multibyteIncrementAssign!('+')(R3, c1+c3);

    // And finally we subtract mid, which is always positive
    subAssignSimple(result[half..$], mid);
}

/* Knuth's Algorithm D, as presented in
 * H.S. Warren, "Hacker's Delight", Addison-Wesley Professional (2002).
 * Also described in "Modern Computer Arithmetic" 0.2, Exercise 1.8.18.
 * Given u and v, calculates  quotient  = u / v, u = u % v.
 * v must be normalized (ie, the MSB of v must be 1).
 * The most significant words of quotient and u may be zero.
 * u[0 .. v.length] holds the remainder.
 */
void schoolbookDivMod(BigDigit [] quotient, BigDigit [] u, in BigDigit [] v)
    pure nothrow @safe
{
    assert(quotient.length == u.length - v.length,
        "quotient has wrong length");
    assert(v.length > 1, "v must not be empty");
    assert(u.length >= v.length, "u must be larger or equal to v");
    assert((v[$ - 1] & 0x8000_0000) != 0, "Invalid value at v[$ - 1]");
    assert(u[$ - 1] < v[$ - 1], "u[$ - 1] must be less than v[$ - 1]");
    // BUG: This code only works if BigDigit is uint.
    uint vhi = v[$-1];
    uint vlo = v[$-2];

    for (ptrdiff_t j = u.length - v.length - 1; j >= 0; j--)
    {
        // Compute estimate of quotient[j],
        // qhat = (three most significant words of u)/(two most sig words of v).
        uint qhat;
        if (u[j + v.length] == vhi)
        {
            // uu/vhi could exceed uint.max (it will be 0x8000_0000 or 0x8000_0001)
            qhat = uint.max;
        }
        else
        {
            uint ulo = u[j + v.length - 2];
            version (D_InlineAsm_X86)
            {
                // Note: On DMD, this is only ~10% faster than the non-asm code.
                uint *p = &u[j + v.length - 1];
                asm pure nothrow @trusted
                {
                    mov EAX, p;
                    mov EDX, [EAX+4];
                    mov EAX, [EAX];
                    div dword ptr [vhi];
                    mov qhat, EAX;
                    mov ECX, EDX;
div3by2correction:
                    mul dword ptr [vlo]; // EDX:EAX = qhat * vlo
                    sub EAX, ulo;
                    sbb EDX, ECX;
                    jbe div3by2done;
                    mov EAX, qhat;
                    dec EAX;
                    mov qhat, EAX;
                    add ECX, dword ptr [vhi];
                    jnc div3by2correction;
div3by2done:    ;
                }
            }
            else
            { // version (InlineAsm)
                ulong uu = (cast(ulong)(u[j + v.length]) << 32) | u[j + v.length - 1];
                immutable bigqhat = uu / vhi;
                ulong rhat =  uu - bigqhat * vhi;
                qhat = cast(uint) bigqhat;
again:
                if (cast(ulong) qhat * vlo > ((rhat << 32) + ulo))
                {
                    --qhat;
                    rhat += vhi;
                    if (!(rhat & 0xFFFF_FFFF_0000_0000L))
                        goto again;
                }
            } // version (InlineAsm)
        }
        // Multiply and subtract.
        uint carry = multibyteMulAdd!('-')(u[j .. j + v.length], v, qhat, 0);

        if (u[j+v.length] < carry)
        {
            // If we subtracted too much, add back
            --qhat;
            carry -= multibyteAdd(u[j .. j + v.length],u[j .. j + v.length], v, 0);
        }
        quotient[j] = qhat;
        u[j + v.length] = u[j + v.length] - carry;
    }
}

// TODO: Replace with a library call
void itoaZeroPadded(char[] output, uint value)
    pure nothrow @safe @nogc
{
    for (auto i = output.length; i--;)
    {
        if (value < 10)
        {
            output[i] = cast(char)(value + '0');
            value = 0;
        }
        else
        {
            output[i] = cast(char)(value % 10 + '0');
            value /= 10;
        }
    }
}

void toHexZeroPadded(char[] output, uint value,
        LetterCase letterCase = LetterCase.upper) pure nothrow @safe
{
    ptrdiff_t x = output.length - 1;
    static immutable string upperHexDigits = "0123456789ABCDEF";
    static immutable string lowerHexDigits = "0123456789abcdef";
    for ( ; x >= 0; --x)
    {
        if (letterCase == LetterCase.upper)
        {
            output[x] = upperHexDigits[value & 0xF];
        }
        else
        {
            output[x] = lowerHexDigits[value & 0xF];
        }
        value >>= 4;
    }
}

// Returns the highest value of i for which left[i]!=right[i],
// or 0 if left[] == right[]
size_t highestDifferentDigit(const BigDigit [] left, const BigDigit [] right)
pure nothrow @nogc @safe
{
    assert(left.length == right.length,
        "left have a length equal to that of right");
    for (ptrdiff_t i = left.length - 1; i>0; --i)
    {
        if (left[i] != right[i])
            return i;
    }
    return 0;
}

// Returns the lowest value of i for which x[i]!=0.
int firstNonZeroDigit(const BigDigit [] x) pure nothrow @nogc @safe
{
    int k = 0;
    while (x[k]==0)
    {
        ++k;
        assert(k < x.length, "k must be less than x.length");
    }
    return k;
}

/*
    Calculate quotient and remainder of u / v using fast recursive division.
    v must be normalised, and must be at least half as long as u.
    Given u and v, v normalised, calculates  quotient  = u/v, u = u%v.
    scratch is temporary storage space, length must be >= quotient + 1.

Returns:
    u[0 .. v.length] is the remainder. u[v.length..$] is corrupted.

    Implements algorithm 1.8 from MCA.
    This algorithm has an annoying special case. After the first recursion, the
    highest bit of the quotient may be set. This means that in the second
    recursive call, the 'in' contract would be violated. (This happens only
    when the top quarter of u is equal to the top half of v. A base 10
    equivalent example of this situation is 5517/56; the first step gives
    55/5 = 11). To maintain the in contract, we pad a zero to the top of both
    u and the quotient. 'mayOverflow' indicates that that the special case
    has occurred.
    (In MCA, a different strategy is used: the in contract is weakened, and
    schoolbookDivMod is more general: it allows the high bit of u to be set).
    See also:
    - C. Burkinel and J. Ziegler, "Fast Recursive Division", MPI-I-98-1-022,
      Max-Planck Institute fuer Informatik, (Oct 1998).
*/
void recursiveDivMod(BigDigit[] quotient, BigDigit[] u, const(BigDigit)[] v,
                     BigDigit[] scratch, bool mayOverflow = false)
                     pure nothrow @safe
in
{
    // v must be normalized
    assert(v.length > 1, "v must not be empty");
    assert((v[$ - 1] & 0x8000_0000) != 0, "Invalid value at v[$ - 1]");
    assert(!(u[$ - 1] & 0x8000_0000), "Invalid value at u[$ - 1]");
    assert(quotient.length == u.length - v.length,
        "quotient must be of equal length of u - v");
    if (mayOverflow)
    {
        assert(u[$-1] == 0, "Invalid value at u[$ - 1]");
        assert(u[$-2] & 0x8000_0000, "Invalid value at u[$ - 2]");
    }

    // Must be symmetric. Use block schoolbook division if not.
    assert((mayOverflow ? u.length-1 : u.length) <= 2 * v.length,
        "Invalid length of u");
    assert((mayOverflow ? u.length-1 : u.length) >= v.length,
        "Invalid length of u");
    assert(scratch.length >= quotient.length + (mayOverflow ? 0 : 1),
        "Invalid quotient length");
}
do
{
    if (quotient.length < FASTDIVLIMIT)
    {
        return schoolbookDivMod(quotient, u, v);
    }

    // Split quotient into two halves, but keep padding in the top half
    auto k = (mayOverflow ?  quotient.length - 1 : quotient.length) >> 1;

    // RECURSION 1: Calculate the high half of the quotient

    // Note that if u and quotient were padded, they remain padded during
    // this call, so in contract is satisfied.
    recursiveDivMod(quotient[k .. $], u[2 * k .. $], v[k .. $],
        scratch, mayOverflow);

    // quotient[k..$] is our guess at the high quotient.
    // u[2*k .. 2.*k + v.length - k = k + v.length] is the high part of the
    // first remainder. u[0 .. 2*k] is the low part.

    // Calculate the full first remainder to be
    //    remainder - highQuotient * lowDivisor
    // reducing highQuotient until the remainder is positive.
    // The low part of the remainder, u[0 .. k], cannot be altered by this.

    adjustRemainder(quotient[k .. $], u[k .. k + v.length], v, k,
            scratch[0 .. quotient.length], mayOverflow);

    // RECURSION 2: Calculate the low half of the quotient
    // The full first remainder is now in u[0 .. k + v.length].

    if (u[k + v.length - 1] & 0x8000_0000)
    {
        // Special case. The high quotient is 0x1_00...000 or 0x1_00...001.
        // This means we need an extra quotient word for the next recursion.
        // We need to restore the invariant for the recursive calls.
        // We do this by padding both u and quotient. Extending u is trivial,
        // because the higher words will not be used again. But for the
        // quotient, we're clobbering the low word of the high quotient,
        // so we need save it, and add it back in after the recursive call.

        auto clobberedQuotient = quotient[k];
        u[k+v.length] = 0;

        recursiveDivMod(quotient[0 .. k+1], u[k .. k + v.length+1],
            v[k .. $], scratch, true);
        adjustRemainder(quotient[0 .. k+1], u[0 .. v.length], v, k,
            scratch[0 .. 2 * k+1], true);

        // Now add the quotient word that got clobbered earlier.
        multibyteIncrementAssign!('+')(quotient[k..$], clobberedQuotient);
    }
    else
    {
        // The special case has NOT happened.
        recursiveDivMod(quotient[0 .. k], u[k .. k + v.length], v[k .. $],
            scratch, false);

        // high remainder is in u[k .. k+(v.length-k)] == u[k .. v.length]

        adjustRemainder(quotient[0 .. k], u[0 .. v.length], v, k,
            scratch[0 .. 2 * k]);
    }
}

// rem -= quot * v[0 .. k].
// If would make rem negative, decrease quot until rem is >= 0.
// Needs (quot.length * k) scratch space to store the result of the multiply.
void adjustRemainder(BigDigit[] quot, BigDigit[] rem, const(BigDigit)[] v,
        ptrdiff_t k,
        BigDigit[] scratch, bool mayOverflow = false) pure nothrow @safe
{
    assert(rem.length == v.length, "rem must be as long as v");
    mulInternal(scratch, quot, v[0 .. k]);
    uint carry = 0;
    if (mayOverflow)
        carry = scratch[$-1] + subAssignSimple(rem, scratch[0..$-1]);
    else
        carry = subAssignSimple(rem, scratch);
    while (carry)
    {
        multibyteIncrementAssign!('-')(quot, 1); // quot--
        carry -= multibyteAdd(rem, rem, v, 0);
    }
}

// Cope with unbalanced division by performing block schoolbook division.
void blockDivMod(BigDigit [] quotient, BigDigit [] u, in BigDigit [] v)
pure nothrow @safe
{
    import core.memory : GC;
    assert(quotient.length == u.length - v.length,
        "quotient must be of equal length of u - v");
    assert(v.length > 1, "v must not be empty");
    assert(u.length >= v.length, "u must be longer or of equal length as v");
    assert((v[$-1] & 0x8000_0000)!=0, "Invalid value at v[$ - 1]");
    assert((u[$-1] & 0x8000_0000)==0, "Invalid value at u[$ - 1]");
    BigDigit [] scratch = new BigDigit[v.length + 1];

    // Perform block schoolbook division, with 'v.length' blocks.
    auto m = u.length - v.length;
    while (m > v.length)
    {
        immutable mayOverflow = (u[m + v.length -1 ] & 0x8000_0000)!=0;
        BigDigit saveq;
        if (mayOverflow)
        {
            u[m + v.length] = 0;
            saveq = quotient[m];
        }
        recursiveDivMod(quotient[m-v.length .. m + (mayOverflow? 1: 0)],
            u[m - v.length .. m + v.length + (mayOverflow? 1: 0)], v, scratch, mayOverflow);
        if (mayOverflow)
        {
            assert(quotient[m] == 0, "quotient must not be 0");
            quotient[m] = saveq;
        }
        m -= v.length;
    }
    recursiveDivMod(quotient[0 .. m], u[0 .. m + v.length], v, scratch);
    () @trusted { GC.free(scratch.ptr); } ();
}

@system unittest
{
    version (none)
    {
        import core.stdc.stdio;

        void printBiguint(const uint [] data)
        {
            char [] buff = biguintToHex(new char[data.length*9], data, '_');
            printf("%.*s\n", cast(int) buff.length, buff.ptr);
        }

        void printDecimalBigUint(BigUint data)
        {
            auto str = data.toDecimalString(0);
            printf("%.*s\n", cast(int) str.length, str.ptr);
        }
    }

    uint [] a, b;
    a = new uint[43];
    b = new uint[179];
    for (int i=0; i<a.length; ++i) a[i] = 0x1234_B6E9 + i;
    for (int i=0; i<b.length; ++i) b[i] = 0x1BCD_8763 - i*546;

    a[$-1] |= 0x8000_0000;
    uint [] r = new uint[a.length];
    uint [] q = new uint[b.length-a.length+1];

    divModInternal(q, r, b, a);
    q = q[0..$-1];
    uint [] r1 = r.dup;
    uint [] q1 = q.dup;
    blockDivMod(q, b, a);
    r = b[0 .. a.length];
    assert(r[] == r1[]);
    assert(q[] == q1[]);
}

// biguintToOctal
@safe unittest
{
    enum bufSize = 5 * BigDigitBits / 3 + 1;
    auto buf = new char[bufSize];
    size_t i;
    BigDigit[] data = [ 342391 ];

    // Basic functionality with single word
    i = biguintToOctal(buf, data);
    assert(i == bufSize - 7 && buf[i .. $] == "1234567");

    // Test carrying bits between words
    data = [ 0x77053977, 0x39770539, 0x00000005 ];
    i = biguintToOctal(buf, data);
    assert(i == bufSize - 23 && buf[i .. $] == "12345670123456701234567");

    // Test carried bits in the last word
    data = [ 0x80000000 ];
    i = biguintToOctal(buf, data);
    assert(buf[i .. $] == "20000000000");

    // Test boundary between 3rd and 4th word where the number of bits is
    // divisible by 3 and no bits should be carried.
    //
    // The 0xC0000000's are for "poisoning" the carry to be non-zero when the
    // rollover happens, so that if any bugs happen in wrongly adding the carry
    // to the next word, non-zero bits will show up in the output.
    data = [ 0xC0000000, 0xC0000000, 0xC0000000, 0x00000010 ];
    i = biguintToOctal(buf, data);
    assert(buf[i .. $] == "2060000000001400000000030000000000");

    // Boundary case: 0
    data = [ 0 ];
    i = biguintToOctal(buf, data);
    assert(buf[i .. $] == "0");
}
