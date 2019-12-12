// Written in the D programming language.

/**
String handling functions.

$(SCRIPT inhibitQuickIndex = 1;)

$(DIVC quickindex,
$(BOOKTABLE ,
$(TR $(TH Category) $(TH Functions) )
$(TR $(TDNW Searching)
    $(TD
         $(MYREF column)
         $(MYREF indexOf)
         $(MYREF indexOfAny)
         $(MYREF indexOfNeither)
         $(MYREF lastIndexOf)
         $(MYREF lastIndexOfAny)
         $(MYREF lastIndexOfNeither)
    )
)
$(TR $(TDNW Comparison)
    $(TD
         $(MYREF isNumeric)
    )
)
$(TR $(TDNW Mutation)
    $(TD
         $(MYREF capitalize)
    )
)
$(TR $(TDNW Pruning and Filling)
    $(TD
         $(MYREF center)
         $(MYREF chomp)
         $(MYREF chompPrefix)
         $(MYREF chop)
         $(MYREF detabber)
         $(MYREF detab)
         $(MYREF entab)
         $(MYREF entabber)
         $(MYREF leftJustify)
         $(MYREF outdent)
         $(MYREF rightJustify)
         $(MYREF strip)
         $(MYREF stripLeft)
         $(MYREF stripRight)
         $(MYREF wrap)
    )
)
$(TR $(TDNW Substitution)
    $(TD
         $(MYREF abbrev)
         $(MYREF soundex)
         $(MYREF soundexer)
         $(MYREF succ)
         $(MYREF tr)
         $(MYREF translate)
    )
)
$(TR $(TDNW Miscellaneous)
    $(TD
         $(MYREF assumeUTF)
         $(MYREF fromStringz)
         $(MYREF lineSplitter)
         $(MYREF representation)
         $(MYREF splitLines)
         $(MYREF toStringz)
    )
)))

Objects of types $(D _string), $(D wstring), and $(D dstring) are value types
and cannot be mutated element-by-element. For using mutation during building
strings, use $(D char[]), $(D wchar[]), or $(D dchar[]). The $(D xxxstring)
types are preferable because they don't exhibit undesired aliasing, thus
making code more robust.

The following functions are publicly imported:

$(BOOKTABLE ,
$(TR $(TH Module) $(TH Functions) )
$(LEADINGROW Publicly imported functions)
    $(TR $(TD std.algorithm)
        $(TD
         $(REF_SHORT cmp, std,algorithm,comparison)
         $(REF_SHORT count, std,algorithm,searching)
         $(REF_SHORT endsWith, std,algorithm,searching)
         $(REF_SHORT startsWith, std,algorithm,searching)
    ))
    $(TR $(TD std.array)
        $(TD
         $(REF_SHORT join, std,array)
         $(REF_SHORT replace, std,array)
         $(REF_SHORT replaceInPlace, std,array)
         $(REF_SHORT split, std,array)
         $(REF_SHORT empty, std,array)
    ))
    $(TR $(TD std.format)
        $(TD
         $(REF_SHORT format, std,format)
         $(REF_SHORT sformat, std,format)
    ))
    $(TR $(TD std.uni)
        $(TD
         $(REF_SHORT icmp, std,uni)
         $(REF_SHORT toLower, std,uni)
         $(REF_SHORT toLowerInPlace, std,uni)
         $(REF_SHORT toUpper, std,uni)
         $(REF_SHORT toUpperInPlace, std,uni)
    ))
)

There is a rich set of functions for _string handling defined in other modules.
Functions related to Unicode and ASCII are found in $(MREF std, uni)
and $(MREF std, ascii), respectively. Other functions that have a
wider generality than just strings can be found in $(MREF std, algorithm)
and $(MREF std, range).

See_Also:
    $(LIST
    $(MREF std, algorithm) and
    $(MREF std, range)
    for generic range algorithms
    ,
    $(MREF std, ascii)
    for functions that work with ASCII strings
    ,
    $(MREF std, uni)
    for functions that work with unicode strings
    )

Copyright: Copyright Digital Mars 2007-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP digitalmars.com, Walter Bright),
         $(HTTP erdani.org, Andrei Alexandrescu),
        Jonathan M Davis,
        and David L. 'SpottedTiger' Davis

Source:    $(PHOBOSSRC std/_string.d)

*/
module std.string;

version (unittest)
{
private:
    struct TestAliasedString
    {
        string get() @safe @nogc pure nothrow { return _s; }
        alias get this;
        @disable this(this);
        string _s;
    }

    bool testAliasedString(alias func, Args...)(string s, Args args)
    {
        import std.algorithm.comparison : equal;
        auto a = func(TestAliasedString(s), args);
        auto b = func(s, args);
        static if (is(typeof(equal(a, b))))
        {
            // For ranges, compare contents instead of object identity.
            return equal(a, b);
        }
        else
        {
            return a == b;
        }
    }
}

public import std.format : format, sformat;
import std.typecons : Flag, Yes, No;
public import std.uni : icmp, toLower, toLowerInPlace, toUpper, toUpperInPlace;

import std.meta; // AliasSeq, staticIndexOf
import std.range.primitives; // back, ElementEncodingType, ElementType, front,
    // hasLength, hasSlicing, isBidirectionalRange, isForwardRange, isInfinite,
    // isInputRange, isOutputRange, isRandomAccessRange, popBack, popFront, put,
    // save;
import std.traits; // isConvertibleToString, isNarrowString, isSomeChar,
    // isSomeString, StringTypeOf, Unqual

//public imports for backward compatibility
public import std.algorithm.comparison : cmp;
public import std.algorithm.searching : startsWith, endsWith, count;
public import std.array : join, replace, replaceInPlace, split, empty;

/* ************* Exceptions *************** */

/++
    Exception thrown on errors in std.string functions.
  +/
class StringException : Exception
{
    import std.exception : basicExceptionCtors;

    ///
    mixin basicExceptionCtors;
}


/++
    Params:
        cString = A null-terminated c-style string.

    Returns: A D-style array of $(D char) referencing the same string.  The
    returned array will retain the same type qualifiers as the input.

    $(RED Important Note:) The returned array is a slice of the original buffer.
    The original data is not changed and not copied.
+/

inout(char)[] fromStringz(inout(char)* cString) @nogc @system pure nothrow {
    import core.stdc.string : strlen;
    return cString ? cString[0 .. strlen(cString)] : null;
}

///
@system pure unittest
{
    assert(fromStringz(null) == null);
    assert(fromStringz("foo") == "foo");
}

/++
    Params:
        s = A D-style string.

    Returns: A C-style null-terminated string equivalent to $(D s). $(D s)
    must not contain embedded $(D '\0')'s as any C function will treat the
    first $(D '\0') that it sees as the end of the string. If $(D s.empty) is
    $(D true), then a string containing only $(D '\0') is returned.

    $(RED Important Note:) When passing a $(D char*) to a C function, and the C
    function keeps it around for any reason, make sure that you keep a
    reference to it in your D code. Otherwise, it may become invalid during a
    garbage collection cycle and cause a nasty bug when the C code tries to use
    it.
  +/
immutable(char)* toStringz(const(char)[] s) @trusted pure nothrow
out (result)
{
    import core.stdc.string : strlen, memcmp;
    if (result)
    {
        auto slen = s.length;
        while (slen > 0 && s[slen-1] == 0) --slen;
        assert(strlen(result) == slen);
        assert(result[0 .. slen] == s[0 .. slen]);
    }
}
body
{
    import std.exception : assumeUnique;
    /+ Unfortunately, this isn't reliable.
     We could make this work if string literals are put
     in read-only memory and we test if s[] is pointing into
     that.

     /* Peek past end of s[], if it's 0, no conversion necessary.
     * Note that the compiler will put a 0 past the end of static
     * strings, and the storage allocator will put a 0 past the end
     * of newly allocated char[]'s.
     */
     char* p = &s[0] + s.length;
     if (*p == 0)
     return s;
     +/

    // Need to make a copy
    auto copy = new char[s.length + 1];
    copy[0 .. s.length] = s[];
    copy[s.length] = 0;

    return &assumeUnique(copy)[0];
}

/++ Ditto +/
immutable(char)* toStringz(in string s) @trusted pure nothrow
{
    if (s.empty) return "".ptr;
    /* Peek past end of s[], if it's 0, no conversion necessary.
     * Note that the compiler will put a 0 past the end of static
     * strings, and the storage allocator will put a 0 past the end
     * of newly allocated char[]'s.
     */
    immutable p = s.ptr + s.length;
    // Is p dereferenceable? A simple test: if the p points to an
    // address multiple of 4, then conservatively assume the pointer
    // might be pointing to a new block of memory, which might be
    // unreadable. Otherwise, it's definitely pointing to valid
    // memory.
    if ((cast(size_t) p & 3) && *p == 0)
        return &s[0];
    return toStringz(cast(const char[]) s);
}

///
pure nothrow @system unittest
{
    import core.stdc.string : strlen;
    import std.conv : to;

    auto p = toStringz("foo");
    assert(strlen(p) == 3);
    const(char)[] foo = "abbzxyzzy";
    p = toStringz(foo[3 .. 5]);
    assert(strlen(p) == 2);

    string test = "";
    p = toStringz(test);
    assert(*p == 0);

    test = "\0";
    p = toStringz(test);
    assert(*p == 0);

    test = "foo\0";
    p = toStringz(test);
    assert(p[0] == 'f' && p[1] == 'o' && p[2] == 'o' && p[3] == 0);

    const string test2 = "";
    p = toStringz(test2);
    assert(*p == 0);
}


/**
   Flag indicating whether a search is case-sensitive.
*/
alias CaseSensitive = Flag!"caseSensitive";

/++
    Searches for character in range.

    Params:
        s = string or InputRange of characters to search in correct UTF format
        c = character to search for
        startIdx = starting index to a well-formed code point
        cs = $(D Yes.caseSensitive) or $(D No.caseSensitive)

    Returns:
        the index of the first occurrence of $(D c) in $(D s) with
        respect to the start index $(D startIdx). If $(D c)
        is not found, then $(D -1) is returned.
        If $(D c) is found the value of the returned index is at least
        $(D startIdx).
        If the parameters are not valid UTF, the result will still
        be in the range [-1 .. s.length], but will not be reliable otherwise.

    Throws:
        If the sequence starting at $(D startIdx) does not represent a well
        formed codepoint, then a $(REF UTFException, std,utf) may be thrown.

    See_Also: $(REF countUntil, std,algorithm,searching)
  +/
ptrdiff_t indexOf(Range)(Range s, in dchar c,
        in CaseSensitive cs = Yes.caseSensitive)
if (isInputRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    static import std.ascii;
    static import std.uni;
    import std.utf : byDchar, byCodeUnit, UTFException, codeLength;
    alias Char = Unqual!(ElementEncodingType!Range);

    if (cs == Yes.caseSensitive)
    {
        static if (Char.sizeof == 1 && isSomeString!Range)
        {
            if (std.ascii.isASCII(c) && !__ctfe)
            {                                               // Plain old ASCII
                static ptrdiff_t trustedmemchr(Range s, char c) @trusted
                {
                    import core.stdc.string : memchr;
                    const p = cast(const(Char)*)memchr(s.ptr, c, s.length);
                    return p ? p - s.ptr : -1;
                }

                return trustedmemchr(s, cast(char) c);
            }
        }

        static if (Char.sizeof == 1)
        {
            if (c <= 0x7F)
            {
                ptrdiff_t i;
                foreach (const c2; s)
                {
                    if (c == c2)
                        return i;
                    ++i;
                }
            }
            else
            {
                ptrdiff_t i;
                foreach (const c2; s.byDchar())
                {
                    if (c == c2)
                        return i;
                    i += codeLength!Char(c2);
                }
            }
        }
        else static if (Char.sizeof == 2)
        {
            if (c <= 0xFFFF)
            {
                ptrdiff_t i;
                foreach (const c2; s)
                {
                    if (c == c2)
                        return i;
                    ++i;
                }
            }
            else if (c <= 0x10FFFF)
            {
                // Encode UTF-16 surrogate pair
                const wchar c1 = cast(wchar)((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
                const wchar c2 = cast(wchar)(((c - 0x10000) & 0x3FF) + 0xDC00);
                ptrdiff_t i;
                for (auto r = s.byCodeUnit(); !r.empty; r.popFront())
                {
                    if (c1 == r.front)
                    {
                        r.popFront();
                        if (r.empty)    // invalid UTF - missing second of pair
                            break;
                        if (c2 == r.front)
                            return i;
                        ++i;
                    }
                    ++i;
                }
            }
        }
        else static if (Char.sizeof == 4)
        {
            ptrdiff_t i;
            foreach (const c2; s)
            {
                if (c == c2)
                    return i;
                ++i;
            }
        }
        else
            static assert(0);
        return -1;
    }
    else
    {
        if (std.ascii.isASCII(c))
        {                                                   // Plain old ASCII
            immutable c1 = cast(char) std.ascii.toLower(c);

            ptrdiff_t i;
            foreach (const c2; s.byCodeUnit())
            {
                if (c1 == std.ascii.toLower(c2))
                    return i;
                ++i;
            }
        }
        else
        {                                                   // c is a universal character
            immutable c1 = std.uni.toLower(c);

            ptrdiff_t i;
            foreach (const c2; s.byDchar())
            {
                if (c1 == std.uni.toLower(c2))
                    return i;
                i += codeLength!Char(c2);
            }
        }
    }
    return -1;
}

/// Ditto
ptrdiff_t indexOf(Range)(Range s, in dchar c, in size_t startIdx,
        in CaseSensitive cs = Yes.caseSensitive)
if (isInputRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    static if (isSomeString!(typeof(s)) ||
                (hasSlicing!(typeof(s)) && hasLength!(typeof(s))))
    {
        if (startIdx < s.length)
        {
            ptrdiff_t foundIdx = indexOf(s[startIdx .. $], c, cs);
            if (foundIdx != -1)
            {
                return foundIdx + cast(ptrdiff_t) startIdx;
            }
        }
    }
    else
    {
        foreach (i; 0 .. startIdx)
        {
            if (s.empty)
                return -1;
            s.popFront();
        }
        ptrdiff_t foundIdx = indexOf(s, c, cs);
        if (foundIdx != -1)
        {
            return foundIdx + cast(ptrdiff_t) startIdx;
        }
    }
    return -1;
}

///
@safe pure unittest
{
    import std.typecons : No;

    string s = "Hello World";
    assert(indexOf(s, 'W') == 6);
    assert(indexOf(s, 'Z') == -1);
    assert(indexOf(s, 'w', No.caseSensitive) == 6);
}

///
@safe pure unittest
{
    import std.typecons : No;

    string s = "Hello World";
    assert(indexOf(s, 'W', 4) == 6);
    assert(indexOf(s, 'Z', 100) == -1);
    assert(indexOf(s, 'w', 3, No.caseSensitive) == 6);
}

ptrdiff_t indexOf(Range)(auto ref Range s, in dchar c,
        in CaseSensitive cs = Yes.caseSensitive)
if (isConvertibleToString!Range)
{
    return indexOf!(StringTypeOf!Range)(s, c, cs);
}

ptrdiff_t indexOf(Range)(auto ref Range s, in dchar c, in size_t startIdx,
        in CaseSensitive cs = Yes.caseSensitive)
if (isConvertibleToString!Range)
{
    return indexOf!(StringTypeOf!Range)(s, c, startIdx, cs);
}

@safe pure unittest
{
    assert(testAliasedString!indexOf("std/string.d", '/'));
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;
    import std.traits : EnumMembers;
    import std.utf : byChar, byWchar, byDchar;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        assert(indexOf(cast(S) null, cast(dchar)'a') == -1);
        assert(indexOf(to!S("def"), cast(dchar)'a') == -1);
        assert(indexOf(to!S("abba"), cast(dchar)'a') == 0);
        assert(indexOf(to!S("def"), cast(dchar)'f') == 2);

        assert(indexOf(to!S("def"), cast(dchar)'a', No.caseSensitive) == -1);
        assert(indexOf(to!S("def"), cast(dchar)'a', No.caseSensitive) == -1);
        assert(indexOf(to!S("Abba"), cast(dchar)'a', No.caseSensitive) == 0);
        assert(indexOf(to!S("def"), cast(dchar)'F', No.caseSensitive) == 2);
        assert(indexOf(to!S("ödef"), 'ö', No.caseSensitive) == 0);

        S sPlts = "Mars: the fourth Rock (Planet) from the Sun.";
        assert(indexOf("def", cast(char)'f', No.caseSensitive) == 2);
        assert(indexOf(sPlts, cast(char)'P', No.caseSensitive) == 23);
        assert(indexOf(sPlts, cast(char)'R', No.caseSensitive) == 2);
    }

    foreach (cs; EnumMembers!CaseSensitive)
    {
        assert(indexOf("hello\U00010143\u0100\U00010143", '\u0100', cs) == 9);
        assert(indexOf("hello\U00010143\u0100\U00010143"w, '\u0100', cs) == 7);
        assert(indexOf("hello\U00010143\u0100\U00010143"d, '\u0100', cs) == 6);

        assert(indexOf("hello\U00010143\u0100\U00010143".byChar, '\u0100', cs) == 9);
        assert(indexOf("hello\U00010143\u0100\U00010143".byWchar, '\u0100', cs) == 7);
        assert(indexOf("hello\U00010143\u0100\U00010143".byDchar, '\u0100', cs) == 6);

        assert(indexOf("hello\U000007FF\u0100\U00010143".byChar, 'l',      cs) == 2);
        assert(indexOf("hello\U000007FF\u0100\U00010143".byChar, '\u0100', cs) == 7);
        assert(indexOf("hello\U0000EFFF\u0100\U00010143".byChar, '\u0100', cs) == 8);

        assert(indexOf("hello\U00010100".byWchar, '\U00010100', cs) == 5);
        assert(indexOf("hello\U00010100".byWchar, '\U00010101', cs) == -1);
    }

    char[10] fixedSizeArray = "0123456789";
    assert(indexOf(fixedSizeArray, '2') == 2);
    });
}

@safe pure unittest
{
    assert(testAliasedString!indexOf("std/string.d", '/', 3));
}

@safe pure unittest
{
    import std.conv : to;
    import std.traits : EnumMembers;
    import std.utf : byCodeUnit, byChar, byWchar;

    assert("hello".byCodeUnit.indexOf(cast(dchar)'l', 1) == 2);
    assert("hello".byWchar.indexOf(cast(dchar)'l', 1) == 2);
    assert("hello".byWchar.indexOf(cast(dchar)'l', 6) == -1);

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        assert(indexOf(cast(S) null, cast(dchar)'a', 1) == -1);
        assert(indexOf(to!S("def"), cast(dchar)'a', 1) == -1);
        assert(indexOf(to!S("abba"), cast(dchar)'a', 1) == 3);
        assert(indexOf(to!S("def"), cast(dchar)'f', 1) == 2);

        assert((to!S("def")).indexOf(cast(dchar)'a', 1,
                No.caseSensitive) == -1);
        assert(indexOf(to!S("def"), cast(dchar)'a', 1,
                No.caseSensitive) == -1);
        assert(indexOf(to!S("def"), cast(dchar)'a', 12,
                No.caseSensitive) == -1);
        assert(indexOf(to!S("AbbA"), cast(dchar)'a', 2,
                No.caseSensitive) == 3);
        assert(indexOf(to!S("def"), cast(dchar)'F', 2, No.caseSensitive) == 2);

        S sPlts = "Mars: the fourth Rock (Planet) from the Sun.";
        assert(indexOf("def", cast(char)'f', cast(uint) 2,
            No.caseSensitive) == 2);
        assert(indexOf(sPlts, cast(char)'P', 12, No.caseSensitive) == 23);
        assert(indexOf(sPlts, cast(char)'R', cast(ulong) 1,
            No.caseSensitive) == 2);
    }

    foreach (cs; EnumMembers!CaseSensitive)
    {
        assert(indexOf("hello\U00010143\u0100\U00010143", '\u0100', 2, cs)
            == 9);
        assert(indexOf("hello\U00010143\u0100\U00010143"w, '\u0100', 3, cs)
            == 7);
        assert(indexOf("hello\U00010143\u0100\U00010143"d, '\u0100', 6, cs)
            == 6);
    }
}

/++
    Searches for substring in $(D s).

    Params:
        s = string or ForwardRange of characters to search in correct UTF format
        sub = substring to search for
        startIdx = the index into s to start searching from
        cs = $(D Yes.caseSensitive) or $(D No.caseSensitive)

    Returns:
        the index of the first occurrence of $(D sub) in $(D s) with
        respect to the start index $(D startIdx). If $(D sub) is not found,
        then $(D -1) is returned.
        If the arguments are not valid UTF, the result will still
        be in the range [-1 .. s.length], but will not be reliable otherwise.
        If $(D sub) is found the value of the returned index is at least
        $(D startIdx).

    Throws:
        If the sequence starting at $(D startIdx) does not represent a well
        formed codepoint, then a $(REF UTFException, std,utf) may be thrown.

    Bugs:
        Does not work with case insensitive strings where the mapping of
        tolower and toupper is not 1:1.
  +/
ptrdiff_t indexOf(Range, Char)(Range s, const(Char)[] sub,
        in CaseSensitive cs = Yes.caseSensitive)
if (isForwardRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    isSomeChar!Char)
{
    alias Char1 = Unqual!(ElementEncodingType!Range);

    static if (isSomeString!Range)
    {
        import std.algorithm.searching : find;

        const(Char1)[] balance;
        if (cs == Yes.caseSensitive)
        {
            balance = find(s, sub);
        }
        else
        {
            balance = find!
                ((a, b) => toLower(a) == toLower(b))
                (s, sub);
        }
        return () @trusted { return balance.empty ? -1 : balance.ptr - s.ptr; } ();
    }
    else
    {
        if (s.empty)
            return -1;
        if (sub.empty)
            return 0;                   // degenerate case

        import std.utf : byDchar, codeLength;
        auto subr = sub.byDchar;        // decode sub[] by dchar's
        dchar sub0 = subr.front;        // cache first character of sub[]
        subr.popFront();

        // Special case for single character search
        if (subr.empty)
            return indexOf(s, sub0, cs);

        if (cs == No.caseSensitive)
            sub0 = toLower(sub0);

        /* Classic double nested loop search algorithm
         */
        ptrdiff_t index = 0;            // count code unit index into s
        for (auto sbydchar = s.byDchar(); !sbydchar.empty; sbydchar.popFront())
        {
            dchar c2 = sbydchar.front;
            if (cs == No.caseSensitive)
                c2 = toLower(c2);
            if (c2 == sub0)
            {
                auto s2 = sbydchar.save;        // why s must be a forward range
                foreach (c; subr.save)
                {
                    s2.popFront();
                    if (s2.empty)
                        return -1;
                    if (cs == Yes.caseSensitive ? c != s2.front
                                                : toLower(c) != toLower(s2.front)
                       )
                        goto Lnext;
                }
                return index;
            }
          Lnext:
            index += codeLength!Char1(c2);
        }
        return -1;
    }
}

/// Ditto
ptrdiff_t indexOf(Char1, Char2)(const(Char1)[] s, const(Char2)[] sub,
        in size_t startIdx, in CaseSensitive cs = Yes.caseSensitive)
@safe
if (isSomeChar!Char1 && isSomeChar!Char2)
{
    if (startIdx < s.length)
    {
        ptrdiff_t foundIdx = indexOf(s[startIdx .. $], sub, cs);
        if (foundIdx != -1)
        {
            return foundIdx + cast(ptrdiff_t) startIdx;
        }
    }
    return -1;
}

///
@safe pure unittest
{
    import std.typecons : No;

    string s = "Hello World";
    assert(indexOf(s, "Wo", 4) == 6);
    assert(indexOf(s, "Zo", 100) == -1);
    assert(indexOf(s, "wo", 3, No.caseSensitive) == 6);
}

///
@safe pure unittest
{
    import std.typecons : No;

    string s = "Hello World";
    assert(indexOf(s, "Wo") == 6);
    assert(indexOf(s, "Zo") == -1);
    assert(indexOf(s, "wO", No.caseSensitive) == 6);
}

ptrdiff_t indexOf(Range, Char)(auto ref Range s, const(Char)[] sub,
        in CaseSensitive cs = Yes.caseSensitive)
if (!(isForwardRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    isSomeChar!Char) &&
    is(StringTypeOf!Range))
{
    return indexOf!(StringTypeOf!Range)(s, sub, cs);
}

@safe pure unittest
{
    assert(testAliasedString!indexOf("std/string.d", "string"));
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;
    import std.traits : EnumMembers;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(indexOf(cast(S) null, to!T("a")) == -1);
            assert(indexOf(to!S("def"), to!T("a")) == -1);
            assert(indexOf(to!S("abba"), to!T("a")) == 0);
            assert(indexOf(to!S("def"), to!T("f")) == 2);
            assert(indexOf(to!S("dfefffg"), to!T("fff")) == 3);
            assert(indexOf(to!S("dfeffgfff"), to!T("fff")) == 6);

            assert(indexOf(to!S("dfeffgfff"), to!T("a"), No.caseSensitive) == -1);
            assert(indexOf(to!S("def"), to!T("a"), No.caseSensitive) == -1);
            assert(indexOf(to!S("abba"), to!T("a"), No.caseSensitive) == 0);
            assert(indexOf(to!S("def"), to!T("f"), No.caseSensitive) == 2);
            assert(indexOf(to!S("dfefffg"), to!T("fff"), No.caseSensitive) == 3);
            assert(indexOf(to!S("dfeffgfff"), to!T("fff"), No.caseSensitive) == 6);

            S sPlts = "Mars: the fourth Rock (Planet) from the Sun.";
            S sMars = "Who\'s \'My Favorite Maritian?\'";

            assert(indexOf(sMars, to!T("MY fAVe"), No.caseSensitive) == -1);
            assert(indexOf(sMars, to!T("mY fAVOriTe"), No.caseSensitive) == 7);
            assert(indexOf(sPlts, to!T("mArS:"), No.caseSensitive) == 0);
            assert(indexOf(sPlts, to!T("rOcK"), No.caseSensitive) == 17);
            assert(indexOf(sPlts, to!T("Un."), No.caseSensitive) == 41);
            assert(indexOf(sPlts, to!T(sPlts), No.caseSensitive) == 0);

            assert(indexOf("\u0100", to!T("\u0100"), No.caseSensitive) == 0);

            // Thanks to Carlos Santander B. and zwang
            assert(indexOf("sus mejores cortesanos. Se embarcaron en el puerto de Dubai y",
                           to!T("page-break-before"), No.caseSensitive) == -1);
        }();

        foreach (cs; EnumMembers!CaseSensitive)
        {
            assert(indexOf("hello\U00010143\u0100\U00010143", to!S("\u0100"), cs) == 9);
            assert(indexOf("hello\U00010143\u0100\U00010143"w, to!S("\u0100"), cs) == 7);
            assert(indexOf("hello\U00010143\u0100\U00010143"d, to!S("\u0100"), cs) == 6);
        }
    }
    });
}

@safe pure @nogc nothrow
unittest
{
    import std.traits : EnumMembers;
    import std.utf : byWchar;

    foreach (cs; EnumMembers!CaseSensitive)
    {
        assert(indexOf("".byWchar, "", cs) == -1);
        assert(indexOf("hello".byWchar, "", cs) == 0);
        assert(indexOf("hello".byWchar, "l", cs) == 2);
        assert(indexOf("heLLo".byWchar, "LL", cs) == 2);
        assert(indexOf("hello".byWchar, "lox", cs) == -1);
        assert(indexOf("hello".byWchar, "betty", cs) == -1);
        assert(indexOf("hello\U00010143\u0100*\U00010143".byWchar, "\u0100*", cs) == 7);
    }
}

@safe pure unittest
{
    import std.conv : to;
    import std.traits : EnumMembers;

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(indexOf(cast(S) null, to!T("a"), 1337) == -1);
            assert(indexOf(to!S("def"), to!T("a"), 0) == -1);
            assert(indexOf(to!S("abba"), to!T("a"), 2) == 3);
            assert(indexOf(to!S("def"), to!T("f"), 1) == 2);
            assert(indexOf(to!S("dfefffg"), to!T("fff"), 1) == 3);
            assert(indexOf(to!S("dfeffgfff"), to!T("fff"), 5) == 6);

            assert(indexOf(to!S("dfeffgfff"), to!T("a"), 1, No.caseSensitive) == -1);
            assert(indexOf(to!S("def"), to!T("a"), 2, No.caseSensitive) == -1);
            assert(indexOf(to!S("abba"), to!T("a"), 3, No.caseSensitive) == 3);
            assert(indexOf(to!S("def"), to!T("f"), 1, No.caseSensitive) == 2);
            assert(indexOf(to!S("dfefffg"), to!T("fff"), 2, No.caseSensitive) == 3);
            assert(indexOf(to!S("dfeffgfff"), to!T("fff"), 4, No.caseSensitive) == 6);
            assert(indexOf(to!S("dfeffgffföä"), to!T("öä"), 9, No.caseSensitive) == 9,
                to!string(indexOf(to!S("dfeffgffföä"), to!T("öä"), 9, No.caseSensitive))
                ~ " " ~ S.stringof ~ " " ~ T.stringof);

            S sPlts = "Mars: the fourth Rock (Planet) from the Sun.";
            S sMars = "Who\'s \'My Favorite Maritian?\'";

            assert(indexOf(sMars, to!T("MY fAVe"), 10,
                No.caseSensitive) == -1);
            assert(indexOf(sMars, to!T("mY fAVOriTe"), 4, No.caseSensitive) == 7);
            assert(indexOf(sPlts, to!T("mArS:"), 0, No.caseSensitive) == 0);
            assert(indexOf(sPlts, to!T("rOcK"), 12, No.caseSensitive) == 17);
            assert(indexOf(sPlts, to!T("Un."), 32, No.caseSensitive) == 41);
            assert(indexOf(sPlts, to!T(sPlts), 0, No.caseSensitive) == 0);

            assert(indexOf("\u0100", to!T("\u0100"), 0, No.caseSensitive) == 0);

            // Thanks to Carlos Santander B. and zwang
            assert(indexOf("sus mejores cortesanos. Se embarcaron en el puerto de Dubai y",
                           to!T("page-break-before"), 10, No.caseSensitive) == -1);

            // In order for indexOf with and without index to be consistent
            assert(indexOf(to!S(""), to!T("")) == indexOf(to!S(""), to!T(""), 0));
        }();

        foreach (cs; EnumMembers!CaseSensitive)
        {
            assert(indexOf("hello\U00010143\u0100\U00010143", to!S("\u0100"),
                3, cs) == 9);
            assert(indexOf("hello\U00010143\u0100\U00010143"w, to!S("\u0100"),
                3, cs) == 7);
            assert(indexOf("hello\U00010143\u0100\U00010143"d, to!S("\u0100"),
                3, cs) == 6);
        }
    }
}

/++
    Params:
        s = string to search
        c = character to search for
        startIdx = the index into s to start searching from
        cs = $(D Yes.caseSensitive) or $(D No.caseSensitive)

    Returns:
        The index of the last occurrence of $(D c) in $(D s). If $(D c) is not
        found, then $(D -1) is returned. The $(D startIdx) slices $(D s) in
        the following way $(D s[0 .. startIdx]). $(D startIdx) represents a
        codeunit index in $(D s).

    Throws:
        If the sequence ending at $(D startIdx) does not represent a well
        formed codepoint, then a $(REF UTFException, std,utf) may be thrown.

    $(D cs) indicates whether the comparisons are case sensitive.
  +/
ptrdiff_t lastIndexOf(Char)(const(Char)[] s, in dchar c,
        in CaseSensitive cs = Yes.caseSensitive) @safe pure
if (isSomeChar!Char)
{
    static import std.ascii, std.uni;
    import std.utf : canSearchInCodeUnits;
    if (cs == Yes.caseSensitive)
    {
        if (canSearchInCodeUnits!Char(c))
        {
            foreach_reverse (i, it; s)
            {
                if (it == c)
                {
                    return i;
                }
            }
        }
        else
        {
            foreach_reverse (i, dchar it; s)
            {
                if (it == c)
                {
                    return i;
                }
            }
        }
    }
    else
    {
        if (std.ascii.isASCII(c))
        {
            immutable c1 = std.ascii.toLower(c);

            foreach_reverse (i, it; s)
            {
                immutable c2 = std.ascii.toLower(it);
                if (c1 == c2)
                {
                    return i;
                }
            }
        }
        else
        {
            immutable c1 = std.uni.toLower(c);

            foreach_reverse (i, dchar it; s)
            {
                immutable c2 = std.uni.toLower(it);
                if (c1 == c2)
                {
                    return i;
                }
            }
        }
    }

    return -1;
}

/// Ditto
ptrdiff_t lastIndexOf(Char)(const(Char)[] s, in dchar c, in size_t startIdx,
        in CaseSensitive cs = Yes.caseSensitive) @safe pure
if (isSomeChar!Char)
{
    if (startIdx <= s.length)
    {
        return lastIndexOf(s[0u .. startIdx], c, cs);
    }

    return -1;
}

///
@safe pure unittest
{
    import std.typecons : No;

    string s = "Hello World";
    assert(lastIndexOf(s, 'l') == 9);
    assert(lastIndexOf(s, 'Z') == -1);
    assert(lastIndexOf(s, 'L', No.caseSensitive) == 9);
}

///
@safe pure unittest
{
    import std.typecons : No;

    string s = "Hello World";
    assert(lastIndexOf(s, 'l', 4) == 3);
    assert(lastIndexOf(s, 'Z', 1337) == -1);
    assert(lastIndexOf(s, 'L', 7, No.caseSensitive) == 3);
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;
    import std.traits : EnumMembers;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        assert(lastIndexOf(cast(S) null, 'a') == -1);
        assert(lastIndexOf(to!S("def"), 'a') == -1);
        assert(lastIndexOf(to!S("abba"), 'a') == 3);
        assert(lastIndexOf(to!S("def"), 'f') == 2);
        assert(lastIndexOf(to!S("ödef"), 'ö') == 0);

        assert(lastIndexOf(cast(S) null, 'a', No.caseSensitive) == -1);
        assert(lastIndexOf(to!S("def"), 'a', No.caseSensitive) == -1);
        assert(lastIndexOf(to!S("AbbA"), 'a', No.caseSensitive) == 3);
        assert(lastIndexOf(to!S("def"), 'F', No.caseSensitive) == 2);
        assert(lastIndexOf(to!S("ödef"), 'ö', No.caseSensitive) == 0);
        assert(lastIndexOf(to!S("i\u0100def"), to!dchar("\u0100"),
            No.caseSensitive) == 1);

        S sPlts = "Mars: the fourth Rock (Planet) from the Sun.";

        assert(lastIndexOf(to!S("def"), 'f', No.caseSensitive) == 2);
        assert(lastIndexOf(sPlts, 'M', No.caseSensitive) == 34);
        assert(lastIndexOf(sPlts, 'S', No.caseSensitive) == 40);
    }

    foreach (cs; EnumMembers!CaseSensitive)
    {
        assert(lastIndexOf("\U00010143\u0100\U00010143hello", '\u0100', cs) == 4);
        assert(lastIndexOf("\U00010143\u0100\U00010143hello"w, '\u0100', cs) == 2);
        assert(lastIndexOf("\U00010143\u0100\U00010143hello"d, '\u0100', cs) == 1);
    }
    });
}

@safe pure unittest
{
    import std.conv : to;
    import std.traits : EnumMembers;

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        assert(lastIndexOf(cast(S) null, 'a') == -1);
        assert(lastIndexOf(to!S("def"), 'a') == -1);
        assert(lastIndexOf(to!S("abba"), 'a', 3) == 0);
        assert(lastIndexOf(to!S("deff"), 'f', 3) == 2);

        assert(lastIndexOf(cast(S) null, 'a', No.caseSensitive) == -1);
        assert(lastIndexOf(to!S("def"), 'a', No.caseSensitive) == -1);
        assert(lastIndexOf(to!S("AbbAa"), 'a', to!ushort(4), No.caseSensitive) == 3,
                to!string(lastIndexOf(to!S("AbbAa"), 'a', 4, No.caseSensitive)));
        assert(lastIndexOf(to!S("def"), 'F', 3, No.caseSensitive) == 2);

        S sPlts = "Mars: the fourth Rock (Planet) from the Sun.";

        assert(lastIndexOf(to!S("def"), 'f', 4, No.caseSensitive) == -1);
        assert(lastIndexOf(sPlts, 'M', sPlts.length -2, No.caseSensitive) == 34);
        assert(lastIndexOf(sPlts, 'S', sPlts.length -2, No.caseSensitive) == 40);
    }

    foreach (cs; EnumMembers!CaseSensitive)
    {
        assert(lastIndexOf("\U00010143\u0100\U00010143hello", '\u0100', cs) == 4);
        assert(lastIndexOf("\U00010143\u0100\U00010143hello"w, '\u0100', cs) == 2);
        assert(lastIndexOf("\U00010143\u0100\U00010143hello"d, '\u0100', cs) == 1);
    }
}

/++
    Params:
        s = string to search
        sub = substring to search for
        startIdx = the index into s to start searching from
        cs = $(D Yes.caseSensitive) or $(D No.caseSensitive)

    Returns:
        the index of the last occurrence of $(D sub) in $(D s). If $(D sub) is
        not found, then $(D -1) is returned. The $(D startIdx) slices $(D s)
        in the following way $(D s[0 .. startIdx]). $(D startIdx) represents a
        codeunit index in $(D s).

    Throws:
        If the sequence ending at $(D startIdx) does not represent a well
        formed codepoint, then a $(REF UTFException, std,utf) may be thrown.

    $(D cs) indicates whether the comparisons are case sensitive.
  +/
ptrdiff_t lastIndexOf(Char1, Char2)(const(Char1)[] s, const(Char2)[] sub,
        in CaseSensitive cs = Yes.caseSensitive) @safe pure
if (isSomeChar!Char1 && isSomeChar!Char2)
{
    import std.algorithm.searching : endsWith;
    import std.conv : to;
    import std.range.primitives : walkLength;
    static import std.uni;
    import std.utf : strideBack;
    if (sub.empty)
        return -1;

    if (walkLength(sub) == 1)
        return lastIndexOf(s, sub.front, cs);

    if (cs == Yes.caseSensitive)
    {
        static if (is(Unqual!Char1 == Unqual!Char2))
        {
            import core.stdc.string : memcmp;

            immutable c = sub[0];

            for (ptrdiff_t i = s.length - sub.length; i >= 0; --i)
            {
                if (s[i] == c)
                {
                    if (__ctfe)
                    {
                        foreach (j; 1 .. sub.length)
                        {
                            if (s[i + j] != sub[j])
                                continue;
                        }
                        return i;
                    }
                    else
                    {
                        auto trustedMemcmp(in void* s1, in void* s2, size_t n) @trusted
                        {
                            return memcmp(s1, s2, n);
                        }
                        if (trustedMemcmp(&s[i + 1], &sub[1],
                                (sub.length - 1) * Char1.sizeof) == 0)
                            return i;
                    }
                }
            }
        }
        else
        {
            for (size_t i = s.length; !s.empty;)
            {
                if (s.endsWith(sub))
                    return cast(ptrdiff_t) i - to!(const(Char1)[])(sub).length;

                i -= strideBack(s, i);
                s = s[0 .. i];
            }
        }
    }
    else
    {
        for (size_t i = s.length; !s.empty;)
        {
            if (endsWith!((a, b) => std.uni.toLower(a) == std.uni.toLower(b))
                         (s, sub))
            {
                return cast(ptrdiff_t) i - to!(const(Char1)[])(sub).length;
            }

            i -= strideBack(s, i);
            s = s[0 .. i];
        }
    }

    return -1;
}

/// Ditto
ptrdiff_t lastIndexOf(Char1, Char2)(const(Char1)[] s, const(Char2)[] sub,
        in size_t startIdx, in CaseSensitive cs = Yes.caseSensitive) @safe pure
if (isSomeChar!Char1 && isSomeChar!Char2)
{
    if (startIdx <= s.length)
    {
        return lastIndexOf(s[0u .. startIdx], sub, cs);
    }

    return -1;
}

///
@safe pure unittest
{
    import std.typecons : No;

    string s = "Hello World";
    assert(lastIndexOf(s, "ll") == 2);
    assert(lastIndexOf(s, "Zo") == -1);
    assert(lastIndexOf(s, "lL", No.caseSensitive) == 2);
}

///
@safe pure unittest
{
    import std.typecons : No;

    string s = "Hello World";
    assert(lastIndexOf(s, "ll", 4) == 2);
    assert(lastIndexOf(s, "Zo", 128) == -1);
    assert(lastIndexOf(s, "lL", 3, No.caseSensitive) == -1);
}

@safe pure unittest
{
    import std.conv : to;

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        auto r = to!S("").lastIndexOf("hello");
        assert(r == -1, to!string(r));

        r = to!S("hello").lastIndexOf("");
        assert(r == -1, to!string(r));

        r = to!S("").lastIndexOf("");
        assert(r == -1, to!string(r));
    }
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;
    import std.traits : EnumMembers;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            enum typeStr = S.stringof ~ " " ~ T.stringof;

            assert(lastIndexOf(cast(S) null, to!T("a")) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("c")) == 6, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("cd")) == 6, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("ef")) == 8, typeStr);
            assert(lastIndexOf(to!S("abcdefCdef"), to!T("c")) == 2, typeStr);
            assert(lastIndexOf(to!S("abcdefCdef"), to!T("cd")) == 2, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("x")) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("xy")) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("")) == -1, typeStr);
            assert(lastIndexOf(to!S("öabcdefcdef"), to!T("ö")) == 0, typeStr);

            assert(lastIndexOf(cast(S) null, to!T("a"), No.caseSensitive) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefCdef"), to!T("c"), No.caseSensitive) == 6, typeStr);
            assert(lastIndexOf(to!S("abcdefCdef"), to!T("cD"), No.caseSensitive) == 6, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("x"), No.caseSensitive) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("xy"), No.caseSensitive) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T(""), No.caseSensitive) == -1, typeStr);
            assert(lastIndexOf(to!S("öabcdefcdef"), to!T("ö"), No.caseSensitive) == 0, typeStr);

            assert(lastIndexOf(to!S("abcdefcdef"), to!T("c"), No.caseSensitive) == 6, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("cd"), No.caseSensitive) == 6, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("def"), No.caseSensitive) == 7, typeStr);

            assert(lastIndexOf(to!S("ödfeffgfff"), to!T("ö"), Yes.caseSensitive) == 0);

            S sPlts = "Mars: the fourth Rock (Planet) from the Sun.";
            S sMars = "Who\'s \'My Favorite Maritian?\'";

            assert(lastIndexOf(sMars, to!T("RiTE maR"), No.caseSensitive) == 14, typeStr);
            assert(lastIndexOf(sPlts, to!T("FOuRTh"), No.caseSensitive) == 10, typeStr);
            assert(lastIndexOf(sMars, to!T("whO\'s \'MY"), No.caseSensitive) == 0, typeStr);
            assert(lastIndexOf(sMars, to!T(sMars), No.caseSensitive) == 0, typeStr);
        }();

        foreach (cs; EnumMembers!CaseSensitive)
        {
            enum csString = to!string(cs);

            assert(lastIndexOf("\U00010143\u0100\U00010143hello", to!S("\u0100"), cs) == 4, csString);
            assert(lastIndexOf("\U00010143\u0100\U00010143hello"w, to!S("\u0100"), cs) == 2, csString);
            assert(lastIndexOf("\U00010143\u0100\U00010143hello"d, to!S("\u0100"), cs) == 1, csString);
        }
    }
    });
}

@safe pure unittest // issue13529
{
    import std.conv : to;
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        {
            enum typeStr = S.stringof ~ " " ~ T.stringof;
            auto idx = lastIndexOf(to!T("Hällö Wörldö ö"),to!S("ö ö"));
            assert(idx != -1, to!string(idx) ~ " " ~ typeStr);

            idx = lastIndexOf(to!T("Hällö Wörldö ö"),to!S("ö öd"));
            assert(idx == -1, to!string(idx) ~ " " ~ typeStr);
        }
    }
}

@safe pure unittest
{
    import std.conv : to;
    import std.traits : EnumMembers;

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            enum typeStr = S.stringof ~ " " ~ T.stringof;

            assert(lastIndexOf(cast(S) null, to!T("a")) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("c"), 5) == 2, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("cd"), 3) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("ef"), 6) == 4, typeStr ~
                format(" %u", lastIndexOf(to!S("abcdefcdef"), to!T("ef"), 6)));
            assert(lastIndexOf(to!S("abcdefCdef"), to!T("c"), 5) == 2, typeStr);
            assert(lastIndexOf(to!S("abcdefCdef"), to!T("cd"), 3) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdefx"), to!T("x"), 1) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdefxy"), to!T("xy"), 6) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T(""), 8) == -1, typeStr);
            assert(lastIndexOf(to!S("öafö"), to!T("ö"), 3) == 0, typeStr ~
                    to!string(lastIndexOf(to!S("öafö"), to!T("ö"), 3))); //BUG 10472

            assert(lastIndexOf(cast(S) null, to!T("a"), 1, No.caseSensitive) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefCdef"), to!T("c"), 5, No.caseSensitive) == 2, typeStr);
            assert(lastIndexOf(to!S("abcdefCdef"), to!T("cD"), 4, No.caseSensitive) == 2, typeStr ~
                " " ~ to!string(lastIndexOf(to!S("abcdefCdef"), to!T("cD"), 3, No.caseSensitive)));
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("x"),3 , No.caseSensitive) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdefXY"), to!T("xy"), 4, No.caseSensitive) == -1, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T(""), 7, No.caseSensitive) == -1, typeStr);

            assert(lastIndexOf(to!S("abcdefcdef"), to!T("c"), 4, No.caseSensitive) == 2, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("cd"), 4, No.caseSensitive) == 2, typeStr);
            assert(lastIndexOf(to!S("abcdefcdef"), to!T("def"), 6, No.caseSensitive) == 3, typeStr);
            assert(lastIndexOf(to!S(""), to!T(""), 0) == lastIndexOf(to!S(""), to!T("")), typeStr);
        }();

        foreach (cs; EnumMembers!CaseSensitive)
        {
            enum csString = to!string(cs);

            assert(lastIndexOf("\U00010143\u0100\U00010143hello", to!S("\u0100"), 6, cs) == 4, csString);
            assert(lastIndexOf("\U00010143\u0100\U00010143hello"w, to!S("\u0100"), 6, cs) == 2, csString);
            assert(lastIndexOf("\U00010143\u0100\U00010143hello"d, to!S("\u0100"), 3, cs) == 1, csString);
        }
    }
}

private ptrdiff_t indexOfAnyNeitherImpl(bool forward, bool any, Char, Char2)(
        const(Char)[] haystack, const(Char2)[] needles,
        in CaseSensitive cs = Yes.caseSensitive) @safe pure
if (isSomeChar!Char && isSomeChar!Char2)
{
    import std.algorithm.searching : canFind, findAmong;
    if (cs == Yes.caseSensitive)
    {
        static if (forward)
        {
            static if (any)
            {
                size_t n = haystack.findAmong(needles).length;
                return n ? haystack.length - n : -1;
            }
            else
            {
                foreach (idx, dchar hay; haystack)
                {
                    if (!canFind(needles, hay))
                    {
                        return idx;
                    }
                }
            }
        }
        else
        {
            static if (any)
            {
                import std.range : retro;
                import std.utf : strideBack;
                size_t n = haystack.retro.findAmong(needles).source.length;
                if (n)
                {
                    return n - haystack.strideBack(n);
                }
            }
            else
            {
                foreach_reverse (idx, dchar hay; haystack)
                {
                    if (!canFind(needles, hay))
                    {
                        return idx;
                    }
                }
            }
        }
    }
    else
    {
        import std.range.primitives : walkLength;
        if (needles.length <= 16 && needles.walkLength(17))
        {
            size_t si = 0;
            dchar[16] scratch = void;
            foreach ( dchar c; needles)
            {
                scratch[si++] = toLower(c);
            }

            static if (forward)
            {
                foreach (i, dchar c; haystack)
                {
                    if (canFind(scratch[0 .. si], toLower(c)) == any)
                    {
                        return i;
                    }
                }
            }
            else
            {
                foreach_reverse (i, dchar c; haystack)
                {
                    if (canFind(scratch[0 .. si], toLower(c)) == any)
                    {
                        return i;
                    }
                }
            }
        }
        else
        {
            static bool f(dchar a, dchar b)
            {
                return toLower(a) == b;
            }

            static if (forward)
            {
                foreach (i, dchar c; haystack)
                {
                    if (canFind!f(needles, toLower(c)) == any)
                    {
                        return i;
                    }
                }
            }
            else
            {
                foreach_reverse (i, dchar c; haystack)
                {
                    if (canFind!f(needles, toLower(c)) == any)
                    {
                        return i;
                    }
                }
            }
        }
    }

    return -1;
}

/**
    Returns the index of the first occurrence of any of the elements in $(D
    needles) in $(D haystack). If no element of $(D needles) is found,
    then $(D -1) is returned. The $(D startIdx) slices $(D haystack) in the
    following way $(D haystack[startIdx .. $]). $(D startIdx) represents a
    codeunit index in $(D haystack). If the sequence ending at $(D startIdx)
    does not represent a well formed codepoint, then a $(REF UTFException, std,utf)
    may be thrown.

    Params:
        haystack = String to search for needles in.
        needles = Strings to search for in haystack.
        startIdx = slices haystack like this $(D haystack[startIdx .. $]). If
            the startIdx is greater equal the length of haystack the functions
            returns $(D -1).
        cs = Indicates whether the comparisons are case sensitive.
*/
ptrdiff_t indexOfAny(Char,Char2)(const(Char)[] haystack, const(Char2)[] needles,
        in CaseSensitive cs = Yes.caseSensitive) @safe pure
if (isSomeChar!Char && isSomeChar!Char2)
{
    return indexOfAnyNeitherImpl!(true, true)(haystack, needles, cs);
}

/// Ditto
ptrdiff_t indexOfAny(Char,Char2)(const(Char)[] haystack, const(Char2)[] needles,
        in size_t startIdx, in CaseSensitive cs = Yes.caseSensitive) @safe pure
if (isSomeChar!Char && isSomeChar!Char2)
{
    if (startIdx < haystack.length)
    {
        ptrdiff_t foundIdx = indexOfAny(haystack[startIdx .. $], needles, cs);
        if (foundIdx != -1)
        {
            return foundIdx + cast(ptrdiff_t) startIdx;
        }
    }

    return -1;
}

///
@safe pure unittest
{
    import std.conv : to;

    ptrdiff_t i = "helloWorld".indexOfAny("Wr");
    assert(i == 5);
    i = "öällo world".indexOfAny("lo ");
    assert(i == 4, to!string(i));
}

///
@safe pure unittest
{
    import std.conv : to;

    ptrdiff_t i = "helloWorld".indexOfAny("Wr", 4);
    assert(i == 5);

    i = "Foo öällo world".indexOfAny("lh", 3);
    assert(i == 8, to!string(i));
}

@safe pure unittest
{
    import std.conv : to;

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        auto r = to!S("").indexOfAny("hello");
        assert(r == -1, to!string(r));

        r = to!S("hello").indexOfAny("");
        assert(r == -1, to!string(r));

        r = to!S("").indexOfAny("");
        assert(r == -1, to!string(r));
    }
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(indexOfAny(cast(S) null, to!T("a")) == -1);
            assert(indexOfAny(to!S("def"), to!T("rsa")) == -1);
            assert(indexOfAny(to!S("abba"), to!T("a")) == 0);
            assert(indexOfAny(to!S("def"), to!T("f")) == 2);
            assert(indexOfAny(to!S("dfefffg"), to!T("fgh")) == 1);
            assert(indexOfAny(to!S("dfeffgfff"), to!T("feg")) == 1);

            assert(indexOfAny(to!S("zfeffgfff"), to!T("ACDC"),
                No.caseSensitive) == -1);
            assert(indexOfAny(to!S("def"), to!T("MI6"),
                No.caseSensitive) == -1);
            assert(indexOfAny(to!S("abba"), to!T("DEA"),
                No.caseSensitive) == 0);
            assert(indexOfAny(to!S("def"), to!T("FBI"), No.caseSensitive) == 2);
            assert(indexOfAny(to!S("dfefffg"), to!T("NSA"), No.caseSensitive)
                == -1);
            assert(indexOfAny(to!S("dfeffgfff"), to!T("BND"),
                No.caseSensitive) == 0);
            assert(indexOfAny(to!S("dfeffgfff"), to!T("BNDabCHIJKQEPÖÖSYXÄ??ß"),
                No.caseSensitive) == 0);

            assert(indexOfAny("\u0100", to!T("\u0100"), No.caseSensitive) == 0);
        }();
    }
    }
    );
}

@safe pure unittest
{
    import std.conv : to;
    import std.traits : EnumMembers;

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(indexOfAny(cast(S) null, to!T("a"), 1337) == -1);
            assert(indexOfAny(to!S("def"), to!T("AaF"), 0) == -1);
            assert(indexOfAny(to!S("abba"), to!T("NSa"), 2) == 3);
            assert(indexOfAny(to!S("def"), to!T("fbi"), 1) == 2);
            assert(indexOfAny(to!S("dfefffg"), to!T("foo"), 2) == 3);
            assert(indexOfAny(to!S("dfeffgfff"), to!T("fsb"), 5) == 6);

            assert(indexOfAny(to!S("dfeffgfff"), to!T("NDS"), 1,
                No.caseSensitive) == -1);
            assert(indexOfAny(to!S("def"), to!T("DRS"), 2,
                No.caseSensitive) == -1);
            assert(indexOfAny(to!S("abba"), to!T("SI"), 3,
                No.caseSensitive) == -1);
            assert(indexOfAny(to!S("deO"), to!T("ASIO"), 1,
                No.caseSensitive) == 2);
            assert(indexOfAny(to!S("dfefffg"), to!T("fbh"), 2,
                No.caseSensitive) == 3);
            assert(indexOfAny(to!S("dfeffgfff"), to!T("fEe"), 4,
                No.caseSensitive) == 4);
            assert(indexOfAny(to!S("dfeffgffföä"), to!T("föä"), 9,
                No.caseSensitive) == 9);

            assert(indexOfAny("\u0100", to!T("\u0100"), 0,
                No.caseSensitive) == 0);
        }();

        foreach (cs; EnumMembers!CaseSensitive)
        {
            assert(indexOfAny("hello\U00010143\u0100\U00010143",
                to!S("e\u0100"), 3, cs) == 9);
            assert(indexOfAny("hello\U00010143\u0100\U00010143"w,
                to!S("h\u0100"), 3, cs) == 7);
            assert(indexOfAny("hello\U00010143\u0100\U00010143"d,
                to!S("l\u0100"), 5, cs) == 6);
        }
    }
}

/**
    Returns the index of the last occurrence of any of the elements in $(D
    needles) in $(D haystack). If no element of $(D needles) is found,
    then $(D -1) is returned. The $(D stopIdx) slices $(D haystack) in the
    following way $(D s[0 .. stopIdx]). $(D stopIdx) represents a codeunit
    index in $(D haystack). If the sequence ending at $(D startIdx) does not
    represent a well formed codepoint, then a $(REF UTFException, std,utf) may be
    thrown.

    Params:
        haystack = String to search for needles in.
        needles = Strings to search for in haystack.
        stopIdx = slices haystack like this $(D haystack[0 .. stopIdx]). If
            the stopIdx is greater equal the length of haystack the functions
            returns $(D -1).
        cs = Indicates whether the comparisons are case sensitive.
*/
ptrdiff_t lastIndexOfAny(Char,Char2)(const(Char)[] haystack,
        const(Char2)[] needles, in CaseSensitive cs = Yes.caseSensitive)
        @safe pure
if (isSomeChar!Char && isSomeChar!Char2)
{
    return indexOfAnyNeitherImpl!(false, true)(haystack, needles, cs);
}

/// Ditto
ptrdiff_t lastIndexOfAny(Char,Char2)(const(Char)[] haystack,
        const(Char2)[] needles, in size_t stopIdx,
        in CaseSensitive cs = Yes.caseSensitive) @safe pure
if (isSomeChar!Char && isSomeChar!Char2)
{
    if (stopIdx <= haystack.length)
    {
        return lastIndexOfAny(haystack[0u .. stopIdx], needles, cs);
    }

    return -1;
}

///
@safe pure unittest
{
    ptrdiff_t i = "helloWorld".lastIndexOfAny("Wlo");
    assert(i == 8);

    i = "Foo öäöllo world".lastIndexOfAny("öF");
    assert(i == 8);
}

///
@safe pure unittest
{
    import std.conv : to;

    ptrdiff_t i = "helloWorld".lastIndexOfAny("Wlo", 4);
    assert(i == 3);

    i = "Foo öäöllo world".lastIndexOfAny("öF", 3);
    assert(i == 0);
}

@safe pure unittest
{
    import std.conv : to;

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        auto r = to!S("").lastIndexOfAny("hello");
        assert(r == -1, to!string(r));

        r = to!S("hello").lastIndexOfAny("");
        assert(r == -1, to!string(r));

        r = to!S("").lastIndexOfAny("");
        assert(r == -1, to!string(r));
    }
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(lastIndexOfAny(cast(S) null, to!T("a")) == -1);
            assert(lastIndexOfAny(to!S("def"), to!T("rsa")) == -1);
            assert(lastIndexOfAny(to!S("abba"), to!T("a")) == 3);
            assert(lastIndexOfAny(to!S("def"), to!T("f")) == 2);
            assert(lastIndexOfAny(to!S("dfefffg"), to!T("fgh")) == 6);

            ptrdiff_t oeIdx = 9;
               if (is(S == wstring) || is(S == dstring))
            {
                oeIdx = 8;
            }

            auto foundOeIdx = lastIndexOfAny(to!S("dfeffgföf"), to!T("feg"));
            assert(foundOeIdx == oeIdx, to!string(foundOeIdx));

            assert(lastIndexOfAny(to!S("zfeffgfff"), to!T("ACDC"),
                No.caseSensitive) == -1);
            assert(lastIndexOfAny(to!S("def"), to!T("MI6"),
                No.caseSensitive) == -1);
            assert(lastIndexOfAny(to!S("abba"), to!T("DEA"),
                No.caseSensitive) == 3);
            assert(lastIndexOfAny(to!S("def"), to!T("FBI"),
                No.caseSensitive) == 2);
            assert(lastIndexOfAny(to!S("dfefffg"), to!T("NSA"),
                No.caseSensitive) == -1);

            oeIdx = 2;
               if (is(S == wstring) || is(S == dstring))
            {
                oeIdx = 1;
            }
            assert(lastIndexOfAny(to!S("ödfeffgfff"), to!T("BND"),
                No.caseSensitive) == oeIdx);

            assert(lastIndexOfAny("\u0100", to!T("\u0100"),
                No.caseSensitive) == 0);
        }();
    }
    }
    );
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            enum typeStr = S.stringof ~ " " ~ T.stringof;

            assert(lastIndexOfAny(cast(S) null, to!T("a"), 1337) == -1,
                typeStr);
            assert(lastIndexOfAny(to!S("abcdefcdef"), to!T("c"), 7) == 6,
                typeStr);
            assert(lastIndexOfAny(to!S("abcdefcdef"), to!T("cd"), 5) == 3,
                typeStr);
            assert(lastIndexOfAny(to!S("abcdefcdef"), to!T("ef"), 6) == 5,
                typeStr);
            assert(lastIndexOfAny(to!S("abcdefCdef"), to!T("c"), 8) == 2,
                typeStr);
            assert(lastIndexOfAny(to!S("abcdefcdef"), to!T("x"), 7) == -1,
                typeStr);
            assert(lastIndexOfAny(to!S("abcdefcdef"), to!T("xy"), 4) == -1,
                typeStr);
            assert(lastIndexOfAny(to!S("öabcdefcdef"), to!T("ö"), 2) == 0,
                typeStr);

            assert(lastIndexOfAny(cast(S) null, to!T("a"), 1337,
                No.caseSensitive) == -1, typeStr);
            assert(lastIndexOfAny(to!S("abcdefcdef"), to!T("C"), 7,
                No.caseSensitive) == 6, typeStr);
            assert(lastIndexOfAny(to!S("ABCDEFCDEF"), to!T("cd"), 5,
                No.caseSensitive) == 3, typeStr);
            assert(lastIndexOfAny(to!S("abcdefcdef"), to!T("EF"), 6,
                No.caseSensitive) == 5, typeStr);
            assert(lastIndexOfAny(to!S("ABCDEFcDEF"), to!T("C"), 8,
                No.caseSensitive) == 6, typeStr);
            assert(lastIndexOfAny(to!S("ABCDEFCDEF"), to!T("x"), 7,
                No.caseSensitive) == -1, typeStr);
            assert(lastIndexOfAny(to!S("abCdefcdef"), to!T("XY"), 4,
                No.caseSensitive) == -1, typeStr);
            assert(lastIndexOfAny(to!S("ÖABCDEFCDEF"), to!T("ö"), 2,
                No.caseSensitive) == 0, typeStr);
        }();
    }
    }
    );
}

/**
    Returns the index of the first occurrence of any character not an elements
    in $(D needles) in $(D haystack). If all element of $(D haystack) are
    element of $(D needles) $(D -1) is returned.

    Params:
        haystack = String to search for needles in.
        needles = Strings to search for in haystack.
        startIdx = slices haystack like this $(D haystack[startIdx .. $]). If
            the startIdx is greater equal the length of haystack the functions
            returns $(D -1).
        cs = Indicates whether the comparisons are case sensitive.
*/
ptrdiff_t indexOfNeither(Char,Char2)(const(Char)[] haystack,
        const(Char2)[] needles, in CaseSensitive cs = Yes.caseSensitive)
        @safe pure
if (isSomeChar!Char && isSomeChar!Char2)
{
    return indexOfAnyNeitherImpl!(true, false)(haystack, needles, cs);
}

/// Ditto
ptrdiff_t indexOfNeither(Char,Char2)(const(Char)[] haystack,
        const(Char2)[] needles, in size_t startIdx,
        in CaseSensitive cs = Yes.caseSensitive)
        @safe pure
if (isSomeChar!Char && isSomeChar!Char2)
{
    if (startIdx < haystack.length)
    {
        ptrdiff_t foundIdx = indexOfAnyNeitherImpl!(true, false)(
            haystack[startIdx .. $], needles, cs);
        if (foundIdx != -1)
        {
            return foundIdx + cast(ptrdiff_t) startIdx;
        }
    }
    return -1;
}

///
@safe pure unittest
{
    assert(indexOfNeither("abba", "a", 2) == 2);
    assert(indexOfNeither("def", "de", 1) == 2);
    assert(indexOfNeither("dfefffg", "dfe", 4) == 6);
}

///
@safe pure unittest
{
    assert(indexOfNeither("def", "a") == 0);
    assert(indexOfNeither("def", "de") == 2);
    assert(indexOfNeither("dfefffg", "dfe") == 6);
}

@safe pure unittest
{
    import std.conv : to;

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        auto r = to!S("").indexOfNeither("hello");
        assert(r == -1, to!string(r));

        r = to!S("hello").indexOfNeither("");
        assert(r == 0, to!string(r));

        r = to!S("").indexOfNeither("");
        assert(r == -1, to!string(r));
    }
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(indexOfNeither(cast(S) null, to!T("a")) == -1);
            assert(indexOfNeither("abba", "a") == 1);

            assert(indexOfNeither(to!S("dfeffgfff"), to!T("a"),
                No.caseSensitive) == 0);
            assert(indexOfNeither(to!S("def"), to!T("D"),
                No.caseSensitive) == 1);
            assert(indexOfNeither(to!S("ABca"), to!T("a"),
                No.caseSensitive) == 1);
            assert(indexOfNeither(to!S("def"), to!T("f"),
                No.caseSensitive) == 0);
            assert(indexOfNeither(to!S("DfEfffg"), to!T("dFe"),
                No.caseSensitive) == 6);
            if (is(S == string))
            {
                assert(indexOfNeither(to!S("äDfEfffg"), to!T("ädFe"),
                    No.caseSensitive) == 8,
                    to!string(indexOfNeither(to!S("äDfEfffg"), to!T("ädFe"),
                    No.caseSensitive)));
            }
            else
            {
                assert(indexOfNeither(to!S("äDfEfffg"), to!T("ädFe"),
                    No.caseSensitive) == 7,
                    to!string(indexOfNeither(to!S("äDfEfffg"), to!T("ädFe"),
                    No.caseSensitive)));
            }
        }();
    }
    }
    );
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(indexOfNeither(cast(S) null, to!T("a"), 1) == -1);
            assert(indexOfNeither(to!S("def"), to!T("a"), 1) == 1,
                to!string(indexOfNeither(to!S("def"), to!T("a"), 1)));

            assert(indexOfNeither(to!S("dfeffgfff"), to!T("a"), 4,
                No.caseSensitive) == 4);
            assert(indexOfNeither(to!S("def"), to!T("D"), 2,
                No.caseSensitive) == 2);
            assert(indexOfNeither(to!S("ABca"), to!T("a"), 3,
                No.caseSensitive) == -1);
            assert(indexOfNeither(to!S("def"), to!T("tzf"), 2,
                No.caseSensitive) == -1);
            assert(indexOfNeither(to!S("DfEfffg"), to!T("dFe"), 5,
                No.caseSensitive) == 6);
            if (is(S == string))
            {
                assert(indexOfNeither(to!S("öDfEfffg"), to!T("äDi"), 2,
                    No.caseSensitive) == 3, to!string(indexOfNeither(
                    to!S("öDfEfffg"), to!T("äDi"), 2, No.caseSensitive)));
            }
            else
            {
                assert(indexOfNeither(to!S("öDfEfffg"), to!T("äDi"), 2,
                    No.caseSensitive) == 2, to!string(indexOfNeither(
                    to!S("öDfEfffg"), to!T("äDi"), 2, No.caseSensitive)));
            }
        }();
    }
    }
    );
}

/**
    Returns the last index of the first occurence of any character that is not
    an elements in $(D needles) in $(D haystack). If all element of
    $(D haystack) are element of $(D needles) $(D -1) is returned.

    Params:
        haystack = String to search for needles in.
        needles = Strings to search for in haystack.
        stopIdx = slices haystack like this $(D haystack[0 .. stopIdx]) If
        the stopIdx is greater equal the length of haystack the functions
        returns $(D -1).
        cs = Indicates whether the comparisons are case sensitive.
*/
ptrdiff_t lastIndexOfNeither(Char,Char2)(const(Char)[] haystack,
        const(Char2)[] needles, in CaseSensitive cs = Yes.caseSensitive)
        @safe pure
if (isSomeChar!Char && isSomeChar!Char2)
{
    return indexOfAnyNeitherImpl!(false, false)(haystack, needles, cs);
}

/// Ditto
ptrdiff_t lastIndexOfNeither(Char,Char2)(const(Char)[] haystack,
        const(Char2)[] needles, in size_t stopIdx,
        in CaseSensitive cs = Yes.caseSensitive)
        @safe pure
if (isSomeChar!Char && isSomeChar!Char2)
{
    if (stopIdx < haystack.length)
    {
        return indexOfAnyNeitherImpl!(false, false)(haystack[0 .. stopIdx],
            needles, cs);
    }
    return -1;
}

///
@safe pure unittest
{
    assert(lastIndexOfNeither("abba", "a") == 2);
    assert(lastIndexOfNeither("def", "f") == 1);
}

///
@safe pure unittest
{
    assert(lastIndexOfNeither("def", "rsa", 3) == -1);
    assert(lastIndexOfNeither("abba", "a", 2) == 1);
}

@safe pure unittest
{
    import std.conv : to;

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        auto r = to!S("").lastIndexOfNeither("hello");
        assert(r == -1, to!string(r));

        r = to!S("hello").lastIndexOfNeither("");
        assert(r == 4, to!string(r));

        r = to!S("").lastIndexOfNeither("");
        assert(r == -1, to!string(r));
    }
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(lastIndexOfNeither(cast(S) null, to!T("a")) == -1);
            assert(lastIndexOfNeither(to!S("def"), to!T("rsa")) == 2);
            assert(lastIndexOfNeither(to!S("dfefffg"), to!T("fgh")) == 2);

            ptrdiff_t oeIdx = 8;
               if (is(S == string))
            {
                oeIdx = 9;
            }

            auto foundOeIdx = lastIndexOfNeither(to!S("ödfefegff"), to!T("zeg"));
            assert(foundOeIdx == oeIdx, to!string(foundOeIdx));

            assert(lastIndexOfNeither(to!S("zfeffgfsb"), to!T("FSB"),
                No.caseSensitive) == 5);
            assert(lastIndexOfNeither(to!S("def"), to!T("MI6"),
                No.caseSensitive) == 2, to!string(lastIndexOfNeither(to!S("def"),
                to!T("MI6"), No.caseSensitive)));
            assert(lastIndexOfNeither(to!S("abbadeafsb"), to!T("fSb"),
                No.caseSensitive) == 6, to!string(lastIndexOfNeither(
                to!S("abbadeafsb"), to!T("fSb"), No.caseSensitive)));
            assert(lastIndexOfNeither(to!S("defbi"), to!T("FBI"),
                No.caseSensitive) == 1);
            assert(lastIndexOfNeither(to!S("dfefffg"), to!T("NSA"),
                No.caseSensitive) == 6);
            assert(lastIndexOfNeither(to!S("dfeffgfffö"), to!T("BNDabCHIJKQEPÖÖSYXÄ??ß"),
                No.caseSensitive) == 8, to!string(lastIndexOfNeither(to!S("dfeffgfffö"),
                to!T("BNDabCHIJKQEPÖÖSYXÄ??ß"), No.caseSensitive)));
        }();
    }
    }
    );
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        foreach (T; AliasSeq!(string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(lastIndexOfNeither(cast(S) null, to!T("a"), 1337) == -1);
            assert(lastIndexOfNeither(to!S("def"), to!T("f")) == 1);
            assert(lastIndexOfNeither(to!S("dfefffg"), to!T("fgh")) == 2);

            ptrdiff_t oeIdx = 4;
               if (is(S == string))
            {
                oeIdx = 5;
            }

            auto foundOeIdx = lastIndexOfNeither(to!S("ödfefegff"), to!T("zeg"),
                7);
            assert(foundOeIdx == oeIdx, to!string(foundOeIdx));

            assert(lastIndexOfNeither(to!S("zfeffgfsb"), to!T("FSB"), 6,
                No.caseSensitive) == 5);
            assert(lastIndexOfNeither(to!S("def"), to!T("MI6"), 2,
                No.caseSensitive) == 1, to!string(lastIndexOfNeither(to!S("def"),
                to!T("MI6"), 2, No.caseSensitive)));
            assert(lastIndexOfNeither(to!S("abbadeafsb"), to!T("fSb"), 6,
                No.caseSensitive) == 5, to!string(lastIndexOfNeither(
                to!S("abbadeafsb"), to!T("fSb"), 6, No.caseSensitive)));
            assert(lastIndexOfNeither(to!S("defbi"), to!T("FBI"), 3,
                No.caseSensitive) == 1);
            assert(lastIndexOfNeither(to!S("dfefffg"), to!T("NSA"), 2,
                No.caseSensitive) == 1, to!string(lastIndexOfNeither(
                    to!S("dfefffg"), to!T("NSA"), 2, No.caseSensitive)));
        }();
    }
    }
    );
}

/**
 * Returns the _representation of a string, which has the same type
 * as the string except the character type is replaced by $(D ubyte),
 * $(D ushort), or $(D uint) depending on the character width.
 *
 * Params:
 *     s = The string to return the _representation of.
 *
 * Returns:
 *     The _representation of the passed string.
 */
auto representation(Char)(Char[] s) @safe pure nothrow @nogc
if (isSomeChar!Char)
{
    import std.traits : ModifyTypePreservingTQ;
    alias ToRepType(T) = AliasSeq!(ubyte, ushort, uint)[T.sizeof / 2];
    return cast(ModifyTypePreservingTQ!(ToRepType, Char)[])s;
}

///
@safe pure unittest
{
    string s = "hello";
    static assert(is(typeof(representation(s)) == immutable(ubyte)[]));
    assert(representation(s) is cast(immutable(ubyte)[]) s);
    assert(representation(s) == [0x68, 0x65, 0x6c, 0x6c, 0x6f]);
}

@system pure unittest
{
    import std.exception : assertCTFEable;
    import std.traits : Fields;
    import std.typecons : Tuple;

    assertCTFEable!(
    {
    void test(Char, T)(Char[] str)
    {
        static assert(is(typeof(representation(str)) == T[]));
        assert(representation(str) is cast(T[]) str);
    }

    foreach (Type; AliasSeq!(Tuple!(char , ubyte ),
                             Tuple!(wchar, ushort),
                             Tuple!(dchar, uint  )))
    {
        alias Char = Fields!Type[0];
        alias Int  = Fields!Type[1];
        enum immutable(Char)[] hello = "hello";

        test!(   immutable Char,    immutable Int)(hello);
        test!(       const Char,        const Int)(hello);
        test!(             Char,              Int)(hello.dup);
        test!(      shared Char,       shared Int)(cast(shared) hello.dup);
        test!(const shared Char, const shared Int)(hello);
    }
    });
}


/**
 * Capitalize the first character of $(D s) and convert the rest of $(D s) to
 * lowercase.
 *
 * Params:
 *     input = The string to _capitalize.
 *
 * Returns:
 *     The capitalized string.
 *
 * See_Also:
 *      $(REF asCapitalized, std,uni) for a lazy range version that doesn't allocate memory
 */
S capitalize(S)(S input) @trusted pure
if (isSomeString!S)
{
    import std.array : array;
    import std.uni : asCapitalized;
    import std.utf : byUTF;

    return input.asCapitalized.byUTF!(ElementEncodingType!(S)).array;
}

///
pure @safe unittest
{
    assert(capitalize("hello") == "Hello");
    assert(capitalize("World") == "World");
}

auto capitalize(S)(auto ref S s)
if (!isSomeString!S && is(StringTypeOf!S))
{
    return capitalize!(StringTypeOf!S)(s);
}

@safe pure unittest
{
    assert(testAliasedString!capitalize("hello"));
}

@safe pure unittest
{
    import std.algorithm.comparison : cmp;
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[]))
    {
        S s1 = to!S("FoL");
        S s2;

        s2 = capitalize(s1);
        assert(cmp(s2, "Fol") == 0);
        assert(s2 !is s1);

        s2 = capitalize(s1[0 .. 2]);
        assert(cmp(s2, "Fo") == 0);

        s1 = to!S("fOl");
        s2 = capitalize(s1);
        assert(cmp(s2, "Fol") == 0);
        assert(s2 !is s1);
        s1 = to!S("\u0131 \u0130");
        s2 = capitalize(s1);
        assert(cmp(s2, "\u0049 i\u0307") == 0);
        assert(s2 !is s1);

        s1 = to!S("\u017F \u0049");
        s2 = capitalize(s1);
        assert(cmp(s2, "\u0053 \u0069") == 0);
        assert(s2 !is s1);
    }
    });
}

/++
    Split $(D s) into an array of lines according to the unicode standard using
    $(D '\r'), $(D '\n'), $(D "\r\n"), $(REF lineSep, std,uni),
    $(REF paraSep, std,uni), $(D U+0085) (NEL), $(D '\v')  and $(D '\f')
    as delimiters. If $(D keepTerm) is set to $(D KeepTerminator.yes), then the
    delimiter is included in the strings returned.

    Does not throw on invalid UTF; such is simply passed unchanged
    to the output.

    Allocates memory; use $(LREF lineSplitter) for an alternative that
    does not.

    Adheres to $(HTTP www.unicode.org/versions/Unicode7.0.0/ch05.pdf, Unicode 7.0).

  Params:
    s = a string of $(D chars), $(D wchars), or $(D dchars), or any custom
        type that casts to a $(D string) type
    keepTerm = whether delimiter is included or not in the results
  Returns:
    array of strings, each element is a line that is a slice of $(D s)
  See_Also:
    $(LREF lineSplitter)
    $(REF splitter, std,algorithm)
    $(REF splitter, std,regex)
 +/
alias KeepTerminator = Flag!"keepTerminator";

/// ditto
S[] splitLines(S)(S s, in KeepTerminator keepTerm = No.keepTerminator) @safe pure
if (isSomeString!S)
{
    import std.array : appender;
    import std.uni : lineSep, paraSep;

    size_t iStart = 0;
    auto retval = appender!(S[])();

    for (size_t i; i < s.length; ++i)
    {
        switch (s[i])
        {
            case '\v', '\f', '\n':
                retval.put(s[iStart .. i + (keepTerm == Yes.keepTerminator)]);
                iStart = i + 1;
                break;

            case '\r':
                if (i + 1 < s.length && s[i + 1] == '\n')
                {
                    retval.put(s[iStart .. i + (keepTerm == Yes.keepTerminator) * 2]);
                    iStart = i + 2;
                    ++i;
                }
                else
                {
                    goto case '\n';
                }
                break;

            static if (s[i].sizeof == 1)
            {
                /* Manually decode:
                 *  lineSep is E2 80 A8
                 *  paraSep is E2 80 A9
                 */
                case 0xE2:
                    if (i + 2 < s.length &&
                        s[i + 1] == 0x80 &&
                        (s[i + 2] == 0xA8 || s[i + 2] == 0xA9)
                       )
                    {
                        retval.put(s[iStart .. i + (keepTerm == Yes.keepTerminator) * 3]);
                        iStart = i + 3;
                        i += 2;
                    }
                    else
                        goto default;
                    break;
                /* Manually decode:
                 *  NEL is C2 85
                 */
                case 0xC2:
                    if (i + 1 < s.length && s[i + 1] == 0x85)
                    {
                        retval.put(s[iStart .. i + (keepTerm == Yes.keepTerminator) * 2]);
                        iStart = i + 2;
                        i += 1;
                    }
                    else
                        goto default;
                    break;
            }
            else
            {
                case lineSep:
                case paraSep:
                case '\u0085':
                    goto case '\n';
            }

            default:
                break;
        }
    }

    if (iStart != s.length)
        retval.put(s[iStart .. $]);

    return retval.data;
}

///
@safe pure nothrow unittest
{
    string s = "Hello\nmy\rname\nis";
    assert(splitLines(s) == ["Hello", "my", "name", "is"]);
}

@safe pure nothrow unittest
{
    string s = "a\xC2\x86b";
    assert(splitLines(s) == [s]);
}

auto splitLines(S)(auto ref S s, in KeepTerminator keepTerm = No.keepTerminator)
if (!isSomeString!S && is(StringTypeOf!S))
{
    return splitLines!(StringTypeOf!S)(s, keepTerm);
}

@safe pure nothrow unittest
{
    assert(testAliasedString!splitLines("hello\nworld"));
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
    {
        auto s = to!S(
            "\rpeter\n\rpaul\r\njerry\u2028ice\u2029cream\n\nsunday\n" ~
            "mon\u2030day\nschadenfreude\vkindergarten\f\vcookies\u0085"
        );
        auto lines = splitLines(s);
        assert(lines.length == 14);
        assert(lines[0] == "");
        assert(lines[1] == "peter");
        assert(lines[2] == "");
        assert(lines[3] == "paul");
        assert(lines[4] == "jerry");
        assert(lines[5] == "ice");
        assert(lines[6] == "cream");
        assert(lines[7] == "");
        assert(lines[8] == "sunday");
        assert(lines[9] == "mon\u2030day");
        assert(lines[10] == "schadenfreude");
        assert(lines[11] == "kindergarten");
        assert(lines[12] == "");
        assert(lines[13] == "cookies");


        ubyte[] u = ['a', 0xFF, 0x12, 'b'];     // invalid UTF
        auto ulines = splitLines(cast(char[]) u);
        assert(cast(ubyte[])(ulines[0]) == u);

        lines = splitLines(s, Yes.keepTerminator);
        assert(lines.length == 14);
        assert(lines[0] == "\r");
        assert(lines[1] == "peter\n");
        assert(lines[2] == "\r");
        assert(lines[3] == "paul\r\n");
        assert(lines[4] == "jerry\u2028");
        assert(lines[5] == "ice\u2029");
        assert(lines[6] == "cream\n");
        assert(lines[7] == "\n");
        assert(lines[8] == "sunday\n");
        assert(lines[9] == "mon\u2030day\n");
        assert(lines[10] == "schadenfreude\v");
        assert(lines[11] == "kindergarten\f");
        assert(lines[12] == "\v");
        assert(lines[13] == "cookies\u0085");

        s.popBack(); // Lop-off trailing \n
        lines = splitLines(s);
        assert(lines.length == 14);
        assert(lines[9] == "mon\u2030day");

        lines = splitLines(s, Yes.keepTerminator);
        assert(lines.length == 14);
        assert(lines[13] == "cookies");
    }
    });
}

private struct LineSplitter(KeepTerminator keepTerm = No.keepTerminator, Range)
{
    import std.conv : unsigned;
    import std.uni : lineSep, paraSep;
private:
    Range _input;

    alias IndexType = typeof(unsigned(_input.length));
    enum IndexType _unComputed = IndexType.max;
    IndexType iStart = _unComputed;
    IndexType iEnd = 0;
    IndexType iNext = 0;

public:
    this(Range input)
    {
        _input = input;
    }

    static if (isInfinite!Range)
    {
        enum bool empty = false;
    }
    else
    {
        @property bool empty()
        {
            return iStart == _unComputed && iNext == _input.length;
        }
    }

    @property typeof(_input) front()
    {
        if (iStart == _unComputed)
        {
            iStart = iNext;
        Loop:
            for (IndexType i = iNext; ; ++i)
            {
                if (i == _input.length)
                {
                    iEnd = i;
                    iNext = i;
                    break Loop;
                }
                switch (_input[i])
                {
                case '\v', '\f', '\n':
                    iEnd = i + (keepTerm == Yes.keepTerminator);
                    iNext = i + 1;
                    break Loop;

                case '\r':
                    if (i + 1 < _input.length && _input[i + 1] == '\n')
                    {
                        iEnd = i + (keepTerm == Yes.keepTerminator) * 2;
                        iNext = i + 2;
                        break Loop;
                    }
                    else
                    {
                        goto case '\n';
                    }

                    static if (_input[i].sizeof == 1)
                    {
                        /* Manually decode:
                         *  lineSep is E2 80 A8
                         *  paraSep is E2 80 A9
                         */
                    case 0xE2:
                        if (i + 2 < _input.length &&
                            _input[i + 1] == 0x80 &&
                            (_input[i + 2] == 0xA8 || _input[i + 2] == 0xA9)
                        )
                        {
                            iEnd = i + (keepTerm == Yes.keepTerminator) * 3;
                            iNext = i + 3;
                            break Loop;
                        }
                        else
                            goto default;
                        /* Manually decode:
                         *  NEL is C2 85
                         */
                    case 0xC2:
                        if (i + 1 < _input.length && _input[i + 1] == 0x85)
                        {
                            iEnd = i + (keepTerm == Yes.keepTerminator) * 2;
                            iNext = i + 2;
                            break Loop;
                        }
                        else
                            goto default;
                    }
                    else
                    {
                    case '\u0085':
                    case lineSep:
                    case paraSep:
                        goto case '\n';
                    }

                default:
                    break;
                }
            }
        }
        return _input[iStart .. iEnd];
    }

    void popFront()
    {
        if (iStart == _unComputed)
        {
            assert(!empty);
            front;
        }
        iStart = _unComputed;
    }

    static if (isForwardRange!Range)
    {
        @property typeof(this) save()
        {
            auto ret = this;
            ret._input = _input.save;
            return ret;
        }
    }
}

/***********************************
 *  Split an array or slicable range of characters into a range of lines
    using $(D '\r'), $(D '\n'), $(D '\v'), $(D '\f'), $(D "\r\n"),
    $(REF lineSep, std,uni), $(REF paraSep, std,uni) and $(D '\u0085') (NEL)
    as delimiters. If $(D keepTerm) is set to $(D Yes.keepTerminator), then the
    delimiter is included in the slices returned.

    Does not throw on invalid UTF; such is simply passed unchanged
    to the output.

    Adheres to $(HTTP www.unicode.org/versions/Unicode7.0.0/ch05.pdf, Unicode 7.0).

    Does not allocate memory.

  Params:
    r = array of $(D chars), $(D wchars), or $(D dchars) or a slicable range
    keepTerm = whether delimiter is included or not in the results
  Returns:
    range of slices of the input range $(D r)

  See_Also:
    $(LREF splitLines)
    $(REF splitter, std,algorithm)
    $(REF splitter, std,regex)
 */
auto lineSplitter(KeepTerminator keepTerm = No.keepTerminator, Range)(Range r)
if ((hasSlicing!Range && hasLength!Range && isSomeChar!(ElementType!Range) ||
    isSomeString!Range) &&
    !isConvertibleToString!Range)
{
    return LineSplitter!(keepTerm, Range)(r);
}

///
@safe pure unittest
{
    import std.array : array;

    string s = "Hello\nmy\rname\nis";

    /* notice the call to 'array' to turn the lazy range created by
    lineSplitter comparable to the string[] created by splitLines.
    */
    assert(lineSplitter(s).array == splitLines(s));
}

auto lineSplitter(KeepTerminator keepTerm = No.keepTerminator, Range)(auto ref Range r)
if (isConvertibleToString!Range)
{
    return LineSplitter!(keepTerm, StringTypeOf!Range)(r);
}

@safe pure unittest
{
    import std.array : array;
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
    {
        auto s = to!S(
            "\rpeter\n\rpaul\r\njerry\u2028ice\u2029cream\n\n" ~
            "sunday\nmon\u2030day\nschadenfreude\vkindergarten\f\vcookies\u0085"
        );

        auto lines = lineSplitter(s).array;
        assert(lines.length == 14);
        assert(lines[0] == "");
        assert(lines[1] == "peter");
        assert(lines[2] == "");
        assert(lines[3] == "paul");
        assert(lines[4] == "jerry");
        assert(lines[5] == "ice");
        assert(lines[6] == "cream");
        assert(lines[7] == "");
        assert(lines[8] == "sunday");
        assert(lines[9] == "mon\u2030day");
        assert(lines[10] == "schadenfreude");
        assert(lines[11] == "kindergarten");
        assert(lines[12] == "");
        assert(lines[13] == "cookies");


        ubyte[] u = ['a', 0xFF, 0x12, 'b'];     // invalid UTF
        auto ulines = lineSplitter(cast(char[]) u).array;
        assert(cast(ubyte[])(ulines[0]) == u);

        lines = lineSplitter!(Yes.keepTerminator)(s).array;
        assert(lines.length == 14);
        assert(lines[0] == "\r");
        assert(lines[1] == "peter\n");
        assert(lines[2] == "\r");
        assert(lines[3] == "paul\r\n");
        assert(lines[4] == "jerry\u2028");
        assert(lines[5] == "ice\u2029");
        assert(lines[6] == "cream\n");
        assert(lines[7] == "\n");
        assert(lines[8] == "sunday\n");
        assert(lines[9] == "mon\u2030day\n");
        assert(lines[10] == "schadenfreude\v");
        assert(lines[11] == "kindergarten\f");
        assert(lines[12] == "\v");
        assert(lines[13] == "cookies\u0085");

        s.popBack(); // Lop-off trailing \n
        lines = lineSplitter(s).array;
        assert(lines.length == 14);
        assert(lines[9] == "mon\u2030day");

        lines = lineSplitter!(Yes.keepTerminator)(s).array;
        assert(lines.length == 14);
        assert(lines[13] == "cookies");
    }
    });
}

///
@nogc @safe pure unittest
{
    auto s = "\rpeter\n\rpaul\r\njerry\u2028ice\u2029cream\n\nsunday\nmon\u2030day\n";
    auto lines = s.lineSplitter();
    static immutable witness = ["", "peter", "", "paul", "jerry", "ice", "cream", "", "sunday", "mon\u2030day"];
    uint i;
    foreach (line; lines)
    {
        assert(line == witness[i++]);
    }
    assert(i == witness.length);
}

@nogc @safe pure unittest
{
    import std.algorithm.comparison : equal;
    auto s = "std/string.d";
    auto as = TestAliasedString(s);
    assert(equal(s.lineSplitter(), as.lineSplitter()));
}

@safe pure unittest
{
    auto s = "line1\nline2";
    auto spl0 = s.lineSplitter!(Yes.keepTerminator);
    auto spl1 = spl0.save;
    spl0.popFront;
    assert(spl1.front ~ spl0.front == s);
    string r = "a\xC2\x86b";
    assert(r.lineSplitter.front == r);
}

/++
    Strips leading whitespace (as defined by $(REF isWhite, std,uni)).

    Params:
        input = string or $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
        of characters

    Returns: $(D input) stripped of leading whitespace.

    Postconditions: $(D input) and the returned value
    will share the same tail (see $(REF sameTail, std,array)).

    See_Also:
        Generic stripping on ranges: $(REF _stripLeft, std, algorithm, mutation)
  +/
auto stripLeft(Range)(Range input)
if (isForwardRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isInfinite!Range && !isConvertibleToString!Range)
{
    static import std.ascii;
    static import std.uni;
    import std.utf : decodeFront;

    while (!input.empty)
    {
        auto c = input.front;
        if (std.ascii.isASCII(c))
        {
            if (!std.ascii.isWhite(c))
                break;
            input.popFront();
        }
        else
        {
            auto save = input.save;
            auto dc = decodeFront(input);
            if (!std.uni.isWhite(dc))
                return save;
        }
    }
    return input;
}

///
@safe pure unittest
{
    import std.uni : lineSep, paraSep;
    assert(stripLeft("     hello world     ") ==
           "hello world     ");
    assert(stripLeft("\n\t\v\rhello world\n\t\v\r") ==
           "hello world\n\t\v\r");
    assert(stripLeft("hello world") ==
           "hello world");
    assert(stripLeft([lineSep] ~ "hello world" ~ lineSep) ==
           "hello world" ~ [lineSep]);
    assert(stripLeft([paraSep] ~ "hello world" ~ paraSep) ==
           "hello world" ~ [paraSep]);

    import std.array : array;
    import std.utf : byChar;
    assert(stripLeft("     hello world     "w.byChar).array ==
           "hello world     ");
}

auto stripLeft(Range)(auto ref Range str)
if (isConvertibleToString!Range)
{
    return stripLeft!(StringTypeOf!Range)(str);
}

@safe pure unittest
{
    assert(testAliasedString!stripLeft("  hello"));
}

/++
    Strips trailing whitespace (as defined by $(REF isWhite, std,uni)).

    Params:
        str = string or random access range of characters

    Returns:
        slice of $(D str) stripped of trailing whitespace.

    See_Also:
        Generic stripping on ranges: $(REF _stripRight, std, algorithm, mutation)
  +/
auto stripRight(Range)(Range str)
if (isSomeString!Range ||
    isRandomAccessRange!Range && hasLength!Range && hasSlicing!Range &&
    !isConvertibleToString!Range &&
    isSomeChar!(ElementEncodingType!Range))
{
    import std.uni : isWhite;
    alias C = Unqual!(ElementEncodingType!(typeof(str)));

    static if (isSomeString!(typeof(str)))
    {
        import std.utf : codeLength;

        foreach_reverse (i, dchar c; str)
        {
            if (!isWhite(c))
                return str[0 .. i + codeLength!C(c)];
        }

        return str[0 .. 0];
    }
    else
    {
        size_t i = str.length;
        while (i--)
        {
            static if (C.sizeof == 4)
            {
                if (isWhite(str[i]))
                    continue;
                break;
            }
            else static if (C.sizeof == 2)
            {
                auto c2 = str[i];
                if (c2 < 0xD800 || c2 >= 0xE000)
                {
                    if (isWhite(c2))
                        continue;
                }
                else if (c2 >= 0xDC00)
                {
                    if (i)
                    {
                        immutable c1 = str[i - 1];
                        if (c1 >= 0xD800 && c1 < 0xDC00)
                        {
                            immutable dchar c = ((c1 - 0xD7C0) << 10) + (c2 - 0xDC00);
                            if (isWhite(c))
                            {
                                --i;
                                continue;
                            }
                        }
                    }
                }
                break;
            }
            else static if (C.sizeof == 1)
            {
                import std.utf : byDchar;

                char cx = str[i];
                if (cx <= 0x7F)
                {
                    if (isWhite(cx))
                        continue;
                    break;
                }
                else
                {
                    size_t stride = 0;

                    while (1)
                    {
                        ++stride;
                        if (!i || (cx & 0xC0) == 0xC0 || stride == 4)
                            break;
                        cx = str[i - 1];
                        if (!(cx & 0x80))
                            break;
                        --i;
                    }

                    if (!str[i .. i + stride].byDchar.front.isWhite)
                        return str[0 .. i + stride];
                }
            }
            else
                static assert(0);
        }

        return str[0 .. i + 1];
    }
}

///
@safe pure
unittest
{
    import std.uni : lineSep, paraSep;
    assert(stripRight("     hello world     ") ==
           "     hello world");
    assert(stripRight("\n\t\v\rhello world\n\t\v\r") ==
           "\n\t\v\rhello world");
    assert(stripRight("hello world") ==
           "hello world");
    assert(stripRight([lineSep] ~ "hello world" ~ lineSep) ==
           [lineSep] ~ "hello world");
    assert(stripRight([paraSep] ~ "hello world" ~ paraSep) ==
           [paraSep] ~ "hello world");
}

auto stripRight(Range)(auto ref Range str)
if (isConvertibleToString!Range)
{
    return stripRight!(StringTypeOf!Range)(str);
}

@safe pure unittest
{
    assert(testAliasedString!stripRight("hello   "));
}

@safe pure unittest
{
    import std.array : array;
    import std.uni : lineSep, paraSep;
    import std.utf : byChar, byDchar, byUTF, byWchar, invalidUTFstrings;
    assert(stripRight("     hello world     ".byChar).array == "     hello world");
    assert(stripRight("\n\t\v\rhello world\n\t\v\r"w.byWchar).array == "\n\t\v\rhello world"w);
    assert(stripRight("hello world"d.byDchar).array == "hello world"d);
    assert(stripRight("\u2028hello world\u2020\u2028".byChar).array == "\u2028hello world\u2020");
    assert(stripRight("hello world\U00010001"w.byWchar).array == "hello world\U00010001"w);

    foreach (C; AliasSeq!(char, wchar, dchar))
    {
        foreach (s; invalidUTFstrings!C())
        {
            cast(void) stripRight(s.byUTF!C).array;
        }
    }

    cast(void) stripRight("a\x80".byUTF!char).array;
    wstring ws = ['a', cast(wchar) 0xDC00];
    cast(void) stripRight(ws.byUTF!wchar).array;
}


/++
    Strips both leading and trailing whitespace (as defined by
    $(REF isWhite, std,uni)).

    Params:
        str = string or random access range of characters

    Returns:
        slice of $(D str) stripped of leading and trailing whitespace.

    See_Also:
        Generic stripping on ranges: $(REF _strip, std, algorithm, mutation)
  +/
auto strip(Range)(Range str)
if (isSomeString!Range ||
    isRandomAccessRange!Range && hasLength!Range && hasSlicing!Range &&
    !isConvertibleToString!Range &&
    isSomeChar!(ElementEncodingType!Range))
{
    return stripRight(stripLeft(str));
}

///
@safe pure unittest
{
    import std.uni : lineSep, paraSep;
    assert(strip("     hello world     ") ==
           "hello world");
    assert(strip("\n\t\v\rhello world\n\t\v\r") ==
           "hello world");
    assert(strip("hello world") ==
           "hello world");
    assert(strip([lineSep] ~ "hello world" ~ [lineSep]) ==
           "hello world");
    assert(strip([paraSep] ~ "hello world" ~ [paraSep]) ==
           "hello world");
}

auto strip(Range)(auto ref Range str)
if (isConvertibleToString!Range)
{
    return strip!(StringTypeOf!Range)(str);
}

@safe pure unittest
{
    assert(testAliasedString!strip("     hello world     "));
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!( char[], const  char[],  string,
                          wchar[], const wchar[], wstring,
                          dchar[], const dchar[], dstring))
    {
        assert(equal(stripLeft(to!S("  foo\t ")), "foo\t "));
        assert(equal(stripLeft(to!S("\u2008  foo\t \u2007")), "foo\t \u2007"));
        assert(equal(stripLeft(to!S("\u0085 μ \u0085 \u00BB \r")), "μ \u0085 \u00BB \r"));
        assert(equal(stripLeft(to!S("1")), "1"));
        assert(equal(stripLeft(to!S("\U0010FFFE")), "\U0010FFFE"));
        assert(equal(stripLeft(to!S("")), ""));

        assert(equal(stripRight(to!S("  foo\t ")), "  foo"));
        assert(equal(stripRight(to!S("\u2008  foo\t \u2007")), "\u2008  foo"));
        assert(equal(stripRight(to!S("\u0085 μ \u0085 \u00BB \r")), "\u0085 μ \u0085 \u00BB"));
        assert(equal(stripRight(to!S("1")), "1"));
        assert(equal(stripRight(to!S("\U0010FFFE")), "\U0010FFFE"));
        assert(equal(stripRight(to!S("")), ""));

        assert(equal(strip(to!S("  foo\t ")), "foo"));
        assert(equal(strip(to!S("\u2008  foo\t \u2007")), "foo"));
        assert(equal(strip(to!S("\u0085 μ \u0085 \u00BB \r")), "μ \u0085 \u00BB"));
        assert(equal(strip(to!S("\U0010FFFE")), "\U0010FFFE"));
        assert(equal(strip(to!S("")), ""));
    }
    });
}

@safe pure unittest
{
    import std.array : sameHead, sameTail;
    import std.exception : assertCTFEable;
    assertCTFEable!(
    {
    wstring s = " ";
    assert(s.sameTail(s.stripLeft()));
    assert(s.sameHead(s.stripRight()));
    });
}


/++
    If $(D str) ends with $(D delimiter), then $(D str) is returned without
    $(D delimiter) on its end. If it $(D str) does $(I not) end with
    $(D delimiter), then it is returned unchanged.

    If no $(D delimiter) is given, then one trailing  $(D '\r'), $(D '\n'),
    $(D "\r\n"), $(D '\f'), $(D '\v'), $(REF lineSep, std,uni), $(REF paraSep, std,uni), or $(REF nelSep, std,uni)
    is removed from the end of $(D str). If $(D str) does not end with any of those characters,
    then it is returned unchanged.

    Params:
        str = string or indexable range of characters
        delimiter = string of characters to be sliced off end of str[]

    Returns:
        slice of str
  +/
Range chomp(Range)(Range str)
if ((isRandomAccessRange!Range && isSomeChar!(ElementEncodingType!Range) ||
    isNarrowString!Range) &&
    !isConvertibleToString!Range)
{
    import std.uni : lineSep, paraSep, nelSep;
    if (str.empty)
        return str;

    alias C = ElementEncodingType!Range;

    switch (str[$ - 1])
    {
        case '\n':
        {
            if (str.length > 1 && str[$ - 2] == '\r')
                return str[0 .. $ - 2];
            goto case;
        }
        case '\r', '\v', '\f':
            return str[0 .. $ - 1];

        // Pop off the last character if lineSep, paraSep, or nelSep
        static if (is(C : const char))
        {
            /* Manually decode:
             *  lineSep is E2 80 A8
             *  paraSep is E2 80 A9
             */
            case 0xA8: // Last byte of lineSep
            case 0xA9: // Last byte of paraSep
                if (str.length > 2 && str[$ - 2] == 0x80 && str[$ - 3] == 0xE2)
                    return str [0 .. $ - 3];
                goto default;

            /* Manually decode:
             *  NEL is C2 85
             */
            case 0x85:
                if (str.length > 1 && str[$ - 2] == 0xC2)
                    return str [0 .. $ - 2];
                goto default;
        }
        else
        {
            case lineSep:
            case paraSep:
            case nelSep:
                return str[0 .. $ - 1];
        }
        default:
            return str;
    }
}

/// Ditto
Range chomp(Range, C2)(Range str, const(C2)[] delimiter)
if ((isBidirectionalRange!Range && isSomeChar!(ElementEncodingType!Range) ||
    isNarrowString!Range) &&
    !isConvertibleToString!Range &&
    isSomeChar!C2)
{
    if (delimiter.empty)
        return chomp(str);

    alias C1 = ElementEncodingType!Range;

    static if (is(Unqual!C1 == Unqual!C2) && (isSomeString!Range || (hasSlicing!Range && C2.sizeof == 4)))
    {
        import std.algorithm.searching : endsWith;
        if (str.endsWith(delimiter))
            return str[0 .. $ - delimiter.length];
        return str;
    }
    else
    {
        auto orig = str.save;

        static if (isSomeString!Range)
            alias C = dchar;    // because strings auto-decode
        else
            alias C = C1;       // and ranges do not

        foreach_reverse (C c; delimiter)
        {
            if (str.empty || str.back != c)
                return orig;

            str.popBack();
        }

        return str;
    }
}

///
@safe pure
unittest
{
    import std.uni : lineSep, paraSep, nelSep;
    import std.utf : decode;
    assert(chomp(" hello world  \n\r") == " hello world  \n");
    assert(chomp(" hello world  \r\n") == " hello world  ");
    assert(chomp(" hello world  \f") == " hello world  ");
    assert(chomp(" hello world  \v") == " hello world  ");
    assert(chomp(" hello world  \n\n") == " hello world  \n");
    assert(chomp(" hello world  \n\n ") == " hello world  \n\n ");
    assert(chomp(" hello world  \n\n" ~ [lineSep]) == " hello world  \n\n");
    assert(chomp(" hello world  \n\n" ~ [paraSep]) == " hello world  \n\n");
    assert(chomp(" hello world  \n\n" ~ [ nelSep]) == " hello world  \n\n");
    assert(chomp(" hello world") == " hello world");
    assert(chomp("") == "");

    assert(chomp(" hello world", "orld") == " hello w");
    assert(chomp(" hello world", " he") == " hello world");
    assert(chomp("", "hello") == "");

    // Don't decode pointlessly
    assert(chomp("hello\xFE", "\r") == "hello\xFE");
}

StringTypeOf!Range chomp(Range)(auto ref Range str)
if (isConvertibleToString!Range)
{
    return chomp!(StringTypeOf!Range)(str);
}

StringTypeOf!Range chomp(Range, C2)(auto ref Range str, const(C2)[] delimiter)
if (isConvertibleToString!Range)
{
    return chomp!(StringTypeOf!Range, C2)(str, delimiter);
}

@safe pure unittest
{
    assert(testAliasedString!chomp(" hello world  \n\r"));
    assert(testAliasedString!chomp(" hello world", "orld"));
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    string s;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
    {
        // @@@ BUG IN COMPILER, MUST INSERT CAST
        assert(chomp(cast(S) null) is null);
        assert(chomp(to!S("hello")) == "hello");
        assert(chomp(to!S("hello\n")) == "hello");
        assert(chomp(to!S("hello\r")) == "hello");
        assert(chomp(to!S("hello\r\n")) == "hello");
        assert(chomp(to!S("hello\n\r")) == "hello\n");
        assert(chomp(to!S("hello\n\n")) == "hello\n");
        assert(chomp(to!S("hello\r\r")) == "hello\r");
        assert(chomp(to!S("hello\nxxx\n")) == "hello\nxxx");
        assert(chomp(to!S("hello\u2028")) == "hello");
        assert(chomp(to!S("hello\u2029")) == "hello");
        assert(chomp(to!S("hello\u0085")) == "hello");
        assert(chomp(to!S("hello\u2028\u2028")) == "hello\u2028");
        assert(chomp(to!S("hello\u2029\u2029")) == "hello\u2029");
        assert(chomp(to!S("hello\u2029\u2129")) == "hello\u2029\u2129");
        assert(chomp(to!S("hello\u2029\u0185")) == "hello\u2029\u0185");

        foreach (T; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            // @@@ BUG IN COMPILER, MUST INSERT CAST
            assert(chomp(cast(S) null, cast(T) null) is null);
            assert(chomp(to!S("hello\n"), cast(T) null) == "hello");
            assert(chomp(to!S("hello"), to!T("o")) == "hell");
            assert(chomp(to!S("hello"), to!T("p")) == "hello");
            // @@@ BUG IN COMPILER, MUST INSERT CAST
            assert(chomp(to!S("hello"), cast(T) null) == "hello");
            assert(chomp(to!S("hello"), to!T("llo")) == "he");
            assert(chomp(to!S("\uFF28ello"), to!T("llo")) == "\uFF28e");
            assert(chomp(to!S("\uFF28el\uFF4co"), to!T("l\uFF4co")) == "\uFF28e");
        }();
    }
    });

    // Ranges
    import std.array : array;
    import std.utf : byChar, byWchar, byDchar;
    assert(chomp("hello world\r\n" .byChar ).array == "hello world");
    assert(chomp("hello world\r\n"w.byWchar).array == "hello world"w);
    assert(chomp("hello world\r\n"d.byDchar).array == "hello world"d);

    assert(chomp("hello world"d.byDchar, "ld").array == "hello wor"d);

    assert(chomp("hello\u2020" .byChar , "\u2020").array == "hello");
    assert(chomp("hello\u2020"d.byDchar, "\u2020"d).array == "hello"d);
}


/++
    If $(D str) starts with $(D delimiter), then the part of $(D str) following
    $(D delimiter) is returned. If $(D str) does $(I not) start with

    $(D delimiter), then it is returned unchanged.

    Params:
        str = string or $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
        of characters
        delimiter = string of characters to be sliced off front of str[]

    Returns:
        slice of str
 +/
Range chompPrefix(Range, C2)(Range str, const(C2)[] delimiter)
if ((isForwardRange!Range && isSomeChar!(ElementEncodingType!Range) ||
    isNarrowString!Range) &&
    !isConvertibleToString!Range &&
    isSomeChar!C2)
{
    alias C1 = ElementEncodingType!Range;

    static if (is(Unqual!C1 == Unqual!C2) && (isSomeString!Range || (hasSlicing!Range && C2.sizeof == 4)))
    {
        import std.algorithm.searching : startsWith;
        if (str.startsWith(delimiter))
            return str[delimiter.length .. $];
        return str;
    }
    else
    {
        auto orig = str.save;

        static if (isSomeString!Range)
            alias C = dchar;    // because strings auto-decode
        else
            alias C = C1;       // and ranges do not

        foreach (C c; delimiter)
        {
            if (str.empty || str.front != c)
                return orig;

            str.popFront();
        }

        return str;
    }
}

///
@safe pure unittest
{
    assert(chompPrefix("hello world", "he") == "llo world");
    assert(chompPrefix("hello world", "hello w") == "orld");
    assert(chompPrefix("hello world", " world") == "hello world");
    assert(chompPrefix("", "hello") == "");
}

StringTypeOf!Range chompPrefix(Range, C2)(auto ref Range str, const(C2)[] delimiter)
if (isConvertibleToString!Range)
{
    return chompPrefix!(StringTypeOf!Range, C2)(str, delimiter);
}

@safe pure
unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : to;
    import std.exception : assertCTFEable;
    assertCTFEable!(
    {
    foreach (S; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
    {
        foreach (T; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(equal(chompPrefix(to!S("abcdefgh"), to!T("abcde")), "fgh"));
            assert(equal(chompPrefix(to!S("abcde"), to!T("abcdefgh")), "abcde"));
            assert(equal(chompPrefix(to!S("\uFF28el\uFF4co"), to!T("\uFF28el\uFF4co")), ""));
            assert(equal(chompPrefix(to!S("\uFF28el\uFF4co"), to!T("\uFF28el")), "\uFF4co"));
            assert(equal(chompPrefix(to!S("\uFF28el"), to!T("\uFF28el\uFF4co")), "\uFF28el"));
        }();
    }
    });

    // Ranges
    import std.array : array;
    import std.utf : byChar, byWchar, byDchar;
    assert(chompPrefix("hello world" .byChar , "hello"d).array == " world");
    assert(chompPrefix("hello world"w.byWchar, "hello" ).array == " world"w);
    assert(chompPrefix("hello world"d.byDchar, "hello"w).array == " world"d);
    assert(chompPrefix("hello world"c.byDchar, "hello"w).array == " world"d);

    assert(chompPrefix("hello world"d.byDchar, "lx").array == "hello world"d);
    assert(chompPrefix("hello world"d.byDchar, "hello world xx").array == "hello world"d);

    assert(chompPrefix("\u2020world" .byChar , "\u2020").array == "world");
    assert(chompPrefix("\u2020world"d.byDchar, "\u2020"d).array == "world"d);
}

@safe pure unittest
{
    assert(testAliasedString!chompPrefix("hello world", "hello"));
}

/++
    Returns $(D str) without its last character, if there is one. If $(D str)
    ends with $(D "\r\n"), then both are removed. If $(D str) is empty, then
    then it is returned unchanged.

    Params:
        str = string (must be valid UTF)
    Returns:
        slice of str
 +/

Range chop(Range)(Range str)
if ((isBidirectionalRange!Range && isSomeChar!(ElementEncodingType!Range) ||
    isNarrowString!Range) &&
    !isConvertibleToString!Range)
{
    if (str.empty)
        return str;

    static if (isSomeString!Range)
    {
        if (str.length >= 2 && str[$ - 1] == '\n' && str[$ - 2] == '\r')
            return str[0 .. $ - 2];
        str.popBack();
        return str;
    }
    else
    {
        alias C = Unqual!(ElementEncodingType!Range);
        C c = str.back;
        str.popBack();
        if (c == '\n')
        {
            if (!str.empty && str.back == '\r')
                str.popBack();
            return str;
        }
        // Pop back a dchar, not just a code unit
        static if (C.sizeof == 1)
        {
            int cnt = 1;
            while ((c & 0xC0) == 0x80)
            {
                if (str.empty)
                    break;
                c = str.back;
                str.popBack();
                if (++cnt > 4)
                    break;
            }
        }
        else static if (C.sizeof == 2)
        {
            if (c >= 0xD800 && c <= 0xDBFF)
            {
                if (!str.empty)
                    str.popBack();
            }
        }
        else static if (C.sizeof == 4)
        {
        }
        else
            static assert(0);
        return str;
    }
}

///
@safe pure unittest
{
    assert(chop("hello world") == "hello worl");
    assert(chop("hello world\n") == "hello world");
    assert(chop("hello world\r") == "hello world");
    assert(chop("hello world\n\r") == "hello world\n");
    assert(chop("hello world\r\n") == "hello world");
    assert(chop("Walter Bright") == "Walter Brigh");
    assert(chop("") == "");
}

StringTypeOf!Range chop(Range)(auto ref Range str)
if (isConvertibleToString!Range)
{
    return chop!(StringTypeOf!Range)(str);
}

@safe pure unittest
{
    assert(testAliasedString!chop("hello world"));
}

@safe pure unittest
{
    import std.array : array;
    import std.utf : byChar, byWchar, byDchar, byCodeUnit, invalidUTFstrings;

    assert(chop("hello world".byChar).array == "hello worl");
    assert(chop("hello world\n"w.byWchar).array == "hello world"w);
    assert(chop("hello world\r"d.byDchar).array == "hello world"d);
    assert(chop("hello world\n\r".byChar).array == "hello world\n");
    assert(chop("hello world\r\n"w.byWchar).array == "hello world"w);
    assert(chop("Walter Bright"d.byDchar).array == "Walter Brigh"d);
    assert(chop("".byChar).array == "");

    assert(chop(`ミツバチと科学者` .byCodeUnit).array == "ミツバチと科学");
    assert(chop(`ミツバチと科学者`w.byCodeUnit).array == "ミツバチと科学"w);
    assert(chop(`ミツバチと科学者`d.byCodeUnit).array == "ミツバチと科学"d);

    auto ca = invalidUTFstrings!char();
    foreach (s; ca)
    {
        foreach (c; chop(s.byCodeUnit))
        {
        }
    }

    auto wa = invalidUTFstrings!wchar();
    foreach (s; wa)
    {
        foreach (c; chop(s.byCodeUnit))
        {
        }
    }
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
    {
        assert(chop(cast(S) null) is null);
        assert(equal(chop(to!S("hello")), "hell"));
        assert(equal(chop(to!S("hello\r\n")), "hello"));
        assert(equal(chop(to!S("hello\n\r")), "hello\n"));
        assert(equal(chop(to!S("Verité")), "Verit"));
        assert(equal(chop(to!S(`さいごの果実`)), "さいごの果"));
        assert(equal(chop(to!S(`ミツバチと科学者`)), "ミツバチと科学"));
    }
    });
}


/++
    Left justify $(D s) in a field $(D width) characters wide. $(D fillChar)
    is the character that will be used to fill up the space in the field that
    $(D s) doesn't fill.

    Params:
        s = string
        width = minimum field width
        fillChar = used to pad end up to $(D width) characters

    Returns:
        GC allocated string

    See_Also:
        $(LREF leftJustifier), which does not allocate
  +/
S leftJustify(S)(S s, size_t width, dchar fillChar = ' ')
if (isSomeString!S)
{
    import std.array : array;
    return leftJustifier(s, width, fillChar).array;
}

///
@safe pure unittest
{
    assert(leftJustify("hello", 7, 'X') == "helloXX");
    assert(leftJustify("hello", 2, 'X') == "hello");
    assert(leftJustify("hello", 9, 'X') == "helloXXXX");
}

/++
    Left justify $(D s) in a field $(D width) characters wide. $(D fillChar)
    is the character that will be used to fill up the space in the field that
    $(D s) doesn't fill.

    Params:
        r = string or range of characters
        width = minimum field width
        fillChar = used to pad end up to $(D width) characters

    Returns:
        a lazy range of the left justified result

    See_Also:
        $(LREF rightJustifier)
  +/

auto leftJustifier(Range)(Range r, size_t width, dchar fillChar = ' ')
if (isInputRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    alias C = Unqual!(ElementEncodingType!Range);

    static if (C.sizeof == 1)
    {
        import std.utf : byDchar, byChar;
        return leftJustifier(r.byDchar, width, fillChar).byChar;
    }
    else static if (C.sizeof == 2)
    {
        import std.utf : byDchar, byWchar;
        return leftJustifier(r.byDchar, width, fillChar).byWchar;
    }
    else static if (C.sizeof == 4)
    {
        static struct Result
        {
          private:
            Range _input;
            size_t _width;
            dchar _fillChar;
            size_t len;

          public:

            @property bool empty()
            {
                return len >= _width && _input.empty;
            }

            @property C front()
            {
                return _input.empty ? _fillChar : _input.front;
            }

            void popFront()
            {
                ++len;
                if (!_input.empty)
                    _input.popFront();
            }

            static if (isForwardRange!Range)
            {
                @property typeof(this) save() return scope
                {
                    auto ret = this;
                    ret._input = _input.save;
                    return ret;
                }
            }
        }

        return Result(r, width, fillChar);
    }
    else
        static assert(0);
}

///
@safe pure @nogc nothrow
unittest
{
    import std.algorithm.comparison : equal;
    import std.utf : byChar;
    assert(leftJustifier("hello", 2).equal("hello".byChar));
    assert(leftJustifier("hello", 7).equal("hello  ".byChar));
    assert(leftJustifier("hello", 7, 'x').equal("helloxx".byChar));
}

auto leftJustifier(Range)(auto ref Range r, size_t width, dchar fillChar = ' ')
if (isConvertibleToString!Range)
{
    return leftJustifier!(StringTypeOf!Range)(r, width, fillChar);
}

@safe pure unittest
{
    auto r = "hello".leftJustifier(8);
    r.popFront();
    auto save = r.save;
    r.popFront();
    assert(r.front == 'l');
    assert(save.front == 'e');
}

@safe pure unittest
{
    assert(testAliasedString!leftJustifier("hello", 2));
}

/++
    Right justify $(D s) in a field $(D width) characters wide. $(D fillChar)
    is the character that will be used to fill up the space in the field that
    $(D s) doesn't fill.

    Params:
        s = string
        width = minimum field width
        fillChar = used to pad end up to $(D width) characters

    Returns:
        GC allocated string

    See_Also:
        $(LREF rightJustifier), which does not allocate
  +/
S rightJustify(S)(S s, size_t width, dchar fillChar = ' ')
if (isSomeString!S)
{
    import std.array : array;
    return rightJustifier(s, width, fillChar).array;
}

///
@safe pure unittest
{
    assert(rightJustify("hello", 7, 'X') == "XXhello");
    assert(rightJustify("hello", 2, 'X') == "hello");
    assert(rightJustify("hello", 9, 'X') == "XXXXhello");
}

/++
    Right justify $(D s) in a field $(D width) characters wide. $(D fillChar)
    is the character that will be used to fill up the space in the field that
    $(D s) doesn't fill.

    Params:
        r = string or $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
        of characters
        width = minimum field width
        fillChar = used to pad end up to $(D width) characters

    Returns:
        a lazy range of the right justified result

    See_Also:
        $(LREF leftJustifier)
  +/

auto rightJustifier(Range)(Range r, size_t width, dchar fillChar = ' ')
if (isForwardRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    alias C = Unqual!(ElementEncodingType!Range);

    static if (C.sizeof == 1)
    {
        import std.utf : byDchar, byChar;
        return rightJustifier(r.byDchar, width, fillChar).byChar;
    }
    else static if (C.sizeof == 2)
    {
        import std.utf : byDchar, byWchar;
        return rightJustifier(r.byDchar, width, fillChar).byWchar;
    }
    else static if (C.sizeof == 4)
    {
        static struct Result
        {
          private:
            Range _input;
            size_t _width;
            alias nfill = _width;       // number of fill characters to prepend
            dchar _fillChar;
            bool inited;

            // Lazy initialization so constructor is trivial and cannot fail
            void initialize()
            {
                // Replace _width with nfill
                // (use alias instead of union because CTFE cannot deal with unions)
                assert(_width);
                static if (hasLength!Range)
                {
                    immutable len = _input.length;
                    nfill = (_width > len) ? _width - len : 0;
                }
                else
                {
                    // Lookahead to see now many fill characters are needed
                    import std.range : take;
                    import std.range.primitives : walkLength;
                    nfill = _width - walkLength(_input.save.take(_width), _width);
                }
                inited = true;
            }

          public:
            this(Range input, size_t width, dchar fillChar) pure nothrow
            {
                _input = input;
                _fillChar = fillChar;
                _width = width;
            }

            @property bool empty()
            {
                return !nfill && _input.empty;
            }

            @property C front()
            {
                if (!nfill)
                    return _input.front;   // fast path
                if (!inited)
                    initialize();
                return nfill ? _fillChar : _input.front;
            }

            void popFront()
            {
                if (!nfill)
                    _input.popFront();  // fast path
                else
                {
                    if (!inited)
                        initialize();
                    if (nfill)
                        --nfill;
                    else
                        _input.popFront();
                }
            }

            @property typeof(this) save()
            {
                auto ret = this;
                ret._input = _input.save;
                return ret;
            }
        }

        return Result(r, width, fillChar);
    }
    else
        static assert(0);
}

///
@safe pure @nogc nothrow
unittest
{
    import std.algorithm.comparison : equal;
    import std.utf : byChar;
    assert(rightJustifier("hello", 2).equal("hello".byChar));
    assert(rightJustifier("hello", 7).equal("  hello".byChar));
    assert(rightJustifier("hello", 7, 'x').equal("xxhello".byChar));
}

auto rightJustifier(Range)(auto ref Range r, size_t width, dchar fillChar = ' ')
if (isConvertibleToString!Range)
{
    return rightJustifier!(StringTypeOf!Range)(r, width, fillChar);
}

@safe pure unittest
{
    assert(testAliasedString!rightJustifier("hello", 2));
}

@safe pure unittest
{
    auto r = "hello"d.rightJustifier(6);
    r.popFront();
    auto save = r.save;
    r.popFront();
    assert(r.front == 'e');
    assert(save.front == 'h');

    auto t = "hello".rightJustifier(7);
    t.popFront();
    assert(t.front == ' ');
    t.popFront();
    assert(t.front == 'h');

    auto u = "hello"d.rightJustifier(5);
    u.popFront();
    u.popFront();
    u.popFront();
}

/++
    Center $(D s) in a field $(D width) characters wide. $(D fillChar)
    is the character that will be used to fill up the space in the field that
    $(D s) doesn't fill.

    Params:
        s = The string to center
        width = Width of the field to center `s` in
        fillChar = The character to use for filling excess space in the field

    Returns:
        The resulting _center-justified string. The returned string is
        GC-allocated. To avoid GC allocation, use $(LREF centerJustifier)
        instead.
  +/
S center(S)(S s, size_t width, dchar fillChar = ' ')
if (isSomeString!S)
{
    import std.array : array;
    return centerJustifier(s, width, fillChar).array;
}

///
@safe pure unittest
{
    assert(center("hello", 7, 'X') == "XhelloX");
    assert(center("hello", 2, 'X') == "hello");
    assert(center("hello", 9, 'X') == "XXhelloXX");
}

@safe pure
unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
    {
        S s = to!S("hello");

        assert(leftJustify(s, 2) == "hello");
        assert(rightJustify(s, 2) == "hello");
        assert(center(s, 2) == "hello");

        assert(leftJustify(s, 7) == "hello  ");
        assert(rightJustify(s, 7) == "  hello");
        assert(center(s, 7) == " hello ");

        assert(leftJustify(s, 8) == "hello   ");
        assert(rightJustify(s, 8) == "   hello");
        assert(center(s, 8) == " hello  ");

        assert(leftJustify(s, 8, '\u0100') == "hello\u0100\u0100\u0100");
        assert(rightJustify(s, 8, '\u0100') == "\u0100\u0100\u0100hello");
        assert(center(s, 8, '\u0100') == "\u0100hello\u0100\u0100");

        assert(leftJustify(s, 8, 'ö') == "helloööö");
        assert(rightJustify(s, 8, 'ö') == "öööhello");
        assert(center(s, 8, 'ö') == "öhelloöö");
    }
    });
}

/++
    Center justify $(D r) in a field $(D width) characters wide. $(D fillChar)
    is the character that will be used to fill up the space in the field that
    $(D r) doesn't fill.

    Params:
        r = string or $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
        of characters
        width = minimum field width
        fillChar = used to pad end up to $(D width) characters

    Returns:
        a lazy range of the center justified result

    See_Also:
        $(LREF leftJustifier)
        $(LREF rightJustifier)
  +/

auto centerJustifier(Range)(Range r, size_t width, dchar fillChar = ' ')
if (isForwardRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    alias C = Unqual!(ElementEncodingType!Range);

    static if (C.sizeof == 1)
    {
        import std.utf : byDchar, byChar;
        return centerJustifier(r.byDchar, width, fillChar).byChar;
    }
    else static if (C.sizeof == 2)
    {
        import std.utf : byDchar, byWchar;
        return centerJustifier(r.byDchar, width, fillChar).byWchar;
    }
    else static if (C.sizeof == 4)
    {
        import std.range : chain, repeat;
        import std.range.primitives : walkLength;

        auto len = walkLength(r.save, width);
        if (len > width)
            len = width;
        const nleft = (width - len) / 2;
        const nright = width - len - nleft;
        return chain(repeat(fillChar, nleft), r, repeat(fillChar, nright));
    }
    else
        static assert(0);
}

///
@safe pure @nogc nothrow
unittest
{
    import std.algorithm.comparison : equal;
    import std.utf : byChar;
    assert(centerJustifier("hello", 2).equal("hello".byChar));
    assert(centerJustifier("hello", 8).equal(" hello  ".byChar));
    assert(centerJustifier("hello", 7, 'x').equal("xhellox".byChar));
}

auto centerJustifier(Range)(auto ref Range r, size_t width, dchar fillChar = ' ')
if (isConvertibleToString!Range)
{
    return centerJustifier!(StringTypeOf!Range)(r, width, fillChar);
}

@safe pure unittest
{
    assert(testAliasedString!centerJustifier("hello", 8));
}

@system unittest
{
    static auto byFwdRange(dstring s)
    {
        static struct FRange
        {
            dstring str;
            this(dstring s) { str = s; }
            @property bool empty() { return str.length == 0; }
            @property dchar front() { return str[0]; }
            void popFront() { str = str[1 .. $]; }
            @property FRange save() { return this; }
        }
        return FRange(s);
    }

    auto r = centerJustifier(byFwdRange("hello"d), 6);
    r.popFront();
    auto save = r.save;
    r.popFront();
    assert(r.front == 'l');
    assert(save.front == 'e');

    auto t = "hello".centerJustifier(7);
    t.popFront();
    assert(t.front == 'h');
    t.popFront();
    assert(t.front == 'e');

    auto u = byFwdRange("hello"d).centerJustifier(6);
    u.popFront();
    u.popFront();
    u.popFront();
    u.popFront();
    u.popFront();
    u.popFront();
}


/++
    Replace each tab character in $(D s) with the number of spaces necessary
    to align the following character at the next tab stop.

    Params:
        s = string
        tabSize = distance between tab stops

    Returns:
        GC allocated string with tabs replaced with spaces
  +/
auto detab(Range)(auto ref Range s, size_t tabSize = 8) pure
if ((isForwardRange!Range && isSomeChar!(ElementEncodingType!Range))
    || __traits(compiles, StringTypeOf!Range))
{
    import std.array : array;
    return detabber(s, tabSize).array;
}

///
@system pure unittest
{
    assert(detab(" \n\tx", 9) == " \n         x");
}

@safe pure unittest
{
    static struct TestStruct
    {
        string s;
        alias s this;
    }

    static struct TestStruct2
    {
        string s;
        alias s this;
        @disable this(this);
    }

    string s = " \n\tx";
    string cmp = " \n         x";
    auto t = TestStruct(s);
    assert(detab(t, 9) == cmp);
    assert(detab(TestStruct(s), 9) == cmp);
    assert(detab(TestStruct(s), 9) == detab(TestStruct(s), 9));
    assert(detab(TestStruct2(s), 9) == detab(TestStruct2(s), 9));
    assert(detab(TestStruct2(s), 9) == cmp);
}

/++
    Replace each tab character in $(D r) with the number of spaces
    necessary to align the following character at the next tab stop.

    Params:
        r = string or $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
        tabSize = distance between tab stops

    Returns:
        lazy forward range with tabs replaced with spaces
  +/
auto detabber(Range)(Range r, size_t tabSize = 8)
if (isForwardRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    import std.uni : lineSep, paraSep, nelSep;
    import std.utf : codeUnitLimit, decodeFront;

    assert(tabSize > 0);

    alias C = Unqual!(ElementEncodingType!(Range));

    static struct Result
    {
    private:
        Range _input;
        size_t _tabSize;
        size_t nspaces;
        int column;
        size_t index;

    public:

        this(Range input, size_t tabSize)
        {
            _input = input;
            _tabSize = tabSize;
        }

        static if (isInfinite!(Range))
        {
            enum bool empty = false;
        }
        else
        {
            @property bool empty()
            {
                return _input.empty && nspaces == 0;
            }
        }

        @property C front()
        {
            if (nspaces)
                return ' ';
            static if (isSomeString!(Range))
                C c = _input[0];
            else
                C c = _input.front;
            if (index)
                return c;
            dchar dc;
            if (c < codeUnitLimit!(immutable(C)[]))
            {
                dc = c;
                index = 1;
            }
            else
            {
                auto r = _input.save;
                dc = decodeFront(r, index);     // lookahead to decode
            }
            switch (dc)
            {
                case '\r':
                case '\n':
                case paraSep:
                case lineSep:
                case nelSep:
                    column = 0;
                    break;

                case '\t':
                    nspaces = _tabSize - (column % _tabSize);
                    column += nspaces;
                    c = ' ';
                    break;

                default:
                    ++column;
                    break;
            }
            return c;
        }

        void popFront()
        {
            if (!index)
                front;
            if (nspaces)
                --nspaces;
            if (!nspaces)
            {
                static if (isSomeString!(Range))
                   _input = _input[1 .. $];
                else
                    _input.popFront();
                --index;
            }
        }

        @property typeof(this) save()
        {
            auto ret = this;
            ret._input = _input.save;
            return ret;
        }
    }

    return Result(r, tabSize);
}

///
@system pure unittest
{
    import std.array : array;

    assert(detabber(" \n\tx", 9).array == " \n         x");
}

auto detabber(Range)(auto ref Range r, size_t tabSize = 8)
if (isConvertibleToString!Range)
{
    return detabber!(StringTypeOf!Range)(r, tabSize);
}

@safe pure unittest
{
    assert(testAliasedString!detabber(  "  ab\t asdf ", 8));
}

@system pure unittest
{
    import std.algorithm.comparison : cmp;
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
    {
        S s = to!S("This \tis\t a fofof\tof list");
        assert(cmp(detab(s), "This    is       a fofof        of list") == 0);

        assert(detab(cast(S) null) is null);
        assert(detab("").empty);
        assert(detab("a") == "a");
        assert(detab("\t") == "        ");
        assert(detab("\t", 3) == "   ");
        assert(detab("\t", 9) == "         ");
        assert(detab(  "  ab\t asdf ") == "  ab     asdf ");
        assert(detab(  "  \U00010000b\tasdf ") == "  \U00010000b    asdf ");
        assert(detab("\r\t", 9) == "\r         ");
        assert(detab("\n\t", 9) == "\n         ");
        assert(detab("\u0085\t", 9) == "\u0085         ");
        assert(detab("\u2028\t", 9) == "\u2028         ");
        assert(detab(" \u2029\t", 9) == " \u2029         ");
    }
    });
}

///
@system pure unittest
{
    import std.array : array;
    import std.utf : byChar, byWchar;

    assert(detabber(" \u2029\t".byChar, 9).array == " \u2029         ");
    auto r = "hel\tx".byWchar.detabber();
    assert(r.front == 'h');
    auto s = r.save;
    r.popFront();
    r.popFront();
    assert(r.front == 'l');
    assert(s.front == 'h');
}

/++
    Replaces spaces in $(D s) with the optimal number of tabs.
    All spaces and tabs at the end of a line are removed.

    Params:
        s       = String to convert.
        tabSize = Tab columns are $(D tabSize) spaces apart.

    Returns:
        GC allocated string with spaces replaced with tabs;
        use $(LREF entabber) to not allocate.

    See_Also:
        $(LREF entabber)
 +/
auto entab(Range)(Range s, size_t tabSize = 8)
if (isForwardRange!Range && isSomeChar!(ElementEncodingType!Range))
{
    import std.array : array;
    return entabber(s, tabSize).array;
}

///
@safe pure unittest
{
    assert(entab("        x \n") == "\tx\n");
}

auto entab(Range)(auto ref Range s, size_t tabSize = 8)
if (!(isForwardRange!Range && isSomeChar!(ElementEncodingType!Range)) &&
    is(StringTypeOf!Range))
{
    return entab!(StringTypeOf!Range)(s, tabSize);
}

@safe pure unittest
{
    assert(testAliasedString!entab("        x \n"));
}

/++
    Replaces spaces in range $(D r) with the optimal number of tabs.
    All spaces and tabs at the end of a line are removed.

    Params:
        r = string or $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
        tabSize = distance between tab stops

    Returns:
        lazy forward range with spaces replaced with tabs

    See_Also:
        $(LREF entab)
  +/
auto entabber(Range)(Range r, size_t tabSize = 8)
if (isForwardRange!Range && !isConvertibleToString!Range)
{
    import std.uni : lineSep, paraSep, nelSep;
    import std.utf : codeUnitLimit, decodeFront;

    assert(tabSize > 0);
    alias C = Unqual!(ElementEncodingType!Range);

    static struct Result
    {
    private:
        Range _input;
        size_t _tabSize;
        size_t nspaces;
        size_t ntabs;
        int column;
        size_t index;

        @property C getFront()
        {
            static if (isSomeString!Range)
                return _input[0];       // avoid autodecode
            else
                return _input.front;
        }

    public:

        this(Range input, size_t tabSize)
        {
            _input = input;
            _tabSize = tabSize;
        }

        @property bool empty()
        {
            if (ntabs || nspaces)
                return false;

            /* Since trailing spaces are removed,
             * look ahead for anything that is not a trailing space
             */
            static if (isSomeString!Range)
            {
                foreach (c; _input)
                {
                    if (c != ' ' && c != '\t')
                        return false;
                }
                return true;
            }
            else
            {
                if (_input.empty)
                    return true;
                immutable c = _input.front;
                if (c != ' ' && c != '\t')
                    return false;
                auto t = _input.save;
                t.popFront();
                foreach (c2; t)
                {
                    if (c2 != ' ' && c2 != '\t')
                        return false;
                }
                return true;
            }
        }

        @property C front()
        {
            //writefln("   front(): ntabs = %s nspaces = %s index = %s front = '%s'", ntabs, nspaces, index, getFront);
            if (ntabs)
                return '\t';
            if (nspaces)
                return ' ';
            C c = getFront;
            if (index)
                return c;
            dchar dc;
            if (c < codeUnitLimit!(immutable(C)[]))
            {
                index = 1;
                dc = c;
                if (c == ' ' || c == '\t')
                {
                    // Consume input until a non-blank is encountered
                    immutable startcol = column;
                    C cx;
                    static if (isSomeString!Range)
                    {
                        while (1)
                        {
                            assert(_input.length);
                            cx = _input[0];
                            if (cx == ' ')
                                ++column;
                            else if (cx == '\t')
                                column += _tabSize - (column % _tabSize);
                            else
                                break;
                            _input = _input[1 .. $];
                        }
                    }
                    else
                    {
                        while (1)
                        {
                            assert(!_input.empty);
                            cx = _input.front;
                            if (cx == ' ')
                                ++column;
                            else if (cx == '\t')
                                column += _tabSize - (column % _tabSize);
                            else
                                break;
                            _input.popFront();
                        }
                    }
                    // Compute ntabs+nspaces to get from startcol to column
                    immutable n = column - startcol;
                    if (n == 1)
                    {
                        nspaces = 1;
                    }
                    else
                    {
                        ntabs = column / _tabSize - startcol / _tabSize;
                        if (ntabs == 0)
                            nspaces = column - startcol;
                        else
                            nspaces = column % _tabSize;
                    }
                    //writefln("\tstartcol = %s, column = %s, _tabSize = %s", startcol, column, _tabSize);
                    //writefln("\tntabs = %s, nspaces = %s", ntabs, nspaces);
                    if (cx < codeUnitLimit!(immutable(C)[]))
                    {
                        dc = cx;
                        index = 1;
                    }
                    else
                    {
                        auto r = _input.save;
                        dc = decodeFront(r, index);     // lookahead to decode
                    }
                    switch (dc)
                    {
                        case '\r':
                        case '\n':
                        case paraSep:
                        case lineSep:
                        case nelSep:
                            column = 0;
                            // Spaces followed by newline are ignored
                            ntabs = 0;
                            nspaces = 0;
                            return cx;

                        default:
                            ++column;
                            break;
                    }
                    return ntabs ? '\t' : ' ';
                }
            }
            else
            {
                auto r = _input.save;
                dc = decodeFront(r, index);     // lookahead to decode
            }
            //writefln("dc = x%x", dc);
            switch (dc)
            {
                case '\r':
                case '\n':
                case paraSep:
                case lineSep:
                case nelSep:
                    column = 0;
                    break;

                default:
                    ++column;
                    break;
            }
            return c;
        }

        void popFront()
        {
            //writefln("popFront(): ntabs = %s nspaces = %s index = %s front = '%s'", ntabs, nspaces, index, getFront);
            if (!index)
                front;
            if (ntabs)
                --ntabs;
            else if (nspaces)
                --nspaces;
            else if (!ntabs && !nspaces)
            {
                static if (isSomeString!Range)
                   _input = _input[1 .. $];
                else
                    _input.popFront();
                --index;
            }
        }

        @property typeof(this) save()
        {
            auto ret = this;
            ret._input = _input.save;
            return ret;
        }
    }

    return Result(r, tabSize);
}

///
@safe pure unittest
{
    import std.array : array;
    assert(entabber("        x \n").array == "\tx\n");
}

auto entabber(Range)(auto ref Range r, size_t tabSize = 8)
if (isConvertibleToString!Range)
{
    return entabber!(StringTypeOf!Range)(r, tabSize);
}

@safe pure unittest
{
    assert(testAliasedString!entabber("  ab    asdf ", 8));
}

@safe pure
unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    assert(entab(cast(string) null) is null);
    assert(entab("").empty);
    assert(entab("a") == "a");
    assert(entab("        ") == "");
    assert(entab("        x") == "\tx");
    assert(entab("  ab    asdf ") == "  ab\tasdf");
    assert(entab("  ab     asdf ") == "  ab\t asdf");
    assert(entab("  ab \t   asdf ") == "  ab\t   asdf");
    assert(entab("1234567 \ta") == "1234567\t\ta");
    assert(entab("1234567  \ta") == "1234567\t\ta");
    assert(entab("1234567   \ta") == "1234567\t\ta");
    assert(entab("1234567    \ta") == "1234567\t\ta");
    assert(entab("1234567     \ta") == "1234567\t\ta");
    assert(entab("1234567      \ta") == "1234567\t\ta");
    assert(entab("1234567       \ta") == "1234567\t\ta");
    assert(entab("1234567        \ta") == "1234567\t\ta");
    assert(entab("1234567         \ta") == "1234567\t\t\ta");

    assert(entab("a               ") == "a");
    assert(entab("a\v") == "a\v");
    assert(entab("a\f") == "a\f");
    assert(entab("a\n") == "a\n");
    assert(entab("a\n\r") == "a\n\r");
    assert(entab("a\r\n") == "a\r\n");
    assert(entab("a\u2028") == "a\u2028");
    assert(entab("a\u2029") == "a\u2029");
    assert(entab("a\u0085") == "a\u0085");
    assert(entab("a  ") == "a");
    assert(entab("a\t") == "a");
    assert(entab("\uFF28\uFF45\uFF4C\uFF4C567      \t\uFF4F \t") ==
                 "\uFF28\uFF45\uFF4C\uFF4C567\t\t\uFF4F");
    assert(entab(" \naa") == "\naa");
    assert(entab(" \r aa") == "\r aa");
    assert(entab(" \u2028 aa") == "\u2028 aa");
    assert(entab(" \u2029 aa") == "\u2029 aa");
    assert(entab(" \u0085 aa") == "\u0085 aa");
    });
}

@safe pure
unittest
{
    import std.array : array;
    import std.utf : byChar;
    assert(entabber(" \u0085 aa".byChar).array == "\u0085 aa");
    assert(entabber(" \u2028\t aa \t".byChar).array == "\u2028\t aa");

    auto r = entabber("1234", 4);
    r.popFront();
    auto rsave = r.save;
    r.popFront();
    assert(r.front == '3');
    assert(rsave.front == '2');
}


/++
    Replaces the characters in $(D str) which are keys in $(D transTable) with
    their corresponding values in $(D transTable). $(D transTable) is an AA
    where its keys are $(D dchar) and its values are either $(D dchar) or some
    type of string. Also, if $(D toRemove) is given, the characters in it are
    removed from $(D str) prior to translation. $(D str) itself is unaltered.
    A copy with the changes is returned.

    See_Also:
        $(LREF tr)
        $(REF replace, std,array)

    Params:
        str        = The original string.
        transTable = The AA indicating which characters to replace and what to
                     replace them with.
        toRemove   = The characters to remove from the string.
  +/
C1[] translate(C1, C2 = immutable char)(C1[] str,
                                        in dchar[dchar] transTable,
                                        const(C2)[] toRemove = null) @safe pure
if (isSomeChar!C1 && isSomeChar!C2)
{
    import std.array : appender;
    auto buffer = appender!(C1[])();
    translateImpl(str, transTable, toRemove, buffer);
    return buffer.data;
}

///
@safe pure unittest
{
    dchar[dchar] transTable1 = ['e' : '5', 'o' : '7', '5': 'q'];
    assert(translate("hello world", transTable1) == "h5ll7 w7rld");

    assert(translate("hello world", transTable1, "low") == "h5 rd");

    string[dchar] transTable2 = ['e' : "5", 'o' : "orange"];
    assert(translate("hello world", transTable2) == "h5llorange worangerld");
}

@safe pure unittest // issue 13018
{
    immutable dchar[dchar] transTable1 = ['e' : '5', 'o' : '7', '5': 'q'];
    assert(translate("hello world", transTable1) == "h5ll7 w7rld");

    assert(translate("hello world", transTable1, "low") == "h5 rd");

    immutable string[dchar] transTable2 = ['e' : "5", 'o' : "orange"];
    assert(translate("hello world", transTable2) == "h5llorange worangerld");
}

@system pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!( char[], const( char)[], immutable( char)[],
                          wchar[], const(wchar)[], immutable(wchar)[],
                          dchar[], const(dchar)[], immutable(dchar)[]))
    {
        assert(translate(to!S("hello world"), cast(dchar[dchar])['h' : 'q', 'l' : '5']) ==
               to!S("qe55o wor5d"));
        assert(translate(to!S("hello world"), cast(dchar[dchar])['o' : 'l', 'l' : '\U00010143']) ==
               to!S("he\U00010143\U00010143l wlr\U00010143d"));
        assert(translate(to!S("hello \U00010143 world"), cast(dchar[dchar])['h' : 'q', 'l': '5']) ==
               to!S("qe55o \U00010143 wor5d"));
        assert(translate(to!S("hello \U00010143 world"), cast(dchar[dchar])['o' : '0', '\U00010143' : 'o']) ==
               to!S("hell0 o w0rld"));
        assert(translate(to!S("hello world"), cast(dchar[dchar]) null) == to!S("hello world"));

        foreach (T; AliasSeq!( char[], const( char)[], immutable( char)[],
                              wchar[], const(wchar)[], immutable(wchar)[],
                              dchar[], const(dchar)[], immutable(dchar)[]))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            foreach (R; AliasSeq!(dchar[dchar], const dchar[dchar],
                        immutable dchar[dchar]))
            {
                R tt = ['h' : 'q', 'l' : '5'];
                assert(translate(to!S("hello world"), tt, to!T("r"))
                    == to!S("qe55o wo5d"));
                assert(translate(to!S("hello world"), tt, to!T("helo"))
                    == to!S(" wrd"));
                assert(translate(to!S("hello world"), tt, to!T("q5"))
                    == to!S("qe55o wor5d"));
            }
        }();

        auto s = to!S("hello world");
        dchar[dchar] transTable = ['h' : 'q', 'l' : '5'];
        static assert(is(typeof(s) == typeof(translate(s, transTable))));
    }
    });
}

/++ Ditto +/
C1[] translate(C1, S, C2 = immutable char)(C1[] str,
                                           in S[dchar] transTable,
                                           const(C2)[] toRemove = null) @safe pure
if (isSomeChar!C1 && isSomeString!S && isSomeChar!C2)
{
    import std.array : appender;
    auto buffer = appender!(C1[])();
    translateImpl(str, transTable, toRemove, buffer);
    return buffer.data;
}

@system pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!( char[], const( char)[], immutable( char)[],
                          wchar[], const(wchar)[], immutable(wchar)[],
                          dchar[], const(dchar)[], immutable(dchar)[]))
    {
        assert(translate(to!S("hello world"), ['h' : "yellow", 'l' : "42"]) ==
               to!S("yellowe4242o wor42d"));
        assert(translate(to!S("hello world"), ['o' : "owl", 'l' : "\U00010143\U00010143"]) ==
               to!S("he\U00010143\U00010143\U00010143\U00010143owl wowlr\U00010143\U00010143d"));
        assert(translate(to!S("hello \U00010143 world"), ['h' : "yellow", 'l' : "42"]) ==
               to!S("yellowe4242o \U00010143 wor42d"));
        assert(translate(to!S("hello \U00010143 world"), ['o' : "owl", 'l' : "\U00010143\U00010143"]) ==
               to!S("he\U00010143\U00010143\U00010143\U00010143owl \U00010143 wowlr\U00010143\U00010143d"));
        assert(translate(to!S("hello \U00010143 world"), ['h' : ""]) ==
               to!S("ello \U00010143 world"));
        assert(translate(to!S("hello \U00010143 world"), ['\U00010143' : ""]) ==
               to!S("hello  world"));
        assert(translate(to!S("hello world"), cast(string[dchar]) null) == to!S("hello world"));

        foreach (T; AliasSeq!( char[], const( char)[], immutable( char)[],
                              wchar[], const(wchar)[], immutable(wchar)[],
                              dchar[], const(dchar)[], immutable(dchar)[]))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396

            foreach (R; AliasSeq!(string[dchar], const string[dchar],
                        immutable string[dchar]))
            {
                R tt = ['h' : "yellow", 'l' : "42"];
                assert(translate(to!S("hello world"), tt, to!T("r")) ==
                       to!S("yellowe4242o wo42d"));
                assert(translate(to!S("hello world"), tt, to!T("helo")) ==
                       to!S(" wrd"));
                assert(translate(to!S("hello world"), tt, to!T("y42")) ==
                       to!S("yellowe4242o wor42d"));
                assert(translate(to!S("hello world"), tt, to!T("hello world")) ==
                       to!S(""));
                assert(translate(to!S("hello world"), tt, to!T("42")) ==
                       to!S("yellowe4242o wor42d"));
            }
        }();

        auto s = to!S("hello world");
        string[dchar] transTable = ['h' : "silly", 'l' : "putty"];
        static assert(is(typeof(s) == typeof(translate(s, transTable))));
    }
    });
}

/++
    This is an overload of $(D translate) which takes an existing buffer to write the contents to.

    Params:
        str        = The original string.
        transTable = The AA indicating which characters to replace and what to
                     replace them with.
        toRemove   = The characters to remove from the string.
        buffer     = An output range to write the contents to.
  +/
void translate(C1, C2 = immutable char, Buffer)(C1[] str,
                                        in dchar[dchar] transTable,
                                        const(C2)[] toRemove,
                                        Buffer buffer)
if (isSomeChar!C1 && isSomeChar!C2 && isOutputRange!(Buffer, C1))
{
    translateImpl(str, transTable, toRemove, buffer);
}

///
@safe pure unittest
{
    import std.array : appender;
    dchar[dchar] transTable1 = ['e' : '5', 'o' : '7', '5': 'q'];
    auto buffer = appender!(dchar[])();
    translate("hello world", transTable1, null, buffer);
    assert(buffer.data == "h5ll7 w7rld");

    buffer.clear();
    translate("hello world", transTable1, "low", buffer);
    assert(buffer.data == "h5 rd");

    buffer.clear();
    string[dchar] transTable2 = ['e' : "5", 'o' : "orange"];
    translate("hello world", transTable2, null, buffer);
    assert(buffer.data == "h5llorange worangerld");
}

@safe pure unittest // issue 13018
{
    import std.array : appender;
    immutable dchar[dchar] transTable1 = ['e' : '5', 'o' : '7', '5': 'q'];
    auto buffer = appender!(dchar[])();
    translate("hello world", transTable1, null, buffer);
    assert(buffer.data == "h5ll7 w7rld");

    buffer.clear();
    translate("hello world", transTable1, "low", buffer);
    assert(buffer.data == "h5 rd");

    buffer.clear();
    immutable string[dchar] transTable2 = ['e' : "5", 'o' : "orange"];
    translate("hello world", transTable2, null, buffer);
    assert(buffer.data == "h5llorange worangerld");
}

/++ Ditto +/
void translate(C1, S, C2 = immutable char, Buffer)(C1[] str,
                                                   in S[dchar] transTable,
                                                   const(C2)[] toRemove,
                                                   Buffer buffer)
if (isSomeChar!C1 && isSomeString!S && isSomeChar!C2 && isOutputRange!(Buffer, S))
{
    translateImpl(str, transTable, toRemove, buffer);
}

private void translateImpl(C1, T, C2, Buffer)(C1[] str,
                                      T transTable,
                                      const(C2)[] toRemove,
                                      Buffer buffer)
{
    bool[dchar] removeTable;

    foreach (dchar c; toRemove)
        removeTable[c] = true;

    foreach (dchar c; str)
    {
        if (c in removeTable)
            continue;

        auto newC = c in transTable;

        if (newC)
            put(buffer, *newC);
        else
            put(buffer, c);
    }
}

/++
    This is an $(I $(RED ASCII-only)) overload of $(LREF _translate). It
    will $(I not) work with Unicode. It exists as an optimization for the
    cases where Unicode processing is not necessary.

    Unlike the other overloads of $(LREF _translate), this one does not take
    an AA. Rather, it takes a $(D string) generated by $(LREF makeTransTable).

    The array generated by $(D makeTransTable) is $(D 256) elements long such that
    the index is equal to the ASCII character being replaced and the value is
    equal to the character that it's being replaced with. Note that translate
    does not decode any of the characters, so you can actually pass it Extended
    ASCII characters if you want to (ASCII only actually uses $(D 128)
    characters), but be warned that Extended ASCII characters are not valid
    Unicode and therefore will result in a $(D UTFException) being thrown from
    most other Phobos functions.

    Also, because no decoding occurs, it is possible to use this overload to
    translate ASCII characters within a proper UTF-8 string without altering the
    other, non-ASCII characters. It's replacing any code unit greater than
    $(D 127) with another code unit or replacing any code unit with another code
    unit greater than $(D 127) which will cause UTF validation issues.

    See_Also:
        $(LREF tr)
        $(REF replace, std,array)

    Params:
        str        = The original string.
        transTable = The string indicating which characters to replace and what
                     to replace them with. It is generated by $(LREF makeTransTable).
        toRemove   = The characters to remove from the string.
  +/
C[] translate(C = immutable char)(in char[] str, in char[] transTable, in char[] toRemove = null) @trusted pure nothrow
if (is(Unqual!C == char))
in
{
    assert(transTable.length == 256);
}
body
{
    bool[256] remTable = false;

    foreach (char c; toRemove)
        remTable[c] = true;

    size_t count = 0;
    foreach (char c; str)
    {
        if (!remTable[c])
            ++count;
    }

    auto buffer = new char[count];

    size_t i = 0;
    foreach (char c; str)
    {
        if (!remTable[c])
            buffer[i++] = transTable[c];
    }

    return cast(C[])(buffer);
}


/**
 * Do same thing as $(LREF makeTransTable) but allocate the translation table
 * on the GC heap.
 *
 * Use $(LREF makeTransTable) instead.
 */
string makeTrans(in char[] from, in char[] to) @trusted pure nothrow
{
    return makeTransTable(from, to)[].idup;
}

///
@safe pure nothrow unittest
{
    auto transTable1 = makeTrans("eo5", "57q");
    assert(translate("hello world", transTable1) == "h5ll7 w7rld");

    assert(translate("hello world", transTable1, "low") == "h5 rd");
}

/*******
 * Construct 256 character translation table, where characters in from[] are replaced
 * by corresponding characters in to[].
 *
 * Params:
 *      from = array of chars, less than or equal to 256 in length
 *      to = corresponding array of chars to translate to
 * Returns:
 *      translation array
 */

char[256] makeTransTable(in char[] from, in char[] to) @safe pure nothrow @nogc
in
{
    import std.ascii : isASCII;
    assert(from.length == to.length);
    assert(from.length <= 256);
    foreach (char c; from)
        assert(isASCII(c));
    foreach (char c; to)
        assert(isASCII(c));
}
body
{
    char[256] result = void;

    foreach (i; 0 .. result.length)
        result[i] = cast(char) i;
    foreach (i, c; from)
        result[c] = to[i];
    return result;
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    foreach (C; AliasSeq!(char, const char, immutable char))
    {
        assert(translate!C("hello world", makeTransTable("hl", "q5")) == to!(C[])("qe55o wor5d"));

        auto s = to!(C[])("hello world");
        auto transTable = makeTransTable("hl", "q5");
        static assert(is(typeof(s) == typeof(translate!C(s, transTable))));
    }

    foreach (S; AliasSeq!(char[], const(char)[], immutable(char)[]))
    {
        assert(translate(to!S("hello world"), makeTransTable("hl", "q5")) == to!S("qe55o wor5d"));
        assert(translate(to!S("hello \U00010143 world"), makeTransTable("hl", "q5")) ==
               to!S("qe55o \U00010143 wor5d"));
        assert(translate(to!S("hello world"), makeTransTable("ol", "1o")) == to!S("heoo1 w1rod"));
        assert(translate(to!S("hello world"), makeTransTable("", "")) == to!S("hello world"));
        assert(translate(to!S("hello world"), makeTransTable("12345", "67890")) == to!S("hello world"));
        assert(translate(to!S("hello \U00010143 world"), makeTransTable("12345", "67890")) ==
               to!S("hello \U00010143 world"));

        foreach (T; AliasSeq!(char[], const(char)[], immutable(char)[]))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            assert(translate(to!S("hello world"), makeTransTable("hl", "q5"), to!T("r")) ==
                   to!S("qe55o wo5d"));
            assert(translate(to!S("hello \U00010143 world"), makeTransTable("hl", "q5"), to!T("r")) ==
                   to!S("qe55o \U00010143 wo5d"));
            assert(translate(to!S("hello world"), makeTransTable("hl", "q5"), to!T("helo")) ==
                   to!S(" wrd"));
            assert(translate(to!S("hello world"), makeTransTable("hl", "q5"), to!T("q5")) ==
                   to!S("qe55o wor5d"));
        }();
    }
    });
}

/++
    This is an $(I $(RED ASCII-only)) overload of $(D translate) which takes an existing buffer to write the contents to.

    Params:
        str        = The original string.
        transTable = The string indicating which characters to replace and what
                     to replace them with. It is generated by $(LREF makeTransTable).
        toRemove   = The characters to remove from the string.
        buffer     = An output range to write the contents to.
  +/
void translate(C = immutable char, Buffer)(in char[] str, in char[] transTable,
        in char[] toRemove, Buffer buffer) @trusted pure
if (is(Unqual!C == char) && isOutputRange!(Buffer, char))
in
{
    assert(transTable.length == 256);
}
body
{
    bool[256] remTable = false;

    foreach (char c; toRemove)
        remTable[c] = true;

    foreach (char c; str)
    {
        if (!remTable[c])
            put(buffer, transTable[c]);
    }
}

///
@safe pure unittest
{
    import std.array : appender;
    auto buffer = appender!(char[])();
    auto transTable1 = makeTransTable("eo5", "57q");
    translate("hello world", transTable1, null, buffer);
    assert(buffer.data == "h5ll7 w7rld");

    buffer.clear();
    translate("hello world", transTable1, "low", buffer);
    assert(buffer.data == "h5 rd");
}

//@@@DEPRECATED_2.086@@@
deprecated("This function is obsolete. It is available in https://github.com/dlang/undeaD if necessary.")
bool inPattern(S)(dchar c, in S pattern) @safe pure @nogc
if (isSomeString!S)
{
    bool result = false;
    int range = 0;
    dchar lastc;

    foreach (size_t i, dchar p; pattern)
    {
        if (p == '^' && i == 0)
        {
            result = true;
            if (i + 1 == pattern.length)
                return (c == p);    // or should this be an error?
        }
        else if (range)
        {
            range = 0;
            if (lastc <= c && c <= p || c == p)
                return !result;
        }
        else if (p == '-' && i > result && i + 1 < pattern.length)
        {
            range = 1;
            continue;
        }
        else if (c == p)
            return !result;
        lastc = p;
    }
    return result;
}


deprecated
@safe pure @nogc unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    assert(inPattern('x', "x") == 1);
    assert(inPattern('x', "y") == 0);
    assert(inPattern('x', string.init) == 0);
    assert(inPattern('x', "^y") == 1);
    assert(inPattern('x', "yxxy") == 1);
    assert(inPattern('x', "^yxxy") == 0);
    assert(inPattern('x', "^abcd") == 1);
    assert(inPattern('^', "^^") == 0);
    assert(inPattern('^', "^") == 1);
    assert(inPattern('^', "a^") == 1);
    assert(inPattern('x', "a-z") == 1);
    assert(inPattern('x', "A-Z") == 0);
    assert(inPattern('x', "^a-z") == 0);
    assert(inPattern('x', "^A-Z") == 1);
    assert(inPattern('-', "a-") == 1);
    assert(inPattern('-', "^A-") == 0);
    assert(inPattern('a', "z-a") == 1);
    assert(inPattern('z', "z-a") == 1);
    assert(inPattern('x', "z-a") == 0);
    });
}

//@@@DEPRECATED_2.086@@@
deprecated("This function is obsolete. It is available in https://github.com/dlang/undeaD if necessary.")
bool inPattern(S)(dchar c, S[] patterns) @safe pure @nogc
if (isSomeString!S)
{
    foreach (string pattern; patterns)
    {
        if (!inPattern(c, pattern))
        {
            return false;
        }
    }
    return true;
}

//@@@DEPRECATED_2.086@@@
deprecated("This function is obsolete. It is available in https://github.com/dlang/undeaD if necessary.")
size_t countchars(S, S1)(S s, in S1 pattern) @safe pure @nogc
if (isSomeString!S && isSomeString!S1)
{
    size_t count;
    foreach (dchar c; s)
    {
        count += inPattern(c, pattern);
    }
    return count;
}

deprecated
@safe pure @nogc unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    assert(countchars("abc", "a-c") == 3);
    assert(countchars("hello world", "or") == 3);
    });
}

//@@@DEPRECATED_2.086@@@
deprecated("This function is obsolete. It is available in https://github.com/dlang/undeaD if necessary.")
S removechars(S)(S s, in S pattern) @safe pure
if (isSomeString!S)
{
    import std.utf : encode;

    Unqual!(typeof(s[0]))[] r;
    bool changed = false;

    foreach (size_t i, dchar c; s)
    {
        if (inPattern(c, pattern))
        {
            if (!changed)
            {
                changed = true;
                r = s[0 .. i].dup;
            }
            continue;
        }
        if (changed)
        {
            encode(r, c);
        }
    }
    if (changed)
        return r;
    else
        return s;
}

deprecated
@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    assert(removechars("abc", "a-c").length == 0);
    assert(removechars("hello world", "or") == "hell wld");
    assert(removechars("hello world", "d") == "hello worl");
    assert(removechars("hah", "h") == "a");
    });
}

deprecated
@safe pure unittest
{
    assert(removechars("abc", "x") == "abc");
}

//@@@DEPRECATED_2.086@@@
deprecated("This function is obsolete. It is available in https://github.com/dlang/undeaD if necessary.")
S squeeze(S)(S s, in S pattern = null)
{
    import std.utf : encode, stride;

    Unqual!(typeof(s[0]))[] r;
    dchar lastc;
    size_t lasti;
    int run;
    bool changed;

    foreach (size_t i, dchar c; s)
    {
        if (run && lastc == c)
        {
            changed = true;
        }
        else if (pattern is null || inPattern(c, pattern))
        {
            run = 1;
            if (changed)
            {
                if (r is null)
                    r = s[0 .. lasti].dup;
                encode(r, c);
            }
            else
                lasti = i + stride(s, i);
            lastc = c;
        }
        else
        {
            run = 0;
            if (changed)
            {
                if (r is null)
                    r = s[0 .. lasti].dup;
                encode(r, c);
            }
        }
    }
    return changed ? ((r is null) ? s[0 .. lasti] : cast(S) r) : s;
}

deprecated
@system pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    string s;

    assert(squeeze("hello") == "helo");

    s = "abcd";
    assert(squeeze(s) is s);
    s = "xyzz";
    assert(squeeze(s).ptr == s.ptr); // should just be a slice

    assert(squeeze("hello goodbyee", "oe") == "hello godbye");
    });
}

//@@@DEPRECATED_2.086@@@
deprecated("This function is obsolete. It is available in https://github.com/dlang/undeaD if necessary.")
S1 munch(S1, S2)(ref S1 s, S2 pattern) @safe pure @nogc
{
    size_t j = s.length;
    foreach (i, dchar c; s)
    {
        if (!inPattern(c, pattern))
        {
            j = i;
            break;
        }
    }
    scope(exit) s = s[j .. $];
    return s[0 .. j];
}

///
deprecated
@safe pure @nogc unittest
{
    string s = "123abc";
    string t = munch(s, "0123456789");
    assert(t == "123" && s == "abc");
    t = munch(s, "0123456789");
    assert(t == "" && s == "abc");
}

deprecated
@safe pure @nogc unittest
{
    string s = "123€abc";
    string t = munch(s, "0123456789");
    assert(t == "123" && s == "€abc");
    t = munch(s, "0123456789");
    assert(t == "" && s == "€abc");
    t = munch(s, "£$€¥");
    assert(t == "€" && s == "abc");
}


/**********************************************
 * Return string that is the 'successor' to s[].
 * If the rightmost character is a-zA-Z0-9, it is incremented within
 * its case or digits. If it generates a carry, the process is
 * repeated with the one to its immediate left.
 */

S succ(S)(S s) @safe pure
if (isSomeString!S)
{
    import std.ascii : isAlphaNum;

    if (s.length && isAlphaNum(s[$ - 1]))
    {
        auto r = s.dup;
        size_t i = r.length - 1;

        while (1)
        {
            dchar c = s[i];
            dchar carry;

            switch (c)
            {
            case '9':
                c = '0';
                carry = '1';
                goto Lcarry;
            case 'z':
            case 'Z':
                c -= 'Z' - 'A';
                carry = c;
            Lcarry:
                r[i] = cast(char) c;
                if (i == 0)
                {
                    auto t = new typeof(r[0])[r.length + 1];
                    t[0] = cast(char) carry;
                    t[1 .. $] = r[];
                    return t;
                }
                i--;
                break;

            default:
                if (isAlphaNum(c))
                    r[i]++;
                return r;
            }
        }
    }
    return s;
}

///
@safe pure unittest
{
    assert(succ("1") == "2");
    assert(succ("9") == "10");
    assert(succ("999") == "1000");
    assert(succ("zz99") == "aaa00");
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    assert(succ(string.init) is null);
    assert(succ("!@#$%") == "!@#$%");
    assert(succ("1") == "2");
    assert(succ("9") == "10");
    assert(succ("999") == "1000");
    assert(succ("zz99") == "aaa00");
    });
}


/++
    Replaces the characters in $(D str) which are in $(D from) with the
    the corresponding characters in $(D to) and returns the resulting string.

    $(D tr) is based on
    $(HTTP pubs.opengroup.org/onlinepubs/9699919799/utilities/_tr.html, Posix's tr),
    though it doesn't do everything that the Posix utility does.

    Params:
        str       = The original string.
        from      = The characters to replace.
        to        = The characters to replace with.
        modifiers = String containing modifiers.

    Modifiers:
        $(BOOKTABLE,
        $(TR $(TD Modifier) $(TD Description))
        $(TR $(TD $(D 'c')) $(TD Complement the list of characters in $(D from)))
        $(TR $(TD $(D 'd')) $(TD Removes matching characters with no corresponding
                              replacement in $(D to)))
        $(TR $(TD $(D 's')) $(TD Removes adjacent duplicates in the replaced
                              characters))
        )

    If the modifier $(D 'd') is present, then the number of characters in
    $(D to) may be only $(D 0) or $(D 1).

    If the modifier $(D 'd') is $(I not) present, and $(D to) is empty, then
    $(D to) is taken to be the same as $(D from).

    If the modifier $(D 'd') is $(I not) present, and $(D to) is shorter than
    $(D from), then $(D to) is extended by replicating the last character in
    $(D to).

    Both $(D from) and $(D to) may contain ranges using the $(D '-') character
    (e.g. $(D "a-d") is synonymous with $(D "abcd").) Neither accept a leading
    $(D '^') as meaning the complement of the string (use the $(D 'c') modifier
    for that).
  +/
C1[] tr(C1, C2, C3, C4 = immutable char)
       (C1[] str, const(C2)[] from, const(C3)[] to, const(C4)[] modifiers = null)
{
    import std.array : appender;
    import std.conv : conv_to = to;
    import std.utf : decode;

    bool mod_c;
    bool mod_d;
    bool mod_s;

    foreach (char c; modifiers)
    {
        switch (c)
        {
        case 'c':   mod_c = 1; break;   // complement
        case 'd':   mod_d = 1; break;   // delete unreplaced chars
        case 's':   mod_s = 1; break;   // squeeze duplicated replaced chars
        default:    assert(0);
        }
    }

    if (to.empty && !mod_d)
        to = conv_to!(typeof(to))(from);

    auto result = appender!(C1[])();
    bool modified;
    dchar lastc;

    foreach (dchar c; str)
    {
        dchar lastf;
        dchar lastt;
        dchar newc;
        int n = 0;

        for (size_t i = 0; i < from.length; )
        {
            immutable f = decode(from, i);
            if (f == '-' && lastf != dchar.init && i < from.length)
            {
                immutable nextf = decode(from, i);
                if (lastf <= c && c <= nextf)
                {
                    n += c - lastf - 1;
                    if (mod_c)
                        goto Lnotfound;
                    goto Lfound;
                }
                n += nextf - lastf;
                lastf = lastf.init;
                continue;
            }

            if (c == f)
            {   if (mod_c)
                    goto Lnotfound;
                goto Lfound;
            }
            lastf = f;
            n++;
        }
        if (!mod_c)
            goto Lnotfound;
        n = 0;          // consider it 'found' at position 0

      Lfound:

        // Find the nth character in to[]
        dchar nextt;
        for (size_t i = 0; i < to.length; )
        {
            immutable t = decode(to, i);
            if (t == '-' && lastt != dchar.init && i < to.length)
            {
                nextt = decode(to, i);
                n -= nextt - lastt;
                if (n < 0)
                {
                    newc = nextt + n + 1;
                    goto Lnewc;
                }
                lastt = dchar.init;
                continue;
            }
            if (n == 0)
            {   newc = t;
                goto Lnewc;
            }
            lastt = t;
            nextt = t;
            n--;
        }
        if (mod_d)
            continue;
        newc = nextt;

      Lnewc:
        if (mod_s && modified && newc == lastc)
            continue;
        result.put(newc);
        assert(newc != dchar.init);
        modified = true;
        lastc = newc;
        continue;

      Lnotfound:
        result.put(c);
        lastc = c;
        modified = false;
    }

    return result.data;
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : to;
    import std.exception : assertCTFEable;

    // Complete list of test types; too slow to test'em all
    // alias TestTypes = AliasSeq!(
    //          char[], const( char)[], immutable( char)[],
    //         wchar[], const(wchar)[], immutable(wchar)[],
    //         dchar[], const(dchar)[], immutable(dchar)[]);

    // Reduced list of test types
    alias TestTypes = AliasSeq!(char[], const(wchar)[], immutable(dchar)[]);

    assertCTFEable!(
    {
    foreach (S; TestTypes)
    {
        foreach (T; TestTypes)
        {
            foreach (U; TestTypes)
            {
                assert(equal(tr(to!S("abcdef"), to!T("cd"), to!U("CD")), "abCDef"));
                assert(equal(tr(to!S("abcdef"), to!T("b-d"), to!U("B-D")), "aBCDef"));
                assert(equal(tr(to!S("abcdefgh"), to!T("b-dh"), to!U("B-Dx")), "aBCDefgx"));
                assert(equal(tr(to!S("abcdefgh"), to!T("b-dh"), to!U("B-CDx")), "aBCDefgx"));
                assert(equal(tr(to!S("abcdefgh"), to!T("b-dh"), to!U("B-BCDx")), "aBCDefgx"));
                assert(equal(tr(to!S("abcdef"), to!T("ef"), to!U("*"), to!S("c")), "****ef"));
                assert(equal(tr(to!S("abcdef"), to!T("ef"), to!U(""), to!T("d")), "abcd"));
                assert(equal(tr(to!S("hello goodbye"), to!T("lo"), to!U(""), to!U("s")), "helo godbye"));
                assert(equal(tr(to!S("hello goodbye"), to!T("lo"), to!U("x"), "s"), "hex gxdbye"));
                assert(equal(tr(to!S("14-Jul-87"), to!T("a-zA-Z"), to!U(" "), "cs"), " Jul "));
                assert(equal(tr(to!S("Abc"), to!T("AAA"), to!U("XYZ")), "Xbc"));
            }
        }

        auto s = to!S("hello world");
        static assert(is(typeof(s) == typeof(tr(s, "he", "if"))));
    }
    });
}

@system pure unittest
{
    import core.exception : AssertError;
    import std.exception : assertThrown;
    assertThrown!AssertError(tr("abcdef", "cd", "CD", "X"));
}

/**
 * Takes a string $(D s) and determines if it represents a number. This function
 * also takes an optional parameter, $(D bAllowSep), which will accept the
 * separator characters $(D ',') and $(D '__') within the string. But these
 * characters should be stripped from the string before using any
 * of the conversion functions like $(D to!int()), $(D to!float()), and etc
 * else an error will occur.
 *
 * Also please note, that no spaces are allowed within the string
 * anywhere whether it's a leading, trailing, or embedded space(s),
 * thus they too must be stripped from the string before using this
 * function, or any of the conversion functions.
 *
 * Params:
 *     s = the string or random access range to check
 *     bAllowSep = accept separator characters or not
 *
 * Returns:
 *     $(D bool)
 */
bool isNumeric(S)(S s, bool bAllowSep = false)
if (isSomeString!S ||
    (isRandomAccessRange!S &&
    hasSlicing!S &&
    isSomeChar!(ElementType!S) &&
    !isInfinite!S))
{
    import std.algorithm.comparison : among;
    import std.ascii : isASCII;

    // ASCII only case insensitive comparison with two ranges
    static bool asciiCmp(S1)(S1 a, string b)
    {
        import std.algorithm.comparison : equal;
        import std.algorithm.iteration : map;
        import std.ascii : toLower;
        import std.utf : byChar;
        return a.map!toLower.equal(b.byChar.map!toLower);
    }

    // auto-decoding special case, we're only comparing characters
    // in the ASCII range so there's no reason to decode
    static if (isSomeString!S)
    {
        import std.utf : byCodeUnit;
        auto codeUnits = s.byCodeUnit;
    }
    else
    {
        alias codeUnits = s;
    }

    if (codeUnits.empty)
        return false;

    // Check for NaN (Not a Number) and for Infinity
    if (codeUnits.among!((a, b) => asciiCmp(a.save, b))
            ("nan", "nani", "nan+nani", "inf", "-inf"))
        return true;

    immutable frontResult = codeUnits.front;
    if (frontResult == '-' || frontResult == '+')
        codeUnits.popFront;

    immutable iLen = codeUnits.length;
    bool bDecimalPoint, bExponent, bComplex, sawDigits;

    for (size_t i = 0; i < iLen; i++)
    {
        immutable c = codeUnits[i];

        if (!c.isASCII)
            return false;

        // Digits are good, skip to the next character
        if (c >= '0' && c <= '9')
        {
            sawDigits = true;
            continue;
        }

        // Check for the complex type, and if found
        // reset the flags for checking the 2nd number.
        if (c == '+')
        {
            if (!i)
                return false;
            bDecimalPoint = false;
            bExponent = false;
            bComplex = true;
            sawDigits = false;
            continue;
        }

        // Allow only one exponent per number
        if (c == 'e' || c == 'E')
        {
            // A 2nd exponent found, return not a number
            if (bExponent || i + 1 >= iLen)
                return false;
            // Look forward for the sign, and if
            // missing then this is not a number.
            if (codeUnits[i + 1] != '-' && codeUnits[i + 1] != '+')
                return false;
            bExponent = true;
            i++;
            continue;
        }
        // Allow only one decimal point per number to be used
        if (c == '.')
        {
            // A 2nd decimal point found, return not a number
            if (bDecimalPoint)
                return false;
            bDecimalPoint = true;
            continue;
        }
        // Check for ending literal characters: "f,u,l,i,ul,fi,li",
        // and whether they're being used with the correct datatype.
        if (i == iLen - 2)
        {
            if (!sawDigits)
                return false;
            // Integer Whole Number
            if (asciiCmp(codeUnits[i .. iLen], "ul") &&
                    (!bDecimalPoint && !bExponent && !bComplex))
                return true;
            // Floating-Point Number
            if (codeUnits[i .. iLen].among!((a, b) => asciiCmp(a, b))("fi", "li") &&
                    (bDecimalPoint || bExponent || bComplex))
                return true;
            if (asciiCmp(codeUnits[i .. iLen], "ul") &&
                    (bDecimalPoint || bExponent || bComplex))
                return false;
            // Could be a Integer or a Float, thus
            // all these suffixes are valid for both
            return codeUnits[i .. iLen].among!((a, b) => asciiCmp(a, b))
                ("ul", "fi", "li") != 0;
        }
        if (i == iLen - 1)
        {
            if (!sawDigits)
                return false;
            // Integer Whole Number
            if (c.among!('u', 'l', 'U', 'L')() &&
                   (!bDecimalPoint && !bExponent && !bComplex))
                return true;
            // Check to see if the last character in the string
            // is the required 'i' character
            if (bComplex)
                return c.among!('i', 'I')() != 0;
            // Floating-Point Number
            return c.among!('l', 'L', 'f', 'F', 'i', 'I')() != 0;
        }
        // Check if separators are allowed to be in the numeric string
        if (!bAllowSep || !c.among!('_', ',')())
            return false;
    }

    return sawDigits;
}

/**
 * Integer Whole Number: (byte, ubyte, short, ushort, int, uint, long, and ulong)
 * ['+'|'-']digit(s)[U|L|UL]
 */
@safe @nogc pure nothrow unittest
{
    assert(isNumeric("123"));
    assert(isNumeric("123UL"));
    assert(isNumeric("123L"));
    assert(isNumeric("+123U"));
    assert(isNumeric("-123L"));
}

/**
 * Floating-Point Number: (float, double, real, ifloat, idouble, and ireal)
 * ['+'|'-']digit(s)[.][digit(s)][[e-|e+]digit(s)][i|f|L|Li|fi]]
 *      or [nan|nani|inf|-inf]
 */
@safe @nogc pure nothrow unittest
{
    assert(isNumeric("+123"));
    assert(isNumeric("-123.01"));
    assert(isNumeric("123.3e-10f"));
    assert(isNumeric("123.3e-10fi"));
    assert(isNumeric("123.3e-10L"));

    assert(isNumeric("nan"));
    assert(isNumeric("nani"));
    assert(isNumeric("-inf"));
}

/**
 * Floating-Point Number: (cfloat, cdouble, and creal)
 * ['+'|'-']digit(s)[.][digit(s)][[e-|e+]digit(s)][+]
 *         [digit(s)[.][digit(s)][[e-|e+]digit(s)][i|f|L|Li|fi]]
 *      or [nan|nani|nan+nani|inf|-inf]
 */
@safe @nogc pure nothrow unittest
{
    assert(isNumeric("-123e-1+456.9e-10Li"));
    assert(isNumeric("+123e+10+456i"));
    assert(isNumeric("123+456"));
}

@safe @nogc pure nothrow unittest
{
    assert(!isNumeric("F"));
    assert(!isNumeric("L"));
    assert(!isNumeric("U"));
    assert(!isNumeric("i"));
    assert(!isNumeric("fi"));
    assert(!isNumeric("ul"));
    assert(!isNumeric("li"));
    assert(!isNumeric("."));
    assert(!isNumeric("-"));
    assert(!isNumeric("+"));
    assert(!isNumeric("e-"));
    assert(!isNumeric("e+"));
    assert(!isNumeric(".f"));
    assert(!isNumeric("e+f"));
    assert(!isNumeric("++1"));
    assert(!isNumeric(""));
    assert(!isNumeric("1E+1E+1"));
    assert(!isNumeric("1E1"));
    assert(!isNumeric("\x81"));
}

// Test string types
@safe unittest
{
    import std.conv : to;

    foreach (T; AliasSeq!(string, char[], wstring, wchar[], dstring, dchar[]))
    {
        assert("123".to!T.isNumeric());
        assert("123UL".to!T.isNumeric());
        assert("123fi".to!T.isNumeric());
        assert("123li".to!T.isNumeric());
        assert(!"--123L".to!T.isNumeric());
    }
}

// test ranges
@system pure unittest
{
    import std.range : refRange;
    import std.utf : byCodeUnit;

    assert("123".byCodeUnit.isNumeric());
    assert("123UL".byCodeUnit.isNumeric());
    assert("123fi".byCodeUnit.isNumeric());
    assert("123li".byCodeUnit.isNumeric());
    assert(!"--123L".byCodeUnit.isNumeric());

    dstring z = "0";
    assert(isNumeric(refRange(&z)));

    dstring nani = "nani";
    assert(isNumeric(refRange(&nani)));
}

/// isNumeric works with CTFE
@safe pure unittest
{
    enum a = isNumeric("123.00E-5+1234.45E-12Li");
    enum b = isNumeric("12345xxxx890");

    static assert( a);
    static assert(!b);
}

@system unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    // Test the isNumeric(in string) function
    assert(isNumeric("1") == true );
    assert(isNumeric("1.0") == true );
    assert(isNumeric("1e-1") == true );
    assert(isNumeric("12345xxxx890") == false );
    assert(isNumeric("567L") == true );
    assert(isNumeric("23UL") == true );
    assert(isNumeric("-123..56f") == false );
    assert(isNumeric("12.3.5.6") == false );
    assert(isNumeric(" 12.356") == false );
    assert(isNumeric("123 5.6") == false );
    assert(isNumeric("1233E-1+1.0e-1i") == true );

    assert(isNumeric("123.00E-5+1234.45E-12Li") == true);
    assert(isNumeric("123.00e-5+1234.45E-12iL") == false);
    assert(isNumeric("123.00e-5+1234.45e-12uL") == false);
    assert(isNumeric("123.00E-5+1234.45e-12lu") == false);

    assert(isNumeric("123fi") == true);
    assert(isNumeric("123li") == true);
    assert(isNumeric("--123L") == false);
    assert(isNumeric("+123.5UL") == false);
    assert(isNumeric("123f") == true);
    assert(isNumeric("123.u") == false);

  // @@@BUG@@ to!string(float) is not CTFEable.
  // Related: formatValue(T) if (is(FloatingPointTypeOf!T))
  if (!__ctfe)
  {
    assert(isNumeric(to!string(real.nan)) == true);
    assert(isNumeric(to!string(-real.infinity)) == true);
    assert(isNumeric(to!string(123e+2+1234.78Li)) == true);
  }

    string s = "$250.99-";
    assert(isNumeric(s[1 .. s.length - 2]) == true);
    assert(isNumeric(s) == false);
    assert(isNumeric(s[0 .. s.length - 1]) == false);
    });

    assert(!isNumeric("-"));
    assert(!isNumeric("+"));
}

/*****************************
 * Soundex algorithm.
 *
 * The Soundex algorithm converts a word into 4 characters
 * based on how the word sounds phonetically. The idea is that
 * two spellings that sound alike will have the same Soundex
 * value, which means that Soundex can be used for fuzzy matching
 * of names.
 *
 * Params:
 *  str = String or InputRange to convert to Soundex representation.
 *
 * Returns:
 *  The four character array with the Soundex result in it.
 *  The array has zero's in it if there is no Soundex representation for the string.
 *
 * See_Also:
 *  $(LINK2 http://en.wikipedia.org/wiki/Soundex, Wikipedia),
 *  $(LUCKY The Soundex Indexing System)
 *  $(LREF soundex)
 *
 * Bugs:
 *  Only works well with English names.
 *  There are other arguably better Soundex algorithms,
 *  but this one is the standard one.
 */
char[4] soundexer(Range)(Range str)
if (isInputRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    alias C = Unqual!(ElementEncodingType!Range);

    static immutable dex =
        // ABCDEFGHIJKLMNOPQRSTUVWXYZ
          "01230120022455012623010202";

    char[4] result = void;
    size_t b = 0;
    C lastc;
    foreach (C c; str)
    {
        if (c >= 'a' && c <= 'z')
            c -= 'a' - 'A';
        else if (c >= 'A' && c <= 'Z')
        {
        }
        else
        {
            lastc = lastc.init;
            continue;
        }
        if (b == 0)
        {
            result[0] = cast(char) c;
            b++;
            lastc = dex[c - 'A'];
        }
        else
        {
            if (c == 'H' || c == 'W')
                continue;
            if (c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U')
                lastc = lastc.init;
            c = dex[c - 'A'];
            if (c != '0' && c != lastc)
            {
                result[b] = cast(char) c;
                b++;
                lastc = c;
            }
            if (b == 4)
                goto Lret;
        }
    }
    if (b == 0)
        result[] = 0;
    else
        result[b .. 4] = '0';
  Lret:
    return result;
}

char[4] soundexer(Range)(auto ref Range str)
if (isConvertibleToString!Range)
{
    return soundexer!(StringTypeOf!Range)(str);
}

/*****************************
 * Like $(LREF soundexer), but with different parameters
 * and return value.
 *
 * Params:
 *  str = String to convert to Soundex representation.
 *  buffer = Optional 4 char array to put the resulting Soundex
 *      characters into. If null, the return value
 *      buffer will be allocated on the heap.
 * Returns:
 *  The four character array with the Soundex result in it.
 *  Returns null if there is no Soundex representation for the string.
 * See_Also:
 *  $(LREF soundexer)
 */
char[] soundex(const(char)[] str, char[] buffer = null)
    @safe pure nothrow
in
{
    assert(buffer is null || buffer.length >= 4);
}
out (result)
{
    if (result !is null)
    {
        assert(result.length == 4);
        assert(result[0] >= 'A' && result[0] <= 'Z');
        foreach (char c; result[1 .. 4])
            assert(c >= '0' && c <= '6');
    }
}
body
{
    char[4] result = soundexer(str);
    if (result[0] == 0)
        return null;
    if (buffer is null)
        buffer = new char[4];
    buffer[] = result[];
    return buffer;
}


@safe pure nothrow unittest
{
    import std.exception : assertCTFEable;
    assertCTFEable!(
    {
    char[4] buffer;

    assert(soundex(null) == null);
    assert(soundex("") == null);
    assert(soundex("0123^&^^**&^") == null);
    assert(soundex("Euler") == "E460");
    assert(soundex(" Ellery ") == "E460");
    assert(soundex("Gauss") == "G200");
    assert(soundex("Ghosh") == "G200");
    assert(soundex("Hilbert") == "H416");
    assert(soundex("Heilbronn") == "H416");
    assert(soundex("Knuth") == "K530");
    assert(soundex("Kant", buffer) == "K530");
    assert(soundex("Lloyd") == "L300");
    assert(soundex("Ladd") == "L300");
    assert(soundex("Lukasiewicz", buffer) == "L222");
    assert(soundex("Lissajous") == "L222");
    assert(soundex("Robert") == "R163");
    assert(soundex("Rupert") == "R163");
    assert(soundex("Rubin") == "R150");
    assert(soundex("Washington") == "W252");
    assert(soundex("Lee") == "L000");
    assert(soundex("Gutierrez") == "G362");
    assert(soundex("Pfister") == "P236");
    assert(soundex("Jackson") == "J250");
    assert(soundex("Tymczak") == "T522");
    assert(soundex("Ashcraft") == "A261");

    assert(soundex("Woo") == "W000");
    assert(soundex("Pilgrim") == "P426");
    assert(soundex("Flingjingwaller") == "F452");
    assert(soundex("PEARSE") == "P620");
    assert(soundex("PIERCE") == "P620");
    assert(soundex("Price") == "P620");
    assert(soundex("CATHY") == "C300");
    assert(soundex("KATHY") == "K300");
    assert(soundex("Jones") == "J520");
    assert(soundex("johnsons") == "J525");
    assert(soundex("Hardin") == "H635");
    assert(soundex("Martinez") == "M635");

    import std.utf : byChar, byDchar, byWchar;
    assert(soundexer("Martinez".byChar ) == "M635");
    assert(soundexer("Martinez".byWchar) == "M635");
    assert(soundexer("Martinez".byDchar) == "M635");
    });
}

@safe pure unittest
{
    assert(testAliasedString!soundexer("Martinez"));
}


/***************************************************
 * Construct an associative array consisting of all
 * abbreviations that uniquely map to the strings in values.
 *
 * This is useful in cases where the user is expected to type
 * in one of a known set of strings, and the program will helpfully
 * auto-complete the string once sufficient characters have been
 * entered that uniquely identify it.
 */

string[string] abbrev(string[] values) @safe pure
{
    import std.algorithm.sorting : sort;

    string[string] result;

    // Make a copy when sorting so we follow COW principles.
    values = values.dup;
    sort(values);

    size_t values_length = values.length;
    size_t lasti = values_length;
    size_t nexti;

    string nv;
    string lv;

    for (size_t i = 0; i < values_length; i = nexti)
    {
        string value = values[i];

        // Skip dups
        for (nexti = i + 1; nexti < values_length; nexti++)
        {
            nv = values[nexti];
            if (value != values[nexti])
                break;
        }

        import std.utf : stride;

        for (size_t j = 0; j < value.length; j += stride(value, j))
        {
            string v = value[0 .. j];

            if ((nexti == values_length || j > nv.length || v != nv[0 .. j]) &&
                (lasti == values_length || j > lv.length || v != lv[0 .. j]))
            {
                result[v] = value;
            }
        }
        result[value] = value;
        lasti = i;
        lv = value;
    }

    return result;
}

///
@safe unittest
{
    import std.string;

    static string[] list = [ "food", "foxy" ];
    auto abbrevs = abbrev(list);
    assert(abbrevs == ["fox": "foxy", "food": "food",
                       "foxy": "foxy", "foo": "food"]);
}


@system pure unittest
{
    import std.algorithm.sorting : sort;
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    string[] values;
    values ~= "hello";
    values ~= "hello";
    values ~= "he";

    string[string] r;

    r = abbrev(values);
    auto keys = r.keys.dup;
    sort(keys);

    assert(keys.length == 4);
    assert(keys[0] == "he");
    assert(keys[1] == "hel");
    assert(keys[2] == "hell");
    assert(keys[3] == "hello");

    assert(r[keys[0]] == "he");
    assert(r[keys[1]] == "hello");
    assert(r[keys[2]] == "hello");
    assert(r[keys[3]] == "hello");
    });
}


/******************************************
 * Compute _column number at the end of the printed form of the string,
 * assuming the string starts in the leftmost _column, which is numbered
 * starting from 0.
 *
 * Tab characters are expanded into enough spaces to bring the _column number
 * to the next multiple of tabsize.
 * If there are multiple lines in the string, the _column number of the last
 * line is returned.
 *
 * Params:
 *    str = string or InputRange to be analyzed
 *    tabsize = number of columns a tab character represents
 *
 * Returns:
 *    column number
 */

size_t column(Range)(Range str, in size_t tabsize = 8)
if ((isInputRange!Range && isSomeChar!(Unqual!(ElementEncodingType!Range)) ||
    isNarrowString!Range) &&
    !isConvertibleToString!Range)
{
    static if (is(Unqual!(ElementEncodingType!Range) == char))
    {
        // decoding needed for chars
        import std.utf : byDchar;

        return str.byDchar.column(tabsize);
    }
    else
    {
        // decoding not needed for wchars and dchars
        import std.uni : lineSep, paraSep, nelSep;

        size_t column;

        foreach (const c; str)
        {
            switch (c)
            {
                case '\t':
                    column = (column + tabsize) / tabsize * tabsize;
                    break;

                case '\r':
                case '\n':
                case paraSep:
                case lineSep:
                case nelSep:
                    column = 0;
                    break;

                default:
                    column++;
                    break;
            }
        }
        return column;
    }
}

///
@safe pure unittest
{
    import std.utf : byChar, byWchar, byDchar;

    assert(column("1234 ") == 5);
    assert(column("1234 "w) == 5);
    assert(column("1234 "d) == 5);

    assert(column("1234 ".byChar()) == 5);
    assert(column("1234 "w.byWchar()) == 5);
    assert(column("1234 "d.byDchar()) == 5);

    // Tab stops are set at 8 spaces by default; tab characters insert enough
    // spaces to bring the column position to the next multiple of 8.
    assert(column("\t") == 8);
    assert(column("1\t") == 8);
    assert(column("\t1") == 9);
    assert(column("123\t") == 8);

    // Other tab widths are possible by specifying it explicitly:
    assert(column("\t", 4) == 4);
    assert(column("1\t", 4) == 4);
    assert(column("\t1", 4) == 5);
    assert(column("123\t", 4) == 4);

    // New lines reset the column number.
    assert(column("abc\n") == 0);
    assert(column("abc\n1") == 1);
    assert(column("abcdefg\r1234") == 4);
    assert(column("abc\u20281") == 1);
    assert(column("abc\u20291") == 1);
    assert(column("abc\u00851") == 1);
    assert(column("abc\u00861") == 5);
}

size_t column(Range)(auto ref Range str, in size_t tabsize = 8)
if (isConvertibleToString!Range)
{
    return column!(StringTypeOf!Range)(str, tabsize);
}

@safe pure unittest
{
    assert(testAliasedString!column("abc\u00861"));
}

@safe @nogc unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    assert(column(string.init) == 0);
    assert(column("") == 0);
    assert(column("\t") == 8);
    assert(column("abc\t") == 8);
    assert(column("12345678\t") == 16);
    });
}

/******************************************
 * Wrap text into a paragraph.
 *
 * The input text string s is formed into a paragraph
 * by breaking it up into a sequence of lines, delineated
 * by \n, such that the number of columns is not exceeded
 * on each line.
 * The last line is terminated with a \n.
 * Params:
 *  s = text string to be wrapped
 *  columns = maximum number of _columns in the paragraph
 *  firstindent = string used to _indent first line of the paragraph
 *  indent = string to use to _indent following lines of the paragraph
 *  tabsize = column spacing of tabs in firstindent[] and indent[]
 * Returns:
 *  resulting paragraph as an allocated string
 */

S wrap(S)(S s, in size_t columns = 80, S firstindent = null,
S indent = null, in size_t tabsize = 8)
if (isSomeString!S)
{
    import std.uni : isWhite;
    typeof(s.dup) result;
    bool inword;
    bool first = true;
    size_t wordstart;

    const indentcol = column(indent, tabsize);

    result.length = firstindent.length + s.length;
    result.length = firstindent.length;
    result[] = firstindent[];
    auto col = column(firstindent, tabsize);
    foreach (size_t i, dchar c; s)
    {
        if (isWhite(c))
        {
            if (inword)
            {
                if (first)
                {
                }
                else if (col + 1 + (i - wordstart) > columns)
                {
                    result ~= '\n';
                    result ~= indent;
                    col = indentcol;
                }
                else
                {
                    result ~= ' ';
                    col += 1;
                }
                result ~= s[wordstart .. i];
                col += i - wordstart;
                inword = false;
                first = false;
            }
        }
        else
        {
            if (!inword)
            {
                wordstart = i;
                inword = true;
            }
        }
    }

    if (inword)
    {
        if (col + 1 + (s.length - wordstart) >= columns)
        {
            result ~= '\n';
            result ~= indent;
        }
        else if (result.length != firstindent.length)
            result ~= ' ';
        result ~= s[wordstart .. s.length];
    }
    result ~= '\n';

    return result;
}

///
@safe pure unittest
{
    assert(wrap("a short string", 7) == "a short\nstring\n");

    // wrap will not break inside of a word, but at the next space
    assert(wrap("a short string", 4) == "a\nshort\nstring\n");

    assert(wrap("a short string", 7, "\t") == "\ta\nshort\nstring\n");
    assert(wrap("a short string", 7, "\t", "    ") == "\ta\n    short\n    string\n");
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
    assert(wrap(string.init) == "\n");
    assert(wrap(" a b   df ") == "a b df\n");
    assert(wrap(" a b   df ", 3) == "a b\ndf\n");
    assert(wrap(" a bc   df ", 3) == "a\nbc\ndf\n");
    assert(wrap(" abcd   df ", 3) == "abcd\ndf\n");
    assert(wrap("x") == "x\n");
    assert(wrap("u u") == "u u\n");
    assert(wrap("abcd", 3) == "\nabcd\n");
    assert(wrap("a de", 10, "\t", "   ", 8) == "\ta\n   de\n");
    });
}

/******************************************
 * Removes one level of indentation from a multi-line string.
 *
 * This uniformly outdents the text as much as possible.
 * Whitespace-only lines are always converted to blank lines.
 *
 * Does not allocate memory if it does not throw.
 *
 * Params:
 *     str = multi-line string
 *
 * Returns:
 *      outdented string
 *
 * Throws:
 *     StringException if indentation is done with different sequences
 *     of whitespace characters.
 */
S outdent(S)(S str) @safe pure
if (isSomeString!S)
{
    return str.splitLines(Yes.keepTerminator).outdent().join();
}

///
@safe pure unittest
{
    enum pretty = q{
       import std.stdio;
       void main() {
           writeln("Hello");
       }
    }.outdent();

    enum ugly = q{
import std.stdio;
void main() {
    writeln("Hello");
}
};

    assert(pretty == ugly);
}


/******************************************
 * Removes one level of indentation from an array of single-line strings.
 *
 * This uniformly outdents the text as much as possible.
 * Whitespace-only lines are always converted to blank lines.
 *
 * Params:
 *     lines = array of single-line strings
 *
 * Returns:
 *      lines[] is rewritten in place with outdented lines
 *
 * Throws:
 *     StringException if indentation is done with different sequences
 *     of whitespace characters.
 */
S[] outdent(S)(S[] lines) @safe pure
if (isSomeString!S)
{
    import std.algorithm.searching : startsWith;

    if (lines.empty)
    {
        return null;
    }

    static S leadingWhiteOf(S str)
    {
        return str[ 0 .. $ - stripLeft(str).length ];
    }

    S shortestIndent;
    foreach (ref line; lines)
    {
        const stripped = line.stripLeft();

        if (stripped.empty)
        {
            line = line[line.chomp().length .. $];
        }
        else
        {
            const indent = leadingWhiteOf(line);

            // Comparing number of code units instead of code points is OK here
            // because this function throws upon inconsistent indentation.
            if (shortestIndent is null || indent.length < shortestIndent.length)
            {
                if (indent.empty)
                    return lines;
                shortestIndent = indent;
            }
        }
    }

    foreach (ref line; lines)
    {
        const stripped = line.stripLeft();

        if (stripped.empty)
        {
            // Do nothing
        }
        else if (line.startsWith(shortestIndent))
        {
            line = line[shortestIndent.length .. $];
        }
        else
        {
            throw new StringException("outdent: Inconsistent indentation");
        }
    }

    return lines;
}

@safe pure unittest
{
    import std.conv : to;
    import std.exception : assertCTFEable;

    template outdent_testStr(S)
    {
        enum S outdent_testStr =
"
 \t\tX
 \t\U00010143X
 \t\t

 \t\t\tX
\t ";
    }

    template outdent_expected(S)
    {
        enum S outdent_expected =
"
\tX
\U00010143X


\t\tX
";
    }

    assertCTFEable!(
    {

    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        enum S blank = "";
        assert(blank.outdent() == blank);
        static assert(blank.outdent() == blank);

        enum S testStr1  = " \n \t\n ";
        enum S expected1 = "\n\n";
        assert(testStr1.outdent() == expected1);
        static assert(testStr1.outdent() == expected1);

        assert(testStr1[0..$-1].outdent() == expected1);
        static assert(testStr1[0..$-1].outdent() == expected1);

        enum S testStr2  = "a\n \t\nb";
        assert(testStr2.outdent() == testStr2);
        static assert(testStr2.outdent() == testStr2);

        enum S testStr3 =
"
 \t\tX
 \t\U00010143X
 \t\t

 \t\t\tX
\t ";

        enum S expected3 =
"
\tX
\U00010143X


\t\tX
";
        assert(testStr3.outdent() == expected3);
        static assert(testStr3.outdent() == expected3);

        enum testStr4 = "  X\r  X\n  X\r\n  X\u2028  X\u2029  X";
        enum expected4 = "X\rX\nX\r\nX\u2028X\u2029X";
        assert(testStr4.outdent() == expected4);
        static assert(testStr4.outdent() == expected4);

        enum testStr5  = testStr4[0..$-1];
        enum expected5 = expected4[0..$-1];
        assert(testStr5.outdent() == expected5);
        static assert(testStr5.outdent() == expected5);

        enum testStr6 = "  \r  \n  \r\n  \u2028  \u2029";
        enum expected6 = "\r\n\r\n\u2028\u2029";
        assert(testStr6.outdent() == expected6);
        static assert(testStr6.outdent() == expected6);

        enum testStr7 = " a \n b ";
        enum expected7 = "a \nb ";
        assert(testStr7.outdent() == expected7);
        static assert(testStr7.outdent() == expected7);
    }
    });
}

@safe pure unittest
{
    import std.exception : assertThrown;
    auto bad = "      a\n\tb\n   c";
    assertThrown!StringException(bad.outdent);
}

/** Assume the given array of integers $(D arr) is a well-formed UTF string and
return it typed as a UTF string.

$(D ubyte) becomes $(D char), $(D ushort) becomes $(D wchar) and $(D uint)
becomes $(D dchar). Type qualifiers are preserved.

When compiled with debug mode, this function performs an extra check to make
sure the return value is a valid Unicode string.

Params:
    arr = array of bytes, ubytes, shorts, ushorts, ints, or uints

Returns:
    arr retyped as an array of chars, wchars, or dchars

See_Also: $(LREF representation)
*/
auto assumeUTF(T)(T[] arr) pure
if (staticIndexOf!(Unqual!T, ubyte, ushort, uint) != -1)
{
    import std.traits : ModifyTypePreservingTQ;
    import std.utf : validate;
    alias ToUTFType(U) = AliasSeq!(char, wchar, dchar)[U.sizeof / 2];
    auto asUTF = cast(ModifyTypePreservingTQ!(ToUTFType, T)[])arr;
    debug validate(asUTF);
    return asUTF;
}

///
@safe pure unittest
{
    string a = "Hölo World";
    immutable(ubyte)[] b = a.representation;
    string c = b.assumeUTF;

    assert(a == c);
}

pure @system unittest
{
    import std.algorithm.comparison : equal;
    foreach (T; AliasSeq!(char[], wchar[], dchar[]))
    {
        immutable T jti = "Hello World";
        T jt = jti.dup;

        static if (is(T == char[]))
        {
            auto gt = cast(ubyte[]) jt;
            auto gtc = cast(const(ubyte)[])jt;
            auto gti = cast(immutable(ubyte)[])jt;
        }
        else static if (is(T == wchar[]))
        {
            auto gt = cast(ushort[]) jt;
            auto gtc = cast(const(ushort)[])jt;
            auto gti = cast(immutable(ushort)[])jt;
        }
        else static if (is(T == dchar[]))
        {
            auto gt = cast(uint[]) jt;
            auto gtc = cast(const(uint)[])jt;
            auto gti = cast(immutable(uint)[])jt;
        }

        auto ht = assumeUTF(gt);
        auto htc = assumeUTF(gtc);
        auto hti = assumeUTF(gti);
        assert(equal(jt, ht));
        assert(equal(jt, htc));
        assert(equal(jt, hti));
    }
}
