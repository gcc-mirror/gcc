/**
Helper functions for working with $(I C strings).

This module is intended to provide fast, safe and garbage free
way to work with $(I C strings).

Copyright: Denis Shelomovskij 2013-2014

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: Denis Shelomovskij

Macros:
COREREF = $(HTTP dlang.org/phobos/core_$1.html#$2, `core.$1.$2`)
*/
module std.internal.cstring;

///
@safe unittest
{
    version (Posix)
    {
        import core.stdc.stdlib : free;
        import core.sys.posix.stdlib : setenv;
        import std.exception : enforce;

        void setEnvironment(scope const(char)[] name, scope const(char)[] value)
        { enforce(setenv(name.tempCString(), value.tempCString(), 1) != -1); }
    }

    version (Windows)
    {
        import core.sys.windows.winbase : SetEnvironmentVariableW;
        import std.exception : enforce;

        void setEnvironment(scope const(char)[] name, scope const(char)[] value)
        { enforce(SetEnvironmentVariableW(name.tempCStringW(), value.tempCStringW())); }
    }
}

import std.range;
import std.traits;

/**
Creates temporary 0-terminated $(I C string) with copy of passed text.

Params:
    To = character type of returned C string
    str = string or input range to be converted

Returns:

The value returned is implicitly convertible to $(D const To*) and
has two properties: `ptr` to access $(I C string) as $(D const To*)
and `buffPtr` to access it as `To*`.

The value returned can be indexed by [] to access it as an array.

The temporary $(I C string) is valid unless returned object is destroyed.
Thus if returned object is assigned to a variable the temporary is
valid unless the variable goes out of scope. If returned object isn't
assigned to a variable it will be destroyed at the end of creating
primary expression.

Implementation_note:
For small strings tempCString will use stack allocated buffer,
for large strings (approximately 250 characters and more) it will
allocate temporary one using C's `malloc`.

Note:
This function is intended to be used in function call expression (like
`strlen(str.tempCString())`). Incorrect usage of this function may
lead to memory corruption.
See $(RED WARNING) in $(B Examples) section.
*/

auto tempCString(To = char, From)(scope From str)
if (isSomeChar!To && (isInputRange!From || isSomeString!From) &&
    isSomeChar!(ElementEncodingType!From))
{
    alias CF = Unqual!(ElementEncodingType!From);

    auto res = TempCStringBuffer!To.trustedVoidInit(); // expensive to fill _buff[]

    // Note: res._ptr can't point to res._buff as structs are movable.

    // https://issues.dlang.org/show_bug.cgi?id=14980
    static if (isSomeString!From)
    {
        if (str is null)
        {
            res._length = 0;
            res._ptr = null;
            return res;
        }
    }

    // Use slice assignment if available.
    static if (To.sizeof == CF.sizeof && is(typeof(res._buff[0 .. str.length] = str[])))
    {
        if (str.length < res._buff.length)
        {
            res._buff[0 .. str.length] = str[];
            res._buff[str.length] = 0;
            res._ptr = res.useStack;
        }
        else
        {
            import std.internal.memory : enforceMalloc;
            if (false)
            {
                // This code is removed by the compiler but causes `@safe`ty
                // to be inferred correctly.
                CF[0] x;
                x[] = str[0 .. 0];
            }
            res._ptr = () @trusted {
                auto p = cast(CF*) enforceMalloc((str.length + 1) * CF.sizeof);
                p[0 .. str.length] = str[];
                p[str.length] = 0;
                return cast(To*) p;
            }();
        }
        res._length = str.length;
        return res;
    }
    else
    {
        static assert(!(isSomeString!From && CF.sizeof == To.sizeof), "Should be using slice assignment.");
        To[] p = res._buff;
        size_t i;

        size_t strLength;
        static if (hasLength!From)
        {
            strLength = str.length;
        }
        import std.utf : byUTF;
        static if (isSomeString!From)
            auto r = cast(const(CF)[])str;  // because inout(CF) causes problems with byUTF
        else
            alias r = str;
        To[] heapBuffer;
        foreach (const c; byUTF!(Unqual!To)(r))
        {
            if (i + 1 == p.length)
            {
                if (heapBuffer is null)
                    heapBuffer = trustedReallocStack(p, strLength);
                else
                    heapBuffer = trustedRealloc(heapBuffer);
                p = heapBuffer;
            }
            p[i++] = c;
        }
        p[i] = 0;
        res._length = i;
        res._ptr = (heapBuffer is null ? res.useStack : &heapBuffer[0]);
        return res;
    }
}

///
nothrow @nogc @system unittest
{
    import core.stdc.string;

    string str = "abc";

    // Intended usage
    assert(strlen(str.tempCString()) == 3);

    // Correct usage
    auto tmp = str.tempCString();
    assert(strlen(tmp) == 3); // or `tmp.ptr`, or `tmp.buffPtr`

    // $(RED WARNING): $(RED Incorrect usage)
    auto pInvalid1 = str.tempCString().ptr;
    const char* pInvalid2 = str.tempCString();
    // Both pointers refer to invalid memory here as
    // returned values aren't assigned to a variable and
    // both primary expressions are ended.
}

@safe pure nothrow @nogc unittest
{
    static inout(C)[] arrayFor(C)(inout(C)* cstr) pure nothrow @nogc @trusted
    {
        assert(cstr);
        size_t length = 0;
        while (cstr[length])
            ++length;
        return cstr[0 .. length];
    }

    assert(arrayFor("abc".tempCString()) == "abc");
    assert(arrayFor("abc"d.tempCString().ptr) == "abc");
    assert(arrayFor("abc".tempCString!wchar().buffPtr) == "abc"w);

    import std.utf : byChar, byWchar;
    char[300] abc = 'a';
    assert(arrayFor(tempCString(abc[].byChar).buffPtr) == abc);
    assert(arrayFor(tempCString(abc[].byWchar).buffPtr) == abc);
    assert(tempCString(abc[].byChar)[] == abc);
}

// https://issues.dlang.org/show_bug.cgi?id=14980
pure nothrow @nogc @safe unittest
{
    const(char[]) str = null;
    auto res = tempCString(str);
    const char* ptr = res;
    assert(ptr is null);
}

version (Windows)
{
    import core.sys.windows.winnt : WCHAR;
    alias tempCStringW = tempCString!(WCHAR, const(char)[]);
}

private struct TempCStringBuffer(To = char)
{
@trusted pure nothrow @nogc:

    @disable this();
    @disable this(this);
    alias ptr this; /// implicitly covert to raw pointer

    @property inout(To)* buffPtr() return inout
    {
        return _ptr == useStack ? _buff.ptr : _ptr;
    }

    @property const(To)* ptr() const
    {
        return buffPtr;
    }

    const(To)[] opIndex() const pure
    {
        return buffPtr[0 .. _length];
    }

    ~this()
    {
        if (_ptr != useStack)
        {
            import core.memory : pureFree;
            pureFree(_ptr);
        }
    }

private:
    enum To* useStack = () @trusted { return cast(To*) size_t.max; }();

    To* _ptr;
    size_t _length;        // length of the string
    version (StdUnittest)
    // the 'small string optimization'
    {
        // smaller size to trigger reallocations. Padding is to account for
        // unittest/non-unittest cross-compilation (to avoid corruption)
        To[16 / To.sizeof] _buff;
        To[(256 - 16) / To.sizeof] _unittest_pad;
    }
    else
    {
        To[256 / To.sizeof] _buff; // production size
    }

    static TempCStringBuffer trustedVoidInit() { TempCStringBuffer res = void; return res; }
}

private To[] trustedRealloc(To)(return scope To[] buf)
    @trusted @nogc pure nothrow
{
    pragma(inline, false);  // because it's rarely called
    import std.internal.memory : enforceRealloc;

    const size_t newlen = buf.length * 3 / 2;
    if (buf.length >= size_t.max / (2 * To.sizeof))
    {
        version (D_Exceptions)
        {
            import core.exception : onOutOfMemoryError;
            onOutOfMemoryError();
        }
        else
        {
            assert(0, "Memory allocation failed");
        }
    }
    auto ptr = cast(To*) enforceRealloc(buf.ptr, newlen * To.sizeof);
    return ptr[0 .. newlen];

}

private To[] trustedReallocStack(To)(scope To[] buf, size_t strLength)
    @trusted @nogc pure nothrow
{
    pragma(inline, false);  // because it's rarely called

    import std.internal.memory : enforceMalloc;

    size_t newlen = buf.length * 3 / 2;
    if (newlen <= strLength)
        newlen = strLength + 1; // +1 for terminating 0
    auto ptr = cast(To*) enforceMalloc(newlen * To.sizeof);
    ptr[0 .. buf.length] = buf[];
    return ptr[0 .. newlen];
}
