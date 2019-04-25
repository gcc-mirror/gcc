/**
Helper functions for working with $(I C strings).

This module is intended to provide fast, safe and garbage free
way to work with $(I C strings).

Copyright: Denis Shelomovskij 2013-2014

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: Denis Shelomovskij

Macros:
COREREF = $(HTTP dlang.org/phobos/core_$1.html#$2, $(D core.$1.$2))
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

        void setEnvironment(in char[] name, in char[] value)
        { enforce(setenv(name.tempCString(), value.tempCString(), 1) != -1); }
    }

    version (Windows)
    {
        import core.sys.windows.windows : SetEnvironmentVariableW;
        import std.exception : enforce;

        void setEnvironment(in char[] name, in char[] value)
        { enforce(SetEnvironmentVariableW(name.tempCStringW(), value.tempCStringW())); }
    }
}

import std.range;
import std.traits;

version (unittest)
@property inout(C)[] asArray(C)(inout C* cstr) pure nothrow @nogc @trusted
if (isSomeChar!C)
in { assert(cstr); }
body
{
    size_t length = 0;
    while (cstr[length])
        ++length;
    return cstr[0 .. length];
}

/**
Creates temporary 0-terminated $(I C string) with copy of passed text.

Params:
    To = character type of returned C string
    str = string or input range to be converted

Returns:

The value returned is implicitly convertible to $(D const To*) and
has two properties: $(D ptr) to access $(I C string) as $(D const To*)
and $(D buffPtr) to access it as $(D To*).

The value returned can be indexed by [] to access it as an array.

The temporary $(I C string) is valid unless returned object is destroyed.
Thus if returned object is assigned to a variable the temporary is
valid unless the variable goes out of scope. If returned object isn't
assigned to a variable it will be destroyed at the end of creating
primary expression.

Implementation_note:
For small strings tempCString will use stack allocated buffer,
for large strings (approximately 250 characters and more) it will
allocate temporary one using C's $(D malloc).

Note:
This function is intended to be used in function call expression (like
$(D strlen(str.tempCString()))). Incorrect usage of this function may
lead to memory corruption.
See $(RED WARNING) in $(B Examples) section.
*/

auto tempCString(To = char, From)(From str)
if (isSomeChar!To && (isInputRange!From || isSomeString!From) &&
    isSomeChar!(ElementEncodingType!From))
{

    alias CF = Unqual!(ElementEncodingType!From);

    enum To* useStack = () @trusted { return cast(To*) size_t.max; }();

    static struct Res
    {
    @trusted:
    nothrow @nogc:

        @disable this();
        @disable this(this);
        alias ptr this;

        @property inout(To)* buffPtr() inout pure
        {
            return _ptr == useStack ? _buff.ptr : _ptr;
        }

        @property const(To)* ptr() const pure
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
                import core.stdc.stdlib : free;
                free(_ptr);
            }
        }

    private:
        To* _ptr;
        size_t _length;        // length of the string

        // the 'small string optimization'
        version (unittest)
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

        static Res trustedVoidInit() { Res res = void; return res; }
    }

    Res res = Res.trustedVoidInit();     // expensive to fill _buff[]

    // Note: res._ptr can't point to res._buff as structs are movable.

    To[] p;
    bool p_is_onstack = true;
    size_t i;

    static To[] trustedRealloc(To[] buf, size_t i, To[] res, size_t strLength, bool res_is_onstack)
        @trusted @nogc nothrow
    {
        pragma(inline, false);  // because it's rarely called

        import core.exception : onOutOfMemoryError;
        import core.stdc.stdlib : malloc, realloc;
        import core.stdc.string : memcpy;

        if (res_is_onstack)
        {
            size_t newlen = res.length * 3 / 2;
            if (newlen <= strLength)
                newlen = strLength + 1; // +1 for terminating 0
            auto ptr = cast(To*) malloc(newlen * To.sizeof);
            if (!ptr)
                onOutOfMemoryError();
            memcpy(ptr, res.ptr, i * To.sizeof);
            return ptr[0 .. newlen];
        }
        else
        {
            if (buf.length >= size_t.max / (2 * To.sizeof))
                onOutOfMemoryError();
            const newlen = buf.length * 3 / 2;
            auto ptr = cast(To*) realloc(buf.ptr, newlen * To.sizeof);
            if (!ptr)
                onOutOfMemoryError();
            return ptr[0 .. newlen];
        }
    }

    size_t strLength;
    static if (hasLength!From)
    {
        strLength = str.length;
    }
    import std.utf : byUTF;
    static if (isSomeString!From)
    {
        auto r = cast(const(CF)[])str;  // because inout(CF) causes problems with byUTF
        if (r is null)  // Bugzilla 14980
        {
            res._ptr = null;
            return res;
        }
    }
    else
        alias r = str;
    To[] q = res._buff;
    foreach (const c; byUTF!(Unqual!To)(r))
    {
        if (i + 1 == q.length)
        {
            p = trustedRealloc(p, i, res._buff, strLength, p_is_onstack);
            p_is_onstack = false;
            q = p;
        }
        q[i++] = c;
    }
    q[i] = 0;
    res._length = i;
    res._ptr = p_is_onstack ? useStack : &p[0];
    return res;
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

@safe nothrow @nogc unittest
{
    assert("abc".tempCString().asArray == "abc");
    assert("abc"d.tempCString().ptr.asArray == "abc");
    assert("abc".tempCString!wchar().buffPtr.asArray == "abc"w);

    import std.utf : byChar, byWchar;
    char[300] abc = 'a';
    assert(tempCString(abc[].byChar).buffPtr.asArray == abc);
    assert(tempCString(abc[].byWchar).buffPtr.asArray == abc);
    assert(tempCString(abc[].byChar)[] == abc);
}

// Bugzilla 14980
nothrow @nogc @safe unittest
{
    const(char[]) str = null;
    auto res = tempCString(str);
    const char* ptr = res;
    assert(ptr is null);
}

version (Windows)
    alias tempCStringW = tempCString!(wchar, const(char)[]);
