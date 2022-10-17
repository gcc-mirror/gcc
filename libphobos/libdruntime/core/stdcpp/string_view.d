/**
 * D header file for interaction with C++ std::string_view.
 *
 * Copyright: Copyright (c) 2018 D Language Foundation
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Manu Evans
 * Source:    $(DRUNTIMESRC core/stdcpp/string_view.d)
 */

module core.stdcpp.string_view;

import core.stdc.stddef : wchar_t;
import core.stdcpp.xutility : StdNamespace;

// hacks to support DMD on Win32
version (CppRuntime_Microsoft)
{
    version = CppRuntime_Windows; // use the MS runtime ABI for win32
}
else version (CppRuntime_DigitalMars)
{
    version = CppRuntime_Windows; // use the MS runtime ABI for win32
    pragma(msg, "std::basic_string_view not supported by DMC");
}

extern(C++, (StdNamespace)):
@nogc:

///
alias string_view = basic_string_view!char;
///
alias u16string_view = basic_string_view!wchar;
///
alias u32string_view = basic_string_view!dchar;
///
alias wstring_view = basic_string_view!wchar_t;


/**
 * Character traits classes specify character properties and provide specific
 * semantics for certain operations on characters and sequences of characters.
 */
extern(C++, struct) struct char_traits(CharT) {}


/**
* D language counterpart to C++ std::basic_string_view.
*
* C++ reference: $(LINK2 hhttps://en.cppreference.com/w/cpp/string/basic_string_view)
*/
extern(C++, class) struct basic_string_view(T, Traits = char_traits!T)
{
extern(D):
pragma(inline, true):
pure nothrow @nogc:

    ///
    enum size_type npos = size_type.max;

    ///
    alias size_type = size_t;
    ///
    alias difference_type = ptrdiff_t;
    ///
    alias value_type = T;
    ///
    alias pointer = T*;
    ///
    alias const_pointer = const(T)*;

    ///
    alias as_array this;
    ///
    alias toString = as_array;

    ///
    this(const(T)[] str) @trusted                   { __data = str.ptr; __size = str.length; }

    ///
    alias length = size;
    ///
    alias opDollar = length;
    ///
    size_type size() const @safe                    { return __size; }
    ///
    bool empty() const @safe                        { return __size == 0; }

    ///
    const(T)* data() const @safe                    { return __data; }
    ///
    const(T)[] as_array() const @trusted            { return __data[0 .. __size]; }

    ///
    ref const(T) at(size_type i) const @trusted     { return __data[0 .. __size][i]; }

    ///
    ref const(T) front() const @safe                { return this[0]; }
    ///
    ref const(T) back() const @safe                 { return this[$-1]; }

private:
    // use the proper field names from C++ so debugging doesn't get weird
    version (CppRuntime_Windows)
    {
        const_pointer _Mydata;
        size_type _Mysize;

        alias __data = _Mydata;
        alias __size = _Mysize;
    }
    else version (CppRuntime_Gcc)
    {
        size_t _M_len;
        const(T)* _M_str;

        alias __data = _M_str;
        alias __size = _M_len;
    }
    else version (CppRuntime_Clang)
    {
        const value_type* __data;
        size_type __size;
    }
    else
    {
        static assert(false, "C++ runtime not supported");
    }
}
