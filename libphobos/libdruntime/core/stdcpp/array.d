/**
 * D header file for interaction with C++ std::array.
 *
 * Copyright: Copyright (c) 2018 D Language Foundation
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Manu Evans
 * Source:    $(DRUNTIMESRC core/stdcpp/array.d)
 */

module core.stdcpp.array;

import core.stdcpp.xutility : StdNamespace;

// hacks to support DMD on Win32
version (CppRuntime_Microsoft)
{
    version = CppRuntime_Windows; // use the MS runtime ABI for win32
}
else version (CppRuntime_DigitalMars)
{
    version = CppRuntime_Windows; // use the MS runtime ABI for win32
    pragma(msg, "std::array not supported by DMC");
}

extern(C++, (StdNamespace)):

/**
 * D language counterpart to C++ std::array.
 *
 * C++ reference: $(LINK2 https://en.cppreference.com/w/cpp/container/array)
 */
extern(C++, class) struct array(T, size_t N)
{
extern(D):
pragma(inline, true):

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

    /// Variadic constructor
    this(T[N] args ...)                                 { this[] = args[]; }

    ///
    void fill()(auto ref const(T) value)                { this[] = value; }

pure nothrow @nogc:
    ///
    size_type size() const @safe                        { return N; }
    ///
    alias length = size;
    ///
    alias opDollar = length;
    ///
    size_type max_size() const @safe                    { return N; }
    ///
    bool empty() const @safe                            { return N == 0; }

    ///
    ref inout(T) front() inout @safe                    { static if (N > 0) { return this[0]; } else { return as_array()[][0]; /* HACK: force OOB */ } }
    ///
    ref inout(T) back() inout @safe                     { static if (N > 0) { return this[N-1]; } else { return as_array()[][0]; /* HACK: force OOB */ } }

    version (CppRuntime_Windows)
    {
        ///
        inout(T)* data() inout @safe                    { return &_Elems[0]; }
        ///
        ref inout(T)[N] as_array() inout @safe          { return _Elems[0 .. N]; }
        ///
        ref inout(T) at(size_type i) inout @safe        { return _Elems[0 .. N][i]; }

    private:
        T[N ? N : 1] _Elems;
    }
    else version (CppRuntime_Gcc)
    {
        ///
        inout(T)* data() inout @safe                    { static if (N > 0) { return &_M_elems[0]; } else { return null; } }
        ///
        ref inout(T)[N] as_array() inout @trusted       { return data()[0 .. N]; }
        ///
        ref inout(T) at(size_type i) inout @trusted     { return data()[0 .. N][i]; }

    private:
        static if (N > 0)
        {
            T[N] _M_elems;
        }
        else
        {
            struct _Placeholder {}
            _Placeholder _M_placeholder;
        }
    }
    else version (CppRuntime_Clang)
    {
        ///
        inout(T)* data() inout @trusted                 { static if (N > 0) { return &__elems_[0]; } else { return cast(inout(T)*)__elems_.ptr; } }
        ///
        ref inout(T)[N] as_array() inout @trusted       { return data()[0 .. N]; }
        ///
        ref inout(T) at(size_type i) inout @trusted     { return data()[0 .. N][i]; }

    private:
        static if (N > 0)
        {
            T[N] __elems_;
        }
        else
        {
            struct _ArrayInStructT { T[1] __data_; }
            align(_ArrayInStructT.alignof)
            byte[_ArrayInStructT.sizeof] __elems_ = void;
        }
    }
    else
    {
        static assert(false, "C++ runtime not supported");
    }
}
