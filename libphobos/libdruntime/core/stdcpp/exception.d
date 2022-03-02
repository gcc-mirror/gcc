// Written in the D programming language.

/**
 * Interface to C++ <exception>
 *
 * Copyright: Copyright (c) 2016 D Language Foundation
 * License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright)
 *            Manu Evans
 * Source:    $(DRUNTIMESRC core/stdcpp/_exception.d)
 */

module core.stdcpp.exception;

import core.stdcpp.xutility : __cplusplus, CppStdRevision;
import core.attribute : weak;

version (CppRuntime_Gcc)
    version = GenericBaseException;
version (CppRuntime_Clang)
    version = GenericBaseException;
version (CppRuntime_Sun)
    version = GenericBaseException;

extern (C++, "std"):
@nogc:

///
alias terminate_handler = void function() nothrow;
///
terminate_handler set_terminate(terminate_handler f) nothrow;
///
terminate_handler get_terminate() nothrow;
///
void terminate() nothrow;

static if (__cplusplus < CppStdRevision.cpp17)
{
    ///
    alias unexpected_handler = void function();
    ///
    deprecated unexpected_handler set_unexpected(unexpected_handler f) nothrow;
    ///
    deprecated unexpected_handler get_unexpected() nothrow;
    ///
    deprecated void unexpected();
}

static if (__cplusplus < CppStdRevision.cpp17)
{
    ///
    bool uncaught_exception() nothrow;
}
else static if (__cplusplus == CppStdRevision.cpp17)
{
    ///
    deprecated bool uncaught_exception() nothrow;
}
static if (__cplusplus >= CppStdRevision.cpp17)
{
    ///
    int uncaught_exceptions() nothrow;
}

version (GenericBaseException)
{
    ///
    class exception
    {
    @nogc:
        ///
        extern(D) this() nothrow {}
        ///
        @weak ~this() nothrow {} // HACK: this should extern, but then we have link errors!

        ///
        @weak const(char)* what() const nothrow { return "unknown"; } // HACK: this should extern, but then we have link errors!

    protected:
        extern(D) this(const(char)*, int = 1) nothrow { this(); } // compat with MS derived classes
    }
}
else version (CppRuntime_DigitalMars)
{
    ///
    class exception
    {
    @nogc:
        ///
        extern(D) this() nothrow {}
        //virtual ~this();
        void dtor() { }     // reserve slot in vtbl[]

        ///
        const(char)* what() const nothrow;

    protected:
        this(const(char)*, int = 1) nothrow { this(); } // compat with MS derived classes
    }
}
else version (CppRuntime_Microsoft)
{
    ///
    class exception
    {
    @nogc:
        ///
        extern(D) this(const(char)* message = "unknown", int = 1) nothrow { msg = message; }
        ///
        @weak ~this() nothrow {}

        ///
        @weak const(char)* what() const nothrow { return msg != null ? msg : "unknown exception"; }

        // TODO: do we want this? exceptions are classes... ref types.
//        final ref exception opAssign(ref const(exception) e) nothrow { msg = e.msg; return this; }

    protected:
        @weak void _Doraise() const { assert(0); }

    protected:
        const(char)* msg;
    }

}
else
    static assert(0, "Missing std::exception binding for this platform");

///
class bad_exception : exception
{
@nogc:
    ///
    extern(D) this(const(char)* message = "bad exception") nothrow { super(message); }

    version (GenericBaseException)
    {
        ///
        @weak override const(char)* what() const nothrow { return "bad exception"; }
    }
}
