// Written in the D programming language.

/**
 * Demangle D mangled names.
 *
 * Copyright: Copyright The D Language Foundation 2000 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright),
 *                        Thomas K$(UUML)hne, Frits van Bommel
 * Source:    $(PHOBOSSRC std/demangle.d)
 * $(SCRIPT inhibitQuickIndex = 1;)
 */
/*
 *          Copyright The D Language Foundation 2000 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.demangle;

/**
Demangle D mangled names.

Params:
    name = the mangled name
Returns:
    A `string`. If it is not a D mangled name, it returns its argument name.
 */
string demangle(string name) @safe pure nothrow
{
    import core.demangle : demangle;
    import std.exception : assumeUnique;
    auto ret = demangle(name);
    return () @trusted { return ret.assumeUnique; } ();
}

///
@safe pure unittest
{
    // int b in module a
    assert(demangle("_D1a1bi") == "int a.b");
    // char array foo in module test
    assert(demangle("_D4test3fooAa") == "char[] test.foo");
}

/**
This program reads standard in and writes it to standard out,
pretty-printing any found D mangled names.
 */
@system unittest
{
    import std.ascii : isAlphaNum;
    import std.algorithm.iteration : chunkBy, joiner, map;
    import std.algorithm.mutation : copy;
    import std.conv : to;
    import std.demangle : demangle;
    import std.functional : pipe;
    import std.stdio : stdin, stdout;

    void main()
    {
        stdin.byLineCopy
            .map!(
                l => l.chunkBy!(a => isAlphaNum(a) || a == '_')
                      .map!(a => a[1].pipe!(to!string, demangle)).joiner
            )
            .copy(stdout.lockingTextWriter);
    }
}
