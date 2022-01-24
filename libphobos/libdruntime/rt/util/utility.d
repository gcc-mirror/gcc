/**
 * Contains various utility functions used by the runtime implementation.
 *
 * Copyright: Copyright Digital Mars 2016.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors: Jacob Carlborg
 * Source: $(DRUNTIMESRC rt/util/_utility.d)
 */
module rt.util.utility;

/**
 * Asserts that the given condition is `true`.
 *
 * The assertion is independent from -release, by abort()ing. Regular assertions
 * throw an AssertError and thus require an initialized GC, which might not be
 * the case (yet or anymore) for the startup/shutdown code in this package
 * (called by CRT ctors/dtors etc.).
 */
package(rt) void safeAssert(
    bool condition, scope string msg, scope string file = __FILE__, size_t line = __LINE__
) nothrow @nogc @safe
{
    import core.internal.abort;
    condition || abort(msg, file, line);
}

// @@@DEPRECATED_2.105@@@
// Remove this when complex types have been removed from the language.
package(rt)
{
    private struct _Complex(T) { T re; T im; }

    enum __c_complex_float : _Complex!float;
    enum __c_complex_double : _Complex!double;
    enum __c_complex_real : _Complex!real;  // This is why we don't use stdc.config

    alias d_cfloat = __c_complex_float;
    alias d_cdouble = __c_complex_double;
    alias d_creal = __c_complex_real;

    enum isComplex(T) = is(T == d_cfloat) || is(T == d_cdouble) || is(T == d_creal);
}
