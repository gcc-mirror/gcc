// Written in the D programming language

/**
 * Internal math utilities.
 *
 * Copyright: The D Language Foundation 2021.
 * License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors: Luís Ferreira
 * Source: $(DRUNTIMESRC core/internal/util/_math.d)
 */
module core.internal.util.math;

/**
 * Calculates the maximum of the passed arguments
 * Params:
 *   a = first value to select the maximum from
 *   b = second value to select the maximum from
 * Returns: The maximum of the passed-in values.
 */
T max(T)(T a, T b) pure nothrow @nogc @safe
{
    return b > a ? b : a;
}

/**
 * Calculates the minimum of the passed arguments
 * Params:
 *   a = first value to select the minimum from
 *   b = second value to select the minimum from
 * Returns: The minimum of the passed-in values.
 */
T min(T)(T a, T b) pure nothrow @nogc @safe
{
    return b < a ? b : a;
}

///
@safe pure @nogc nothrow
unittest
{
    assert(max(1,3) == 3);
    assert(max(3,1) == 3);
    assert(max(1,1) == 1);
}

///
@safe pure @nogc nothrow
unittest
{
    assert(min(1,3) == 1);
    assert(min(3,1) == 1);
    assert(min(1,1) == 1);
}
