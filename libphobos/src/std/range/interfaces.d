/**
This module is a submodule of $(MREF std, range).

The main $(MREF std, range) module provides template-based tools for working with
ranges, but sometimes an object-based interface for ranges is needed, such as
when runtime polymorphism is required. For this purpose, this submodule
provides a number of object and `interface` definitions that can be used to
wrap around range objects created by the $(MREF std, range) templates.

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE ,
    $(TR $(TD $(LREF InputRange))
        $(TD Wrapper for input ranges.
    ))
    $(TR $(TD $(LREF InputAssignable))
        $(TD Wrapper for input ranges with assignable elements.
    ))
    $(TR $(TD $(LREF ForwardRange))
        $(TD Wrapper for forward ranges.
    ))
    $(TR $(TD $(LREF ForwardAssignable))
        $(TD Wrapper for forward ranges with assignable elements.
    ))
    $(TR $(TD $(LREF BidirectionalRange))
        $(TD Wrapper for bidirectional ranges.
    ))
    $(TR $(TD $(LREF BidirectionalAssignable))
        $(TD Wrapper for bidirectional ranges with assignable elements.
    ))
    $(TR $(TD $(LREF RandomAccessFinite))
        $(TD Wrapper for finite random-access ranges.
    ))
    $(TR $(TD $(LREF RandomAccessAssignable))
        $(TD Wrapper for finite random-access ranges with assignable elements.
    ))
    $(TR $(TD $(LREF RandomAccessInfinite))
        $(TD Wrapper for infinite random-access ranges.
    ))
    $(TR $(TD $(LREF OutputRange))
        $(TD Wrapper for output ranges.
    ))
    $(TR $(TD $(LREF OutputRangeObject))
        $(TD Class that implements the `OutputRange` interface and wraps the
        `put` methods in virtual functions.
    ))
    $(TR $(TD $(LREF outputRangeObject))
        $(TD Convenience function for creating an `OutputRangeObject` with a base
        range of type R that accepts types E.
    ))
    $(TR $(TD $(LREF InputRangeObject))
        $(TD Class that implements the `InputRange` interface and wraps the
        input range methods in virtual functions.
    ))
    $(TR $(TD $(LREF inputRangeObject))
        $(TD Convenience function for creating an `InputRangeObject`
        of the proper type.
    ))
    $(TR $(TD $(LREF MostDerivedInputRange))
        $(TD Returns the interface type that best matches the range.
    ))
))


Source: $(PHOBOSSRC std/range/interfaces.d)

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu), David Simcha, and
         $(HTTP jmdavisprog.com, Jonathan M Davis). Credit for some of the ideas
         in building this module goes to
         $(HTTP fantascienza.net/leonardo/so/, Leonardo Maffi).
*/
module std.range.interfaces;

import std.meta;
import std.range.primitives;
import std.traits;

/**These interfaces are intended to provide virtual function-based wrappers
 * around input ranges with element type E.  This is useful where a well-defined
 * binary interface is required, such as when a DLL function or virtual function
 * needs to accept a generic range as a parameter. Note that
 * $(REF_ALTTEXT isInputRange, isInputRange, std, range, primitives)
 * and friends check for conformance to structural interfaces
 * not for implementation of these `interface` types.
 *
 * Limitations:
 *
 * These interfaces are not capable of forwarding `ref` access to elements.
 *
 * Infiniteness of the wrapped range is not propagated.
 *
 * Length is not propagated in the case of non-random access ranges.
 *
 * See_Also:
 * $(LREF inputRangeObject)
 */
interface InputRange(E) {
    ///
    @property E front();

    /**Calls $(REF moveFront, std, range, primitives) on the wrapped range, if
     * possible. Otherwise, throws an $(LREF UnsupportedRangeMethod) exception.
     */
    E moveFront();

    ///
    void popFront();

    ///
    @property bool empty();

    /* Measurements of the benefits of using opApply instead of range primitives
     * for foreach, using timings for iterating over an iota(100_000_000) range
     * with an empty loop body, using the same hardware in each case:
     *
     * Bare Iota struct, range primitives:  278 milliseconds
     * InputRangeObject, opApply:           436 milliseconds  (1.57x penalty)
     * InputRangeObject, range primitives:  877 milliseconds  (3.15x penalty)
     */

    /**`foreach` iteration uses opApply, since one delegate call per loop
     * iteration is faster than three virtual function calls.
     */
    int opApply(scope int delegate(E));

    /// Ditto
    int opApply(scope int delegate(size_t, E));

}

///
@safe unittest
{
    import std.algorithm.iteration : map;
    import std.range : iota;

    void useRange(InputRange!int range) {
        // Function body.
    }

    // Create a range type.
    auto squares = map!"a * a"(iota(10));

    // Wrap it in an interface.
    auto squaresWrapped = inputRangeObject(squares);

    // Use it.
    useRange(squaresWrapped);
}

/**Interface for a forward range of type `E`.*/
interface ForwardRange(E) : InputRange!E {
    ///
    @property ForwardRange!E save();
}

/**Interface for a bidirectional range of type `E`.*/
interface BidirectionalRange(E) : ForwardRange!(E) {
    ///
    @property BidirectionalRange!E save();

    ///
    @property E back();

    /**Calls $(REF moveBack, std, range, primitives) on the wrapped range, if
     * possible. Otherwise, throws an $(LREF UnsupportedRangeMethod) exception
     */
    E moveBack();

    ///
    void popBack();
}

/**Interface for a finite random access range of type `E`.*/
interface RandomAccessFinite(E) : BidirectionalRange!(E) {
    ///
    @property RandomAccessFinite!E save();

    ///
    E opIndex(size_t);

    ///
    E moveAt(size_t);

    ///
    @property size_t length();

    ///
    alias opDollar = length;

    // Can't support slicing until issues with requiring slicing for all
    // finite random access ranges are fully resolved.
    version (none)
    {
        ///
        RandomAccessFinite!E opSlice(size_t, size_t);
    }
}

/**Interface for an infinite random access range of type `E`.*/
interface RandomAccessInfinite(E) : ForwardRange!E {
    ///
    enum bool empty = false;

    /**Calls $(REF moveAt, std, range, primitives) on the wrapped range, if
     * possible. Otherwise, throws an $(LREF UnsupportedRangeMethod) exception.
     */
    E moveAt(size_t);

    ///
    @property RandomAccessInfinite!E save();

    ///
    E opIndex(size_t);
}

// https://issues.dlang.org/show_bug.cgi?id=22608
@safe unittest
{
    static assert(isRandomAccessRange!(RandomAccessInfinite!int));
}

/**Adds assignable elements to InputRange.*/
interface InputAssignable(E) : InputRange!E {
    ///
    @property void front(E newVal);

    alias front = InputRange!E.front; // overload base interface method
}

@safe unittest
{
    static assert(isInputRange!(InputAssignable!int));
}

/**Adds assignable elements to ForwardRange.*/
interface ForwardAssignable(E) : InputAssignable!E, ForwardRange!E {
    ///
    @property ForwardAssignable!E save();
}

/**Adds assignable elements to BidirectionalRange.*/
interface BidirectionalAssignable(E) : ForwardAssignable!E, BidirectionalRange!E {
    ///
    @property BidirectionalAssignable!E save();

    ///
    @property void back(E newVal);
}

/**Adds assignable elements to RandomAccessFinite.*/
interface RandomFiniteAssignable(E) : RandomAccessFinite!E, BidirectionalAssignable!E {
    ///
    @property RandomFiniteAssignable!E save();

    ///
    void opIndexAssign(E val, size_t index);
}

/**Interface for an output range of type `E`.  Usage is similar to the
 * `InputRange` interface and descendants.*/
interface OutputRange(E) {
    ///
    void put(E);
}

// https://issues.dlang.org/show_bug.cgi?id=6973
@safe unittest
{
    static assert(isOutputRange!(OutputRange!int, int));
}


// CTFE function that generates mixin code for one put() method for each
// type E.
private string putMethods(E...)()
{
    import std.conv : to;

    string ret;

    foreach (ti, Unused; E)
    {
        ret ~= "void put(E[" ~ to!string(ti) ~ "] e) { .put(_range, e); }";
    }

    return ret;
}

/**Implements the `OutputRange` interface for all types E and wraps the
 * `put` method for each type `E` in a virtual function.
 */
class OutputRangeObject(R, E...) : staticMap!(OutputRange, E) {
    // @BUG 4689:  There should be constraints on this template class, but
    // DMD won't let me put them in.
    private R _range;

    ///
    this(R range) {
        this._range = range;
    }

    mixin(putMethods!E());
}


/**Returns the interface type that best matches `R`.*/
template MostDerivedInputRange(R)
if (isInputRange!(Unqual!R))
{
    private alias E = ElementType!R;

    static if (isRandomAccessRange!R)
    {
        static if (isInfinite!R)
        {
            alias MostDerivedInputRange = RandomAccessInfinite!E;
        }
        else static if (hasAssignableElements!R)
        {
            alias MostDerivedInputRange = RandomFiniteAssignable!E;
        }
        else
        {
            alias MostDerivedInputRange = RandomAccessFinite!E;
        }
    }
    else static if (isBidirectionalRange!R)
    {
        static if (hasAssignableElements!R)
        {
            alias MostDerivedInputRange = BidirectionalAssignable!E;
        }
        else
        {
            alias MostDerivedInputRange = BidirectionalRange!E;
        }
    }
    else static if (isForwardRange!R)
    {
        static if (hasAssignableElements!R)
        {
            alias MostDerivedInputRange = ForwardAssignable!E;
        }
        else
        {
            alias MostDerivedInputRange = ForwardRange!E;
        }
    }
    else
    {
        static if (hasAssignableElements!R)
        {
            alias MostDerivedInputRange = InputAssignable!E;
        }
        else
        {
            alias MostDerivedInputRange = InputRange!E;
        }
    }
}

/**Implements the most derived interface that `R` works with and wraps
 * all relevant range primitives in virtual functions.  If `R` is already
 * derived from the `InputRange` interface, aliases itself away.
 */
template InputRangeObject(R)
if (isInputRange!(Unqual!R))
{
    static if (is(R : InputRange!(ElementType!R)))
    {
        alias InputRangeObject = R;
    }
    else static if (!is(Unqual!R == R))
    {
        alias InputRangeObject = InputRangeObject!(Unqual!R);
    }
    else
    {

        ///
        class InputRangeObject : MostDerivedInputRange!(R) {
            private R _range;
            private alias E = ElementType!R;

            this(R range) {
                this._range = range;
            }

            @property E front() { return _range.front; }

            E moveFront() {
                static if (__traits(compiles, _range.moveFront()))
                    return _range.moveFront();
                else
                    throw new UnsupportedRangeMethod(
                        "Cannot move the front of a(n) `" ~ R.stringof ~ "`");
            }

            void popFront() { _range.popFront(); }
            @property bool empty() { return _range.empty; }

            static if (isForwardRange!R)
            {
                @property typeof(this) save() {
                    return new typeof(this)(_range.save);
                }
            }

            static if (hasAssignableElements!R)
            {
                @property void front(E newVal) {
                    _range.front = newVal;
                }
            }

            static if (isBidirectionalRange!R)
            {
                @property E back() { return _range.back; }

                E moveBack() {
                    static if (__traits(compiles, _range.moveFront()))
                        return _range.moveBack();
                    else
                        throw new UnsupportedRangeMethod(
                            "Cannot move the back of a(n) `" ~ R.stringof ~ "`");
                }

                void popBack() { return _range.popBack(); }

                static if (hasAssignableElements!R)
                {
                    @property void back(E newVal) {
                        _range.back = newVal;
                    }
                }
            }

            static if (isRandomAccessRange!R)
            {
                E opIndex(size_t index) {
                    return _range[index];
                }

                E moveAt(size_t index) {
                    static if (__traits(compiles, _range.moveAt(index)))
                        return _range.moveAt(index);
                    else
                        throw new UnsupportedRangeMethod(
                            "Cannot move an element of a(n) `" ~ R.stringof ~ "`");
                }

                static if (hasAssignableElements!R)
                {
                    void opIndexAssign(E val, size_t index) {
                        _range[index] = val;
                    }
                }

                static if (!isInfinite!R)
                {
                    @property size_t length() {
                        return _range.length;
                    }

                    alias opDollar = length;

                    // Can't support slicing until all the issues with
                    // requiring slicing support for finite random access
                    // ranges are resolved.
                    version (none)
                    {
                        typeof(this) opSlice(size_t lower, size_t upper) {
                            return new typeof(this)(_range[lower .. upper]);
                        }
                    }
                }
            }

            // Optimization:  One delegate call is faster than three virtual
            // function calls.  Use opApply for foreach syntax.
            int opApply(scope int delegate(E) dg) {
                int res;

                for (auto r = _range; !r.empty; r.popFront())
                {
                    res = dg(r.front);
                    if (res) break;
                }

                return res;
            }

            int opApply(scope int delegate(size_t, E) dg) {
                int res;

                size_t i = 0;
                for (auto r = _range; !r.empty; r.popFront())
                {
                    res = dg(i, r.front);
                    if (res) break;
                    i++;
                }

                return res;
            }
        }
    }
}

/**Convenience function for creating an `InputRangeObject` of the proper type.
 * See $(LREF InputRange) for an example.
 */
InputRangeObject!R inputRangeObject(R)(R range)
if (isInputRange!R)
{
    static if (is(R : InputRange!(ElementType!R)))
    {
        return range;
    }
    else
    {
        return new InputRangeObject!R(range);
    }
}

/**Convenience function for creating an `OutputRangeObject` with a base range
 * of type `R` that accepts types `E`.
*/
template outputRangeObject(E...) {

    ///
    OutputRangeObject!(R, E) outputRangeObject(R)(R range) {
        return new OutputRangeObject!(R, E)(range);
    }
}

///
@safe unittest
{
     import std.array;
     auto app = appender!(uint[])();
     auto appWrapped = outputRangeObject!(uint, uint[])(app);
     static assert(is(typeof(appWrapped) : OutputRange!(uint[])));
     static assert(is(typeof(appWrapped) : OutputRange!(uint)));
}

/// Thrown when an interface method is not supported by the wrapped range
class UnsupportedRangeMethod : Exception
{
    import std.exception : basicExceptionCtors;

    mixin basicExceptionCtors;
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.array;
    import std.internal.test.dummyrange;

    static void testEquality(R)(iInputRange r1, R r2) {
        assert(equal(r1, r2));
    }

    auto arr = [1,2,3,4];
    RandomFiniteAssignable!int arrWrapped = inputRangeObject(arr);
    static assert(isRandomAccessRange!(typeof(arrWrapped)));
    //    static assert(hasSlicing!(typeof(arrWrapped)));
    static assert(hasLength!(typeof(arrWrapped)));
    arrWrapped[0] = 0;
    assert(arr[0] == 0);
    assert(arr.moveFront() == 0);
    assert(arr.moveBack() == 4);
    assert(arr.moveAt(1) == 2);

    foreach (elem; arrWrapped) {}
    foreach (i, elem; arrWrapped) {}

    assert(inputRangeObject(arrWrapped) is arrWrapped);

    foreach (DummyType; AllDummyRanges)
    {
        auto d = DummyType.init;
        static assert(propagatesRangeType!(DummyType,
                        typeof(inputRangeObject(d))));
        static assert(propagatesRangeType!(DummyType,
                        MostDerivedInputRange!DummyType));
        InputRange!uint wrapped = inputRangeObject(d);
        assert(equal(wrapped, d));
    }

    // Test output range stuff.
    auto app = appender!(uint[])();
    auto appWrapped = outputRangeObject!(uint, uint[])(app);
    static assert(is(typeof(appWrapped) : OutputRange!(uint[])));
    static assert(is(typeof(appWrapped) : OutputRange!(uint)));

    appWrapped.put(1);
    appWrapped.put([2, 3]);
    assert(app.data.length == 3);
    assert(equal(app.data, [1,2,3]));
}

// https://issues.dlang.org/show_bug.cgi?id=19544
@safe unittest
{
    import std.range : repeat;

    static struct HasCC
    {
        inout this(ref inout typeof(this)) {}
    }

    auto r = repeat(HasCC.init).inputRangeObject;
}
