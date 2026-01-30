/**
 This module contains utility functions to help the implementation of the runtime hook

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/internal/_array/_utils.d)
*/
module core.internal.array.utils;

import core.internal.traits : Parameters;
import core.memory : GC;

alias BlkAttr = GC.BlkAttr;

auto gcStatsPure() nothrow pure
{
    import core.memory : GC;
    auto impureBypass = cast(GC.Stats function() pure nothrow)&GC.stats;
    return impureBypass();
}

ulong accumulatePure(string file, int line, string funcname, string name, ulong size) nothrow pure
{
    static ulong impureBypass(string file, int line, string funcname, string name, ulong size) @nogc nothrow
    {
        import core.internal.traits : externDFunc;

        alias accumulate = externDFunc!("rt.profilegc.accumulate", void function(string file, uint line, string funcname, string type, ulong sz) @nogc nothrow);
        accumulate(file, line, funcname, name, size);
        return size;
    }

    auto func = cast(ulong function(string file, int line, string funcname, string name, ulong size) @nogc nothrow pure)&impureBypass;
    return func(file, line, funcname, name, size);
}

version (D_ProfileGC)
{
    /**
     * TraceGC wrapper generator around the runtime hook `Hook`.
     * Params:
     *   Type = The type of hook to report to accumulate
     *   Hook = The name hook to wrap
     */
    template TraceHook(string Type, string Hook)
    {
        const char[] TraceHook = q{
            import core.internal.array.utils : gcStatsPure, accumulatePure;

            pragma(inline, false);
            string name = } ~ "`" ~ Type ~ "`;" ~ q{

            // FIXME: use rt.tracegc.accumulator when it is accessable in the future.
            ulong currentlyAllocated = gcStatsPure().allocatedInCurrentThread;

            scope(exit)
            {
                ulong size = gcStatsPure().allocatedInCurrentThread - currentlyAllocated;
                if (size > 0)
                    if (!accumulatePure(file, line, funcname, name, size)) {
                        // This 'if' and 'assert' is needed to force the compiler to not remove the call to
                        // `accumulatePure`. It really want to do that while optimizing as the function is
                        // `pure` and it does not influence the result of this hook.

                        // `accumulatePure` returns the value of `size`, which can never be zero due to the
                        // previous 'if'. So this assert will never be triggered.
                        assert(0);
                    }
            }
        };
    }

    /**
     * TraceGC wrapper around runtime hook `Hook`.
     * Params:
     *  T = Type of hook to report to accumulate
     *  Hook = The hook to wrap
     *  errorMessage = The error message incase `version != D_TypeInfo`
     *  file = File that called `_d_HookTraceImpl`
     *  line = Line inside of `file` that called `_d_HookTraceImpl`
     *  funcname = Function that called `_d_HookTraceImpl`
     *  parameters = Parameters that will be used to call `Hook`
     * Bugs:
     *  This function template needs be between the compiler and a much older runtime hook that bypassed safety,
     *  purity, and throwabilty checks. To prevent breaking existing code, this function template
     *  is temporarily declared `@trusted pure` until the implementation can be brought up to modern D expectations.
    */
    auto _d_HookTraceImpl(T, alias Hook, string errorMessage)(Parameters!Hook parameters, string file = __FILE__, int line = __LINE__, string funcname = __FUNCTION__) @trusted pure
    {
        version (D_TypeInfo)
        {
            mixin(TraceHook!(T.stringof, __traits(identifier, Hook)));
            return Hook(parameters);
        }
        else
            assert(0, errorMessage);
    }
}

/**
 * Check if the function `F` is calleable in a `nothrow` scope.
 * Params:
 *  F = Function that does not take any parameters
 * Returns:
 *  if the function is callable in a `nothrow` scope.
 */
enum isNoThrow(alias F) = is(typeof(() nothrow { F(); }));

/**
 * Check if the type `T`'s postblit is called in nothrow, if it exist
 * Params:
 *  T = Type to check
 * Returns:
 *  if the postblit is callable in a `nothrow` scope, if it exist.
 *  if it does not exist, return true.
 */
template isPostblitNoThrow(T) {
    static if (__traits(isStaticArray, T))
        enum isPostblitNoThrow = isPostblitNoThrow!(typeof(T.init[0]));
    else static if (__traits(hasMember, T, "__xpostblit") &&
        // Bugzilla 14746: Check that it's the exact member of S.
        __traits(isSame, T, __traits(parent, T.init.__xpostblit)))
        enum isPostblitNoThrow = isNoThrow!(T.init.__xpostblit);
    else
        enum isPostblitNoThrow = true;
}

/**
 * Allocate a memory block with appendable capabilities for array usage.
 *
 * Params:
 *  arrSize = size of the allocated array in bytes
 * Returns:
 *  `void[]` matching requested size on success, `null` on failure.
 */
void[] __arrayAlloc(T)(size_t arrSize) @trusted
{
    import core.lifetime : TypeInfoSize;
    import core.internal.traits : hasIndirections;

    enum typeInfoSize = TypeInfoSize!T;
    BlkAttr attr = BlkAttr.APPENDABLE;

    /* `extern(C++)` classes don't have a classinfo pointer in their vtable,
     * so the GC can't finalize them.
     */
    static if (typeInfoSize)
        attr |= BlkAttr.FINALIZE;
    static if (!hasIndirections!T)
        attr |= BlkAttr.NO_SCAN;

    auto ptr = GC.malloc(arrSize, attr, typeid(T));
    if (ptr)
        return ptr[0 .. arrSize];
    return null;
}

/**
Given an array of length `size` that needs to be expanded to `newlength`,
compute a new capacity.

Better version by Dave Fladebo, enhanced by Steven Schveighoffer:
This uses an inverse logorithmic algorithm to pre-allocate a bit more
space for larger arrays.
- The maximum "extra" space is about 80% of the requested space. This is for
PAGE size and smaller.
- As the arrays grow, the relative pre-allocated space shrinks.
- Perhaps most importantly, overall memory usage and stress on the GC
is decreased significantly for demanding environments.
- The algorithm is tuned to avoid any division at runtime.

Params:
    newlength = new `.length`
    elemsize = size of the element in the new array
Returns: new capacity for array
*/
size_t newCapacity(size_t newlength, size_t elemsize) pure nothrow
{
    size_t newcap = newlength * elemsize;

    /*
     * Max growth factor numerator is 234, so allow for multiplying by 256.
     * But also, the resulting size cannot be more than 2x, so prevent
     * growing if 2x would fill up the address space (for 32-bit)
     */
    enum largestAllowed = (ulong.max >> 8) & (size_t.max >> 1);
    if (!newcap || (newcap & ~largestAllowed))
        return newcap;

    /*
     * The calculation for "extra" space depends on the requested capacity.
     * We use an inverse logarithm of the new capacity to add an extra 15%
     * to 83% capacity. Note that normally we humans think in terms of
     * percent, but using 128 instead of 100 for the denominator means we
     * can avoid all division by simply bit-shifthing. Since there are only
     * 64 bits in a long, the bsr of a size_t is going to be 0 - 63. Using
     * a lookup table allows us to precalculate the multiplier based on the
     * inverse logarithm. The formula rougly is:
     *
     * newcap = request * (1.0 + min(0.83, 10.0 / (log(request) + 1)))
     */
    import core.bitop;
    static immutable multTable = (){
        assert(__ctfe);
        ulong[size_t.sizeof * 8] result;
        foreach (i; 0 .. result.length)
        {
            auto factor = 128 + 1280 / (i + 1);
            result[i] = factor > 234 ? 234 : factor;
        }
        return result;
    }();

    auto mult = multTable[bsr(newcap)];

    // if this were per cent, then the code would look like:
    // ((newlength * mult + 99) / 100) * elemsize
    newcap = cast(size_t)((newlength * mult + 127) >> 7) * elemsize;
    debug(PRINTF) printf("mult: %2.2f, alloc: %2.2f\n",mult/128.0,newcap / cast(double)elemsize);
    debug(PRINTF) printf("newcap = %zd, newlength = %zd, elemsize = %zd\n", newcap, newlength, elemsize);
    return newcap;
}

uint __typeAttrs(T)(void *copyAttrsFrom = null)
{
    import core.internal.traits : hasIndirections, hasElaborateDestructor;
    import core.memory : GC;

    alias BlkAttr = GC.BlkAttr;

    if (copyAttrsFrom)
    {
        // try to copy attrs from the given block
        auto info = GC.query(copyAttrsFrom);
        if (info.base)
            return info.attr;
    }

    uint attrs = 0;
    static if (!hasIndirections!T)
        attrs |= BlkAttr.NO_SCAN;

    static if (is(T == struct) && hasElaborateDestructor!T)
        attrs |= BlkAttr.FINALIZE;

    return attrs;
}
