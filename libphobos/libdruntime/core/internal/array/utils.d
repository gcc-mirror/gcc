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

alias BlkInfo = GC.BlkInfo;
alias BlkAttr = GC.BlkAttr;

private
{
    enum : size_t
    {
        PAGESIZE = 4096,
        BIGLENGTHMASK = ~(PAGESIZE - 1),
        SMALLPAD = 1,
        MEDPAD = ushort.sizeof,
        LARGEPREFIX = 16, // 16 bytes padding at the front of the array
        LARGEPAD = LARGEPREFIX + 1,
        MAXSMALLSIZE = 256-SMALLPAD,
        MAXMEDSIZE = (PAGESIZE / 2) - MEDPAD
    }
}

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
            version (tracegc)
        } ~ "{\n" ~ q{
                import core.stdc.stdio;

                printf("%sTrace file = '%.*s' line = %d function = '%.*s' type = %.*s\n",
                } ~ "\"" ~ Hook ~ "\".ptr," ~ q{
                    file.length, file.ptr,
                    line,
                    funcname.length, funcname.ptr,
                    name.length, name.ptr
                );
            } ~ "}\n" ~ q{
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
    auto _d_HookTraceImpl(T, alias Hook, string errorMessage)(string file, int line, string funcname, Parameters!Hook parameters) @trusted pure
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
 * Clear padding that might not be zeroed by the GC (it assumes it is within the
 * requested size from the start, but it is actually at the end of the allocated
 * block).
 *
 * Params:
 *  info = array allocation data
 *  arrSize = size of the array in bytes
 *  padSize = size of the padding in bytes
 */
void __arrayClearPad()(ref BlkInfo info, size_t arrSize, size_t padSize) nothrow pure
{
    import core.stdc.string;
    if (padSize > MEDPAD && !(info.attr & BlkAttr.NO_SCAN) && info.base)
    {
        if (info.size < PAGESIZE)
            memset(info.base + arrSize, 0, padSize);
        else
            memset(info.base, 0, LARGEPREFIX);
    }
}

/**
 * Allocate an array memory block by applying the proper padding and assigning
 * block attributes if not inherited from the existing block.
 *
 * Params:
 *  arrSize = size of the allocated array in bytes
 * Returns:
 *  `BlkInfo` with allocation metadata
 */
BlkInfo __arrayAlloc(T)(size_t arrSize) @trusted
{
    import core.checkedint;
    import core.lifetime : TypeInfoSize;
    import core.internal.traits : hasIndirections;

    enum typeInfoSize = TypeInfoSize!T;
    BlkAttr attr = BlkAttr.APPENDABLE;

    size_t padSize = arrSize > MAXMEDSIZE ?
        LARGEPAD :
        ((arrSize > MAXSMALLSIZE ? MEDPAD : SMALLPAD) + typeInfoSize);

    bool overflow;
    auto paddedSize = addu(arrSize, padSize, overflow);

    if (overflow)
        return BlkInfo();

    /* `extern(C++)` classes don't have a classinfo pointer in their vtable,
     * so the GC can't finalize them.
     */
    static if (typeInfoSize)
        attr |= BlkAttr.STRUCTFINAL | BlkAttr.FINALIZE;
    static if (!hasIndirections!T)
        attr |= BlkAttr.NO_SCAN;

    auto bi = GC.qalloc(paddedSize, attr, typeid(T));
    __arrayClearPad(bi, arrSize, padSize);
    return bi;
}

/**
 * Get the start of the array for the given block.
 *
 * Params:
 *  info = array metadata
 * Returns:
 *  pointer to the start of the array
 */
void *__arrayStart()(return scope BlkInfo info) nothrow pure
{
    return info.base + ((info.size & BIGLENGTHMASK) ? LARGEPREFIX : 0);
}

/**
 * Set the allocated length of the array block.  This is called when an array
 * is appended to or its length is set.
 *
 * The allocated block looks like this for blocks < PAGESIZE:
 * `|elem0|elem1|elem2|...|elemN-1|emptyspace|N*elemsize|`
 *
 * The size of the allocated length at the end depends on the block size:
 *      a block of 16 to 256 bytes has an 8-bit length.
 *      a block with 512 to pagesize/2 bytes has a 16-bit length.
 *
 * For blocks >= pagesize, the length is a size_t and is at the beginning of the
 * block.  The reason we have to do this is because the block can extend into
 * more pages, so we cannot trust the block length if it sits at the end of the
 * block, because it might have just been extended.  If we can prove in the
 * future that the block is unshared, we may be able to change this, but I'm not
 * sure it's important.
 *
 * In order to do put the length at the front, we have to provide 16 bytes
 * buffer space in case the block has to be aligned properly.  In x86, certain
 * SSE instructions will only work if the data is 16-byte aligned.  In addition,
 * we need the sentinel byte to prevent accidental pointers to the next block.
 * Because of the extra overhead, we only do this for page size and above, where
 * the overhead is minimal compared to the block size.
 *
 * So for those blocks, it looks like:
 * `|N*elemsize|padding|elem0|elem1|...|elemN-1|emptyspace|sentinelbyte|``
 *
 * where `elem0` starts 16 bytes after the first byte.
 */
bool __setArrayAllocLength(T)(ref BlkInfo info, size_t newLength, bool isShared, size_t oldLength = ~0)
{
    import core.atomic;
    import core.lifetime : TypeInfoSize;

    size_t typeInfoSize = TypeInfoSize!T;

    if (info.size <= 256)
    {
        import core.checkedint;

        bool overflow;
        auto newLengthPadded = addu(newLength,
                                     addu(SMALLPAD, typeInfoSize, overflow),
                                     overflow);

        if (newLengthPadded > info.size || overflow)
            // new size does not fit inside block
            return false;

        auto length = cast(ubyte *)(info.base + info.size - typeInfoSize - SMALLPAD);
        if (oldLength != ~0)
        {
            if (isShared)
            {
                return cas(cast(shared)length, cast(ubyte)oldLength, cast(ubyte)newLength);
            }
            else
            {
                if (*length == cast(ubyte)oldLength)
                    *length = cast(ubyte)newLength;
                else
                    return false;
            }
        }
        else
        {
            // setting the initial length, no cas needed
            *length = cast(ubyte)newLength;
        }
        if (typeInfoSize)
        {
            auto typeInfo = cast(TypeInfo*)(info.base + info.size - size_t.sizeof);
            *typeInfo = cast()typeid(T);
        }
    }
    else if (info.size < PAGESIZE)
    {
        if (newLength + MEDPAD + typeInfoSize > info.size)
            // new size does not fit inside block
            return false;
        auto length = cast(ushort *)(info.base + info.size - typeInfoSize - MEDPAD);
        if (oldLength != ~0)
        {
            if (isShared)
            {
                return cas(cast(shared)length, cast(ushort)oldLength, cast(ushort)newLength);
            }
            else
            {
                if (*length == oldLength)
                    *length = cast(ushort)newLength;
                else
                    return false;
            }
        }
        else
        {
            // setting the initial length, no cas needed
            *length = cast(ushort)newLength;
        }
        if (typeInfoSize)
        {
            auto typeInfo = cast(TypeInfo*)(info.base + info.size - size_t.sizeof);
            *typeInfo = cast()typeid(T);
        }
    }
    else
    {
        if (newLength + LARGEPAD > info.size)
            // new size does not fit inside block
            return false;
        auto length = cast(size_t *)(info.base);
        if (oldLength != ~0)
        {
            if (isShared)
            {
                return cas(cast(shared)length, cast(size_t)oldLength, cast(size_t)newLength);
            }
            else
            {
                if (*length == oldLength)
                    *length = newLength;
                else
                    return false;
            }
        }
        else
        {
            // setting the initial length, no cas needed
            *length = newLength;
        }
        if (typeInfoSize)
        {
            auto typeInfo = cast(TypeInfo*)(info.base + size_t.sizeof);
            *typeInfo = cast()typeid(T);
        }
    }
    return true; // resize succeeded
}
