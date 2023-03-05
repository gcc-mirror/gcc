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
