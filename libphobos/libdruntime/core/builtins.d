/**********************************************
To provide access to features that would be otherwise counterproductive or
difficult to implement, compilers provide an interface consisting of a set
of builtins (also called intrinsics) which can be called like normal functions.

This module exposes builtins both common to all D compilers
(those provided by the frontend) and specific to the host compiler i.e. those
specific to either LLVM or GCC (`ldc.intrinsics` and `gcc.builtins` are publicly imported, respectively).
Host-specific intrinsics cannot be reliably listed here, however listings can be found
at the documentation for the relevant backends, i.e.
$(LINK2 https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html, GCC) and
$(LINK2 https://llvm.org/docs/LangRef.html, LLVM). It should be noted that not all
builtins listed are necessarily supported by the host compiler, please file a bug
if this is the case for your workload.

Use of this module reduces the amount of conditional compilation needed
to use a given builtin. For example, to write a target independent function
that uses prefetching we can write the following:
---
float usePrefetch(float[] x)
{
    // There is only one import statement required rather than two (versioned) imports
    import core.builtins;
    version (GNU)
        __builtin_prefetch(x.ptr);
    version (LDC)
        /+
            For the curious: 0, 3, 1 mean `x` will only be read-from (0), it will be used
            very often (3), and it should be fetched to the data-cache (1).
        +/
        llvm_prefetch(x.ptr, 0, 3, 1);
    const doMath = blahBlahBlah;
    return doMath;
}
---


Copyright: Copyright © 2021, The D Language Foundation
License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
Authors:   Walter Bright
Source:    $(DRUNTIMESRC core/builtins.d)
*/

module core.builtins;

version (GNU)
    public import gcc.builtins;

version (LDC)
    public import ldc.intrinsics;

/// Writes `s` to `stderr` during CTFE (does nothing at runtime).
void __ctfeWrite(scope const(char)[] s) @nogc @safe pure nothrow {}

version (GNU)
{
    /// https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html#index-_005f_005fbuiltin_005fexpect
    alias expect = __builtin_expect;
    /// https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html#index-_005f_005fbuiltin_005ftrap
    alias trap = __builtin_trap;
}
else version (LDC)
{
    /// https://llvm.org/docs/LangRef.html#llvm-expect-intrinsic
    alias expect = llvm_expect;
    debug
        /// https://llvm.org/docs/LangRef.html#llvm-debugtrap-intrinsic
        alias trap = llvm_debugtrap;
    else
        /// https://llvm.org/docs/LangRef.html#llvm-trap-intrinsic
        alias trap = llvm_trap;
}
else version (DigitalMars)
{
    pragma(inline, true)
    T expect(T)(T val, T expected) if (__traits(isIntegral, T))
    {
        return val;
    }

    /// Execute target dependent trap instruction, if supported.
    /// Otherwise, abort execution.
    pragma(inline, true)
    void trap()
    {
        debug
        {
            version(D_InlineAsm_X86)
                asm nothrow @nogc pure @trusted { int 3; }
        }
        assert(0);
    }
}

/// Provide static branch and value hints for the LDC/GDC compilers.
/// DMD ignores these hints.
pragma(inline, true) bool likely()(bool b)   { return !!expect(b, true);  }
/// ditto
pragma(inline, true) bool unlikely()(bool b) { return !!expect(b, false); }

///
@nogc nothrow pure @safe unittest
{
    int x = 12;

    expect(x, 12);

    if (likely(x > 0))
    {
        // ...
    }
    else if (unlikely(x == int.min))
    {
        // ...
    }
}
