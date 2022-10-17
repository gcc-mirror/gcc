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
