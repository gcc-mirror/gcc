/**
 This module contains support for controlling dynamic arrays' concatenation
  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/internal/_array/_concatenation.d)
*/
module core.internal.array.concatenation;

/// See $(REF _d_arraycatnTX, rt,lifetime)
private extern (C) void[] _d_arraycatnTX(const TypeInfo ti, scope byte[][] arrs) pure nothrow;

/// Implementation of `_d_arraycatnTX` and `_d_arraycatnTXTrace`
template _d_arraycatnTXImpl(Tarr : ResultArrT[], ResultArrT : T[], T)
{
    private enum errorMessage = "Cannot concatenate arrays if compiling without support for runtime type information!";

    /**
    * Concatenating the arrays inside of `arrs`.
    * `_d_arraycatnTX([a, b, c])` means `a ~ b ~ c`.
    * Params:
    *  arrs = Array containing arrays that will be concatenated.
    * Returns:
    *  A newly allocated array that contains all the elements from all the arrays in `arrs`.
    * Bugs:
    *  This function template was ported from a much older runtime hook that bypassed safety,
    *  purity, and throwabilty checks. To prevent breaking existing code, this function template
    *  is temporarily declared `@trusted pure nothrow` until the implementation can be brought up to modern D expectations.
    */
    ResultArrT _d_arraycatnTX(scope const Tarr arrs) @trusted pure nothrow
    {
        pragma(inline, false);
        version (D_TypeInfo)
        {
            auto ti = typeid(ResultArrT);

            byte[][] arrs2 = (cast(byte[]*)arrs.ptr)[0 .. arrs.length];
            void[] result = ._d_arraycatnTX(ti, arrs2);
            return (cast(T*)result.ptr)[0 .. result.length];
        }
        else
            assert(0, errorMessage);
    }

    version (D_ProfileGC)
    {
        import core.internal.array.utils : _d_HookTraceImpl;

        /**
         * TraceGC wrapper around $(REF _d_arraycatnTX, core,internal,array,concat).
         * Bugs:
         *  This function template was ported from a much older runtime hook that bypassed safety,
         *  purity, and throwabilty checks. To prevent breaking existing code, this function template
         *  is temporarily declared `@trusted pure nothrow` until the implementation can be brought up to modern D expectations.
         */
        alias _d_arraycatnTXTrace = _d_HookTraceImpl!(ResultArrT, _d_arraycatnTX, errorMessage);
    }
}

@safe unittest
{
    int counter;
    struct S
    {
        int val;
        this(this)
        {
            counter++;
        }
    }

    S[][] arr = [[S(0), S(1), S(2), S(3)], [S(4), S(5), S(6), S(7)]];
    S[] result = _d_arraycatnTXImpl!(typeof(arr))._d_arraycatnTX(arr);

    assert(counter == 8);
    assert(result == [S(0), S(1), S(2), S(3), S(4), S(5), S(6), S(7)]);
}
