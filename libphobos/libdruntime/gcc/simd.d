// GNU D Compiler SIMD support functions and intrinsics.
// Copyright (C) 2022-2025 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

module gcc.simd;

pure:
nothrow:
@safe:
@nogc:
pragma(inline, true):

/**
* Emit prefetch instruction.
* Params:
*    address = address to be prefetched
*    writeFetch = true for write fetch, false for read fetch
*    locality = 0..3 (0 meaning least local, 3 meaning most local)
*/
void prefetch(bool writeFetch, ubyte locality)(const(void)* address)
{
    static assert(locality < 4, "0..3 expected for locality");
    import gcc.builtins : __builtin_prefetch;
    __builtin_prefetch(address, writeFetch, locality);
}

/**
 * Load unaligned vector from address.
 * This is a compiler intrinsic.
 * Params:
 *    p = pointer to vector
 * Returns:
 *    vector
 */
V loadUnaligned(V)(const V* p) if (isVectorType!V);

/**
 * Store vector to unaligned address.
 * This is a compiler intrinsic.
 * Params:
 *    p = pointer to vector
 *    value = value to store
 * Returns:
 *    value
 */
V storeUnaligned(V)(V* p, V value) if (isVectorType!V);

/**
 * Construct a permutation of elements from one or two vectors, returning a
 * vector of the same type as the input vector(s). The `mask` is an integral
 * vector with the same width and element count as the output vector.
 * Params:
 *    op1 = input vector
 *    op2 = input vector
 *    mask = integer vector mask
 * Returns:
 *    vector with the same type as `op1` and `op2`
 * Example:
 * ---
 * int4 a = [1, 2, 3, 4];
 * int4 b = [5, 6, 7, 8];
 * int4 mask1 = [0, 1, 1, 3];
 * int4 mask2 = [0, 4, 2, 5];
 * assert(shuffle(a, mask1).array == [1, 2, 2, 4]);
 * assert(shuffle(a, b, mask2).array == [1, 5, 3, 6]);
 * ---
 */
template shuffle(V0, V1, M)
{
    static assert(isVectorType!V0, "first argument must be vector");
    static assert(isVectorType!V1, "second argument must be vector");
    static assert(is(BaseType!V0 == BaseType!V1),
                  "first and second argument vectors must have the same element type");
    static assert(isVectorType!M && is(BaseType!M : long),
                  "last argument must be an integer vector");
    static assert(numElements!V0 == numElements!M && numElements!V1 == numElements!M,
                  "argument vectors and mask vector should have the same number of elements");
    static assert(BaseType!V0.sizeof == BaseType!M.sizeof,
                  "argument vectors and mask vector should have the same element type size");

    V0 shuffle(V0 op1, V1 op2, M mask);
}

/// Ditto
template shuffle(V, M)
{
    static assert(isVectorType!V, "first argument must be a vector");
    static assert(isVectorType!M && is(BaseType!M : long),
                  "last argument must be an integer vector");
    static assert(numElements!V == numElements!M,
                  "argument vector and mask vector should have the same number of elements");
    static assert(BaseType!V.sizeof == BaseType!M.sizeof,
                  "argument vector and mask vector should have the same element type size");

    V shuffle(V op1, M mask)
    {
        return shuffle(op1, op1, mask);
    }
}

/**
 * Construct a permutation of elements from two vectors, returning a vector with
 * the same element type as the input vector(s), and same length as the `mask`.
 * Params:
 *    op1 = input vector
 *    op2 = input vector
 *    index = elements indices of the vectors that should be extracted and returned
 * Returns:
 *    vector with the same element type as `op1` and `op2`, but has an element count
 *    equal to the number of indices in `index`.
 * Example:
 * ---
 * int8 a = [1, -2, 3, -4, 5, -6, 7, -8];
 * int4 b = shufflevector(a, a, 0, 2, 4, 6);
 * assert(b.array == [1, 3, 5, 7]);
 * int4 c = [-2, -4, -6, -8];
 * int d = shufflevector(c, b, 4, 0, 5, 1, 6, 2, 7, 3);
 * assert(d.array == a.array);
 * ---
 */
template shufflevector(V1, V2, M...)
{
    static assert(isVectorType!V1, "first argument must be vector");
    static assert(isVectorType!V2, "second argument must be vector");
    static assert(is(BaseType!V1 == BaseType!V2),
                  "first and second argument vectors must have the same element type");
    static assert(isPowerOf2!(M.length),
                  "number of index arguments must be a power of 2");

    __vector(BaseType!V1[M.length]) shufflevector(V1 op1, V2 op2, M index);
}

/// Ditto
template shufflevector(V, index...)
{
    // Defined for compatibility with LDC.
    static assert(isVectorType!V, "first argument must be a vector type");
    static assert(numElements!V == index.length,
                  "number of index arguments must be the same number of vector elements");

    private template ctfeConstants(m...)
    {
        static if (m.length == 0) enum ctfeConstants = 1;
        else enum ctfeConstants = m[0] | ctfeConstants!(m[1 .. $]);
    }
    static assert(__traits(compiles, ctfeConstants!index),
                  "all index arguments must be compile time constants");

    private template validIndexes(m...)
    {
        static if (m.length == 0) enum validIndexes = true;
        else enum validIndexes = (cast(long)m[0] > -1) && validIndexes!(m[1 .. $]);
    }
    static assert(validIndexes!index,
                  "all index arguments must be greater than or equal to 0");

    V shufflevector(V op1, V op2)
    {
        return shufflevector(op1, op2, index);
    }
}

/**
 * Extracts a single scalar element from a vector at a specified index.
 * Defined for compatibility with LDC.
 * Params:
 *    val = vector to extract element from
 *    idx = index indicating the position from which to extract the element
 * Returns:
 *    scalar of the same type as the element type of val
 * Example:
 * ---
 * int4 a = [0, 10, 20, 30];
 * int k = extractelement!(int4, 2)(a);
 * assert(k == 20);
 * ---
 */
BaseType!V extractelement(V, int idx)(V val)
    if (isVectorType!V && idx < numElements!V)
{
    return val[idx];
}

/**
 * Inserts a scalar element into a vector at a specified index.
 * Defined for compatibility with LDC.
 * Params:
 *    val = vector to assign element to
 *    elt = scalar whose type is the element type of val
 *    idx = index indicating the position from which to extract the element
 * Returns:
 *    vector of the same type as val
 * Example:
 * ---
 * int4 a = [0, 10, 20, 30];
 * int4 b = insertelement!(int4, 2)(a, 50);
 * assert(b.array == [0, 10, 50, 30]);
 * ---
 */
V insertelement(V, int idx)(V val, BaseType!V elt)
    if (isVectorType!V && idx < numElements!V)
{
    val[idx] = elt;
    return val;
}

/**
 * Convert a vector from one integral or floating vector type to another.
 * The result is an integral or floating vector that has had every element
 * cast to the element type of the return type.
 * Params:
 *    from = input vector
 * Returns:
 *    converted vector
 * Example:
 * ---
 * int4 a = [1, -2, 3, -4];
 * float4 b = [1.5, -2.5, 3, 7];
 * assert(convertvector!float4(a).array == [1, -2, 3, -4]);
 * assert(convertvector!double4(a).array == [1, -2, 3, -4]);
 * assert(convertvector!double4(b).array == [1.5, -2.5, 3, 7]);
 * assert(convertvector!int4(b).array == [1, -2, 3, 7]);
 * ---
 */

template convertvector(V, T)
{
    static assert(isVectorType!V && (is(BaseType!V : long) || is(BaseType!V : real)),
                  "first argument must be an integer or floating vector type");
    static assert(isVectorType!T && (is(BaseType!T : long) || is(BaseType!T : real)),
                  "second argument must be an integer or floating vector");
    static assert(numElements!V == numElements!T,
                  "first and second argument vectors should have the same number of elements");

    V convertvector(T);
}

/**
 * Construct a conditional merge of elements from two vectors, returning a
 * vector of the same type as the input vector(s). The `mask` is an integral
 * vector with the same width and element count as the output vector.
 * Params:
 *    op1 = input vector
 *    op2 = input vector
 *    mask = integer vector mask
 * Returns:
 *    vector with the same type as `op1` and `op2`
 * Example:
 * ---
 * int4 a = [1, 2, 3, 4];
 * int4 b = [5, 6, 7, 8];
 * int4 mask1 = [0, 1, 1, 3];
 * int4 mask2 = [0, 4, 2, 5];
 * assert(shuffle(a, mask1).array == [1, 2, 2, 4]);
 * assert(shuffle(a, b, mask2).array == [1, 5, 3, 6]);
 * ---
 */
template blendvector(V0, V1, M)
{
    static assert(isVectorType!V0, "first argument must be vector");
    static assert(isVectorType!V1, "second argument must be vector");
    static assert(is(BaseType!V0 == BaseType!V1),
                  "first and second argument vectors must have the same element type");
    static assert(isVectorType!M && is(BaseType!M : long),
                  "last argument must be an integer vector");
    static assert(numElements!V0 == numElements!M && numElements!V1 == numElements!M,
                  "argument vectors and mask vector should have the same number of elements");
    static assert(BaseType!V0.sizeof == BaseType!M.sizeof,
                  "argument vectors and mask vector should have the same element type size");

    V0 blendvector(V0 op1, V1 op2, M mask);
}

/**
 * Perform an element-wise comparison between two vectors, producing `0` when
 * the comparison is false and `-1` (all bits are set to 1) otherwise.
 * Params:
 *    op1 = input vector
 *    op2 = input vector
 * Returns:
 *    vector of the same width and number of elements as the comparison
 *    operands with a signed integral element type
 * Example:
 * ---
 * float4 a = [1, 3, 5, 7];
 * float4 b = [2, 3, 4, 5];
 * int4 c = greaterMask!float4(a, b);
 * assert(c.array == [0, 0, -1, -1]);
 * ---
 */
V equalMask(V)(V op1, V op2) if (isVectorType!V)
{
    return op1 == op2;
}
/// Ditto
V notEqualMask(V)(V op1, V op2) if (isVectorType!V)
{
    return op1 != op2;
}
/// Ditto
V greaterMask(V)(V op1, V op2) if (isVectorType!V)
{
    return op1 > op2;
}
/// Ditto
V greaterOrEqualMask(V)(V op1, V op2) if (isVectorType!V)
{
    return op1 >= op2;
}

/**
 * Perform an element-wise logical comparison between two vectors, producing
 * `0` when the comparison is false and `-1` (all bits are set to 1) otherwise.
 * Params:
 *    op1 = input vector
 *    op2 = input vector
 * Returns:
 *    vector of the same width and number of elements as the comparison
 *    operands with a signed integral element type
 */
V notMask(V)(V op1) if (isVectorType!V)
{
    return op1 == 0;
}

/// Ditto
V andAndMask(V)(V op1, V op2) if (isVectorType!V)
{
    return (op1 != 0) & (op2 != 0);
}

/// Ditto
V orOrMask(V)(V op1, V op2) if (isVectorType!V)
{
    return (op1 != 0) | (op2 != 0);
}

// Private helper templates.
private:

enum bool isVectorType(T) = is(T : __vector(V[N]), V, size_t N);

template BaseType(V)
{
    alias typeof(V.array[0]) BaseType;
}

template numElements(V)
{
    enum numElements = V.sizeof / BaseType!(V).sizeof;
}

enum bool isPowerOf2(int Y) = Y && (Y & -Y) == Y;
