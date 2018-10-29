/**
This module defines `TypedAllocator`, a statically-typed allocator that
aggregates multiple untyped allocators and uses them depending on the static
properties of the types allocated. For example, distinct allocators may be used
for thread-local vs. thread-shared data, or for fixed-size data (`struct`,
`class` objects) vs. resizable data (arrays).

Macros:
T2=$(TR <td style="text-align:left">$(D $1)</td> $(TD $(ARGS $+)))
*/

module std.experimental.allocator.typed;

import std.experimental.allocator;
import std.experimental.allocator.common;
import std.range : isInputRange, isForwardRange, walkLength, save, empty,
    front, popFront;
import std.traits : isPointer, hasElaborateDestructor;
import std.typecons : Flag, Yes, No;

/**
Allocation-related flags dictated by type characteristics. `TypedAllocator`
deduces these flags from the type being allocated and uses the appropriate
allocator accordingly.
*/
enum AllocFlag : uint
{
    init = 0,
    /**
    Fixed-size allocation (unlikely to get reallocated later). Examples: `int`,
    `double`, any `struct` or `class` type. By default it is assumed that the
    allocation is variable-size, i.e. susceptible to later reallocation
    (for example all array types). This flag is advisory, i.e. in-place resizing
    may be attempted for `fixedSize` allocations and may succeed. The flag is
    just a hint to the compiler it may use allocation strategies that work well
    with objects of fixed size.
    */
    fixedSize = 1,
    /**
    The type being allocated embeds no pointers. Examples: `int`, `int[]`, $(D
    Tuple!(int, float)). The implicit conservative assumption is that the type
    has members with indirections so it needs to be scanned if garbage
    collected. Example of types with pointers: `int*[]`, $(D Tuple!(int,
    string)).
    */
    hasNoIndirections = 4,
    /**
    By default it is conservatively assumed that allocated memory may be `cast`
    to `shared`, passed across threads, and deallocated in a different thread
    than the one that allocated it. If that's not the case, there are two
    options. First, `immutableShared` means the memory is allocated for
    `immutable` data and will be deallocated in the same thread it was
    allocated in. Second, `threadLocal` means the memory is not to be shared
    across threads at all. The two flags cannot be simultaneously present.
    */
    immutableShared = 8,
    /// ditto
    threadLocal = 16,
}

/**
`TypedAllocator` acts like a chassis on which several specialized allocators
can be assembled. To let the system make a choice about a particular kind of
allocation, use `Default` for the respective parameters.

There is a hierarchy of allocation kinds. When an allocator is implemented for
a given combination of flags, it is used. Otherwise, the next down the list is
chosen.

$(BOOKTABLE ,

$(TR $(TH `AllocFlag` combination) $(TH Description))

$(T2 AllocFlag.threadLocal |$(NBSP)AllocFlag.hasNoIndirections
|$(NBSP)AllocFlag.fixedSize,
This is the most specific allocation policy: the memory being allocated is
thread local, has no indirections at all, and will not be reallocated. Examples
of types fitting this description: `int`, `double`, $(D Tuple!(int, long)), but
not $(D Tuple!(int, string)), which contains an indirection.)

$(T2 AllocFlag.threadLocal |$(NBSP)AllocFlag.hasNoIndirections,
As above, but may be reallocated later. Examples of types fitting this
description are $(D int[]), $(D double[]), $(D Tuple!(int, long)[]), but not
$(D Tuple!(int, string)[]), which contains an indirection.)

$(T2 AllocFlag.threadLocal,
As above, but may embed indirections. Examples of types fitting this
description are $(D int*[]), $(D Object[]), $(D Tuple!(int, string)[]).)

$(T2 AllocFlag.immutableShared |$(NBSP)AllocFlag.hasNoIndirections
|$(NBSP)AllocFlag.fixedSize,
The type being allocated is `immutable` and has no pointers. The thread that
allocated it must also deallocate it. Example: `immutable(int)`.)

$(T2 AllocFlag.immutableShared |$(NBSP)AllocFlag.hasNoIndirections,
As above, but the type may be appended to in the future. Example: `string`.)

$(T2 AllocFlag.immutableShared,
As above, but the type may embed references. Example: `immutable(Object)[]`.)

$(T2 AllocFlag.hasNoIndirections |$(NBSP)AllocFlag.fixedSize,
The type being allocated may be shared across threads, embeds no indirections,
and has fixed size.)

$(T2 AllocFlag.hasNoIndirections,
The type being allocated may be shared across threads, may embed indirections,
and has variable size.)

$(T2 AllocFlag.fixedSize,
The type being allocated may be shared across threads, may embed indirections,
and has fixed size.)

$(T2 0, The most conservative/general allocation: memory may be shared,
deallocated in a different thread, may or may not be resized, and may embed
references.)
)

Params:
PrimaryAllocator = The default allocator.
Policies = Zero or more pairs consisting of an `AllocFlag` and an allocator
type.
*/
struct TypedAllocator(PrimaryAllocator, Policies...)
{
    import std.algorithm.sorting : isSorted;
    import std.meta : AliasSeq;
    import std.typecons : Tuple;

    static assert(Policies.length == 0 || isSorted([Stride2!Policies]));

    private template Stride2(T...)
    {
        static if (T.length >= 2)
        {
            alias Stride2 = AliasSeq!(T[0], Stride2!(T[2 .. $]));
        }
        else
        {
            alias Stride2 = AliasSeq!(T[0 .. $]);
        }
    }

    // state
    static if (stateSize!PrimaryAllocator) private PrimaryAllocator primary;
    else alias primary = PrimaryAllocator.instance;
    static if (Policies.length > 0)
        private Tuple!(Stride2!(Policies[1 .. $])) extras;

    private static bool match(uint have, uint want)
    {
        enum uint maskAway =
            ~(AllocFlag.immutableShared | AllocFlag.threadLocal);
        // Do we offer thread local?
        if (have & AllocFlag.threadLocal)
        {
            if (want & AllocFlag.threadLocal)
                return match(have & maskAway, want & maskAway);
            return false;
        }
        if (have & AllocFlag.immutableShared)
        {
            // Okay to ask for either thread local or immutable shared
            if (want & (AllocFlag.threadLocal
                    | AllocFlag.immutableShared))
                return match(have & maskAway, want & maskAway);
            return false;
        }
        // From here on we have full-blown thread sharing.
        if (have & AllocFlag.hasNoIndirections)
        {
            if (want & AllocFlag.hasNoIndirections)
                return match(have & ~AllocFlag.hasNoIndirections,
                    want & ~AllocFlag.hasNoIndirections);
            return false;
        }
        // Fixed size or variable size both match.
        return true;
    }

    /**
    Given `flags` as a combination of `AllocFlag` values, or a type `T`, returns
    the allocator that's a closest fit in capabilities.
    */
    auto ref allocatorFor(uint flags)()
    {
        static if (Policies.length == 0 || !match(Policies[0], flags))
        {
            return primary;
        }
        else static if (Policies.length && match(Policies[$ - 2], flags))
        {
            return extras[$ - 1];
        }
        else
        {
            foreach (i, choice; Stride2!Policies)
            {
                static if (!match(choice, flags))
                {
                    return extras[i - 1];
                }
            }
            assert(0);
        }
    }

    /// ditto
    auto ref allocatorFor(T)()
    {
        static if (is(T == void[]))
        {
            return primary;
        }
        else
        {
            return allocatorFor!(type2flags!T)();
        }
    }

    /**
    Given a type `T`, returns its allocation-related flags as a combination of
    `AllocFlag` values.
    */
    static uint type2flags(T)()
    {
        uint result;
        static if (is(T == immutable))
            result |= AllocFlag.immutableShared;
        else static if (is(T == shared))
            result |= AllocFlag.forSharing;
        static if (!is(T == U[], U))
            result |= AllocFlag.fixedSize;
        import std.traits : hasIndirections;
        static if (!hasIndirections!T)
            result |= AllocFlag.hasNoIndirections;
        return result;
    }

    /**
    Dynamically allocates (using the appropriate allocator chosen with
    `allocatorFor!T`) and then creates in the memory allocated an object of
    type `T`, using `args` (if any) for its initialization. Initialization
    occurs in the memory allocated and is otherwise semantically the same as
    `T(args)`. (Note that using `make!(T[])` creates a pointer to an
    (empty) array of `T`s, not an array. To allocate and initialize an
    array, use `makeArray!T` described below.)

    Params:
    T = Type of the object being created.
    args = Optional arguments used for initializing the created object. If not
    present, the object is default constructed.

    Returns: If `T` is a class type, returns a reference to the created `T`
    object. Otherwise, returns a `T*` pointing to the created object. In all
    cases, returns `null` if allocation failed.

    Throws: If `T`'s constructor throws, deallocates the allocated memory and
    propagates the exception.
    */
    auto make(T, A...)(auto ref A args)
    {
        return .make!T(allocatorFor!T, args);
    }

    /**
    Create an array of `T` with `length` elements. The array is either
    default-initialized, filled with copies of `init`, or initialized with
    values fetched from `range`.

    Params:
    T = element type of the array being created
    length = length of the newly created array
    init = element used for filling the array
    range = range used for initializing the array elements

    Returns:
    The newly-created array, or `null` if either `length` was `0` or
    allocation failed.

    Throws:
    The first two overloads throw only if the used allocator's primitives do.
    The overloads that involve copy initialization deallocate memory and propagate the exception if the copy operation throws.
    */
    T[] makeArray(T)(size_t length)
    {
        return .makeArray!T(allocatorFor!(T[]), length);
    }

    /// Ditto
    T[] makeArray(T)(size_t length, auto ref T init)
    {
        return .makeArray!T(allocatorFor!(T[]), init, length);
    }

    /// Ditto
    T[] makeArray(T, R)(R range)
    if (isInputRange!R)
    {
        return .makeArray!T(allocatorFor!(T[]), range);
    }

    /**
    Grows `array` by appending `delta` more elements. The needed memory is
    allocated using the same allocator that was used for the array type. The
    extra elements added are either default-initialized, filled with copies of
    `init`, or initialized with values fetched from `range`.

    Params:
    T = element type of the array being created
    array = a reference to the array being grown
    delta = number of elements to add (upon success the new length of `array`
    is $(D array.length + delta))
    init = element used for filling the array
    range = range used for initializing the array elements

    Returns:
    `true` upon success, `false` if memory could not be allocated. In the
    latter case `array` is left unaffected.

    Throws:
    The first two overloads throw only if the used allocator's primitives do.
    The overloads that involve copy initialization deallocate memory and
    propagate the exception if the copy operation throws.
    */
    bool expandArray(T)(ref T[] array, size_t delta)
    {
        return .expandArray(allocatorFor!(T[]), array, delta);
    }
    /// Ditto
    bool expandArray(T)(T[] array, size_t delta, auto ref T init)
    {
        return .expandArray(allocatorFor!(T[]), array, delta, init);
    }
    /// Ditto
    bool expandArray(T, R)(ref T[] array, R range)
    if (isInputRange!R)
    {
        return .expandArray(allocatorFor!(T[]), array, range);
    }

    /**
    Shrinks an array by `delta` elements using `allocatorFor!(T[])`.

    If $(D arr.length < delta), does nothing and returns `false`. Otherwise,
    destroys the last $(D arr.length - delta) elements in the array and then
    reallocates the array's buffer. If reallocation fails, fills the array with
    default-initialized data.

    Params:
    T = element type of the array being created
    arr = a reference to the array being shrunk
    delta = number of elements to remove (upon success the new length of
    `arr` is $(D arr.length - delta))

    Returns:
    `true` upon success, `false` if memory could not be reallocated. In the
    latter case $(D arr[$ - delta .. $]) is left with default-initialized
    elements.

    Throws:
    The first two overloads throw only if the used allocator's primitives do.
    The overloads that involve copy initialization deallocate memory and
    propagate the exception if the copy operation throws.
    */
    bool shrinkArray(T)(ref T[] arr, size_t delta)
    {
        return .shrinkArray(allocatorFor!(T[]), arr, delta);
    }

    /**
    Destroys and then deallocates (using `allocatorFor!T`) the object pointed
    to by a pointer, the class object referred to by a `class` or `interface`
    reference, or an entire array. It is assumed the respective entities had
    been allocated with the same allocator.
    */
    void dispose(T)(T* p)
    {
        return .dispose(allocatorFor!T, p);
    }
    /// Ditto
    void dispose(T)(T p)
    if (is(T == class) || is(T == interface))
    {
        return .dispose(allocatorFor!T, p);
    }
    /// Ditto
    void dispose(T)(T[] array)
    {
        return .dispose(allocatorFor!(T[]), array);
    }
}

///
@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.mmap_allocator : MmapAllocator;
    alias MyAllocator = TypedAllocator!(GCAllocator,
        AllocFlag.fixedSize | AllocFlag.threadLocal, Mallocator,
        AllocFlag.fixedSize | AllocFlag.threadLocal
                | AllocFlag.hasNoIndirections,
            MmapAllocator,
    );
    MyAllocator a;
    auto b = &a.allocatorFor!0();
    static assert(is(typeof(*b) == shared GCAllocator));
    enum f1 = AllocFlag.fixedSize | AllocFlag.threadLocal;
    auto c = &a.allocatorFor!f1();
    static assert(is(typeof(*c) == Mallocator));
    enum f2 = AllocFlag.fixedSize | AllocFlag.threadLocal;
    static assert(is(typeof(a.allocatorFor!f2()) == Mallocator));
    // Partial match
    enum f3 = AllocFlag.threadLocal;
    static assert(is(typeof(a.allocatorFor!f3()) == Mallocator));

    int* p = a.make!int;
    scope(exit) a.dispose(p);
    int[] arr = a.makeArray!int(42);
    scope(exit) a.dispose(arr);
    assert(a.expandArray(arr, 3));
    assert(a.shrinkArray(arr, 4));
}
