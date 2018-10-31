// Written in the D programming language.

/**
This module defines generic containers.

Construction:

To implement the different containers both struct and class based
approaches have been used. $(REF make, std,_container,util) allows for
uniform construction with either approach.

---
import std.container;
// Construct a red-black tree and an array both containing the values 1, 2, 3.
// RedBlackTree should typically be allocated using `new`
RedBlackTree!int rbTree = new RedBlackTree!int(1, 2, 3);
// But `new` should not be used with Array
Array!int array = Array!int(1, 2, 3);
// `make` hides the differences
RedBlackTree!int rbTree2 = make!(RedBlackTree!int)(1, 2, 3);
Array!int array2 = make!(Array!int)(1, 2, 3);
---

Note that $(D make) can infer the element type from the given arguments.

---
import std.container;
auto rbTree = make!RedBlackTree(1, 2, 3); // RedBlackTree!int
auto array = make!Array("1", "2", "3"); // Array!string
---

Reference_semantics:

All containers have reference semantics, which means that after
assignment both variables refer to the same underlying data.

To make a copy of a _container, use the $(D c._dup) _container primitive.
---
import std.container, std.range;
Array!int originalArray = make!(Array!int)(1, 2, 3);
Array!int secondArray = originalArray;
assert(equal(originalArray[], secondArray[]));

// changing one instance changes the other one as well!
originalArray[0] = 12;
assert(secondArray[0] == 12);

// secondArray now refers to an independent copy of originalArray
secondArray = originalArray.dup;
secondArray[0] = 1;
// assert that originalArray has not been affected
assert(originalArray[0] == 12);
---

$(B Attention:) If the _container is implemented as a class, using an
uninitialized instance can cause a null pointer dereference.

---
import std.container;

RedBlackTree!int rbTree;
rbTree.insert(5); // null pointer dereference
---

Using an uninitialized struct-based _container will work, because the struct
intializes itself upon use; however, up to this point the _container will not
have an identity and assignment does not create two references to the same
data.

---
import std.container;

// create an uninitialized array
Array!int array1;
// array2 does _not_ refer to array1
Array!int array2 = array1;
array2.insertBack(42);
// thus array1 will not be affected
assert(array1.empty);

// after initialization reference semantics work as expected
array1 = array2;
// now affects array2 as well
array1.removeBack();
assert(array2.empty);
---
It is therefore recommended to always construct containers using
$(REF make, std,_container,util).

This is in fact necessary to put containers into another _container.
For example, to construct an $(D Array) of ten empty $(D Array)s, use
the following that calls $(D make) ten times.

---
import std.container, std.range;

auto arrOfArrs = make!Array(generate!(() => make!(Array!int)).take(10));
---

Submodules:

This module consists of the following submodules:

$(UL
    $(LI
        The $(MREF std, _container, array) module provides
        an array type with deterministic control of memory, not reliant on
        the GC unlike built-in arrays.
    )
    $(LI
        The $(MREF std, _container, binaryheap) module
        provides a binary heap implementation that can be applied to any
        user-provided random-access range.
    )
    $(LI
        The $(MREF std, _container, dlist) module provides
        a doubly-linked list implementation.
    )
    $(LI
        The $(MREF std, _container, rbtree) module
        implements red-black trees.
    )
    $(LI
        The $(MREF std, _container, slist) module
        implements singly-linked lists.
    )
    $(LI
        The $(MREF std, _container, util) module contains
        some generic tools commonly used by _container implementations.
    )
)

The_primary_range_of_a_container:

While some _containers offer direct access to their elements e.g. via
$(D opIndex), $(D c.front) or $(D c.back), access
and modification of a _container's contents is generally done through
its primary $(MREF_ALTTEXT range, std, range) type,
which is aliased as $(D C.Range). For example, the primary range type of
$(D Array!int) is $(D Array!int.Range).

If the documentation of a member function of a _container takes
a parameter of type $(D Range), then it refers to the primary range type of
this _container. Oftentimes $(D Take!Range) will be used, in which case
the range refers to a span of the elements in the _container. Arguments to
these parameters $(B must) be obtained from the same _container instance
as the one being worked with. It is important to note that many generic range
algorithms return the same range type as their input range.

---
import std.algorithm.comparison : equal;
import std.algorithm.iteration : find;
import std.container;
import std.range : take;

auto array = make!Array(1, 2, 3);

// `find` returns an Array!int.Range advanced to the element "2"
array.linearRemove(array[].find(2));

assert(array[].equal([1]));

array = make!Array(1, 2, 3);

// the range given to `linearRemove` is a Take!(Array!int.Range)
// spanning just the element "2"
array.linearRemove(array[].find(2).take(1));

assert(array[].equal([1, 3]));
---

When any $(MREF_ALTTEXT range, std, range) can be passed as an argument to
a member function, the documention usually refers to the parameter's templated
type as $(D Stuff).

---
import std.algorithm.comparison : equal;
import std.container;
import std.range : iota;

auto array = make!Array(1, 2);

// the range type returned by `iota` is completely unrelated to Array,
// which is fine for Array.insertBack:
array.insertBack(iota(3, 10));

assert(array[].equal([1, 2, 3, 4, 5, 6, 7, 8, 9]));
---

Container_primitives:

Containers do not form a class hierarchy, instead they implement a
common set of primitives (see table below). These primitives each guarantee
a specific worst case complexity and thus allow generic code to be written
independently of the _container implementation.

For example the primitives $(D c.remove(r)) and $(D c.linearRemove(r)) both
remove the sequence of elements in range $(D r) from the _container $(D c).
The primitive $(D c.remove(r)) guarantees
$(BIGOH n$(SUBSCRIPT r) log n$(SUBSCRIPT c)) complexity in the worst case and
$(D c.linearRemove(r)) relaxes this guarantee to $(BIGOH n$(SUBSCRIPT c)).

Since a sequence of elements can be removed from a $(MREF_ALTTEXT doubly linked list,std,_container,dlist)
in constant time, $(D DList) provides the primitive $(D c.remove(r))
as well as $(D c.linearRemove(r)). On the other hand
$(MREF_ALTTEXT Array, std,_container, array) only offers $(D c.linearRemove(r)).

The following table describes the common set of primitives that containers
implement.  A _container need not implement all primitives, but if a
primitive is implemented, it must support the syntax described in the $(B
syntax) column with the semantics described in the $(B description) column, and
it must not have a worst-case complexity worse than denoted in big-O notation in
the $(BIGOH &middot;) column.  Below, $(D C) means a _container type, $(D c) is
a value of _container type, $(D n$(SUBSCRIPT x)) represents the effective length of
value $(D x), which could be a single element (in which case $(D n$(SUBSCRIPT x)) is
$(D 1)), a _container, or a range.

$(BOOKTABLE Container primitives,
$(TR
    $(TH Syntax)
    $(TH $(BIGOH &middot;))
    $(TH Description)
)
$(TR
    $(TDNW $(D C(x)))
    $(TDNW $(D n$(SUBSCRIPT x)))
    $(TD Creates a _container of type $(D C) from either another _container or a range.
    The created _container must not be a null reference even if x is empty.)
)
$(TR
    $(TDNW $(D c.dup))
    $(TDNW $(D n$(SUBSCRIPT c)))
    $(TD Returns a duplicate of the _container.)
)
$(TR
    $(TDNW $(D c ~ x))
    $(TDNW $(D n$(SUBSCRIPT c) + n$(SUBSCRIPT x)))
    $(TD Returns the concatenation of $(D c) and $(D r). $(D x) may be a single
        element or an input range.)
)
$(TR
    $(TDNW $(D x ~ c))
    $(TDNW $(D n$(SUBSCRIPT c) + n$(SUBSCRIPT x)))
    $(TD Returns the concatenation of $(D x) and $(D c).  $(D x) may be a
        single element or an input range type.)
)
$(LEADINGROWN 3, Iteration
)
$(TR
    $(TD $(D c.Range))
    $(TD)
    $(TD The primary range type associated with the _container.)
)
$(TR
    $(TD $(D c[]))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Returns a range
         iterating over the entire _container, in a _container-defined order.)
)
$(TR
    $(TDNW $(D c[a .. b]))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Fetches a portion of the _container from key $(D a) to key $(D b).)
)
$(LEADINGROWN 3, Capacity
)
$(TR
    $(TD $(D c.empty))
    $(TD $(D 1))
    $(TD Returns $(D true) if the _container has no elements, $(D false) otherwise.)
)
$(TR
    $(TD $(D c.length))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Returns the number of elements in the _container.)
)
$(TR
    $(TDNW $(D c.length = n))
    $(TDNW $(D n$(SUBSCRIPT c) + n))
    $(TD Forces the number of elements in the _container to $(D n).
        If the _container ends up growing, the added elements are initialized
        in a _container-dependent manner (usually with $(D T.init)).)
)
$(TR
    $(TD $(D c.capacity))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Returns the maximum number of elements that can be stored in the
    _container without triggering a reallocation.)
)
$(TR
    $(TD $(D c.reserve(x)))
    $(TD $(D n$(SUBSCRIPT c)))
    $(TD Forces $(D capacity) to at least $(D x) without reducing it.)
)
$(LEADINGROWN 3, Access
)
$(TR
    $(TDNW $(D c.front))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Returns the first element of the _container, in a _container-defined order.)
)
$(TR
    $(TDNW $(D c.moveFront))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Destructively reads and returns the first element of the
         _container. The slot is not removed from the _container; it is left
         initialized with $(D T.init). This routine need not be defined if $(D
         front) returns a $(D ref).)
)
$(TR
    $(TDNW $(D c.front = v))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Assigns $(D v) to the first element of the _container.)
)
$(TR
    $(TDNW $(D c.back))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Returns the last element of the _container, in a _container-defined order.)
)
$(TR
    $(TDNW $(D c.moveBack))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Destructively reads and returns the last element of the
         _container. The slot is not removed from the _container; it is left
         initialized with $(D T.init). This routine need not be defined if $(D
         front) returns a $(D ref).)
)
$(TR
    $(TDNW $(D c.back = v))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Assigns $(D v) to the last element of the _container.)
)
$(TR
    $(TDNW $(D c[x]))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Provides indexed access into the _container. The index type is
         _container-defined. A _container may define several index types (and
         consequently overloaded indexing).)
)
$(TR
    $(TDNW $(D c.moveAt(x)))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Destructively reads and returns the value at position $(D x). The slot
         is not removed from the _container; it is left initialized with $(D
         T.init).)
)
$(TR
    $(TDNW $(D c[x] = v))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Sets element at specified index into the _container.)
)
$(TR
    $(TDNW $(D c[x] $(I op)= v))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Performs read-modify-write operation at specified index into the
        _container.)
)
$(LEADINGROWN 3, Operations
)
$(TR
    $(TDNW $(D e in c))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Returns nonzero if e is found in $(D c).)
)
$(TR
    $(TDNW $(D c.lowerBound(v)))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Returns a range of all elements strictly less than $(D v).)
)
$(TR
    $(TDNW $(D c.upperBound(v)))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Returns a range of all elements strictly greater than $(D v).)
)
$(TR
    $(TDNW $(D c.equalRange(v)))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Returns a range of all elements in $(D c) that are equal to $(D v).)
)
$(LEADINGROWN 3, Modifiers
)
$(TR
    $(TDNW $(D c ~= x))
    $(TDNW $(D n$(SUBSCRIPT c) + n$(SUBSCRIPT x)))
    $(TD Appends $(D x) to $(D c). $(D x) may be a single element or an input range type.)
)
$(TR
    $(TDNW $(D c.clear()))
    $(TDNW $(D n$(SUBSCRIPT c)))
    $(TD Removes all elements in $(D c).)
)
$(TR
    $(TDNW $(D c.insert(x)))
    $(TDNW $(D n$(SUBSCRIPT x) * log n$(SUBSCRIPT c)))
    $(TD Inserts $(D x) in $(D c) at a position (or positions) chosen by $(D c).)
)
$(TR
    $(TDNW $(D c.stableInsert(x)))
    $(TDNW $(D n$(SUBSCRIPT x) * log n$(SUBSCRIPT c)))
    $(TD Same as $(D c.insert(x)), but is guaranteed to not invalidate any ranges.)
)
$(TR
    $(TDNW $(D c.linearInsert(v)))
    $(TDNW $(D n$(SUBSCRIPT c)))
    $(TD Same as $(D c.insert(v)) but relaxes complexity to linear.)
)
$(TR
    $(TDNW $(D c.stableLinearInsert(v)))
    $(TDNW $(D n$(SUBSCRIPT c)))
    $(TD Same as $(D c.stableInsert(v)) but relaxes complexity to linear.)
)
$(TR
    $(TDNW $(D c.removeAny()))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Removes some element from $(D c) and returns it.)
)
$(TR
    $(TDNW $(D c.stableRemoveAny()))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Same as $(D c.removeAny()), but is guaranteed to not invalidate any
         iterators.)
)
$(TR
    $(TDNW $(D c.insertFront(v)))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Inserts $(D v) at the front of $(D c).)
)
$(TR
    $(TDNW $(D c.stableInsertFront(v)))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Same as $(D c.insertFront(v)), but guarantees no ranges will be
         invalidated.)
)
$(TR
    $(TDNW $(D c.insertBack(v)))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Inserts $(D v) at the back of $(D c).)
)
$(TR
    $(TDNW $(D c.stableInsertBack(v)))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Same as $(D c.insertBack(v)), but guarantees no ranges will be
         invalidated.)
)
$(TR
    $(TDNW $(D c.removeFront()))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Removes the element at the front of $(D c).)
)
$(TR
    $(TDNW $(D c.stableRemoveFront()))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Same as $(D c.removeFront()), but guarantees no ranges will be
         invalidated.)
)
$(TR
    $(TDNW $(D c.removeBack()))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Removes the value at the back of $(D c).)
)
$(TR
    $(TDNW $(D c.stableRemoveBack()))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Same as $(D c.removeBack()), but guarantees no ranges will be
         invalidated.)
)
$(TR
    $(TDNW $(D c.remove(r)))
    $(TDNW $(D n$(SUBSCRIPT r) * log n$(SUBSCRIPT c)))
    $(TD Removes range $(D r) from $(D c).)
)
$(TR
    $(TDNW $(D c.stableRemove(r)))
    $(TDNW $(D n$(SUBSCRIPT r) * log n$(SUBSCRIPT c)))
    $(TD Same as $(D c.remove(r)), but guarantees iterators are not
         invalidated.)
)
$(TR
    $(TDNW $(D c.linearRemove(r)))
    $(TDNW $(D n$(SUBSCRIPT c)))
    $(TD Removes range $(D r) from $(D c).)
)
$(TR
    $(TDNW $(D c.stableLinearRemove(r)))
    $(TDNW $(D n$(SUBSCRIPT c)))
    $(TD Same as $(D c.linearRemove(r)), but guarantees iterators are not
         invalidated.)
)
$(TR
    $(TDNW $(D c.removeKey(k)))
    $(TDNW $(D log n$(SUBSCRIPT c)))
    $(TD Removes an element from $(D c) by using its key $(D k).
         The key's type is defined by the _container.)
)
$(TR
    $(TDNW $(D ))
    $(TDNW $(D ))
    $(TD )
)
)

Source: $(PHOBOSSRC std/_container/package.d)

Copyright: Red-black tree code copyright (C) 2008- by Steven Schveighoffer. Other code
copyright 2010- Andrei Alexandrescu. All rights reserved by the respective holders.

License: Distributed under the Boost Software License, Version 1.0.
(See accompanying file LICENSE_1_0.txt or copy at $(HTTP
boost.org/LICENSE_1_0.txt)).

Authors: Steven Schveighoffer, $(HTTP erdani.com, Andrei Alexandrescu)
 */

module std.container;

public import std.container.array;
public import std.container.binaryheap;
public import std.container.dlist;
public import std.container.rbtree;
public import std.container.slist;

import std.meta;


/* The following documentation and type $(D TotalContainer) are
intended for developers only.

$(D TotalContainer) is an unimplemented container that illustrates a
host of primitives that a container may define. It is to some extent
the bottom of the conceptual container hierarchy. A given container
most often will choose to only implement a subset of these primitives,
and define its own additional ones. Adhering to the standard primitive
names below allows generic code to work independently of containers.

Things to remember: any container must be a reference type, whether
implemented as a $(D class) or $(D struct). No primitive below
requires the container to escape addresses of elements, which means
that compliant containers can be defined to use reference counting or
other deterministic memory management techniques.

A container may choose to define additional specific operations. The
only requirement is that those operations bear different names than
the ones below, lest user code gets confused.

Complexity of operations should be interpreted as "at least as good
as". If an operation is required to have $(BIGOH n) complexity, it
could have anything lower than that, e.g. $(BIGOH log(n)). Unless
specified otherwise, $(D n) inside a $(BIGOH) expression stands for
the number of elements in the container.
 */
struct TotalContainer(T)
{
/**
If the container has a notion of key-value mapping, $(D KeyType)
defines the type of the key of the container.
 */
    alias KeyType = T;

/**
If the container has a notion of multikey-value mapping, $(D
KeyTypes[k]), where $(D k) is a zero-based unsigned number, defines
the type of the $(D k)th key of the container.

A container may define both $(D KeyType) and $(D KeyTypes), e.g. in
the case it has the notion of primary/preferred key.
 */
    alias KeyTypes = AliasSeq!T;

/**
If the container has a notion of key-value mapping, $(D ValueType)
defines the type of the value of the container. Typically, a map-style
container mapping values of type $(D K) to values of type $(D V)
defines $(D KeyType) to be $(D K) and $(D ValueType) to be $(D V).
 */
    alias ValueType = T;

/**
Defines the container's primary range, which embodies one of the
ranges defined in $(MREF std,range).

Generally a container may define several types of ranges.
 */
    struct Range
    {
        /++
        Range primitives.
        +/
        @property bool empty()
        {
            assert(0);
        }
        /// Ditto
        @property ref T front() //ref return optional
        {
            assert(0);
        }
        /// Ditto
        @property void front(T value) //Only when front does not return by ref
        {
            assert(0);
        }
        /// Ditto
        T moveFront()
        {
            assert(0);
        }
        /// Ditto
        void popFront()
        {
            assert(0);
        }
        /// Ditto
        @property ref T back() //ref return optional
        {
            assert(0);
        }
        /// Ditto
        @property void back(T value) //Only when front does not return by ref
        {
            assert(0);
        }
        /// Ditto
        T moveBack()
        {
            assert(0);
        }
        /// Ditto
        void popBack()
        {
            assert(0);
        }
        /// Ditto
        T opIndex(size_t i) //ref return optional
        {
            assert(0);
        }
        /// Ditto
        void opIndexAssign(size_t i, T value) //Only when front does not return by ref
        {
            assert(0);
        }
        /// Ditto
        T opIndexUnary(string op)(size_t i) //Only when front does not return by ref
        {
            assert(0);
        }
        /// Ditto
        void opIndexOpAssign(string op)(size_t i, T value) //Only when front does not return by ref
        {
            assert(0);
        }
        /// Ditto
        T moveAt(size_t i)
        {
            assert(0);
        }
        /// Ditto
        @property size_t length()
        {
            assert(0);
        }
    }

/**
Property returning $(D true) if and only if the container has no
elements.

Complexity: $(BIGOH 1)
 */
    @property bool empty()
    {
        assert(0);
    }

/**
Returns a duplicate of the container. The elements themselves are not
transitively duplicated.

Complexity: $(BIGOH n).
 */
    @property TotalContainer dup()
    {
        assert(0);
    }

/**
Returns the number of elements in the container.

Complexity: $(BIGOH log(n)).
*/
    @property size_t length()
    {
        assert(0);
    }

/**
Returns the maximum number of elements the container can store without
(a) allocating memory, (b) invalidating iterators upon insertion.

Complexity: $(BIGOH log(n)).
 */
    @property size_t capacity()
    {
        assert(0);
    }

/**
Ensures sufficient capacity to accommodate $(D n) elements.

Postcondition: $(D capacity >= n)

Complexity: $(BIGOH log(e - capacity)) if $(D e > capacity), otherwise
$(BIGOH 1).
 */
    void reserve(size_t e)
    {
        assert(0);
    }

/**
Returns a range that iterates over all elements of the container, in a
container-defined order. The container should choose the most
convenient and fast method of iteration for $(D opSlice()).

Complexity: $(BIGOH log(n))
 */
    Range opSlice()
    {
        assert(0);
    }

    /**
       Returns a range that iterates the container between two
       specified positions.

       Complexity: $(BIGOH log(n))
     */
    Range opSlice(size_t a, size_t b)
    {
        assert(0);
    }

/**
Forward to $(D opSlice().front) and $(D opSlice().back), respectively.

Complexity: $(BIGOH log(n))
 */
    @property ref T front() //ref return optional
    {
        assert(0);
    }
    /// Ditto
    @property void front(T value) //Only when front does not return by ref
    {
        assert(0);
    }
    /// Ditto
    T moveFront()
    {
        assert(0);
    }
    /// Ditto
    @property ref T back() //ref return optional
    {
        assert(0);
    }
    /// Ditto
    @property void back(T value) //Only when front does not return by ref
    {
        assert(0);
    }
    /// Ditto
    T moveBack()
    {
        assert(0);
    }

/**
Indexing operators yield or modify the value at a specified index.
 */
    ref T opIndex(KeyType) //ref return optional
    {
        assert(0);
    }
    /// ditto
    void opIndexAssign(KeyType i, T value) //Only when front does not return by ref
    {
        assert(0);
    }
    /// ditto
    T opIndexUnary(string op)(KeyType i) //Only when front does not return by ref
    {
        assert(0);
    }
    /// ditto
    void opIndexOpAssign(string op)(KeyType i, T value) //Only when front does not return by ref
    {
        assert(0);
    }
    /// ditto
    T moveAt(KeyType i)
    {
        assert(0);
    }

/**
$(D k in container) returns true if the given key is in the container.
 */
    bool opBinaryRight(string op)(KeyType k) if (op == "in")
    {
        assert(0);
    }

/**
Returns a range of all elements containing $(D k) (could be empty or a
singleton range).
 */
    Range equalRange(KeyType k)
    {
        assert(0);
    }

/**
Returns a range of all elements with keys less than $(D k) (could be
empty or a singleton range). Only defined by containers that store
data sorted at all times.
 */
    Range lowerBound(KeyType k)
    {
        assert(0);
    }

/**
Returns a range of all elements with keys larger than $(D k) (could be
empty or a singleton range).  Only defined by containers that store
data sorted at all times.
 */
    Range upperBound(KeyType k)
    {
        assert(0);
    }

/**
Returns a new container that's the concatenation of $(D this) and its
argument. $(D opBinaryRight) is only defined if $(D Stuff) does not
define $(D opBinary).

Complexity: $(BIGOH n + m), where m is the number of elements in $(D
stuff)
 */
    TotalContainer opBinary(string op)(Stuff rhs) if (op == "~")
    {
        assert(0);
    }

    /// ditto
    TotalContainer opBinaryRight(string op)(Stuff lhs) if (op == "~")
    {
        assert(0);
    }

/**
Forwards to $(D insertAfter(this[], stuff)).
 */
    void opOpAssign(string op)(Stuff stuff) if (op == "~")
    {
        assert(0);
    }

/**
Removes all contents from the container. The container decides how $(D
capacity) is affected.

Postcondition: $(D empty)

Complexity: $(BIGOH n)
 */
    void clear()
    {
        assert(0);
    }

/**
Sets the number of elements in the container to $(D newSize). If $(D
newSize) is greater than $(D length), the added elements are added to
unspecified positions in the container and initialized with $(D
.init).

Complexity: $(BIGOH abs(n - newLength))

Postcondition: $(D _length == newLength)
 */
    @property void length(size_t newLength)
    {
        assert(0);
    }

/**
Inserts $(D stuff) in an unspecified position in the
container. Implementations should choose whichever insertion means is
the most advantageous for the container, but document the exact
behavior. $(D stuff) can be a value convertible to the element type of
the container, or a range of values convertible to it.

The $(D stable) version guarantees that ranges iterating over the
container are never invalidated. Client code that counts on
non-invalidating insertion should use $(D stableInsert). Such code would
not compile against containers that don't support it.

Returns: The number of elements added.

Complexity: $(BIGOH m * log(n)), where $(D m) is the number of
elements in $(D stuff)
 */
    size_t insert(Stuff)(Stuff stuff)
    {
        assert(0);
    }
    ///ditto
    size_t stableInsert(Stuff)(Stuff stuff)
    {
        assert(0);
    }

/**
Same as $(D insert(stuff)) and $(D stableInsert(stuff)) respectively,
but relax the complexity constraint to linear.
 */
    size_t linearInsert(Stuff)(Stuff stuff)
    {
        assert(0);
    }
    ///ditto
    size_t stableLinearInsert(Stuff)(Stuff stuff)
    {
        assert(0);
    }

/**
Picks one value in an unspecified position in the container, removes
it from the container, and returns it. Implementations should pick the
value that's the most advantageous for the container. The stable version
behaves the same, but guarantees that ranges iterating over the container
are never invalidated.

Precondition: $(D !empty)

Returns: The element removed.

Complexity: $(BIGOH log(n)).
 */
    T removeAny()
    {
        assert(0);
    }
    /// ditto
    T stableRemoveAny()
    {
        assert(0);
    }

/**
Inserts $(D value) to the front or back of the container. $(D stuff)
can be a value convertible to the container's element type or a range
of values convertible to it. The stable version behaves the same, but
guarantees that ranges iterating over the container are never
invalidated.

Returns: The number of elements inserted

Complexity: $(BIGOH log(n)).
 */
    size_t insertFront(Stuff)(Stuff stuff)
    {
        assert(0);
    }
    /// ditto
    size_t stableInsertFront(Stuff)(Stuff stuff)
    {
        assert(0);
    }
    /// ditto
    size_t insertBack(Stuff)(Stuff stuff)
    {
        assert(0);
    }
    /// ditto
    size_t stableInsertBack(T value)
    {
        assert(0);
    }

/**
Removes the value at the front or back of the container. The stable
version behaves the same, but guarantees that ranges iterating over
the container are never invalidated. The optional parameter $(D
howMany) instructs removal of that many elements. If $(D howMany > n),
all elements are removed and no exception is thrown.

Precondition: $(D !empty)

Complexity: $(BIGOH log(n)).
 */
    void removeFront()
    {
        assert(0);
    }
    /// ditto
    void stableRemoveFront()
    {
        assert(0);
    }
    /// ditto
    void removeBack()
    {
        assert(0);
    }
    /// ditto
    void stableRemoveBack()
    {
        assert(0);
    }

/**
Removes $(D howMany) values at the front or back of the
container. Unlike the unparameterized versions above, these functions
do not throw if they could not remove $(D howMany) elements. Instead,
if $(D howMany > n), all elements are removed. The returned value is
the effective number of elements removed. The stable version behaves
the same, but guarantees that ranges iterating over the container are
never invalidated.

Returns: The number of elements removed

Complexity: $(BIGOH howMany * log(n)).
 */
    size_t removeFront(size_t howMany)
    {
        assert(0);
    }
    /// ditto
    size_t stableRemoveFront(size_t howMany)
    {
        assert(0);
    }
    /// ditto
    size_t removeBack(size_t howMany)
    {
        assert(0);
    }
    /// ditto
    size_t stableRemoveBack(size_t howMany)
    {
        assert(0);
    }

/**
Removes all values corresponding to key $(D k).

Complexity: $(BIGOH m * log(n)), where $(D m) is the number of
elements with the same key.

Returns: The number of elements removed.
 */
    size_t removeKey(KeyType k)
    {
        assert(0);
    }

/**
Inserts $(D stuff) before, after, or instead range $(D r), which must
be a valid range previously extracted from this container. $(D stuff)
can be a value convertible to the container's element type or a range
of objects convertible to it. The stable version behaves the same, but
guarantees that ranges iterating over the container are never
invalidated.

Returns: The number of values inserted.

Complexity: $(BIGOH n + m), where $(D m) is the length of $(D stuff)
 */
    size_t insertBefore(Stuff)(Range r, Stuff stuff)
    {
        assert(0);
    }
    /// ditto
    size_t stableInsertBefore(Stuff)(Range r, Stuff stuff)
    {
        assert(0);
    }
    /// ditto
    size_t insertAfter(Stuff)(Range r, Stuff stuff)
    {
        assert(0);
    }
    /// ditto
    size_t stableInsertAfter(Stuff)(Range r, Stuff stuff)
    {
        assert(0);
    }
    /// ditto
    size_t replace(Stuff)(Range r, Stuff stuff)
    {
        assert(0);
    }
    /// ditto
    size_t stableReplace(Stuff)(Range r, Stuff stuff)
    {
        assert(0);
    }

/**
Removes all elements belonging to $(D r), which must be a range
obtained originally from this container. The stable version behaves the
same, but guarantees that ranges iterating over the container are
never invalidated.

Returns: A range spanning the remaining elements in the container that
initially were right after $(D r).

Complexity: $(BIGOH m * log(n)), where $(D m) is the number of
elements in $(D r)
 */
    Range remove(Range r)
    {
        assert(0);
    }
    /// ditto
    Range stableRemove(Range r)
    {
        assert(0);
    }

/**
Same as $(D remove) above, but has complexity relaxed to linear.

Returns: A range spanning the remaining elements in the container that
initially were right after $(D r).

Complexity: $(BIGOH n)
 */
    Range linearRemove(Range r)
    {
        assert(0);
    }
    /// ditto
    Range stableLinearRemove(Range r)
    {
        assert(0);
    }
}

@safe unittest
{
    TotalContainer!int test;
}
