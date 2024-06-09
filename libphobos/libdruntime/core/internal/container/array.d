/**
 * Array container for internal usage.
 *
 * Copyright: Copyright Martin Nowak 2013.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 */
module core.internal.container.array;

static import common = core.internal.container.common;

import core.exception : onOutOfMemoryError;

struct Array(T)
{
nothrow:
    @disable this(this);

    ~this()
    {
        reset();
    }

    void reset()
    {
        length = 0;
    }

    @property size_t length() const
    {
        return _length;
    }

    @property void length(size_t nlength)
    {
        import core.checkedint : mulu;

        bool overflow = false;
        size_t reqsize = mulu(T.sizeof, nlength, overflow);
        if (!overflow)
        {
            if (nlength < _length)
                foreach (ref val; _ptr[nlength .. _length]) common.destroy(val);
            _ptr = cast(T*)common.xrealloc(_ptr, reqsize);
            if (nlength > _length)
                foreach (ref val; _ptr[_length .. nlength]) common.initialize(val);
            _length = nlength;
        }
        else
            onOutOfMemoryError();

    }

    @property bool empty() const
    {
        return !length;
    }

    @property ref inout(T) front() inout
    in { assert(!empty); }
    do
    {
        return _ptr[0];
    }

    @property ref inout(T) back() inout
    in { assert(!empty); }
    do
    {
        return _ptr[_length - 1];
    }

    ref inout(T) opIndex(size_t idx) inout
    in { assert(idx < length); }
    do
    {
        return _ptr[idx];
    }

    inout(T)[] opSlice() inout
    {
        return _ptr[0 .. _length];
    }

    inout(T)[] opSlice(size_t a, size_t b) inout
    in { assert(a < b && b <= length); }
    do
    {
        return _ptr[a .. b];
    }

    alias length opDollar;

    void insertBack()(auto ref T val)
    {
        import core.checkedint : addu;

        bool overflow = false;
        size_t newlength = addu(length, 1, overflow);
        if (!overflow)
        {
            length = newlength;
            back = val;
        }
        else
            onOutOfMemoryError();
    }

    void popBack()
    {
        length = length - 1;
    }

    void remove(size_t idx)
    in { assert(idx < length); }
    do
    {
        foreach (i; idx .. length - 1)
            _ptr[i] = _ptr[i+1];
        popBack();
    }

    void swap(ref Array other)
    {
        auto ptr = _ptr;
        _ptr = other._ptr;
        other._ptr = ptr;
        immutable len = _length;
        _length = other._length;
        other._length = len;
    }

    invariant
    {
        assert(!_ptr == !_length);
    }

private:
    T* _ptr;
    size_t _length;
}

unittest
{
    Array!size_t ary;

    assert(ary[] == []);
    ary.insertBack(5);
    assert(ary[] == [5]);
    assert(ary[$-1] == 5);
    ary.popBack();
    assert(ary[] == []);
    ary.insertBack(0);
    ary.insertBack(1);
    assert(ary[] == [0, 1]);
    assert(ary[0 .. 1] == [0]);
    assert(ary[1 .. 2] == [1]);
    assert(ary[$ - 2 .. $] == [0, 1]);
    size_t idx;
    foreach (val; ary) assert(idx++ == val);
    foreach_reverse (val; ary) assert(--idx == val);
    foreach (i, val; ary) assert(i == val);
    foreach_reverse (i, val; ary) assert(i == val);

    ary.insertBack(2);
    ary.remove(1);
    assert(ary[] == [0, 2]);

    assert(!ary.empty);
    ary.reset();
    assert(ary.empty);
    ary.insertBack(0);
    assert(!ary.empty);
    destroy(ary);
    assert(ary.empty);

    // not copyable
    static assert(!__traits(compiles, { Array!size_t ary2 = ary; }));
    Array!size_t ary2;
    static assert(!__traits(compiles, ary = ary2));
    static void foo(Array!size_t copy) {}
    static assert(!__traits(compiles, foo(ary)));

    ary2.insertBack(0);
    assert(ary.empty);
    assert(ary2[] == [0]);
    ary.swap(ary2);
    assert(ary[] == [0]);
    assert(ary2.empty);
}

unittest
{
    alias RC = common.RC!();
    Array!RC ary;

    size_t cnt;
    assert(cnt == 0);
    ary.insertBack(RC(&cnt));
    assert(cnt == 1);
    ary.insertBack(RC(&cnt));
    assert(cnt == 2);
    ary.back = ary.front;
    assert(cnt == 2);
    ary.popBack();
    assert(cnt == 1);
    ary.popBack();
    assert(cnt == 0);
}

unittest
{
    import core.exception;
    try
    {
        // Overflow ary.length.
        auto ary = Array!size_t(cast(size_t*)0xdeadbeef, -1);
        ary.insertBack(0);
    }
    catch (OutOfMemoryError)
    {
    }
    try
    {
        // Overflow requested memory size for common.xrealloc().
        auto ary = Array!size_t(cast(size_t*)0xdeadbeef, -2);
        ary.insertBack(0);
    }
    catch (OutOfMemoryError)
    {
    }
}
