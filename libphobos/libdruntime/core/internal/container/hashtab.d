/**
 * HashTab container for internal usage.
 *
 * Copyright: Copyright Martin Nowak 2013.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 */
module core.internal.container.hashtab;

import core.internal.container.array;
static import common = core.internal.container.common;

struct HashTab(Key, Value)
{
    static struct Node
    {
        Key _key;
        Value _value;
        Node* _next;
    }

    @disable this(this);

    ~this()
    {
        reset();
    }

    void reset()
    {
        foreach (p; _buckets)
        {
            while (p !is null)
            {
                auto pn = p._next;
                common.destroy(*p);
                common.free(p);
                p = pn;
            }
        }
        _buckets.reset();
        _length = 0;
    }

    @property size_t length() const
    {
        return _length;
    }

    @property bool empty() const
    {
        return !_length;
    }

    void remove(in Key key)
    in { assert(key in this); }
    do
    {
        ensureNotInOpApply();

        immutable hash = hashOf(key) & mask;
        auto pp = &_buckets[hash];
        while (*pp)
        {
            auto p = *pp;
            if (p._key == key)
            {
                *pp = p._next;
                common.destroy(*p);
                common.free(p);
                if (--_length < _buckets.length && _length >= 4)
                    shrink();
                return;
            }
            else
            {
                pp = &p._next;
            }
        }
        assert(0);
    }

    ref inout(Value) opIndex(Key key) inout
    {
        return *opBinaryRight!("in")(key);
    }

    void opIndexAssign(Value value, Key key)
    {
        *get(key) = value;
    }

    inout(Value)* opBinaryRight(string op)(const scope Key key) inout
        if (op == "in")
    {
        if (_buckets.length)
        {
            immutable hash = hashOf(key) & mask;
            for (inout(Node)* p = _buckets[hash]; p !is null; p = p._next)
            {
                if (p._key == key)
                    return &p._value;
            }
        }
        return null;
    }

    int opApply(scope int delegate(ref Key, ref Value) dg)
    {
        immutable save = _inOpApply;
        _inOpApply = true;
        scope (exit) _inOpApply = save;
        foreach (p; _buckets)
        {
            while (p !is null)
            {
                if (auto res = dg(p._key, p._value))
                    return res;
                p = p._next;
            }
        }
        return 0;
    }

private:

    Value* get(Key key)
    {
        if (auto p = opBinaryRight!("in")(key))
            return p;

        ensureNotInOpApply();

        if (!_buckets.length)
            _buckets.length = 4;

        immutable hash = hashOf(key) & mask;
        auto p = cast(Node*)common.xmalloc(Node.sizeof);
        common.initialize(*p);
        p._key = key;
        p._next = _buckets[hash];
        _buckets[hash] = p;
        if (++_length >= 2 * _buckets.length)
            grow();
        return &p._value;
    }

    static hash_t hashOf(const scope ref Key key) @trusted
    {
        static if (is(Key U : U[]))
            return .hashOf(key, 0);
        else
            return .hashOf((&key)[0 .. 1], 0);
    }

    @property hash_t mask() const
    {
        return _buckets.length - 1;
    }

    void grow()
    in
    {
        assert(_buckets.length);
    }
    do
    {
        immutable ocnt = _buckets.length;
        immutable nmask = 2 * ocnt - 1;
        _buckets.length = 2 * ocnt;
        for (size_t i = 0; i < ocnt; ++i)
        {
            auto pp = &_buckets[i];
            while (*pp)
            {
                auto p = *pp;

                immutable nidx = hashOf(p._key) & nmask;
                if (nidx != i)
                {
                    *pp = p._next;
                    p._next = _buckets[nidx];
                    _buckets[nidx] = p;
                }
                else
                {
                    pp = &p._next;
                }
            }
        }
    }

    void shrink()
    in
    {
        assert(_buckets.length >= 2);
    }
    do
    {
        immutable ocnt = _buckets.length;
        immutable ncnt = ocnt >> 1;
        immutable nmask = ncnt - 1;

        for (size_t i = ncnt; i < ocnt; ++i)
        {
            if (auto tail = _buckets[i])
            {
                immutable nidx = i & nmask;
                auto pp = &_buckets[nidx];
                while (*pp)
                    pp = &(*pp)._next;
                *pp = tail;
                _buckets[i] = null;
            }
        }
        _buckets.length = ncnt;
    }

    void ensureNotInOpApply()
    {
        if (_inOpApply)
            assert(0, "Invalid HashTab manipulation during opApply iteration.");
    }

    Array!(Node*) _buckets;
    size_t _length;
    bool _inOpApply;
}

unittest
{
    HashTab!(int, int) tab;

    foreach (i; 0 .. 100)
        tab[i] = 100 - i;

    foreach (i; 0 .. 100)
        assert(tab[i] == 100 - i);

    foreach (k, v; tab)
        assert(v == 100 - k);

    foreach (i; 0 .. 50)
        tab.remove(2 * i);

    assert(tab.length == 50);

    foreach (i; 0 .. 50)
        assert(tab[2 * i + 1] == 100 - 2 * i - 1);

    assert(tab.length == 50);

    tab.reset();
    assert(tab.empty);
    tab[0] = 0;
    assert(!tab.empty);
    destroy(tab);
    assert(tab.empty);

    // not copyable
    static assert(!__traits(compiles, { HashTab!(int, int) tab2 = tab; }));
    HashTab!(int, int) tab2;
    static assert(!__traits(compiles, tab = tab2));
    static void foo(HashTab!(int, int) copy) {}
    static assert(!__traits(compiles, foo(tab)));
}

unittest
{
    HashTab!(string, size_t) tab;

    tab["foo"] = 0;
    assert(tab["foo"] == 0);
    ++tab["foo"];
    assert(tab["foo"] == 1);
    tab["foo"]++;
    assert(tab["foo"] == 2);

    auto s = "fo";
    s ~= "o";
    assert(tab[s] == 2);
    assert(tab.length == 1);
    tab[s] -= 2;
    assert(tab[s] == 0);
    tab["foo"] = 12;
    assert(tab[s] == 12);

    tab.remove("foo");
    assert(tab.empty);
}

unittest
{
    alias RC = common.RC!();
    HashTab!(size_t, RC) tab;

    size_t cnt;
    assert(cnt == 0);
    tab[0] = RC(&cnt);
    assert(cnt == 1);
    tab[1] = tab[0];
    assert(cnt == 2);
    tab.remove(0);
    assert(cnt == 1);
    tab.remove(1);
    assert(cnt == 0);
}

unittest
{
    import core.exception;

    HashTab!(uint, uint) tab;
    foreach (i; 0 .. 5)
        tab[i] = i;
    bool thrown;
    foreach (k, v; tab)
    {
        try
        {
            if (k == 3) tab.remove(k);
        }
        catch (AssertError e)
        {
            thrown = true;
        }
    }
    assert(thrown);
    assert(tab[3] == 3);
}
