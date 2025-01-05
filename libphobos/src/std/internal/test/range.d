/**
For testing only.
Contains tests related to member privacy that cannot be verified inside
std.range itself.
*/
module std.internal.test.range;

// Note: currently can't be @safe because RefCounted, which is used by chunks,
// isn't.
@system /*@safe*/ unittest
{
    import std.algorithm.comparison : equal;
    import std.range : chunks;

    struct R
    {
        int state = 0;
        @property bool empty() { return state >= 5; }
        @property int front() { return state; }
        void popFront() { state++; }
    }

    auto r = R().chunks(3);
    assert(r.equal!equal([[ 0, 1, 2 ], [ 3, 4 ]]));
}

// https://issues.dlang.org/show_bug.cgi?id=24415
@safe unittest
{
    import std.range : only;

    static struct S(T)
    {
        T i;

        this(ref return scope inout(S) rhs) scope @safe inout pure nothrow
        {
            i = rhs.i;
        }
    }
    {
        auto a = only(S!int(42));
        auto b = a;
        assert(!b.empty);
        assert(b.front == S!int(42));

        a.popFront();
        auto c = a;
        assert(c.empty);
    }
    {
        auto a = only(S!(const int)(42));
        auto b = a;
        assert(!b.empty);
        assert(b.front == S!(const int)(42));

        a.popFront();
        auto c = a;
        assert(c.empty);
    }
    {
        auto a = only(S!(immutable int)(42));
        auto b = a;
        assert(!b.empty);
        assert(b.front == S!(immutable int)(42));

        a.popFront();
        auto c = a;
        assert(c.empty);
    }
    {
        auto a = only(S!int(42), S!int(192));
        auto b = a;
        assert(!b.empty);
        assert(b.front == S!int(42));

        a.popFront();
        auto c = a;
        assert(!c.empty);
        assert(c.front == S!int(192));

        a.popFront();
        auto d = a;
        assert(d.empty);
    }
    {
        auto a = only(S!(const int)(42), S!(const int)(192));
        auto b = a;
        assert(!b.empty);
        assert(b.front == S!(const int)(42));

        a.popFront();
        auto c = a;
        assert(!c.empty);
        assert(c.front == S!(const int)(192));

        a.popFront();
        auto d = a;
        assert(d.empty);
    }
    {
        auto a = only(S!(immutable int)(42), S!(immutable int)(192));
        auto b = a;
        assert(!b.empty);
        assert(b.front == S!(immutable int)(42));

        a.popFront();
        auto c = a;
        assert(!c.empty);
        assert(c.front == S!(immutable int)(192));

        a.popFront();
        auto d = a;
        assert(d.empty);
    }
}
