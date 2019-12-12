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
