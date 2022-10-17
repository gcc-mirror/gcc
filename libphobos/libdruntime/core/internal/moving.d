/**
 This module contains the implementation of move semantics of DIP 1014

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/_internal/_moving.d)
*/
module core.internal.moving;

/**
Recursively calls the `opPostMove` callbacks of a struct and its members if
they're defined.

When moving a struct instance, the compiler emits a call to this function
after blitting the instance and before releasing the original instance's
memory.

Params:
     newLocation = reference to struct instance being moved into
     oldLocation = reference to the original instance

Note:
     This function is tentatively defined as `nothrow` to prevent
     `opPostMove` from being defined without `nothrow`, which would allow
     for possibly confusing changes in program flow.
*/
void __move_post_blt(S)(ref S newLocation, ref S oldLocation) nothrow
    if (is(S == struct))
{
    import core.internal.traits : hasElaborateMove;
    static foreach (i, M; typeof(S.tupleof))
    {
        static if (hasElaborateMove!M)
        {
            __move_post_blt(newLocation.tupleof[i], oldLocation.tupleof[i]);
        }
    }

    static if (__traits(hasMember, S, "opPostMove"))
    {
        import core.internal.traits : lvalueOf, rvalueOf;
        static assert( is(typeof(S.init.opPostMove(lvalueOf!S))) &&
                      !is(typeof(S.init.opPostMove(rvalueOf!S))),
                "`" ~ S.stringof ~ ".opPostMove` must take exactly one argument of type `" ~ S.stringof ~ "` by reference");

        newLocation.opPostMove(oldLocation);
    }
}

void __move_post_blt(S)(ref S newLocation, ref S oldLocation) nothrow
    if (__traits(isStaticArray, S))
{
    import core.internal.traits : hasElaborateMove;
    static if (S.length && hasElaborateMove!(typeof(newLocation[0])))
    {
        foreach (i; 0 .. S.length)
            __move_post_blt(newLocation[i], oldLocation[i]);
    }
}

@safe nothrow unittest
{
    struct A
    {
        bool movedInto;
        void opPostMove(const ref A oldLocation)
        {
            movedInto = true;
        }
    }
    A src, dest;
    __move_post_blt(dest, src);
    assert(dest.movedInto);
}

@safe nothrow unittest
{
    struct A
    {
        bool movedInto;
        void opPostMove(const ref A oldLocation)
        {
            movedInto = true;
        }
    }
    struct B
    {
        A a;

        bool movedInto;
        void opPostMove(const ref B oldLocation)
        {
            movedInto = true;
        }
    }
    B src, dest;
    __move_post_blt(dest, src);
    assert(dest.movedInto && dest.a.movedInto);
}

@safe nothrow unittest
{
    static struct DoNotMove
    {
        bool movedInto;
        void opPostMove(const ref DoNotMove oldLocation)
        {
            movedInto = true;
        }
    }
    static DoNotMove doNotMove;

    struct A
    {
        @property ref DoNotMove member()
        {
            return doNotMove;
        }
    }
    A src, dest;
    __move_post_blt(dest, src);
    assert(!doNotMove.movedInto);
}

@safe nothrow unittest
{
    static struct A
    {
        bool movedInto;
        void opPostMove(const ref A oldLocation)
        {
            movedInto = true;
        }
    }
    static struct B
    {
        A[2] a;
    }
    B src, dest;
    __move_post_blt(dest, src);
    foreach (ref a; src.a)
        assert(!a.movedInto);
    foreach (ref a; dest.a)
        assert(a.movedInto);
}
