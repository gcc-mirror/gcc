/**
 * This module was renamed to disambiguate the term tuple, use
 * $(MREF std, meta) instead.
 *
 * Copyright: Copyright The D Language Foundation 2005 - 2015.
 * License: $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:
 * Source:    $(PHOBOSSRC std/typetuple.d)
 *
 * $(SCRIPT inhibitQuickIndex = 1;)
 */
module std.typetuple;

public import std.meta;

/**
 * Alternate name for $(REF AliasSeq, std,meta) for legacy compatibility.
 */
alias TypeTuple = AliasSeq;

///
@safe unittest
{
    import std.typetuple;
    alias TL = TypeTuple!(int, double);

    int foo(TL td)  // same as int foo(int, double);
    {
        return td[0] + cast(int) td[1];
    }
    assert(foo(1, 2.5) == 3);
}

///
@safe unittest
{
    alias TL = TypeTuple!(int, double);

    alias Types = TypeTuple!(TL, char);
    static assert(is(Types == TypeTuple!(int, double, char)));
}
