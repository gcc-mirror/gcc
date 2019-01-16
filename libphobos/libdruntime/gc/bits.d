/**
 * Contains a bitfield used by the GC.
 *
 * Copyright: Copyright Digital Mars 2005 - 2013.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, David Friedman, Sean Kelly
 */

/*          Copyright Digital Mars 2005 - 2013.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module gc.bits;


import core.bitop;
import core.stdc.string;
import core.stdc.stdlib;
import core.exception : onOutOfMemoryError;

struct GCBits
{
    alias size_t wordtype;

    enum BITS_PER_WORD = (wordtype.sizeof * 8);
    enum BITS_SHIFT = (wordtype.sizeof == 8 ? 6 : 5);
    enum BITS_MASK = (BITS_PER_WORD - 1);
    enum BITS_1 = cast(wordtype)1;

    wordtype* data;
    size_t nbits;

    void Dtor() nothrow
    {
        if (data)
        {
            free(data);
            data = null;
        }
    }

    void alloc(size_t nbits) nothrow
    {
        this.nbits = nbits;
        data = cast(typeof(data[0])*)calloc(nwords, data[0].sizeof);
        if (!data)
            onOutOfMemoryError();
    }

    wordtype test(size_t i) const nothrow
    in
    {
        assert(i < nbits);
    }
    body
    {
        return core.bitop.bt(data, i);
    }

    int set(size_t i) nothrow
    in
    {
        assert(i < nbits);
    }
    body
    {
        return core.bitop.bts(data, i);
    }

    int clear(size_t i) nothrow
    in
    {
        assert(i <= nbits);
    }
    body
    {
        return core.bitop.btr(data, i);
    }

    void zero() nothrow
    {
        memset(data, 0, nwords * wordtype.sizeof);
    }

    void copy(GCBits *f) nothrow
    in
    {
        assert(nwords == f.nwords);
    }
    body
    {
        memcpy(data, f.data, nwords * wordtype.sizeof);
    }

    @property size_t nwords() const pure nothrow
    {
        return (nbits + (BITS_PER_WORD - 1)) >> BITS_SHIFT;
    }
}

unittest
{
    GCBits b;

    b.alloc(786);
    assert(!b.test(123));
    assert(!b.clear(123));
    assert(!b.set(123));
    assert(b.test(123));
    assert(b.clear(123));
    assert(!b.test(123));

    b.set(785);
    b.set(0);
    assert(b.test(785));
    assert(b.test(0));
    b.zero();
    assert(!b.test(785));
    assert(!b.test(0));

    GCBits b2;
    b2.alloc(786);
    b2.set(38);
    b.copy(&b2);
    assert(b.test(38));
    b2.Dtor();
    b.Dtor();
}
