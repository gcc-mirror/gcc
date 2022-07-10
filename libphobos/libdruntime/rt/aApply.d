/**
 * This code handles decoding UTF strings for foreach loops.
 *
 * Copyright: Copyright Digital Mars 2004 - 2010.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright
 * Source: $(DRUNTIMESRC rt/_aApply.d)
 */
module rt.aApply;

import core.internal.utf : decode, toUTF8;

/**********************************************/
/* 1 argument versions */

/**
Delegate type corresponding to transformed loop body

The parameter is a pointer to the current `char`, `wchar` or `dchar`

Returns: non-zero when a `break` statement is hit
*/
extern (D) alias dg_t = int delegate(void* c);

// Note: dg is extern(D), but _aApplycd() is extern(C)

/**
Loop over a string while changing the UTF encoding

There are 6 combinations of conversions between `char`, `wchar`, and `dchar`,
and 2 of each of those.

The naming convention is as follows:

_aApply{c,d,w}{c,d,w}{1,2}

The first letter corresponds to the input string encoding, and the second letter corresponds to the target character type.

- c = `char`
- w = `wchar`
- d = `dchar`

The `1` variant only produces the character, the `2` variant also produces a loop index.

Examples:
---
void main()
{
    string str;
    wtring wstr;
    dstring dstr;

    foreach (dchar c; str) {}
    // _aApplycd1

    foreach (wchar c; dstr) {}
    // _aApplydw1

    foreach (i, wchar c; str) {}
    // _aApplycw2

    foreach (wchar w; wstr) {}
    // no conversion
}
---

Params:
    aa = input string
    dg = foreach body transformed into a delegate, similar to `opApply`

Returns:
    non-zero when the loop was exited through a `break`
*/
extern (C) int _aApplycd1(in char[] aa, dg_t dg)
{
    int result;
    size_t len = aa.length;

    debug(apply) printf("_aApplycd1(), len = %d\n", len);
    for (size_t i = 0; i < len; )
    {
        dchar d = aa[i];
        if (d & 0x80)
            d = decode(aa, i);
        else
            ++i;
        result = dg(cast(void *)&d);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplycd1.unittest\n");

    auto s = "hello"c[];
    int i;

    foreach (dchar d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (dchar d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'a'); break;
            case 1:     assert(d == '\u1234'); break;
            case 2:     assert(d == '\U000A0456'); break;
            case 3:     assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 4);
}

/// ditto
extern (C) int _aApplywd1(in wchar[] aa, dg_t dg)
{
    int result;
    size_t len = aa.length;

    debug(apply) printf("_aApplywd1(), len = %d\n", len);
    for (size_t i = 0; i < len; )
    {
        dchar d = aa[i];
        if (d >= 0xD800)
            d = decode(aa, i);
        else
            ++i;
        result = dg(cast(void *)&d);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplywd1.unittest\n");

    auto s = "hello"w[];
    int i;

    foreach (dchar d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (dchar d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'a'); break;
            case 1:     assert(d == '\u1234'); break;
            case 2:     assert(d == '\U000A0456'); break;
            case 3:     assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 4);
}

/// ditto
extern (C) int _aApplycw1(in char[] aa, dg_t dg)
{
    int result;
    size_t len = aa.length;

    debug(apply) printf("_aApplycw1(), len = %d\n", len);
    for (size_t i = 0; i < len; )
    {
        wchar w = aa[i];
        if (w & 0x80)
        {
            dchar d = decode(aa, i);
            if (d <= 0xFFFF)
                w = cast(wchar) d;
            else
            {
                w = cast(wchar)((((d - 0x10000) >> 10) & 0x3FF) + 0xD800);
                result = dg(cast(void *)&w);
                if (result)
                    break;
                w = cast(wchar)(((d - 0x10000) & 0x3FF) + 0xDC00);
            }
        }
        else
            ++i;
        result = dg(cast(void *)&w);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplycw1.unittest\n");

    auto s = "hello"c[];
    int i;

    foreach (wchar d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (wchar d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'a'); break;
            case 1:     assert(d == 0x1234); break;
            case 2:     assert(d == 0xDA41); break;
            case 3:     assert(d == 0xDC56); break;
            case 4:     assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);
}

/// ditto
extern (C) int _aApplywc1(in wchar[] aa, dg_t dg)
{
    int result;
    size_t len = aa.length;

    debug(apply) printf("_aApplywc1(), len = %d\n", len);
    for (size_t i = 0; i < len; )
    {
        wchar w = aa[i];
        if (w & ~0x7F)
        {
            char[4] buf = void;

            dchar d = decode(aa, i);
            auto b = toUTF8(buf, d);
            foreach (char c2; b)
            {
                result = dg(cast(void *)&c2);
                if (result)
                    return result;
            }
        }
        else
        {
            char c = cast(char)w;
            ++i;
            result = dg(cast(void *)&c);
            if (result)
                break;
        }
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplywc1.unittest\n");

    auto s = "hello"w[];
    int i;

    foreach (char d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (char d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'a'); break;
            case 1:     assert(d == 0xE1); break;
            case 2:     assert(d == 0x88); break;
            case 3:     assert(d == 0xB4); break;
            case 4:     assert(d == 0xF2); break;
            case 5:     assert(d == 0xA0); break;
            case 6:     assert(d == 0x91); break;
            case 7:     assert(d == 0x96); break;
            case 8:     assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 9);
}

/// ditto
extern (C) int _aApplydc1(in dchar[] aa, dg_t dg)
{
    int result;

    debug(apply) printf("_aApplydc1(), len = %d\n", aa.length);
    foreach (dchar d; aa)
    {
        if (d & ~0x7F)
        {
            char[4] buf = void;

            auto b = toUTF8(buf, d);
            foreach (char c2; b)
            {
                result = dg(cast(void *)&c2);
                if (result)
                    return result;
            }
        }
        else
        {
            char c = cast(char)d;
            result = dg(cast(void *)&c);
            if (result)
                break;
        }
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRdc1.unittest\n");

    auto s = "hello"d[];
    int i;

    foreach (char d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (char d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'a'); break;
            case 1:     assert(d == 0xE1); break;
            case 2:     assert(d == 0x88); break;
            case 3:     assert(d == 0xB4); break;
            case 4:     assert(d == 0xF2); break;
            case 5:     assert(d == 0xA0); break;
            case 6:     assert(d == 0x91); break;
            case 7:     assert(d == 0x96); break;
            case 8:     assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 9);
}

/// ditto
extern (C) int _aApplydw1(in dchar[] aa, dg_t dg)
{
    int result;

    debug(apply) printf("_aApplydw1(), len = %d\n", aa.length);
    foreach (dchar d; aa)
    {
        wchar w;

        if (d <= 0xFFFF)
            w = cast(wchar) d;
        else
        {
            w = cast(wchar)((((d - 0x10000) >> 10) & 0x3FF) + 0xD800);
            result = dg(cast(void *)&w);
            if (result)
                break;
            w = cast(wchar)(((d - 0x10000) & 0x3FF) + 0xDC00);
        }
        result = dg(cast(void *)&w);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplydw1.unittest\n");

    auto s = "hello"d[];
    int i;

    foreach (wchar d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (wchar d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'a'); break;
            case 1:     assert(d == 0x1234); break;
            case 2:     assert(d == 0xDA41); break;
            case 3:     assert(d == 0xDC56); break;
            case 4:     assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);
}


/****************************************************************************/
/* 2 argument versions */

/**
Delegate type corresponding to transformed loop body

Parameters are pointers to a `size_t` loop index, and the current `char`, `wchar` or `dchar`.

Returns: non-zero when a `break` statement is hit
*/
extern (D) alias dg2_t = int delegate(void* i, void* c);

// Note: dg is extern(D), but _aApplycd2() is extern(C)

/**
Variants of _aApplyXXX that include a loop index.
*/
extern (C) int _aApplycd2(in char[] aa, dg2_t dg)
{
    int result;
    size_t len = aa.length;

    debug(apply) printf("_aApplycd2(), len = %d\n", len);
    size_t n;
    for (size_t i = 0; i < len; i += n)
    {
        dchar d = aa[i];
        if (d & 0x80)
        {
            n = i;
            d = decode(aa, n);
            n -= i;
        }
        else
            n = 1;
        result = dg(&i, cast(void *)&d);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplycd2.unittest\n");

    auto s = "hello"c[];
    int i;

    foreach (k, dchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == i);
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (k, dchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(d == 'a'); assert(k == 0); break;
            case 1:     assert(d == '\u1234'); assert(k == 1); break;
            case 2:     assert(d == '\U000A0456'); assert(k == 4); break;
            case 3:     assert(d == 'b'); assert(k == 8); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 4);
}

/// ditto
extern (C) int _aApplywd2(in wchar[] aa, dg2_t dg)
{
    int result;
    size_t len = aa.length;

    debug(apply) printf("_aApplywd2(), len = %d\n", len);
    size_t n;
    for (size_t i = 0; i < len; i += n)
    {
        dchar d = aa[i];
        if (d & ~0x7F)
        {
            n = i;
            d = decode(aa, n);
            n -= i;
        }
        else
            n = 1;
        result = dg(&i, cast(void *)&d);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplywd2.unittest\n");

    auto s = "hello"w[];
    int i;

    foreach (k, dchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == i);
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (k, dchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 0); assert(d == 'a'); break;
            case 1:     assert(k == 1); assert(d == '\u1234'); break;
            case 2:     assert(k == 2); assert(d == '\U000A0456'); break;
            case 3:     assert(k == 4); assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 4);
}

/// ditto
extern (C) int _aApplycw2(in char[] aa, dg2_t dg)
{
    int result;
    size_t len = aa.length;

    debug(apply) printf("_aApplycw2(), len = %d\n", len);
    size_t n;
    for (size_t i = 0; i < len; i += n)
    {
        wchar w = aa[i];
        if (w & 0x80)
        {
            n = i;
            dchar d = decode(aa, n);
            n -= i;
            if (d <= 0xFFFF)
                w = cast(wchar) d;
            else
            {
                w = cast(wchar) ((((d - 0x10000) >> 10) & 0x3FF) + 0xD800);
                result = dg(&i, cast(void *)&w);
                if (result)
                    break;
                w = cast(wchar) (((d - 0x10000) & 0x3FF) + 0xDC00);
            }
        }
        else
            n = 1;
        result = dg(&i, cast(void *)&w);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplycw2.unittest\n");

    auto s = "hello"c[];
    int i;

    foreach (k, wchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == i);
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (k, wchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 0); assert(d == 'a'); break;
            case 1:     assert(k == 1); assert(d == 0x1234); break;
            case 2:     assert(k == 4); assert(d == 0xDA41); break;
            case 3:     assert(k == 4); assert(d == 0xDC56); break;
            case 4:     assert(k == 8); assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);
}

/// ditto
extern (C) int _aApplywc2(in wchar[] aa, dg2_t dg)
{
    int result;
    size_t len = aa.length;

    debug(apply) printf("_aApplywc2(), len = %d\n", len);
    size_t n;
    for (size_t i = 0; i < len; i += n)
    {
        wchar w = aa[i];
        if (w & ~0x7F)
        {
            char[4] buf = void;

            n = i;
            dchar d = decode(aa, n);
            n -= i;
            auto b = toUTF8(buf, d);
            foreach (char c2; b)
            {
                result = dg(&i, cast(void *)&c2);
                if (result)
                    return result;
            }
        }
        else
        {
            char c = cast(char)w;
            n = 1;
            result = dg(&i, cast(void *)&c);
            if (result)
                break;
        }
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplywc2.unittest\n");

    auto s = "hello"w[];
    int i;

    foreach (k, char d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == i);
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (k, char d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 0); assert(d == 'a'); break;
            case 1:     assert(k == 1); assert(d == 0xE1); break;
            case 2:     assert(k == 1); assert(d == 0x88); break;
            case 3:     assert(k == 1); assert(d == 0xB4); break;
            case 4:     assert(k == 2); assert(d == 0xF2); break;
            case 5:     assert(k == 2); assert(d == 0xA0); break;
            case 6:     assert(k == 2); assert(d == 0x91); break;
            case 7:     assert(k == 2); assert(d == 0x96); break;
            case 8:     assert(k == 4); assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 9);
}

/// ditto
extern (C) int _aApplydc2(in dchar[] aa, dg2_t dg)
{
    int result;
    size_t len = aa.length;

    debug(apply) printf("_aApplydc2(), len = %d\n", len);
    for (size_t i = 0; i < len; i++)
    {
        dchar d = aa[i];
        if (d & ~0x7F)
        {
            char[4] buf = void;

            auto b = toUTF8(buf, d);
            foreach (char c2; b)
            {
                result = dg(&i, cast(void *)&c2);
                if (result)
                    return result;
            }
        }
        else
        {
            char c = cast(char)d;
            result = dg(&i, cast(void *)&c);
            if (result)
                break;
        }
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplydc2.unittest\n");

    auto s = "hello"d[];
    int i;

    foreach (k, char d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == i);
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (k, char d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 0); assert(d == 'a'); break;
            case 1:     assert(k == 1); assert(d == 0xE1); break;
            case 2:     assert(k == 1); assert(d == 0x88); break;
            case 3:     assert(k == 1); assert(d == 0xB4); break;
            case 4:     assert(k == 2); assert(d == 0xF2); break;
            case 5:     assert(k == 2); assert(d == 0xA0); break;
            case 6:     assert(k == 2); assert(d == 0x91); break;
            case 7:     assert(k == 2); assert(d == 0x96); break;
            case 8:     assert(k == 3); assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 9);
}

/// ditto
extern (C) int _aApplydw2(in dchar[] aa, dg2_t dg)
{   int result;

    debug(apply) printf("_aApplydw2(), len = %d\n", aa.length);
    foreach (size_t i, dchar d; aa)
    {
        wchar w;
        auto j = i;

        if (d <= 0xFFFF)
            w = cast(wchar) d;
        else
        {
            w = cast(wchar) ((((d - 0x10000) >> 10) & 0x3FF) + 0xD800);
            result = dg(&j, cast(void *)&w);
            if (result)
                break;
            w = cast(wchar) (((d - 0x10000) & 0x3FF) + 0xDC00);
        }
        result = dg(&j, cast(void *)&w);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplydw2.unittest\n");

    auto s = "hello"d[];
    int i;

    foreach (k, wchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == i);
        switch (i)
        {
            case 0:     assert(d == 'h'); break;
            case 1:     assert(d == 'e'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'l'); break;
            case 4:     assert(d == 'o'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach (k, wchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 0); assert(d == 'a'); break;
            case 1:     assert(k == 1); assert(d == 0x1234); break;
            case 2:     assert(k == 2); assert(d == 0xDA41); break;
            case 3:     assert(k == 2); assert(d == 0xDC56); break;
            case 4:     assert(k == 3); assert(d == 'b'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);
}
