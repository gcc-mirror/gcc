/**
 * This code handles decoding UTF strings for `foreach_reverse` loops.
 *
 * Copyright: Copyright Digital Mars 2004 - 2010.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Sean Kelly
 * Source: $(DRUNTIMESRC rt/_aApplyR.d)
 */
module rt.aApplyR;

import core.internal.utf;

debug (apply) import core.stdc.stdio : printf;

/**********************************************/
/* 1 argument versions */

// Note: dg is extern(D), but _aApplyRcd() is extern(C)

/**
Delegate type corresponding to transformed loop body

The parameter is a pointer to the current `char`, `wchar` or `dchar`

Returns: non-zero when a `break` statement is hit
*/
extern (D) alias dg_t = int delegate(void* c);

/**
Same as `_aApplyXXX` functions, but for `foreach_reverse`

Params:
    aa = input string
    dg = foreach body transformed into a delegate, similar to `opApply`

Returns:
    non-zero when the loop was exited through a `break`
*/
extern (C) int _aApplyRcd1(scope const(char)[] aa, dg_t dg)
{   int result;

    debug(apply) printf("_aApplyRcd1(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d;

        i--;
        d = aa[i];
        if (d & 0x80)
        {   char c = cast(char)d;
            uint j;
            uint m = 0x3F;
            d = 0;
            while ((c & 0xC0) != 0xC0)
            {   if (i == 0)
                    onUnicodeError("Invalid UTF-8 sequence", 0);
                i--;
                d |= (c & 0x3F) << j;
                j += 6;
                m >>= 1;
                c = aa[i];
            }
            d |= (c & m) << j;
        }
        result = dg(cast(void *)&d);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRcd1.unittest\n");

    auto s = "hello"c[];
    int i;

    foreach_reverse (dchar d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (dchar d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'b'); break;
            case 1:     assert(d == '\U000A0456'); break;
            case 2:     assert(d == '\u1234'); break;
            case 3:     assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 4);
}

/// ditto
extern (C) int _aApplyRwd1(scope const(wchar)[] aa, dg_t dg)
{   int result;

    debug(apply) printf("_aApplyRwd1(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d;

        i--;
        d = aa[i];
        if (d >= 0xDC00 && d <= 0xDFFF)
        {   if (i == 0)
                onUnicodeError("Invalid UTF-16 sequence", 0);
            i--;
            d = ((aa[i] - 0xD7C0) << 10) + (d - 0xDC00);
        }
        result = dg(cast(void *)&d);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRwd1.unittest\n");

    auto s = "hello"w[];
    int i;

    foreach_reverse (dchar d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (dchar d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'b'); break;
            case 1:     assert(d == '\U000A0456'); break;
            case 2:     assert(d == '\u1234'); break;
            case 3:     assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 4);
}

/// ditto
extern (C) int _aApplyRcw1(scope const(char)[] aa, dg_t dg)
{   int result;

    debug(apply) printf("_aApplyRcw1(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d;
        wchar w;

        i--;
        w = aa[i];
        if (w & 0x80)
        {   char c = cast(char)w;
            uint j;
            uint m = 0x3F;
            d = 0;
            while ((c & 0xC0) != 0xC0)
            {   if (i == 0)
                    onUnicodeError("Invalid UTF-8 sequence", 0);
                i--;
                d |= (c & 0x3F) << j;
                j += 6;
                m >>= 1;
                c = aa[i];
            }
            d |= (c & m) << j;

            if (d <= 0xFFFF)
                w = cast(wchar) d;
            else
            {
                w = cast(wchar) ((((d - 0x10000) >> 10) & 0x3FF) + 0xD800);
                result = dg(cast(void *)&w);
                if (result)
                    break;
                w = cast(wchar) (((d - 0x10000) & 0x3FF) + 0xDC00);
            }
        }
        result = dg(cast(void *)&w);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRcw1.unittest\n");

    auto s = "hello"c[];
    int i;

    foreach_reverse (wchar d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (wchar d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'b'); break;
            case 1:     assert(d == 0xDA41); break;
            case 2:     assert(d == 0xDC56); break;
            case 3:     assert(d == 0x1234); break;
            case 4:     assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);
}

/// ditto
extern (C) int _aApplyRwc1(scope const(wchar)[] aa, dg_t dg)
{   int result;

    debug(apply) printf("_aApplyRwc1(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d;
        char c;

        i--;
        d = aa[i];
        if (d >= 0xDC00 && d <= 0xDFFF)
        {   if (i == 0)
                onUnicodeError("Invalid UTF-16 sequence", 0);
            i--;
            d = ((aa[i] - 0xD7C0) << 10) + (d - 0xDC00);
        }

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
            continue;
        }
        c = cast(char)d;
        result = dg(cast(void *)&c);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRwc1.unittest\n");

    auto s = "hello"w[];
    int i;

    foreach_reverse (char d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (char d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'b'); break;
            case 1:     assert(d == 0xF2); break;
            case 2:     assert(d == 0xA0); break;
            case 3:     assert(d == 0x91); break;
            case 4:     assert(d == 0x96); break;
            case 5:     assert(d == 0xE1); break;
            case 6:     assert(d == 0x88); break;
            case 7:     assert(d == 0xB4); break;
            case 8:     assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 9);
}

/// ditto
extern (C) int _aApplyRdc1(scope const(dchar)[] aa, dg_t dg)
{   int result;

    debug(apply) printf("_aApplyRdc1(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0;)
    {   dchar d = aa[--i];
        char c;

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
            continue;
        }
        else
        {
            c = cast(char)d;
        }
        result = dg(cast(void *)&c);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRdc1.unittest\n");

    auto s = "hello"d[];
    int i;

    foreach_reverse (char d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (char d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'b'); break;
            case 1:     assert(d == 0xF2); break;
            case 2:     assert(d == 0xA0); break;
            case 3:     assert(d == 0x91); break;
            case 4:     assert(d == 0x96); break;
            case 5:     assert(d == 0xE1); break;
            case 6:     assert(d == 0x88); break;
            case 7:     assert(d == 0xB4); break;
            case 8:     assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 9);
}

/// ditto
extern (C) int _aApplyRdw1(scope const(dchar)[] aa, dg_t dg)
{   int result;

    debug(apply) printf("_aApplyRdw1(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d = aa[--i];
        wchar w;

        if (d <= 0xFFFF)
            w = cast(wchar) d;
        else
        {
            w = cast(wchar) ((((d - 0x10000) >> 10) & 0x3FF) + 0xD800);
            result = dg(cast(void *)&w);
            if (result)
                break;
            w = cast(wchar) (((d - 0x10000) & 0x3FF) + 0xDC00);
        }
        result = dg(cast(void *)&w);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRdw1.unittest\n");

    auto s = "hello"d[];
    int i;

    foreach_reverse (wchar d; s)
    {
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (wchar d; s)
    {
        //printf("i = %d, d = %x\n", i, d);
        switch (i)
        {
            case 0:     assert(d == 'b'); break;
            case 1:     assert(d == 0xDA41); break;
            case 2:     assert(d == 0xDC56); break;
            case 3:     assert(d == 0x1234); break;
            case 4:     assert(d == 'a'); break;
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

// Note: dg is extern(D), but _aApplyRcd2() is extern(C)

/**
Variants of _aApplyRXXX that include a loop index.
*/
extern (C) int _aApplyRcd2(scope const(char)[] aa, dg2_t dg)
{   int result;
    size_t i;
    size_t len = aa.length;

    debug(apply) printf("_aApplyRcd2(), len = %zd\n", len);
    for (i = len; i != 0; )
    {   dchar d;

        i--;
        d = aa[i];
        if (d & 0x80)
        {   char c = cast(char)d;
            uint j;
            uint m = 0x3F;
            d = 0;
            while ((c & 0xC0) != 0xC0)
            {   if (i == 0)
                    onUnicodeError("Invalid UTF-8 sequence", 0);
                i--;
                d |= (c & 0x3F) << j;
                j += 6;
                m >>= 1;
                c = aa[i];
            }
            d |= (c & m) << j;
        }
        result = dg(&i, cast(void *)&d);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRcd2.unittest\n");

    auto s = "hello"c[];
    int i;

    foreach_reverse (k, dchar d; s)
    {
        assert(k == 4 - i);
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (k, dchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(d == 'b'); assert(k == 8); break;
            case 1:     assert(d == '\U000A0456'); assert(k == 4); break;
            case 2:     assert(d == '\u1234'); assert(k == 1); break;
            case 3:     assert(d == 'a'); assert(k == 0); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 4);
}

/// ditto
extern (C) int _aApplyRwd2(scope const(wchar)[] aa, dg2_t dg)
{   int result;

    debug(apply) printf("_aApplyRwd2(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d;

        i--;
        d = aa[i];
        if (d >= 0xDC00 && d <= 0xDFFF)
        {   if (i == 0)
                onUnicodeError("Invalid UTF-16 sequence", 0);
            i--;
            d = ((aa[i] - 0xD7C0) << 10) + (d - 0xDC00);
        }
        result = dg(&i, cast(void *)&d);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRwd2.unittest\n");

    auto s = "hello"w[];
    int i;

    foreach_reverse (k, dchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == 4 - i);
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (k, dchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 4); assert(d == 'b'); break;
            case 1:     assert(k == 2); assert(d == '\U000A0456'); break;
            case 2:     assert(k == 1); assert(d == '\u1234'); break;
            case 3:     assert(k == 0); assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 4);
}

/// ditto
extern (C) int _aApplyRcw2(scope const(char)[] aa, dg2_t dg)
{   int result;

    debug(apply) printf("_aApplyRcw2(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d;
        wchar w;

        i--;
        w = aa[i];
        if (w & 0x80)
        {   char c = cast(char)w;
            uint j;
            uint m = 0x3F;
            d = 0;
            while ((c & 0xC0) != 0xC0)
            {   if (i == 0)
                    onUnicodeError("Invalid UTF-8 sequence", 0);
                i--;
                d |= (c & 0x3F) << j;
                j += 6;
                m >>= 1;
                c = aa[i];
            }
            d |= (c & m) << j;

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
        result = dg(&i, cast(void *)&w);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRcw2.unittest\n");

    auto s = "hello"c[];
    int i;

    foreach_reverse (k, wchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == 4 - i);
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (k, wchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 8); assert(d == 'b'); break;
            case 1:     assert(k == 4); assert(d == 0xDA41); break;
            case 2:     assert(k == 4); assert(d == 0xDC56); break;
            case 3:     assert(k == 1); assert(d == 0x1234); break;
            case 4:     assert(k == 0); assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);
}

/// ditto
extern (C) int _aApplyRwc2(scope const(wchar)[] aa, dg2_t dg)
{   int result;

    debug(apply) printf("_aApplyRwc2(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d;
        char c;

        i--;
        d = aa[i];
        if (d >= 0xDC00 && d <= 0xDFFF)
        {   if (i == 0)
                onUnicodeError("Invalid UTF-16 sequence", 0);
            i--;
            d = ((aa[i] - 0xD7C0) << 10) + (d - 0xDC00);
        }

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
            continue;
        }
        c = cast(char)d;
        result = dg(&i, cast(void *)&c);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRwc2.unittest\n");

    auto s = "hello"w[];
    int i;

    foreach_reverse (k, char d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == 4 - i);
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (k, char d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 4); assert(d == 'b'); break;
            case 1:     assert(k == 2); assert(d == 0xF2); break;
            case 2:     assert(k == 2); assert(d == 0xA0); break;
            case 3:     assert(k == 2); assert(d == 0x91); break;
            case 4:     assert(k == 2); assert(d == 0x96); break;
            case 5:     assert(k == 1); assert(d == 0xE1); break;
            case 6:     assert(k == 1); assert(d == 0x88); break;
            case 7:     assert(k == 1); assert(d == 0xB4); break;
            case 8:     assert(k == 0); assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 9);
}

/// ditto
extern (C) int _aApplyRdc2(scope const(dchar)[] aa, dg2_t dg)
{   int result;

    debug(apply) printf("_aApplyRdc2(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d = aa[--i];
        char c;

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
            continue;
        }
        else
        {   c = cast(char)d;
        }
        result = dg(&i, cast(void *)&c);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRdc2.unittest\n");

    auto s = "hello"d[];
    int i;

    foreach_reverse (k, char d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == 4 - i);
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (k, char d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 3); assert(d == 'b'); break;
            case 1:     assert(k == 2); assert(d == 0xF2); break;
            case 2:     assert(k == 2); assert(d == 0xA0); break;
            case 3:     assert(k == 2); assert(d == 0x91); break;
            case 4:     assert(k == 2); assert(d == 0x96); break;
            case 5:     assert(k == 1); assert(d == 0xE1); break;
            case 6:     assert(k == 1); assert(d == 0x88); break;
            case 7:     assert(k == 1); assert(d == 0xB4); break;
            case 8:     assert(k == 0); assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 9);
}

/// ditto
extern (C) int _aApplyRdw2(scope const(dchar)[] aa, dg2_t dg)
{   int result;

    debug(apply) printf("_aApplyRdw2(), len = %zd\n", aa.length);
    for (size_t i = aa.length; i != 0; )
    {   dchar d = aa[--i];
        wchar w;

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
        result = dg(&i, cast(void *)&w);
        if (result)
            break;
    }
    return result;
}

unittest
{
    debug(apply) printf("_aApplyRdw2.unittest\n");

    auto s = "hello"d[];
    int i;

    foreach_reverse (k, wchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        assert(k == 4 - i);
        switch (i)
        {
            case 0:     assert(d == 'o'); break;
            case 1:     assert(d == 'l'); break;
            case 2:     assert(d == 'l'); break;
            case 3:     assert(d == 'e'); break;
            case 4:     assert(d == 'h'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);

    s = "a\u1234\U000A0456b";
    i = 0;
    foreach_reverse (k, wchar d; s)
    {
        //printf("i = %d, k = %d, d = %x\n", i, k, d);
        switch (i)
        {
            case 0:     assert(k == 3); assert(d == 'b'); break;
            case 1:     assert(k == 2); assert(d == 0xDA41); break;
            case 2:     assert(k == 2); assert(d == 0xDC56); break;
            case 3:     assert(k == 1); assert(d == 0x1234); break;
            case 4:     assert(k == 0); assert(d == 'a'); break;
            default:    assert(0);
        }
        i++;
    }
    assert(i == 5);
}
