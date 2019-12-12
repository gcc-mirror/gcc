/**
 * Implementation of dynamic array property support routines.
 *
 * Copyright: Copyright Digital Mars 2000 - 2015.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Walter Bright
 * Source: $(DRUNTIMESRC src/rt/_adi.d)
 */

module rt.adi;

//debug=adi;            // uncomment to turn on debugging printf's

private
{
    debug(adi) import core.stdc.stdio;
    import core.stdc.string;
    import core.stdc.stdlib;
    import core.memory;
    import rt.util.utf;

    extern (C) void[] _adSort(void[] a, TypeInfo ti);
}

private dchar[] mallocUTF32(C)(in C[] s)
{
    size_t j = 0;
    auto p = cast(dchar*)malloc(dchar.sizeof * s.length);
    auto r = p[0..s.length]; // r[] will never be longer than s[]
    foreach (dchar c; s)
        r[j++] = c;
    return r[0 .. j];
}

/**********************************************
 * Sort array of chars.
 */

extern (C) char[] _adSortChar(char[] a)
{
    if (a.length > 1)
    {
        auto da = mallocUTF32(a);
        _adSort(*cast(void[]*)&da, typeid(da[0]));
        size_t i = 0;
        foreach (dchar d; da)
        {   char[4] buf;
            auto t = toUTF8(buf, d);
            a[i .. i + t.length] = t[];
            i += t.length;
        }
        free(da.ptr);
    }
    return a;
}

/**********************************************
 * Sort array of wchars.
 */

extern (C) wchar[] _adSortWchar(wchar[] a)
{
    if (a.length > 1)
    {
        auto da = mallocUTF32(a);
        _adSort(*cast(void[]*)&da, typeid(da[0]));
        size_t i = 0;
        foreach (dchar d; da)
        {   wchar[2] buf;
            auto t = toUTF16(buf, d);
            a[i .. i + t.length] = t[];
            i += t.length;
        }
        free(da.ptr);
    }
    return a;
}

/***************************************
 * Support for array equality test.
 * Returns:
 *      1       equal
 *      0       not equal
 */

extern (C) int _adEq(void[] a1, void[] a2, TypeInfo ti)
{
    debug(adi) printf("_adEq(a1.length = %d, a2.length = %d)\n", a1.length, a2.length);
    if (a1.length != a2.length)
        return 0; // not equal
    auto sz = ti.tsize;
    auto p1 = a1.ptr;
    auto p2 = a2.ptr;

    if (sz == 1)
        // We should really have a ti.isPOD() check for this
        return (memcmp(p1, p2, a1.length) == 0);

    for (size_t i = 0; i < a1.length; i++)
    {
        if (!ti.equals(p1 + i * sz, p2 + i * sz))
            return 0; // not equal
    }
    return 1; // equal
}

extern (C) int _adEq2(void[] a1, void[] a2, TypeInfo ti)
{
    debug(adi) printf("_adEq2(a1.length = %d, a2.length = %d)\n", a1.length, a2.length);
    if (a1.length != a2.length)
        return 0;               // not equal
    if (!ti.equals(&a1, &a2))
        return 0;
    return 1;
}
unittest
{
    debug(adi) printf("array.Eq unittest\n");

    auto a = "hello"c;

    assert(a != "hel");
    assert(a != "helloo");
    assert(a != "betty");
    assert(a == "hello");
    assert(a != "hxxxx");

    float[] fa = [float.nan];
    assert(fa != fa);
}

/***************************************
 * Support for array compare test.
 */

extern (C) int _adCmp(void[] a1, void[] a2, TypeInfo ti)
{
    debug(adi) printf("adCmp()\n");
    auto len = a1.length;
    if (a2.length < len)
        len = a2.length;
    auto sz = ti.tsize;
    void *p1 = a1.ptr;
    void *p2 = a2.ptr;

    if (sz == 1)
    {   // We should really have a ti.isPOD() check for this
        auto c = memcmp(p1, p2, len);
        if (c)
            return c;
    }
    else
    {
        for (size_t i = 0; i < len; i++)
        {
            auto c = ti.compare(p1 + i * sz, p2 + i * sz);
            if (c)
                return c;
        }
    }
    if (a1.length == a2.length)
        return 0;
    return (a1.length > a2.length) ? 1 : -1;
}

extern (C) int _adCmp2(void[] a1, void[] a2, TypeInfo ti)
{
    debug(adi) printf("_adCmp2(a1.length = %d, a2.length = %d)\n", a1.length, a2.length);
    return ti.compare(&a1, &a2);
}
unittest
{
    debug(adi) printf("array.Cmp unittest\n");

    auto a = "hello"c;

    assert(a >  "hel");
    assert(a >= "hel");
    assert(a <  "helloo");
    assert(a <= "helloo");
    assert(a >  "betty");
    assert(a >= "betty");
    assert(a == "hello");
    assert(a <= "hello");
    assert(a >= "hello");
    assert(a <  "я");
}

/***************************************
 * Support for array compare test.
 */

extern (C) int _adCmpChar(void[] a1, void[] a2)
{
  version (D_InlineAsm_X86)
  {
    asm
    {   naked                   ;

        push    EDI             ;
        push    ESI             ;

        mov    ESI,a1+4[4+ESP]  ;
        mov    EDI,a2+4[4+ESP]  ;

        mov    ECX,a1[4+ESP]    ;
        mov    EDX,a2[4+ESP]    ;

        cmp     ECX,EDX         ;
        jb      GotLength       ;

        mov     ECX,EDX         ;

GotLength:
        cmp    ECX,4            ;
        jb    DoBytes           ;

        // Do alignment if neither is dword aligned
        test    ESI,3           ;
        jz    Aligned           ;

        test    EDI,3           ;
        jz    Aligned           ;
DoAlign:
        mov    AL,[ESI]         ; //align ESI to dword bounds
        mov    DL,[EDI]         ;

        cmp    AL,DL            ;
        jnz    Unequal          ;

        inc    ESI              ;
        inc    EDI              ;

        test    ESI,3           ;

        lea    ECX,[ECX-1]      ;
        jnz    DoAlign          ;
Aligned:
        mov    EAX,ECX          ;

        // do multiple of 4 bytes at a time

        shr    ECX,2            ;
        jz    TryOdd            ;

        repe                    ;
        cmpsd                   ;

        jnz    UnequalQuad      ;

TryOdd:
        mov    ECX,EAX          ;
DoBytes:
        // if still equal and not end of string, do up to 3 bytes slightly
        // slower.

        and    ECX,3            ;
        jz    Equal             ;

        repe                    ;
        cmpsb                   ;

        jnz    Unequal          ;
Equal:
        mov    EAX,a1[4+ESP]    ;
        mov    EDX,a2[4+ESP]    ;

        sub    EAX,EDX          ;
        pop    ESI              ;

        pop    EDI              ;
        ret                     ;

UnequalQuad:
        mov    EDX,[EDI-4]      ;
        mov    EAX,[ESI-4]      ;

        cmp    AL,DL            ;
        jnz    Unequal          ;

        cmp    AH,DH            ;
        jnz    Unequal          ;

        shr    EAX,16           ;

        shr    EDX,16           ;

        cmp    AL,DL            ;
        jnz    Unequal          ;

        cmp    AH,DH            ;
Unequal:
        sbb    EAX,EAX          ;
        pop    ESI              ;

        or     EAX,1            ;
        pop    EDI              ;

        ret                     ;
    }
  }
  else
  {
    debug(adi) printf("adCmpChar()\n");
    auto len = a1.length;
    if (a2.length < len)
        len = a2.length;
    auto c = memcmp(cast(char *)a1.ptr, cast(char *)a2.ptr, len);
    if (!c)
        c = cast(int)a1.length - cast(int)a2.length;
    return c;
  }
}

unittest
{
    debug(adi) printf("array.CmpChar unittest\n");

    auto a = "hello"c;

    assert(a >  "hel");
    assert(a >= "hel");
    assert(a <  "helloo");
    assert(a <= "helloo");
    assert(a >  "betty");
    assert(a >= "betty");
    assert(a == "hello");
    assert(a <= "hello");
    assert(a >= "hello");
}
