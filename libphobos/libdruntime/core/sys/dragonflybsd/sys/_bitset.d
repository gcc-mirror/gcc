/**
 * D header file for DragonFlyBSD.
 *
 * Authors: Martin Nowak, Diederik de Groot(port:DragonFlyBSD)
 * Copied:  From core/sys/freebsd/sys
 */
module core.sys.dragonflybsd.sys._bitset;

version (DragonFlyBSD):
extern (C) pure nothrow @nogc @system:

import core.stdc.config : c_long;

enum NBBY = 8; // number of bits per byte

enum _BITSET_BITS = c_long.sizeof * NBBY;

enum __bitset_words(size_t s) = (s + _BITSET_BITS - 1) / _BITSET_BITS;

c_long __bitset_mask(size_t s)(size_t n)
{
    static if (__bitset_words!s == 1)
        return (cast(c_long)1) << n;
    else
        return (cast(c_long)1) << n % _BITSET_BITS;
}

size_t __bitset_word(size_t s)(size_t n)
{
    static if (__bitset_words!s == 1)
        return 0;
    else
        return n / _BITSET_BITS;
}

struct BITSET_DEFINE(size_t s)
{
    c_long[__bitset_words!s] __bits;
}

// no idea how to translate those
//#define BITSET_T_INITIALIZER(x)                     \
//    { .__bits = { x } }
//
//#define BITSET_FSET(n)                          \
//    [ 0 ... ((n) - 1) ] = (-1L)
