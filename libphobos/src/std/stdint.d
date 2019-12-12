// Written in the D programming language.

/**
 *
    D constrains integral types to specific sizes. But efficiency
    of different sizes varies from machine to machine,
    pointer sizes vary, and the maximum integer size varies.
    <b>stdint</b> offers a portable way of trading off size
    vs efficiency, in a manner compatible with the <tt>stdint.h</tt>
    definitions in C.

    In the table below, the $(B exact alias)es are types of exactly the
    specified number of bits.
    The $(B at least alias)es are at least the specified number of bits
    large, and can be larger.
    The $(B fast alias)es are the fastest integral type supported by the
    processor that is at least as wide as the specified number of bits.

    The aliases are:

    $(ATABLE $(TR
    $(TH Exact Alias)
    $(TH Description)
    $(TH At Least Alias)
    $(TH Description)
    $(TH Fast Alias)
    $(TH Description)
    )$(TR
    $(TD int8_t)
    $(TD exactly 8 bits signed)
    $(TD int_least8_t)
    $(TD at least 8 bits signed)
    $(TD int_fast8_t)
    $(TD fast 8 bits signed)
    )$(TR
    $(TD uint8_t)
    $(TD exactly 8 bits unsigned)
    $(TD uint_least8_t)
    $(TD at least 8 bits unsigned)
    $(TD uint_fast8_t)
    $(TD fast 8 bits unsigned)

    )$(TR
    $(TD int16_t)
    $(TD exactly 16 bits signed)
    $(TD int_least16_t)
    $(TD at least 16 bits signed)
    $(TD int_fast16_t)
    $(TD fast 16 bits signed)
    )$(TR
    $(TD uint16_t)
    $(TD exactly 16 bits unsigned)
    $(TD uint_least16_t)
    $(TD at least 16 bits unsigned)
    $(TD uint_fast16_t)
    $(TD fast 16 bits unsigned)

    )$(TR
    $(TD int32_t)
    $(TD exactly 32 bits signed)
    $(TD int_least32_t)
    $(TD at least 32 bits signed)
    $(TD int_fast32_t)
    $(TD fast 32 bits signed)
    )$(TR
    $(TD uint32_t)
    $(TD exactly 32 bits unsigned)
    $(TD uint_least32_t)
    $(TD at least 32 bits unsigned)
    $(TD uint_fast32_t)
    $(TD fast 32 bits unsigned)

    )$(TR
    $(TD int64_t)
    $(TD exactly 64 bits signed)
    $(TD int_least64_t)
    $(TD at least 64 bits signed)
    $(TD int_fast64_t)
    $(TD fast 64 bits signed)
    )$(TR
    $(TD uint64_t)
    $(TD exactly 64 bits unsigned)
    $(TD uint_least64_t)
    $(TD at least 64 bits unsigned)
    $(TD uint_fast64_t)
    $(TD fast 64 bits unsigned)
    ))

    The ptr aliases are integral types guaranteed to be large enough
    to hold a pointer without losing bits:

    $(ATABLE $(TR
    $(TH Alias)
    $(TH Description)
    )$(TR
    $(TD intptr_t)
    $(TD signed integral type large enough to hold a pointer)
    )$(TR
    $(TD uintptr_t)
    $(TD unsigned integral type large enough to hold a pointer)
    ))

    The max aliases are the largest integral types:

    $(ATABLE $(TR
    $(TH Alias)
    $(TH Description)
    )$(TR
    $(TD intmax_t)
    $(TD the largest signed integral type)
    )$(TR
    $(TD uintmax_t)
    $(TD the largest unsigned integral type)
    ))

 * Macros:
 *  ATABLE=<table border="1" cellspacing="0" cellpadding="5">$0</table>
 *
 * Copyright: Copyright Digital Mars 2000 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright)
 * Source:    $(PHOBOSSRC std/_stdint.d)
 */
/*          Copyright Digital Mars 2000 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.stdint;

public import core.stdc.stdint;
