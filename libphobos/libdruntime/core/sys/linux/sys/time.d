/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sociomantic Labs GmbH.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Leandro Lucarella
 */

/*          Copyright Sociomantic Labs GmbH.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.linux.sys.time;

import core.sys.linux.config;
public import core.sys.posix.sys.time;  // timeval

version (linux):

/* macros provided to operate on timeval structures
 * they are extern(D) because they are not really C symbols, just macros
 */
extern (D) pure @safe @nogc nothrow {

    void timeradd(const timeval* a, const timeval* b,
            timeval* result)
    {
        result.tv_sec = a.tv_sec + b.tv_sec;
        result.tv_usec = a.tv_usec + b.tv_usec;
        if (result.tv_usec >= 1_000_000)
        {
            ++result.tv_sec;
            result.tv_usec -= 1_000_000;
        }
    }

    void timersub(const timeval* a, const timeval* b,
            timeval *result)
    {
        result.tv_sec = a.tv_sec - b.tv_sec;
        result.tv_usec = a.tv_usec - b.tv_usec;
        if (result.tv_usec < 0) {
            --result.tv_sec;
            result.tv_usec += 1_000_000;
        }
    }

    void timerclear(timeval* tvp)
    {
        (tvp.tv_sec = tvp.tv_usec = 0);
    }

    int timerisset(timeval* tvp)
    {
        return cast(int) (tvp.tv_sec || tvp.tv_usec);
    }

    int timercmp(string CMP)(const timeval* a, const timeval* b)
    {
        return cast(int)
               mixin("((a.tv_sec == b.tv_sec) ?" ~
                     "(a.tv_usec" ~ CMP ~ "b.tv_usec) :" ~
                     "(a.tv_sec"  ~ CMP ~ "b.tv_sec))");
    }

}
