/**
 * OS X support for dynamic libraries.
 *
 * Copyright: Copyright Digital Mars 2010 - 2010.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright
 */

/*          Copyright Digital Mars 2010 - 2010.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
void* _Dmain __attribute__((weak));

char rt_init ();
char rt_term ();

__attribute__((constructor)) static void initializer ()
{
    rt_init();
}

__attribute__((destructor)) static void finalizer ()
{
    rt_term();
}

