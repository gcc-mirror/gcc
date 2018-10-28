/**
 * D header file for POSIX.
 *
 * Copyright: 2016 Sociomantic Labs GmbH
 * License:   <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
 * Authors:   Leandro Lucarella
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sociomantic Labs GmbH 2016.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.libgen;

@nogc nothrow:
extern (C):
version (Posix):

char* basename(char*);
char* dirname(char*);
