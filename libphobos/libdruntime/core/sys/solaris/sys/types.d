/**
 * D header file that defines Solaris-specific types.
 *
 * Copyright:   Copyright 2014 Jason King.
 * License:     $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:     Jason King
 */

/*
 * Copyright 2014 Jason King.
 * Distributed under the Boost Software License, Version 1.0.
 * See accompanying file LICENSE or copy at
 *  http://www.boost.org/LICENSE_1_0.txt
 */

module core.sys.solaris.sys.types;

version (Solaris):
nothrow:
@nogc:

alias short pri_t;

enum P_MYID = -1;
