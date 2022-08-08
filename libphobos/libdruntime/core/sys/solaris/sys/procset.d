/**
 * D header file defining a process set.
 *
 * Copyright:   Copyright 2014 Jason King.
 * License:     $(HTTP www.boost.org/LICENSE_1.0.txt, Boost License 1.0).
 * Authors:     Jason King
 */

/*
 * Copyright 2014 Jason King.
 * Distributed under the Boost Software License, Version 1.0.
 * See accompanying file LICENSE or copy at
 *  http://www.boost.org/LICENSE_1_0.txt
 */
module core.sys.solaris.sys.procset;

version (Solaris):
nothrow:
@nogc:

import core.sys.posix.sys.types : id_t;
import core.sys.posix.sys.wait : idtype_t;

enum P_INITPID  = 1;
enum P_INITUID  = 0;
enum P_INITPGID = 0;

enum idop_t
{
    POP_DIFF,
    POP_AND,
    POP_OR,
    POP_XOR
}

struct procset_t
{
    idop_t      p_op;
    idtype_t    p_lidtype;
    id_t        p_lid;
    idtype_t    p_ridtype;
    id_t        p_rid;
}

void setprocset(ref procset_t psp, idop_t op, idtype_t ltype, id_t lid, idtype_t rtype, id_t rid)
{
    psp.p_op = op;
    psp.p_lidtype = ltype;
    psp.p_lid = lid;
    psp.p_ridtype = rtype;
    psp.p_rid = rid;
}
