/**
 * D header file for the Solaris priocntl(2) and priocntlset(2) functions.
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
module core.sys.solaris.sys.priocntl;

version (Solaris):
nothrow:
@nogc:
extern (C):

import core.sys.posix.sys.types : caddr_t, id_t;
import core.sys.posix.sys.wait : idtype_t;
import core.stdc.config : c_long;
import core.sys.solaris.sys.procset;
import core.sys.solaris.sys.types : pri_t;

c_long priocntl(idtype_t, id_t, int, ...);
c_long priocntlset(procset_t*, int, ...);


enum PC_GETCID       = 0;       /* Get class ID */
enum PC_GETCLINFO    = 1;       /* Get info about a configured class */
enum PC_SETPARMS     = 2;       /* Set scheduling parameters */
enum PC_GETPARMS     = 3;       /* Get scheduling parameters */
enum PC_ADMIN        = 4;       /* Scheduler administration (used by */
                                /* dispadmin(1M), not for general use) */
enum PC_GETPRIRANGE  = 5;       /* Get priority range for a class */
                                /* posix.4; scheduling, not for general use */
enum PC_DONICE       = 6;       /* Set or get nice value */
enum PC_SETXPARMS    = 7;       /* Set extended scheduling parameters */
enum PC_GETXPARMS    = 8;       /* Get extended scheduling parameters */
enum PC_SETDFLCL     = 9;       /* Set default class, not for general use */
enum PC_GETDFLCL     = 10;      /* Get default class, not for general use */
enum PC_DOPRIO       = 11;      /* Set or get priority, not for general use */

enum PC_CLNULL       = -1;

enum PC_CLNMSZ       = 16;
enum PC_CLINFOSZ     = (32 / int.sizeof);
enum PC_CLPARMSZ     = (32 / int.sizeof);

enum PC_GETNICE = 0;
enum PC_SETNICE = 1;

enum PC_GETPRIO      = 0;
enum PC_SETPRIO      = 1;

struct pcinfo_t
{
    id_t                pc_cid;     // class id
    char[PC_CLNMSZ]     pc_clname=0;// class name
    int[PC_CLINFOSZ]    pc_clinfo;  // class information
}

struct pcparms_t
{
    id_t                pc_cid;     // process class
    int[PC_CLPARMSZ]    pc_clparms; //class specific parameters
}

struct pcnice_t
{
    int pc_val; // nice value
    int pc_op;  // type of operation, set or get
}

struct pcprio_t
{
    int     pc_op;  // type of operation, set or get
    id_t    pc_cid; // class id
    int     pc_val; // priority value
}

// Used by the priocntl varargs interface
// PC_SETXPARMS and PC_GETXPARMS
enum PC_VAPARMCNT = 8;  // max # of kv pairs
enum PC_KY_NULL   = 0;  // kv chain terminator
enum PC_KY_CLNAME = 1;  // get the class name of a process or LWP

struct pc_vaparm_t
{
    int     pc_key;
    ulong   pc_parm;
}

struct pc_vaparms_t
{
    int                         pc_vaparmscnt; // # of kv pairs
    pc_vaparm_t[PC_VAPARMCNT]   pc_parm;
}

// Not for general use
struct pcpri_t
{
    id_t    pc_cid;
    pri_t   pc_clpmax;
    pri_t   pc_clpmin;
}

struct pcadmin_t
{
    id_t    pc_cid;
    caddr_t pc_cladmin;
}
