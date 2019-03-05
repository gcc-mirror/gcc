/**
 * D header file for GNU/Linux.
 *
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Martin Nowak
 */
module core.sys.linux.sys.sysinfo;

version (linux) extern(C) @nogc nothrow:

import core.sys.linux.config;

// linux/sysinfo.h
enum SI_LOAD_SHIFT = 16;

struct sysinfo_
{
    c_long uptime;     /* Seconds since boot */
    c_ulong[3] loads;  /* 1, 5, and 15 minute load averages */
    c_ulong totalram;  /* Total usable main memory size */
    c_ulong freeram;   /* Available memory size */
    c_ulong sharedram; /* Amount of shared memory */
    c_ulong bufferram; /* Memory used by buffers */
    c_ulong totalswap; /* Total swap space size */
    c_ulong freeswap;  /* swap space still available */
    ushort procs;      /* Number of current processes */
    ushort pad;        /* Explicit padding for m68k */
    c_ulong totalhigh; /* Total high memory size */
    c_ulong freehigh;  /* Available high memory size */
    uint mem_unit;     /* Memory unit size in bytes */
    ubyte[20-2 * c_ulong.sizeof - uint.sizeof] _f; /* Padding: libc5 uses this.. */
}


int sysinfo(sysinfo_ *info);
int get_nprocs_conf();
int get_nprocs();
c_long get_phys_pages();
c_long get_avphys_pages();
