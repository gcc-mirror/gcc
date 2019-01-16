/**
 * D header file for FreeBSD.
 *
 * Authors:   Martin Nowak
 */
module core.sys.freebsd.sys._cpuset;

version (FreeBSD):
extern (C) pure nothrow @nogc:

public import core.sys.freebsd.sys._bitset;

static if (is(typeof(_KERNEL)))
    alias CPU_SETSIZE = MAXCPU;

enum CPU_MAXSIZE = 256;

static if (!is(typeof(CPU_SETSIZE)))
    alias CPU_SETSIZE = CPU_MAXSIZE;

enum _NCPUBITS = _BITSET_BITS;
enum _NCPUWORDS = __bitset_words!CPU_SETSIZE;

alias _cpuset = BITSET_DEFINE!(CPU_SETSIZE);
alias cpuset_t = _cpuset;

// no idea how to translate those
//#define CPUSET_FSET BITSET_FSET(_NCPUWORDS)
//#define CPUSET_T_INITIALIZER BITSET_T_INITIALIZER
