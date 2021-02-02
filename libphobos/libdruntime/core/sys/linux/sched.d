/*******************************************************************************

    D binding for Linux specific scheduler control and thread spawning
    methods.

    Defines functions sched_setaffinity and sched_getaffinity and the data
    types they operate on, as well as clone and unshare and their related
    constants.

    Copyright:  Copyright (c) 2016 Sociomantic Labs. All rights reserved.
    License:    $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:    Nemanja Boric

*******************************************************************************/


module core.sys.linux.sched;

import core.bitop : popcnt;
import core.stdc.stdlib : malloc, free;
import core.sys.posix.sched;
import core.sys.posix.config;
import core.sys.posix.sys.types;

version (linux):
extern (C):
@nogc:
nothrow:
@system:


private // helpers
{

    /* Size definition for CPU sets.  */
    enum
    {
        __CPU_SETSIZE = 1024,
        __NCPUBITS  = 8 * cpu_mask.sizeof,
    }

    /* Macros */

    /* Basic access functions.  */
    size_t __CPUELT(size_t cpu) pure
    {
        return cpu / __NCPUBITS;
    }
    cpu_mask __CPUMASK(size_t cpu) pure
    {
        return 1UL << (cpu % __NCPUBITS);
    }

    cpu_set_t* __CPU_ALLOC(size_t count)
    {
        return cast(cpu_set_t*) malloc(__CPU_ALLOC_SIZE(count));
    }

    size_t __CPU_ALLOC_SIZE(size_t count) pure
    {
        return ((count + __NCPUBITS - 1) / __NCPUBITS) * cpu_mask.sizeof;
    }

    void __CPU_FREE(cpu_set_t* set)
    {
        free(cast(void*) set);
    }

    cpu_mask __CPU_SET_S(size_t cpu, size_t setsize, cpu_set_t* cpusetp) pure
    {
        if (cpu < 8 * setsize)
        {
            cpusetp.__bits[__CPUELT(cpu)] |= __CPUMASK(cpu);
            return __CPUMASK(cpu);
        }

        return 0;
    }

    bool __CPU_ISSET_S(size_t cpu, size_t setsize, cpu_set_t* cpusetp) pure
    {
        if (cpu < 8 * setsize)
            return (cpusetp.__bits[__CPUELT(cpu)] & __CPUMASK(cpu)) != 0;
        return false;
    }

    int __CPU_COUNT_S(size_t setsize, cpu_set_t* cpusetp) pure
    {
        int s = 0;
        foreach (i; cpusetp.__bits[0 .. (setsize / cpu_mask.sizeof)])
            s += popcnt(i);
        return s;
    }
}

/// Type for array elements in 'cpu_set_t'.
alias c_ulong cpu_mask;

/// Data structure to describe CPU mask.
struct cpu_set_t
{
    cpu_mask[__CPU_SETSIZE / __NCPUBITS] __bits;
}

/// Access macros for 'cpu_set' (missing a lot of them)

cpu_set_t* CPU_ALLOC(size_t count)
{
    return __CPU_ALLOC(count);
}

size_t CPU_ALLOC_SIZE(size_t count) pure
{
    return __CPU_ALLOC_SIZE(count);
}

void CPU_FREE(cpu_set_t* set)
{
    __CPU_FREE(set);
}

cpu_mask CPU_SET(size_t cpu, cpu_set_t* cpusetp) pure
{
     return __CPU_SET_S(cpu, cpu_set_t.sizeof, cpusetp);
}

bool CPU_ISSET(size_t cpu, cpu_set_t* cpusetp) pure
{
    return __CPU_ISSET_S(cpu, cpu_set_t.sizeof, cpusetp);
}

int CPU_COUNT(cpu_set_t* cpusetp) pure
{
    return __CPU_COUNT_S(cpu_set_t.sizeof, cpusetp);
}

int CPU_COUNT_S(size_t setsize, cpu_set_t* cpusetp) pure
{
    return __CPU_COUNT_S(setsize, cpusetp);
}

/* Scheduler control functions */
int sched_setaffinity(pid_t pid, size_t cpusetsize, cpu_set_t *mask);
int sched_getaffinity(pid_t pid, size_t cpusetsize, cpu_set_t *mask);

/* Clone and related functions and constants */
int clone(int function(void*), void* child_stack, int flags, void* arg, ...);
int unshare(int flags) @trusted;

version (CRuntime_Glibc)
{
    /* Determine CPU on which the calling thread is running */
    int sched_getcpu();
}

enum CLONE_FILES = 0x400;
enum CLONE_FS = 0x200;
enum CLONE_NEWCGROUP = 0x2000000;
enum CLONE_NEWIPC = 0x8000000;
enum CLONE_NEWNET = 0x40000000;
enum CLONE_NEWNS = 0x20000;
enum CLONE_NEWPID = 0x20000000;
enum CLONE_NEWUSER = 0x10000000;
enum CLONE_NEWUTS = 0x4000000;
enum CLONE_SIGHAND = 0x800;
enum CLONE_SYSVSEM = 0x40000;
enum CLONE_THREAD = 0x10000;
enum CLONE_VFORK = 0x4000;
enum CLONE_VM = 0x100;
