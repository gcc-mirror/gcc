/**
 * Contains the internal GC interface.
 *
 * Copyright: Copyright Digital Mars 2016.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Sean Kelly, Jeremy DeHaan
 */

 /*          Copyright Digital Mars 2016.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module gc.gcinterface;

static import core.memory;
alias BlkAttr = core.memory.GC.BlkAttr;
alias BlkInfo = core.memory.GC.BlkInfo;

alias RootIterator = int delegate(scope int delegate(ref Root) nothrow dg);
alias RangeIterator = int delegate(scope int delegate(ref Range) nothrow dg);


struct Root
{
    void* proot;
    alias proot this;
}

struct Range
{
    void* pbot;
    void* ptop;
    TypeInfo ti; // should be tail const, but doesn't exist for references
    alias pbot this; // only consider pbot for relative ordering (opCmp)
}

interface GC
{

    /*
     *
     */
    void Dtor();

    /**
     *
     */
    void enable();

    /**
     *
     */
    void disable();

    /**
     *
     */
    void collect() nothrow;

    /**
     *
     */
    void collectNoStack() nothrow;

    /**
     * minimize free space usage
     */
    void minimize() nothrow;

    /**
     *
     */
    uint getAttr(void* p) nothrow;

    /**
     *
     */
    uint setAttr(void* p, uint mask) nothrow;

    /**
     *
     */
    uint clrAttr(void* p, uint mask) nothrow;

    /**
     *
     */
    void* malloc(size_t size, uint bits, const TypeInfo ti) nothrow;

    /*
     *
     */
    BlkInfo qalloc(size_t size, uint bits, const TypeInfo ti) nothrow;

    /*
     *
     */
    void* calloc(size_t size, uint bits, const TypeInfo ti) nothrow;

    /*
     *
     */
    void* realloc(void* p, size_t size, uint bits, const TypeInfo ti) nothrow;

    /**
     * Attempt to in-place enlarge the memory block pointed to by p by at least
     * minsize bytes, up to a maximum of maxsize additional bytes.
     * This does not attempt to move the memory block (like realloc() does).
     *
     * Returns:
     *  0 if could not extend p,
     *  total size of entire memory block if successful.
     */
    size_t extend(void* p, size_t minsize, size_t maxsize, const TypeInfo ti) nothrow;

    /**
     *
     */
    size_t reserve(size_t size) nothrow;

    /**
     *
     */
    void free(void* p) nothrow;

    /**
     * Determine the base address of the block containing p.  If p is not a gc
     * allocated pointer, return null.
     */
    void* addrOf(void* p) nothrow;

    /**
     * Determine the allocated size of pointer p.  If p is an interior pointer
     * or not a gc allocated pointer, return 0.
     */
    size_t sizeOf(void* p) nothrow;

    /**
     * Determine the base address of the block containing p.  If p is not a gc
     * allocated pointer, return null.
     */
    BlkInfo query(void* p) nothrow;

    /**
     * Retrieve statistics about garbage collection.
     * Useful for debugging and tuning.
     */
    core.memory.GC.Stats stats() nothrow;

    /**
     * add p to list of roots
     */
    void addRoot(void* p) nothrow @nogc;

    /**
     * remove p from list of roots
     */
    void removeRoot(void* p) nothrow @nogc;

    /**
     *
     */
    @property RootIterator rootIter() @nogc;

    /**
     * add range to scan for roots
     */
    void addRange(void* p, size_t sz, const TypeInfo ti) nothrow @nogc;

    /**
     * remove range
     */
    void removeRange(void* p) nothrow @nogc;

    /**
     *
     */
    @property RangeIterator rangeIter() @nogc;

    /**
     * run finalizers
     */
    void runFinalizers(in void[] segment) nothrow;

    /*
     *
     */
    bool inFinalizer() nothrow;
}
