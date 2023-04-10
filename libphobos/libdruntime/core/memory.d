/**
 * This module provides an interface to the garbage collector used by
 * applications written in the D programming language. It allows the
 * garbage collector in the runtime to be swapped without affecting
 * binary compatibility of applications.
 *
 * Using this module is not necessary in typical D code. It is mostly
 * useful when doing low-level _memory management.
 *
 * Notes_to_users:
 *
   $(OL
   $(LI The GC is a conservative mark-and-sweep collector. It only runs a
        collection cycle when an allocation is requested of it, never
        otherwise. Hence, if the program is not doing allocations,
        there will be no GC collection pauses. The pauses occur because
        all threads the GC knows about are halted so the threads' stacks
        and registers can be scanned for references to GC allocated data.
   )

   $(LI The GC does not know about threads that were created by directly calling
        the OS/C runtime thread creation APIs and D threads that were detached
        from the D runtime after creation.
        Such threads will not be paused for a GC collection, and the GC might not detect
        references to GC allocated data held by them. This can cause memory corruption.
        There are several ways to resolve this issue:
        $(OL
        $(LI Do not hold references to GC allocated data in such threads.)
        $(LI Register/unregister such data with calls to $(LREF addRoot)/$(LREF removeRoot) and
        $(LREF addRange)/$(LREF removeRange).)
        $(LI Maintain another reference to that same data in another thread that the
        GC does know about.)
        $(LI Disable GC collection cycles while that thread is active with $(LREF disable)/$(LREF enable).)
        $(LI Register the thread with the GC using $(REF thread_attachThis, core,thread,osthread)/$(REF thread_detachThis, core,thread,threadbase).)
        )
   )
   )
 *
 * Notes_to_implementors:
 * $(UL
 * $(LI On POSIX systems, the signals `SIGRTMIN` and `SIGRTMIN + 1` are reserved
 *   by this module for use in the garbage collector implementation.
 *   Typically, they will be used to stop and resume other threads
 *   when performing a collection, but an implementation may choose
 *   not to use this mechanism (or not stop the world at all, in the
 *   case of concurrent garbage collectors).)
 *
 * $(LI Registers, the stack, and any other _memory locations added through
 *   the $(D GC.$(LREF addRange)) function are always scanned conservatively.
 *   This means that even if a variable is e.g. of type $(D float),
 *   it will still be scanned for possible GC pointers. And, if the
 *   word-interpreted representation of the variable matches a GC-managed
 *   _memory block's address, that _memory block is considered live.)
 *
 * $(LI Implementations are free to scan the non-root heap in a precise
 *   manner, so that fields of types like $(D float) will not be considered
 *   relevant when scanning the heap. Thus, casting a GC pointer to an
 *   integral type (e.g. $(D size_t)) and storing it in a field of that
 *   type inside the GC heap may mean that it will not be recognized
 *   if the _memory block was allocated with precise type info or with
 *   the $(D GC.BlkAttr.$(LREF NO_SCAN)) attribute.)
 *
 * $(LI Destructors will always be executed while other threads are
 *   active; that is, an implementation that stops the world must not
 *   execute destructors until the world has been resumed.)
 *
 * $(LI A destructor of an object must not access object references
 *   within the object. This means that an implementation is free to
 *   optimize based on this rule.)
 *
 * $(LI An implementation is free to perform heap compaction and copying
 *   so long as no valid GC pointers are invalidated in the process.
 *   However, _memory allocated with $(D GC.BlkAttr.$(LREF NO_MOVE)) must
 *   not be moved/copied.)
 *
 * $(LI Implementations must support interior pointers. That is, if the
 *   only reference to a GC-managed _memory block points into the
 *   middle of the block rather than the beginning (for example), the
 *   GC must consider the _memory block live. The exception to this
 *   rule is when a _memory block is allocated with the
 *   $(D GC.BlkAttr.$(LREF NO_INTERIOR)) attribute; it is the user's
 *   responsibility to make sure such _memory blocks have a proper pointer
 *   to them when they should be considered live.)
 *
 * $(LI It is acceptable for an implementation to store bit flags into
 *   pointer values and GC-managed _memory blocks, so long as such a
 *   trick is not visible to the application. In practice, this means
 *   that only a stop-the-world collector can do this.)
 *
 * $(LI Implementations are free to assume that GC pointers are only
 *   stored on word boundaries. Unaligned pointers may be ignored
 *   entirely.)
 *
 * $(LI Implementations are free to run collections at any point. It is,
 *   however, recommendable to only do so when an allocation attempt
 *   happens and there is insufficient _memory available.)
 * )
 *
 * Copyright: Copyright Sean Kelly 2005 - 2015.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Sean Kelly, Alex Rønne Petersen
 * Source:    $(DRUNTIMESRC core/_memory.d)
 */

module core.memory;

version (ARM)
    version = AnyARM;
else version (AArch64)
    version = AnyARM;

version (iOS)
    version = iOSDerived;
else version (TVOS)
    version = iOSDerived;
else version (WatchOS)
    version = iOSDerived;

private
{
    extern (C) uint gc_getAttr( void* p ) pure nothrow;
    extern (C) uint gc_setAttr( void* p, uint a ) pure nothrow;
    extern (C) uint gc_clrAttr( void* p, uint a ) pure nothrow;

    extern (C) void*   gc_addrOf( void* p ) pure nothrow @nogc;
    extern (C) size_t  gc_sizeOf( void* p ) pure nothrow @nogc;

    struct BlkInfo_
    {
        void*  base;
        size_t size;
        uint   attr;
    }

    extern (C) BlkInfo_ gc_query(return scope void* p) pure nothrow;
    extern (C) GC.Stats gc_stats ( ) @safe nothrow @nogc;
    extern (C) GC.ProfileStats gc_profileStats ( ) nothrow @nogc @safe;
}

version (CoreDoc)
{
    /**
     * The minimum size of a system page in bytes.
     *
     * This is a compile time, platform specific value. This value might not
     * be accurate, since it might be possible to change this value. Whenever
     * possible, please use $(LREF pageSize) instead, which is initialized
     * during runtime.
     *
     * The minimum size is useful when the context requires a compile time known
     * value, like the size of a static array: `ubyte[minimumPageSize] buffer`.
     */
    enum minimumPageSize : size_t;
}
else version (AnyARM)
{
    version (iOSDerived)
        enum size_t minimumPageSize = 16384;
    else
        enum size_t minimumPageSize = 4096;
}
else
    enum size_t minimumPageSize = 4096;

///
unittest
{
    ubyte[minimumPageSize] buffer;
}

/**
 * The size of a system page in bytes.
 *
 * This value is set at startup time of the application. It's safe to use
 * early in the start process, like in shared module constructors and
 * initialization of the D runtime itself.
 */
immutable size_t pageSize;

///
unittest
{
    ubyte[] buffer = new ubyte[pageSize];
}

// The reason for this elaborated way of declaring a function is:
//
// * `pragma(crt_constructor)` is used to declare a constructor that is called by
// the C runtime, before C main. This allows the `pageSize` value to be used
// during initialization of the D runtime. This also avoids any issues with
// static module constructors and circular references.
//
// * `pragma(mangle)` is used because `pragma(crt_constructor)` requires a
// function with C linkage. To avoid any name conflict with other C symbols,
// standard D mangling is used.
//
// * The extra function declaration, without the body, is to be able to get the
// D mangling of the function without the need to hardcode the value.
//
// * The extern function declaration also has the side effect of making it
// impossible to manually call the function with standard syntax. This is to
// make it more difficult to call the function again, manually.
private void initialize();
pragma(crt_constructor)
pragma(mangle, initialize.mangleof)
private extern (C) void _initialize() @system
{
    version (Posix)
    {
        import core.sys.posix.unistd : sysconf, _SC_PAGESIZE;

        (cast() pageSize) = cast(size_t) sysconf(_SC_PAGESIZE);
    }
    else version (Windows)
    {
        import core.sys.windows.winbase : GetSystemInfo, SYSTEM_INFO;

        SYSTEM_INFO si;
        GetSystemInfo(&si);
        (cast() pageSize) = cast(size_t) si.dwPageSize;
    }
    else
        static assert(false, __FUNCTION__ ~ " is not implemented on this platform");
}

/**
 * This struct encapsulates all garbage collection functionality for the D
 * programming language.
 */
struct GC
{
    @disable this();

    /**
     * Aggregation of GC stats to be exposed via public API
     */
    static struct Stats
    {
        /// number of used bytes on the GC heap (might only get updated after a collection)
        size_t usedSize;
        /// number of free bytes on the GC heap (might only get updated after a collection)
        size_t freeSize;
        /// number of bytes allocated for current thread since program start
        ulong allocatedInCurrentThread;
    }

    /**
     * Aggregation of current profile information
     */
    static struct ProfileStats
    {
        import core.time : Duration;
        /// total number of GC cycles
        size_t numCollections;
        /// total time spent doing GC
        Duration totalCollectionTime;
        /// total time threads were paused doing GC
        Duration totalPauseTime;
        /// largest time threads were paused during one GC cycle
        Duration maxPauseTime;
        /// largest time spent doing one GC cycle
        Duration maxCollectionTime;
    }

extern(C):

    /**
     * Enables automatic garbage collection behavior if collections have
     * previously been suspended by a call to disable.  This function is
     * reentrant, and must be called once for every call to disable before
     * automatic collections are enabled.
     */
    pragma(mangle, "gc_enable") static void enable() nothrow pure;


    /**
     * Disables automatic garbage collections performed to minimize the
     * process footprint.  Collections may continue to occur in instances
     * where the implementation deems necessary for correct program behavior,
     * such as during an out of memory condition.  This function is reentrant,
     * but enable must be called once for each call to disable.
     */
    pragma(mangle, "gc_disable") static void disable() nothrow pure;


    /**
     * Begins a full collection.  While the meaning of this may change based
     * on the garbage collector implementation, typical behavior is to scan
     * all stack segments for roots, mark accessible memory blocks as alive,
     * and then to reclaim free space.  This action may need to suspend all
     * running threads for at least part of the collection process.
     */
    pragma(mangle, "gc_collect") static void collect() nothrow pure;

    /**
     * Indicates that the managed memory space be minimized by returning free
     * physical memory to the operating system.  The amount of free memory
     * returned depends on the allocator design and on program behavior.
     */
    pragma(mangle, "gc_minimize") static void minimize() nothrow pure;

extern(D):

    /**
     * Elements for a bit field representing memory block attributes.  These
     * are manipulated via the getAttr, setAttr, clrAttr functions.
     */
    enum BlkAttr : uint
    {
        NONE        = 0b0000_0000, /// No attributes set.
        FINALIZE    = 0b0000_0001, /// Finalize the data in this block on collect.
        NO_SCAN     = 0b0000_0010, /// Do not scan through this block on collect.
        NO_MOVE     = 0b0000_0100, /// Do not move this memory block on collect.
        /**
        This block contains the info to allow appending.

        This can be used to manually allocate arrays. Initial slice size is 0.

        Note: The slice's usable size will not match the block size. Use
        $(LREF capacity) to retrieve actual usable capacity.

        Example:
        ----
        // Allocate the underlying array.
        int*  pToArray = cast(int*)GC.malloc(10 * int.sizeof, GC.BlkAttr.NO_SCAN | GC.BlkAttr.APPENDABLE);
        // Bind a slice. Check the slice has capacity information.
        int[] slice = pToArray[0 .. 0];
        assert(capacity(slice) > 0);
        // Appending to the slice will not relocate it.
        slice.length = 5;
        slice ~= 1;
        assert(slice.ptr == p);
        ----
        */
        APPENDABLE  = 0b0000_1000,

        /**
        This block is guaranteed to have a pointer to its base while it is
        alive.  Interior pointers can be safely ignored.  This attribute is
        useful for eliminating false pointers in very large data structures
        and is only implemented for data structures at least a page in size.
        */
        NO_INTERIOR = 0b0001_0000,

        STRUCTFINAL = 0b0010_0000, // the block has a finalizer for (an array of) structs
    }


    /**
     * Contains aggregate information about a block of managed memory.  The
     * purpose of this struct is to support a more efficient query style in
     * instances where detailed information is needed.
     *
     * base = A pointer to the base of the block in question.
     * size = The size of the block, calculated from base.
     * attr = Attribute bits set on the memory block.
     */
    alias BlkInfo = BlkInfo_;


    /**
     * Returns a bit field representing all block attributes set for the memory
     * referenced by p.  If p references memory not originally allocated by
     * this garbage collector, points to the interior of a memory block, or if
     * p is null, zero will be returned.
     *
     * Params:
     *  p = A pointer to the root of a valid memory block or to null.
     *
     * Returns:
     *  A bit field containing any bits set for the memory block referenced by
     *  p or zero on error.
     */
    static uint getAttr( const scope void* p ) nothrow
    {
        return gc_getAttr(cast(void*) p);
    }


    /// ditto
    static uint getAttr(void* p) pure nothrow
    {
        return gc_getAttr( p );
    }


    /**
     * Sets the specified bits for the memory references by p.  If p references
     * memory not originally allocated by this garbage collector, points to the
     * interior of a memory block, or if p is null, no action will be
     * performed.
     *
     * Params:
     *  p = A pointer to the root of a valid memory block or to null.
     *  a = A bit field containing any bits to set for this memory block.
     *
     * Returns:
     *  The result of a call to getAttr after the specified bits have been
     *  set.
     */
    static uint setAttr( const scope void* p, uint a ) nothrow
    {
        return gc_setAttr(cast(void*) p, a);
    }


    /// ditto
    static uint setAttr(void* p, uint a) pure nothrow
    {
        return gc_setAttr( p, a );
    }


    /**
     * Clears the specified bits for the memory references by p.  If p
     * references memory not originally allocated by this garbage collector,
     * points to the interior of a memory block, or if p is null, no action
     * will be performed.
     *
     * Params:
     *  p = A pointer to the root of a valid memory block or to null.
     *  a = A bit field containing any bits to clear for this memory block.
     *
     * Returns:
     *  The result of a call to getAttr after the specified bits have been
     *  cleared.
     */
    static uint clrAttr( const scope void* p, uint a ) nothrow
    {
        return gc_clrAttr(cast(void*) p, a);
    }


    /// ditto
    static uint clrAttr(void* p, uint a) pure nothrow
    {
        return gc_clrAttr( p, a );
    }

extern(C):

    /**
     * Requests an aligned block of managed memory from the garbage collector.
     * This memory may be deleted at will with a call to free, or it may be
     * discarded and cleaned up automatically during a collection run.  If
     * allocation fails, this function will call onOutOfMemory which is
     * expected to throw an OutOfMemoryError.
     *
     * Params:
     *  sz = The desired allocation size in bytes.
     *  ba = A bitmask of the attributes to set on this block.
     *  ti = TypeInfo to describe the memory. The GC might use this information
     *       to improve scanning for pointers or to call finalizers.
     *
     * Returns:
     *  A reference to the allocated memory or null if insufficient memory
     *  is available.
     *
     * Throws:
     *  OutOfMemoryError on allocation failure.
     */
    version (D_ProfileGC)
        pragma(mangle, "gc_mallocTrace") static void* malloc(size_t sz, uint ba = 0, const scope TypeInfo ti = null,
            string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) pure nothrow;
    else
        pragma(mangle, "gc_malloc") static void* malloc(size_t sz, uint ba = 0, const scope TypeInfo ti = null) pure nothrow;

    /**
     * Requests an aligned block of managed memory from the garbage collector.
     * This memory may be deleted at will with a call to free, or it may be
     * discarded and cleaned up automatically during a collection run.  If
     * allocation fails, this function will call onOutOfMemory which is
     * expected to throw an OutOfMemoryError.
     *
     * Params:
     *  sz = The desired allocation size in bytes.
     *  ba = A bitmask of the attributes to set on this block.
     *  ti = TypeInfo to describe the memory. The GC might use this information
     *       to improve scanning for pointers or to call finalizers.
     *
     * Returns:
     *  Information regarding the allocated memory block or BlkInfo.init on
     *  error.
     *
     * Throws:
     *  OutOfMemoryError on allocation failure.
     */
    version (D_ProfileGC)
        pragma(mangle, "gc_qallocTrace") static BlkInfo qalloc(size_t sz, uint ba = 0, const scope TypeInfo ti = null,
            string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) pure nothrow;
    else
        pragma(mangle, "gc_qalloc") static BlkInfo qalloc(size_t sz, uint ba = 0, const scope TypeInfo ti = null) pure nothrow;


    /**
     * Requests an aligned block of managed memory from the garbage collector,
     * which is initialized with all bits set to zero.  This memory may be
     * deleted at will with a call to free, or it may be discarded and cleaned
     * up automatically during a collection run.  If allocation fails, this
     * function will call onOutOfMemory which is expected to throw an
     * OutOfMemoryError.
     *
     * Params:
     *  sz = The desired allocation size in bytes.
     *  ba = A bitmask of the attributes to set on this block.
     *  ti = TypeInfo to describe the memory. The GC might use this information
     *       to improve scanning for pointers or to call finalizers.
     *
     * Returns:
     *  A reference to the allocated memory or null if insufficient memory
     *  is available.
     *
     * Throws:
     *  OutOfMemoryError on allocation failure.
     */
    version (D_ProfileGC)
        pragma(mangle, "gc_callocTrace") static void* calloc(size_t sz, uint ba = 0, const TypeInfo ti = null,
            string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) pure nothrow;
    else
        pragma(mangle, "gc_calloc") static void* calloc(size_t sz, uint ba = 0, const TypeInfo ti = null) pure nothrow;


    /**
     * Extend, shrink or allocate a new block of memory keeping the contents of
     * an existing block
     *
     * If `sz` is zero, the memory referenced by p will be deallocated as if
     * by a call to `free`.
     * If `p` is `null`, new memory will be allocated via `malloc`.
     * If `p` is pointing to memory not allocated from the GC or to the interior
     * of an allocated memory block, no operation is performed and null is returned.
     *
     * Otherwise, a new memory block of size `sz` will be allocated as if by a
     * call to `malloc`, or the implementation may instead resize or shrink the memory
     * block in place.
     * The contents of the new memory block will be the same as the contents
     * of the old memory block, up to the lesser of the new and old sizes.
     *
     * The caller guarantees that there are no other live pointers to the
     * passed memory block, still it might not be freed immediately by `realloc`.
     * The garbage collector can reclaim the memory block in a later
     * collection if it is unused.
     * If allocation fails, this function will throw an `OutOfMemoryError`.
     *
     * If `ba` is zero (the default) the attributes of the existing memory
     * will be used for an allocation.
     * If `ba` is not zero and no new memory is allocated, the bits in ba will
     * replace those of the current memory block.
     *
     * Params:
     *  p  = A pointer to the base of a valid memory block or to `null`.
     *  sz = The desired allocation size in bytes.
     *  ba = A bitmask of the BlkAttr attributes to set on this block.
     *  ti = TypeInfo to describe the memory. The GC might use this information
     *       to improve scanning for pointers or to call finalizers.
     *
     * Returns:
     *  A reference to the allocated memory on success or `null` if `sz` is
     *  zero or the pointer does not point to the base of an GC allocated
     *  memory block.
     *
     * Throws:
     *  `OutOfMemoryError` on allocation failure.
     */
    version (D_ProfileGC)
        pragma(mangle, "gc_reallocTrace") static void* realloc(return scope void* p, size_t sz, uint ba = 0, const TypeInfo ti = null,
            string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) pure nothrow;
    else
        pragma(mangle, "gc_realloc") static void* realloc(return scope void* p, size_t sz, uint ba = 0, const TypeInfo ti = null) pure nothrow;

    // https://issues.dlang.org/show_bug.cgi?id=13111
    ///
    unittest
    {
        enum size1 = 1 << 11 + 1; // page in large object pool
        enum size2 = 1 << 22 + 1; // larger than large object pool size

        auto data1 = cast(ubyte*)GC.calloc(size1);
        auto data2 = cast(ubyte*)GC.realloc(data1, size2);

        GC.BlkInfo info = GC.query(data2);
        assert(info.size >= size2);
    }


    /**
     * Requests that the managed memory block referenced by p be extended in
     * place by at least mx bytes, with a desired extension of sz bytes.  If an
     * extension of the required size is not possible or if p references memory
     * not originally allocated by this garbage collector, no action will be
     * taken.
     *
     * Params:
     *  p  = A pointer to the root of a valid memory block or to null.
     *  mx = The minimum extension size in bytes.
     *  sz = The desired extension size in bytes.
     *  ti = TypeInfo to describe the full memory block. The GC might use
     *       this information to improve scanning for pointers or to
     *       call finalizers.
     *
     * Returns:
     *  The size in bytes of the extended memory block referenced by p or zero
     *  if no extension occurred.
     *
     * Note:
     *  Extend may also be used to extend slices (or memory blocks with
     *  $(LREF APPENDABLE) info). However, use the return value only
     *  as an indicator of success. $(LREF capacity) should be used to
     *  retrieve actual usable slice capacity.
     */
    version (D_ProfileGC)
        pragma(mangle, "gc_extendTrace") static size_t extend(void* p, size_t mx, size_t sz, const TypeInfo ti = null,
            string file = __FILE__, int line = __LINE__, string func = __FUNCTION__) pure nothrow;
    else
        pragma(mangle, "gc_extend") static size_t extend(void* p, size_t mx, size_t sz, const TypeInfo ti = null) pure nothrow;

    /// Standard extending
    unittest
    {
        size_t size = 1000;
        int* p = cast(int*)GC.malloc(size * int.sizeof, GC.BlkAttr.NO_SCAN);

        //Try to extend the allocated data by 1000 elements, preferred 2000.
        size_t u = GC.extend(p, 1000 * int.sizeof, 2000 * int.sizeof);
        if (u != 0)
            size = u / int.sizeof;
    }
    /// slice extending
    unittest
    {
        int[] slice = new int[](1000);
        int*  p     = slice.ptr;

        //Check we have access to capacity before attempting the extend
        if (slice.capacity)
        {
            //Try to extend slice by 1000 elements, preferred 2000.
            size_t u = GC.extend(p, 1000 * int.sizeof, 2000 * int.sizeof);
            if (u != 0)
            {
                slice.length = slice.capacity;
                assert(slice.length >= 2000);
            }
        }
    }


    /**
     * Requests that at least sz bytes of memory be obtained from the operating
     * system and marked as free.
     *
     * Params:
     *  sz = The desired size in bytes.
     *
     * Returns:
     *  The actual number of bytes reserved or zero on error.
     */
    pragma(mangle, "gc_reserve") static size_t reserve(size_t sz) nothrow pure;


    /**
     * Deallocates the memory referenced by p.  If p is null, no action occurs.
     * If p references memory not originally allocated by this garbage
     * collector, if p points to the interior of a memory block, or if this
     * method is called from a finalizer, no action will be taken.  The block
     * will not be finalized regardless of whether the FINALIZE attribute is
     * set.  If finalization is desired, call $(REF1 destroy, object) prior to `GC.free`.
     *
     * Params:
     *  p = A pointer to the root of a valid memory block or to null.
     */
    pragma(mangle, "gc_free") static void free(void* p) pure nothrow @nogc;

extern(D):

    /**
     * Returns the base address of the memory block containing p.  This value
     * is useful to determine whether p is an interior pointer, and the result
     * may be passed to routines such as sizeOf which may otherwise fail.  If p
     * references memory not originally allocated by this garbage collector, if
     * p is null, or if the garbage collector does not support this operation,
     * null will be returned.
     *
     * Params:
     *  p = A pointer to the root or the interior of a valid memory block or to
     *      null.
     *
     * Returns:
     *  The base address of the memory block referenced by p or null on error.
     */
    static inout(void)* addrOf( inout(void)* p ) nothrow @nogc pure @trusted
    {
        return cast(inout(void)*)gc_addrOf(cast(void*)p);
    }

    /// ditto
    static void* addrOf(void* p) pure nothrow @nogc @trusted
    {
        return gc_addrOf(p);
    }

    /**
     * Returns the true size of the memory block referenced by p.  This value
     * represents the maximum number of bytes for which a call to realloc may
     * resize the existing block in place.  If p references memory not
     * originally allocated by this garbage collector, points to the interior
     * of a memory block, or if p is null, zero will be returned.
     *
     * Params:
     *  p = A pointer to the root of a valid memory block or to null.
     *
     * Returns:
     *  The size in bytes of the memory block referenced by p or zero on error.
     */
    static size_t sizeOf( const scope void* p ) nothrow @nogc /* FIXME pure */
    {
        return gc_sizeOf(cast(void*)p);
    }


    /// ditto
    static size_t sizeOf(void* p) pure nothrow @nogc
    {
        return gc_sizeOf( p );
    }

    // verify that the reallocation doesn't leave the size cache in a wrong state
    unittest
    {
        auto data = cast(int*)realloc(null, 4096);
        size_t size = GC.sizeOf(data);
        assert(size >= 4096);
        data = cast(int*)GC.realloc(data, 4100);
        size = GC.sizeOf(data);
        assert(size >= 4100);
    }

    /**
     * Returns aggregate information about the memory block containing p.  If p
     * references memory not originally allocated by this garbage collector, if
     * p is null, or if the garbage collector does not support this operation,
     * BlkInfo.init will be returned.  Typically, support for this operation
     * is dependent on support for addrOf.
     *
     * Params:
     *  p = A pointer to the root or the interior of a valid memory block or to
     *      null.
     *
     * Returns:
     *  Information regarding the memory block referenced by p or BlkInfo.init
     *  on error.
     */
    static BlkInfo query(return scope const void* p) nothrow
    {
        return gc_query(cast(void*)p);
    }


    /// ditto
    static BlkInfo query(return scope void* p) pure nothrow
    {
        return gc_query( p );
    }

    /**
     * Returns runtime stats for currently active GC implementation
     * See `core.memory.GC.Stats` for list of available metrics.
     */
    static Stats stats() @safe nothrow @nogc
    {
        return gc_stats();
    }

    /**
     * Returns runtime profile stats for currently active GC implementation
     * See `core.memory.GC.ProfileStats` for list of available metrics.
     */
    static ProfileStats profileStats() nothrow @nogc @safe
    {
        return gc_profileStats();
    }

extern(C):

    /**
     * Adds an internal root pointing to the GC memory block referenced by p.
     * As a result, the block referenced by p itself and any blocks accessible
     * via it will be considered live until the root is removed again.
     *
     * If p is null, no operation is performed.
     *
     * Params:
     *  p = A pointer into a GC-managed memory block or null.
     *
     * Example:
     * ---
     * // Typical C-style callback mechanism; the passed function
     * // is invoked with the user-supplied context pointer at a
     * // later point.
     * extern(C) void addCallback(void function(void*), void*);
     *
     * // Allocate an object on the GC heap (this would usually be
     * // some application-specific context data).
     * auto context = new Object;
     *
     * // Make sure that it is not collected even if it is no
     * // longer referenced from D code (stack, GC heap, …).
     * GC.addRoot(cast(void*)context);
     *
     * // Also ensure that a moving collector does not relocate
     * // the object.
     * GC.setAttr(cast(void*)context, GC.BlkAttr.NO_MOVE);
     *
     * // Now context can be safely passed to the C library.
     * addCallback(&myHandler, cast(void*)context);
     *
     * extern(C) void myHandler(void* ctx)
     * {
     *     // Assuming that the callback is invoked only once, the
     *     // added root can be removed again now to allow the GC
     *     // to collect it later.
     *     GC.removeRoot(ctx);
     *     GC.clrAttr(ctx, GC.BlkAttr.NO_MOVE);
     *
     *     auto context = cast(Object)ctx;
     *     // Use context here…
     * }
     * ---
     */
    pragma(mangle, "gc_addRoot") static void addRoot(const void* p) nothrow @nogc pure;


    /**
     * Removes the memory block referenced by p from an internal list of roots
     * to be scanned during a collection.  If p is null or is not a value
     * previously passed to addRoot() then no operation is performed.
     *
     * Params:
     *  p = A pointer into a GC-managed memory block or null.
     */
    pragma(mangle, "gc_removeRoot") static void removeRoot(const void* p) nothrow @nogc pure;


    /**
     * Adds $(D p[0 .. sz]) to the list of memory ranges to be scanned for
     * pointers during a collection. If p is null, no operation is performed.
     *
     * Note that $(D p[0 .. sz]) is treated as an opaque range of memory assumed
     * to be suitably managed by the caller. In particular, if p points into a
     * GC-managed memory block, addRange does $(I not) mark this block as live.
     *
     * Params:
     *  p  = A pointer to a valid memory address or to null.
     *  sz = The size in bytes of the block to add. If sz is zero then the
     *       no operation will occur. If p is null then sz must be zero.
     *  ti = TypeInfo to describe the memory. The GC might use this information
     *       to improve scanning for pointers or to call finalizers
     *
     * Example:
     * ---
     * // Allocate a piece of memory on the C heap.
     * enum size = 1_000;
     * auto rawMemory = core.stdc.stdlib.malloc(size);
     *
     * // Add it as a GC range.
     * GC.addRange(rawMemory, size);
     *
     * // Now, pointers to GC-managed memory stored in
     * // rawMemory will be recognized on collection.
     * ---
     */
    pragma(mangle, "gc_addRange")
    static void addRange(const void* p, size_t sz, const TypeInfo ti = null) @nogc nothrow pure;


    /**
     * Removes the memory range starting at p from an internal list of ranges
     * to be scanned during a collection. If p is null or does not represent
     * a value previously passed to addRange() then no operation is
     * performed.
     *
     * Params:
     *  p  = A pointer to a valid memory address or to null.
     */
    pragma(mangle, "gc_removeRange") static void removeRange(const void* p) nothrow @nogc pure;


    /**
     * Runs any finalizer that is located in address range of the
     * given code segment.  This is used before unloading shared
     * libraries.  All matching objects which have a finalizer in this
     * code segment are assumed to be dead, using them while or after
     * calling this method has undefined behavior.
     *
     * Params:
     *  segment = address range of a code segment.
     */
    pragma(mangle, "gc_runFinalizers") static void runFinalizers(const scope void[] segment);

    /**
     * Queries the GC whether the current thread is running object finalization
     * as part of a GC collection, or an explicit call to runFinalizers.
     *
     * As some GC implementations (such as the current conservative one) don't
     * support GC memory allocation during object finalization, this function
     * can be used to guard against such programming errors.
     *
     * Returns:
     *  true if the current thread is in a finalizer, a destructor invoked by
     *  the GC.
     */
    pragma(mangle, "gc_inFinalizer") static bool inFinalizer() nothrow @nogc @safe;

    ///
    @safe nothrow @nogc unittest
    {
        // Only code called from a destructor is executed during finalization.
        assert(!GC.inFinalizer);
    }

    ///
    unittest
    {
        enum Outcome
        {
            notCalled,
            calledManually,
            calledFromDruntime
        }

        static class Resource
        {
            static Outcome outcome;

            this()
            {
                outcome = Outcome.notCalled;
            }

            ~this()
            {
                if (GC.inFinalizer)
                {
                    outcome = Outcome.calledFromDruntime;

                    import core.exception : InvalidMemoryOperationError;
                    try
                    {
                        /*
                         * Presently, allocating GC memory during finalization
                         * is forbidden and leads to
                         * `InvalidMemoryOperationError` being thrown.
                         *
                         * `GC.inFinalizer` can be used to guard against
                         * programming erros such as these and is also a more
                         * efficient way to verify whether a destructor was
                         * invoked by the GC.
                         */
                        cast(void) GC.malloc(1);
                        assert(false);
                    }
                    catch (InvalidMemoryOperationError e)
                    {
                        return;
                    }
                    assert(false);
                }
                else
                    outcome = Outcome.calledManually;
            }
        }

        static void createGarbage()
        {
            auto r = new Resource;
            r = null;
        }

        assert(Resource.outcome == Outcome.notCalled);
        createGarbage();
        GC.collect;
        assert(
            Resource.outcome == Outcome.notCalled ||
            Resource.outcome == Outcome.calledFromDruntime);

        auto r = new Resource;
        GC.runFinalizers((cast(const void*)typeid(Resource).destructor)[0..1]);
        assert(Resource.outcome == Outcome.calledFromDruntime);
        Resource.outcome = Outcome.notCalled;

        debug(MEMSTOMP) {} else
        {
            // assume Resource data is still available
            r.destroy;
            assert(Resource.outcome == Outcome.notCalled);
        }

        r = new Resource;
        assert(Resource.outcome == Outcome.notCalled);
        r.destroy;
        assert(Resource.outcome == Outcome.calledManually);
    }

    /**
     * Returns the number of bytes allocated for the current thread
     * since program start. It is the same as
     * GC.stats().allocatedInCurrentThread, but faster.
     */
    pragma(mangle, "gc_allocatedInCurrentThread") static ulong allocatedInCurrentThread() nothrow;

    /// Using allocatedInCurrentThread
    nothrow unittest
    {
        ulong currentlyAllocated = GC.allocatedInCurrentThread();
        struct DataStruct
        {
            long l1;
            long l2;
            long l3;
            long l4;
        }
        DataStruct* unused = new DataStruct;
        assert(GC.allocatedInCurrentThread() == currentlyAllocated + 32);
        assert(GC.stats().allocatedInCurrentThread == currentlyAllocated + 32);
    }
}

/**
 * Pure variants of C's memory allocation functions `malloc`, `calloc`, and
 * `realloc` and deallocation function `free`.
 *
 * UNIX 98 requires that errno be set to ENOMEM upon failure.
 * Purity is achieved by saving and restoring the value of `errno`, thus
 * behaving as if it were never changed.
 *
 * See_Also:
 *     $(LINK2 https://dlang.org/spec/function.html#pure-functions, D's rules for purity),
 *     which allow for memory allocation under specific circumstances.
 */
void* pureMalloc()(size_t size) @trusted pure @nogc nothrow
{
    const errnosave = fakePureErrno;
    void* ret = fakePureMalloc(size);
    fakePureErrno = errnosave;
    return ret;
}
/// ditto
void* pureCalloc()(size_t nmemb, size_t size) @trusted pure @nogc nothrow
{
    const errnosave = fakePureErrno;
    void* ret = fakePureCalloc(nmemb, size);
    fakePureErrno = errnosave;
    return ret;
}
/// ditto
void* pureRealloc()(void* ptr, size_t size) @system pure @nogc nothrow
{
    const errnosave = fakePureErrno;
    void* ret = fakePureRealloc(ptr, size);
    fakePureErrno = errnosave;
    return ret;
}

/// ditto
void pureFree()(void* ptr) @system pure @nogc nothrow
{
    version (Posix)
    {
        // POSIX free doesn't set errno
        fakePureFree(ptr);
    }
    else
    {
        const errnosave = fakePureErrno;
        fakePureFree(ptr);
        fakePureErrno = errnosave;
    }
}

///
@system pure nothrow @nogc unittest
{
    ubyte[] fun(size_t n) pure
    {
        void* p = pureMalloc(n);
        p !is null || n == 0 || assert(0);
        scope(failure) p = pureRealloc(p, 0);
        p = pureRealloc(p, n *= 2);
        p !is null || n == 0 || assert(0);
        return cast(ubyte[]) p[0 .. n];
    }

    auto buf = fun(100);
    assert(buf.length == 200);
    pureFree(buf.ptr);
}

@system pure nothrow @nogc unittest
{
    const int errno = fakePureErrno();

    void* x = pureMalloc(10);            // normal allocation
    assert(errno == fakePureErrno()); // errno shouldn't change
    assert(x !is null);                   // allocation should succeed

    x = pureRealloc(x, 10);              // normal reallocation
    assert(errno == fakePureErrno()); // errno shouldn't change
    assert(x !is null);                   // allocation should succeed

    fakePureFree(x);

    void* y = pureCalloc(10, 1);         // normal zeroed allocation
    assert(errno == fakePureErrno()); // errno shouldn't change
    assert(y !is null);                   // allocation should succeed

    fakePureFree(y);

    // Workaround bug in glibc 2.26
    // See also: https://issues.dlang.org/show_bug.cgi?id=17956
    void* z = pureMalloc(size_t.max & ~255); // won't affect `errno`
    assert(errno == fakePureErrno()); // errno shouldn't change
    assert(z is null);
}

// locally purified for internal use here only

static import core.stdc.errno;
static if (__traits(getOverloads, core.stdc.errno, "errno").length == 1
    && __traits(getLinkage, core.stdc.errno.errno) == "C")
{
    extern(C) pragma(mangle, __traits(identifier, core.stdc.errno.errno))
    private ref int fakePureErrno() @nogc nothrow pure @system;
}
else
{
    extern(C) private @nogc nothrow pure @system
    {
        pragma(mangle, __traits(identifier, core.stdc.errno.getErrno))
        @property int fakePureErrno();

        pragma(mangle, __traits(identifier, core.stdc.errno.setErrno))
        @property int fakePureErrno(int);
    }
}

version (D_BetterC) {}
else // TODO: remove this function after Phobos no longer needs it.
extern (C) private @system @nogc nothrow
{
    ref int fakePureErrnoImpl()
    {
        import core.stdc.errno;
        return errno();
    }
}

extern (C) private pure @system @nogc nothrow
{
    pragma(mangle, "malloc") void* fakePureMalloc(size_t);
    pragma(mangle, "calloc") void* fakePureCalloc(size_t nmemb, size_t size);
    pragma(mangle, "realloc") void* fakePureRealloc(void* ptr, size_t size);

    pragma(mangle, "free") void fakePureFree(void* ptr);
}

/**
Destroys and then deallocates an object.

In detail, `__delete(x)` returns with no effect if `x` is `null`. Otherwise, it
performs the following actions in sequence:
$(UL
    $(LI
        Calls the destructor `~this()` for the object referred to by `x`
        (if `x` is a class or interface reference) or
        for the object pointed to by `x` (if `x` is a pointer to a `struct`).
        Arrays of structs call the destructor, if defined, for each element in the array.
        If no destructor is defined, this step has no effect.
    )
    $(LI
        Frees the memory allocated for `x`. If `x` is a reference to a class
        or interface, the memory allocated for the underlying instance is freed. If `x` is
        a pointer, the memory allocated for the pointed-to object is freed. If `x` is a
        built-in array, the memory allocated for the array is freed.
        If `x` does not refer to memory previously allocated with `new` (or the lower-level
        equivalents in the GC API), the behavior is undefined.
    )
    $(LI
        Lastly, `x` is set to `null`. Any attempt to read or write the freed memory via
        other references will result in undefined behavior.
    )
)

Note: Users should prefer $(REF1 destroy, object) to explicitly finalize objects,
and only resort to $(REF __delete, core,memory) when $(REF destroy, object)
wouldn't be a feasible option.

Params:
    x = aggregate object that should be destroyed

See_Also: $(REF1 destroy, object), $(REF free, core,GC)

History:

The `delete` keyword allowed to free GC-allocated memory.
As this is inherently not `@safe`, it has been deprecated.
This function has been added to provide an easy transition from `delete`.
It performs the same functionality as the former `delete` keyword.
*/
void __delete(T)(ref T x) @system
{
    static void _destructRecurse(S)(ref S s)
    if (is(S == struct))
    {
        static if (__traits(hasMember, S, "__xdtor") &&
                   // Bugzilla 14746: Check that it's the exact member of S.
                   __traits(isSame, S, __traits(parent, s.__xdtor)))
            s.__xdtor();
    }

    // See also: https://github.com/dlang/dmd/blob/v2.078.0/src/dmd/e2ir.d#L3886
    static if (is(T == interface))
    {
        .object.destroy(x);
    }
    else static if (is(T == class))
    {
        .object.destroy(x);
    }
    else static if (is(T == U*, U))
    {
        static if (is(U == struct))
        {
            if (x)
                _destructRecurse(*x);
        }
    }
    else static if (is(T : E[], E))
    {
        static if (is(E == struct))
        {
            foreach_reverse (ref e; x)
                _destructRecurse(e);
        }
    }
    else
    {
        static assert(0, "It is not possible to delete: `" ~ T.stringof ~ "`");
    }

    static if (is(T == interface) ||
              is(T == class) ||
              is(T == U2*, U2))
    {
        GC.free(GC.addrOf(cast(void*) x));
        x = null;
    }
    else static if (is(T : E2[], E2))
    {
        GC.free(GC.addrOf(cast(void*) x.ptr));
        x = null;
    }
}

/// Deleting classes
unittest
{
    bool dtorCalled;
    class B
    {
        int test;
        ~this()
        {
            dtorCalled = true;
        }
    }
    B b = new B();
    B a = b;
    b.test = 10;

    assert(GC.addrOf(cast(void*) b) != null);
    __delete(b);
    assert(b is null);
    assert(dtorCalled);
    assert(GC.addrOf(cast(void*) b) == null);
    // but be careful, a still points to it
    assert(a !is null);
    assert(GC.addrOf(cast(void*) a) == null); // but not a valid GC pointer
}

/// Deleting interfaces
unittest
{
    bool dtorCalled;
    interface A
    {
        int quack();
    }
    class B : A
    {
        int a;
        int quack()
        {
            a++;
            return a;
        }
        ~this()
        {
            dtorCalled = true;
        }
    }
    A a = new B();
    a.quack();

    assert(GC.addrOf(cast(void*) a) != null);
    __delete(a);
    assert(a is null);
    assert(dtorCalled);
    assert(GC.addrOf(cast(void*) a) == null);
}

/// Deleting structs
unittest
{
    bool dtorCalled;
    struct A
    {
        string test;
        ~this()
        {
            dtorCalled = true;
        }
    }
    auto a = new A("foo");

    assert(GC.addrOf(cast(void*) a) != null);
    __delete(a);
    assert(a is null);
    assert(dtorCalled);
    assert(GC.addrOf(cast(void*) a) == null);

    // https://issues.dlang.org/show_bug.cgi?id=22779
    A *aptr;
    __delete(aptr);
}

/// Deleting arrays
unittest
{
    int[] a = [1, 2, 3];
    auto b = a;

    assert(GC.addrOf(b.ptr) != null);
    __delete(b);
    assert(b is null);
    assert(GC.addrOf(b.ptr) == null);
    // but be careful, a still points to it
    assert(a !is null);
    assert(GC.addrOf(a.ptr) == null); // but not a valid GC pointer
}

/// Deleting arrays of structs
unittest
{
    int dtorCalled;
    struct A
    {
        int a;
        ~this()
        {
            assert(dtorCalled == a);
            dtorCalled++;
        }
    }
    auto arr = [A(1), A(2), A(3)];
    arr[0].a = 2;
    arr[1].a = 1;
    arr[2].a = 0;

    assert(GC.addrOf(arr.ptr) != null);
    __delete(arr);
    assert(dtorCalled == 3);
    assert(GC.addrOf(arr.ptr) == null);
}

// Deleting raw memory
unittest
{
    import core.memory : GC;
    auto a = GC.malloc(5);
    assert(GC.addrOf(cast(void*) a) != null);
    __delete(a);
    assert(a is null);
    assert(GC.addrOf(cast(void*) a) == null);
}

// __delete returns with no effect if x is null
unittest
{
    Object x = null;
    __delete(x);

    struct S { ~this() { } }
    class C { }
    interface I { }

    int[] a; __delete(a);
    S[] as; __delete(as);
    C c; __delete(c);
    I i; __delete(i);
    C* pc = &c; __delete(*pc);
    I* pi = &i; __delete(*pi);
    int* pint; __delete(pint);
    S* ps; __delete(ps);
}

// https://issues.dlang.org/show_bug.cgi?id=19092
unittest
{
    const(int)[] x = [1, 2, 3];
    assert(GC.addrOf(x.ptr) != null);
    __delete(x);
    assert(x is null);
    assert(GC.addrOf(x.ptr) == null);

    immutable(int)[] y = [1, 2, 3];
    assert(GC.addrOf(y.ptr) != null);
    __delete(y);
    assert(y is null);
    assert(GC.addrOf(y.ptr) == null);
}

// test realloc behaviour
unittest
{
    static void set(int* p, size_t size)
    {
        foreach (i; 0 .. size)
            *p++ = cast(int) i;
    }
    static void verify(int* p, size_t size)
    {
        foreach (i; 0 .. size)
            assert(*p++ == i);
    }
    static void test(size_t memsize)
    {
        int* p = cast(int*) GC.malloc(memsize * int.sizeof);
        assert(p);
        set(p, memsize);
        verify(p, memsize);

        int* q = cast(int*) GC.realloc(p + 4, 2 * memsize * int.sizeof);
        assert(q == null);

        q = cast(int*) GC.realloc(p + memsize / 2, 2 * memsize * int.sizeof);
        assert(q == null);

        q = cast(int*) GC.realloc(p + memsize - 1, 2 * memsize * int.sizeof);
        assert(q == null);

        int* r = cast(int*) GC.realloc(p, 5 * memsize * int.sizeof);
        verify(r, memsize);
        set(r, 5 * memsize);

        int* s = cast(int*) GC.realloc(r, 2 * memsize * int.sizeof);
        verify(s, 2 * memsize);

        assert(GC.realloc(s, 0) == null); // free
        assert(GC.addrOf(p) == null);
    }

    test(16);
    test(200);
    test(800); // spans large and small pools
    test(1200);
    test(8000);

    void* p = GC.malloc(100);
    assert(GC.realloc(&p, 50) == null); // non-GC pointer
}

// test GC.profileStats
unittest
{
    auto stats = GC.profileStats();
    GC.collect();
    auto nstats = GC.profileStats();
    assert(nstats.numCollections > stats.numCollections);
}

// in rt.lifetime:
private extern (C) void* _d_newitemU(scope const TypeInfo _ti) @system pure nothrow;

/**
Moves a value to a new GC allocation.

Params:
    value = Value to be moved. If the argument is an lvalue and a struct with a
            destructor or postblit, it will be reset to its `.init` value.

Returns:
    A pointer to the new GC-allocated value.
*/
T* moveToGC(T)(auto ref T value)
{
    static T* doIt(ref T value) @trusted
    {
        import core.lifetime : moveEmplace;
        auto mem = cast(T*) _d_newitemU(typeid(T)); // allocate but don't initialize
        moveEmplace(value, *mem);
        return mem;
    }

    return doIt(value); // T dtor might be @system
}

///
@safe pure nothrow unittest
{
    struct S
    {
        int x;
        this(this) @disable;
        ~this() @safe pure nothrow @nogc {}
    }

    S* p;

    // rvalue
    p = moveToGC(S(123));
    assert(p.x == 123);

    // lvalue
    auto lval = S(456);
    p = moveToGC(lval);
    assert(p.x == 456);
    assert(lval.x == 0);
}

// @system dtor
unittest
{
    struct S
    {
        int x;
        ~this() @system {}
    }

    // lvalue case is @safe, ref param isn't destructed
    static assert(__traits(compiles, (ref S lval) @safe { moveToGC(lval); }));

    // rvalue case is @system, value param is destructed
    static assert(!__traits(compiles, () @safe { moveToGC(S(0)); }));
}
