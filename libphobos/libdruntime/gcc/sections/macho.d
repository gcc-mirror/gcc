// MACHO-specific support for sections.
// Copyright (C) 2021-2023 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

module gcc.sections.macho;

version (OSX):

import core.memory;
import core.stdc.stdlib;
import core.sys.darwin.dlfcn;
import core.sys.darwin.mach.dyld;
import core.sys.darwin.mach.getsect;
import core.sys.posix.pthread;
import rt.minfo;
import core.internal.container.array;
import core.internal.container.hashtab;
import gcc.sections.common;

version (GNU_EMUTLS)
    import gcc.emutls;

alias DSO SectionGroup;
struct DSO
{
    static int opApply(scope int delegate(ref DSO) dg)
    {
        foreach (dso; _loadedDSOs)
        {
            if (auto res = dg(*dso))
                return res;
        }
        return 0;
    }

    static int opApplyReverse(scope int delegate(ref DSO) dg)
    {
        foreach_reverse (dso; _loadedDSOs)
        {
            if (auto res = dg(*dso))
                return res;
        }
        return 0;
    }

    @property immutable(ModuleInfo*)[] modules() const nothrow @nogc
    {
        return _moduleGroup.modules;
    }

    @property ref inout(ModuleGroup) moduleGroup() inout return nothrow @nogc
    {
        return _moduleGroup;
    }

    @property inout(void[])[] gcRanges() inout nothrow @nogc
    {
        return _gcRanges[];
    }

private:

    invariant()
    {
        safeAssert(_moduleGroup.modules.length > 0, "No modules for DSO.");
    }

    void** _slot;
    ModuleGroup _moduleGroup;
    Array!(void[]) _gcRanges;

    version (Shared)
    {
        Array!(void[]) _codeSegments; // array of code segments
        Array!(DSO*) _deps; // D libraries needed by this DSO
        void* _handle; // corresponding handle
    }
}

/****
 * Boolean flag set to true while the runtime is initialized.
 */
__gshared bool _isRuntimeInitialized;

/****
 * Gets called on program startup just before GC is initialized.
 */
void initSections() nothrow @nogc
{
    _isRuntimeInitialized = true;
}

/***
 * Gets called on program shutdown just after GC is terminated.
 */
void finiSections() nothrow @nogc
{
    _isRuntimeInitialized = false;
}

alias ScanDG = void delegate(void* pbeg, void* pend) nothrow;

version (Shared)
{
    import gcc.sections : pinLoadedLibraries, unpinLoadedLibraries,
           inheritLoadedLibraries, cleanupLoadedLibraries;

    /***
     * Called once per thread; returns array of thread local storage ranges
     */
    Array!(ThreadDSO)* initTLSRanges() @nogc nothrow
    {
        return &_loadedDSOs();
    }

    void finiTLSRanges(Array!(ThreadDSO)* tdsos) @nogc nothrow
    {
        // Nothing to do here. tdsos used to point to the _loadedDSOs instance
        // in the dying thread's TLS segment and as such is not valid anymore.
        // The memory for the array contents was already reclaimed in
        // cleanupLoadedLibraries().
    }

    void scanTLSRanges(Array!(ThreadDSO)* tdsos, scope ScanDG dg) nothrow
    {
        version (GNU_EMUTLS)
            _d_emutls_scan(dg);
        else
            static assert(0, "Native TLS unimplemented");
    }

    // interface for core.thread to inherit loaded libraries
    pragma(mangle, gcc.sections.pinLoadedLibraries.mangleof)
    void* pinLoadedLibraries() nothrow @nogc
    {
        auto res = cast(Array!(ThreadDSO)*)calloc(1, Array!(ThreadDSO).sizeof);
        res.length = _loadedDSOs.length;
        foreach (i, ref tdso; _loadedDSOs)
        {
            (*res)[i] = tdso;
            if (tdso._addCnt)
            {
                // Increment the dlopen ref for explicitly loaded libraries to pin them.
                const success = .dlopen(nameForDSO(tdso._pdso), RTLD_LAZY) !is null;
                safeAssert(success, "Failed to increment dlopen ref.");
                (*res)[i]._addCnt = 1; // new array takes over the additional ref count
            }
        }
        return res;
    }

    pragma(mangle, gcc.sections.unpinLoadedLibraries.mangleof)
    void unpinLoadedLibraries(void* p) nothrow @nogc
    {
        auto pary = cast(Array!(ThreadDSO)*)p;
        // In case something failed we need to undo the pinning.
        foreach (ref tdso; *pary)
        {
            if (tdso._addCnt)
            {
                auto handle = tdso._pdso._handle;
                safeAssert(handle !is null, "Invalid library handle.");
                .dlclose(handle);
            }
        }
        pary.reset();
        .free(pary);
    }

    // Called before TLS ctors are ran, copy over the loaded libraries
    // of the parent thread.
    pragma(mangle, gcc.sections.inheritLoadedLibraries.mangleof)
    void inheritLoadedLibraries(void* p) nothrow @nogc
    {
        safeAssert(_loadedDSOs.empty, "DSOs have already been registered for this thread.");
        _loadedDSOs.swap(*cast(Array!(ThreadDSO)*)p);
        .free(p);
    }

    // Called after all TLS dtors ran, decrements all remaining dlopen refs.
    pragma(mangle, gcc.sections.cleanupLoadedLibraries.mangleof)
    void cleanupLoadedLibraries() nothrow @nogc
    {
        foreach (ref tdso; _loadedDSOs)
        {
            if (tdso._addCnt == 0) continue;

            auto handle = tdso._pdso._handle;
            safeAssert(handle !is null, "Invalid DSO handle.");
            for (; tdso._addCnt > 0; --tdso._addCnt)
                .dlclose(handle);
        }

        // Free the memory for the array contents.
        _loadedDSOs.reset();
    }
}
else
{
    /***
     * Called once per thread; returns array of thread local storage ranges
     */
    Array!(void[])* initTLSRanges() nothrow @nogc
    {
        return null;
    }

    void finiTLSRanges(Array!(void[])* rngs) nothrow @nogc
    {
    }

    void scanTLSRanges(Array!(void[])* rngs, scope ScanDG dg) nothrow
    {
        version (GNU_EMUTLS)
            _d_emutls_scan(dg);
        else
            static assert(0, "Native TLS unimplemented");
    }
}

private:

version (Shared)
{
    /*
     * Array of thread local DSO metadata for all libraries loaded and
     * initialized in this thread.
     *
     * Note:
     *     A newly spawned thread will inherit these libraries.
     * Note:
     *     We use an array here to preserve the order of
     *     initialization.  If that became a performance issue, we
     *     could use a hash table and enumerate the DSOs during
     *     loading so that the hash table values could be sorted when
     *     necessary.
     */
    struct ThreadDSO
    {
        DSO* _pdso;
        static if (_pdso.sizeof == 8) uint _refCnt, _addCnt;
        else static if (_pdso.sizeof == 4) ushort _refCnt, _addCnt;
        else static assert(0, "unimplemented");
        alias _pdso this;
    }

    @property ref Array!(ThreadDSO) _loadedDSOs() @nogc nothrow
    {
        static Array!(ThreadDSO) x;
        return x;
    }

    /*
     * Set to true during rt_loadLibrary/rt_unloadLibrary calls.
     */
    bool _rtLoading;

    /*
     * Hash table to map the native handle (as returned by dlopen)
     * to the corresponding DSO*, protected by a mutex.
     */
    __gshared pthread_mutex_t _handleToDSOMutex;
    @property ref HashTab!(void*, DSO*) _handleToDSO() @nogc nothrow
    {
        __gshared HashTab!(void*, DSO*) x;
        return x;
    }
}
else
{
    /*
     * Static DSOs loaded by the runtime linker. This includes the
     * executable. These can't be unloaded.
     */
    @property ref Array!(DSO*) _loadedDSOs() @nogc nothrow
    {
        __gshared Array!(DSO*) x;
        return x;
    }

    enum _rtLoading = false;
}

///////////////////////////////////////////////////////////////////////////////
// Compiler to runtime interface.
///////////////////////////////////////////////////////////////////////////////

struct MachHeader
{
    const(mach_header)* header; // the mach header of the image
    intptr_t slide;             // virtural memory address slide amount
}

/****
 * This data structure is generated by the compiler, and then passed to
 * _d_dso_registry().
 */
struct CompilerDSOData
{
    size_t _version;                                       // currently 1
    void** _slot;                                          // can be used to store runtime data
    immutable(object.ModuleInfo*)* _minfo_beg, _minfo_end; // array of modules in this object file
}

T[] toRange(T)(T* beg, T* end) { return beg[0 .. end - beg]; }

/* For each shared library and executable, the compiler generates code that
 * sets up CompilerDSOData and calls _d_dso_registry().
 * A pointer to that code is inserted into both the .ctors and .dtors
 * segment so it gets called by the loader on startup and shutdown.
 */
extern(C) void _d_dso_registry(CompilerDSOData* data)
{
    // only one supported currently
    safeAssert(data._version >= 1, "Incompatible compiler-generated DSO data version.");

    // no backlink => register
    if (*data._slot is null)
    {
        immutable firstDSO = _loadedDSOs.empty;
        if (firstDSO) initLocks();

        DSO* pdso = cast(DSO*).calloc(1, DSO.sizeof);
        assert(typeid(DSO).initializer().ptr is null);
        pdso._slot = data._slot;
        *data._slot = pdso; // store backlink in library record

        pdso._moduleGroup = ModuleGroup(toRange(data._minfo_beg, data._minfo_end));

        MachHeader header = void;
        const headerFound = findImageHeaderForAddr(data._slot, header);
        safeAssert(headerFound, "Failed to find image header.");

        scanSegments(header, pdso);

        version (Shared)
        {
            auto handle = handleForAddr(data._slot);

            getDependencies(header, pdso._deps);
            pdso._handle = handle;
            setDSOForHandle(pdso, pdso._handle);

            if (!_rtLoading)
            {
                /* This DSO was not loaded by rt_loadLibrary which
                 * happens for all dependencies of an executable or
                 * the first dlopen call from a C program.
                 * In this case we add the DSO to the _loadedDSOs of this
                 * thread with a refCnt of 1 and call the TlsCtors.
                 */
                immutable ushort refCnt = 1, addCnt = 0;
                _loadedDSOs.insertBack(ThreadDSO(pdso, refCnt, addCnt));
            }
        }
        else
        {
            foreach (p; _loadedDSOs)
                safeAssert(p !is pdso, "DSO already registered.");
            _loadedDSOs.insertBack(pdso);
        }

        // don't initialize modules before rt_init was called
        if (_isRuntimeInitialized)
        {
            registerGCRanges(pdso);
            // rt_loadLibrary will run tls ctors, so do this only for dlopen
            immutable runTlsCtors = !_rtLoading;
            runModuleConstructors(pdso, runTlsCtors);
        }
    }
    // has backlink => unregister
    else
    {
        DSO* pdso = cast(DSO*)*data._slot;
        *data._slot = null;

        // don't finalizes modules after rt_term was called (see Bugzilla 11378)
        if (_isRuntimeInitialized)
        {
            // rt_unloadLibrary already ran tls dtors, so do this only for dlclose
            immutable runTlsDtors = !_rtLoading;
            runModuleDestructors(pdso, runTlsDtors);
            unregisterGCRanges(pdso);
            // run finalizers after module dtors (same order as in rt_term)
            version (Shared) runFinalizers(pdso);
        }

        version (Shared)
        {
            if (!_rtLoading)
            {
                /* This DSO was not unloaded by rt_unloadLibrary so we
                 * have to remove it from _loadedDSOs here.
                 */
                foreach (i, ref tdso; _loadedDSOs)
                {
                    if (tdso._pdso == pdso)
                    {
                        _loadedDSOs.remove(i);
                        break;
                    }
                }
            }

            unsetDSOForHandle(pdso, pdso._handle);
        }
        else
        {
            // static DSOs are unloaded in reverse order
            safeAssert(pdso == _loadedDSOs.back, "DSO being unregistered isn't current last one.");
            _loadedDSOs.popBack();
        }

        freeDSO(pdso);

        // last DSO being unloaded => shutdown registry
        if (_loadedDSOs.empty)
        {
            version (GNU_EMUTLS)
                _d_emutls_destroy();
            version (Shared)
            {
                safeAssert(_handleToDSO.empty, "_handleToDSO not in sync with _loadedDSOs.");
                _handleToDSO.reset();
            }
            finiLocks();
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
// dynamic loading
///////////////////////////////////////////////////////////////////////////////

// Shared D libraries are only supported when linking against a shared druntime library.

version (Shared)
{
    ThreadDSO* findThreadDSO(DSO* pdso) nothrow @nogc
    {
        foreach (ref tdata; _loadedDSOs)
            if (tdata._pdso == pdso) return &tdata;
        return null;
    }

    void incThreadRef(DSO* pdso, bool incAdd)
    {
        if (auto tdata = findThreadDSO(pdso)) // already initialized
        {
            if (incAdd && ++tdata._addCnt > 1) return;
            ++tdata._refCnt;
        }
        else
        {
            foreach (dep; pdso._deps)
                incThreadRef(dep, false);
            immutable ushort refCnt = 1, addCnt = incAdd ? 1 : 0;
            _loadedDSOs.insertBack(ThreadDSO(pdso, refCnt, addCnt));
            pdso._moduleGroup.runTlsCtors();
        }
    }

    void decThreadRef(DSO* pdso, bool decAdd)
    {
        auto tdata = findThreadDSO(pdso);
        safeAssert(tdata !is null, "Failed to find thread DSO.");
        safeAssert(!decAdd || tdata._addCnt > 0, "Mismatching rt_unloadLibrary call.");

        if (decAdd && --tdata._addCnt > 0) return;
        if (--tdata._refCnt > 0) return;

        pdso._moduleGroup.runTlsDtors();
        foreach (i, ref td; _loadedDSOs)
            if (td._pdso == pdso) _loadedDSOs.remove(i);
        foreach (dep; pdso._deps)
            decThreadRef(dep, false);
    }

    extern(C) void* rt_loadLibrary(const char* name)
    {
        immutable save = _rtLoading;
        _rtLoading = true;
        scope (exit) _rtLoading = save;

        auto handle = .dlopen(name, RTLD_LAZY);
        if (handle is null) return null;

        // if it's a D library
        if (auto pdso = dsoForHandle(handle))
            incThreadRef(pdso, true);
        return handle;
    }

    extern(C) int rt_unloadLibrary(void* handle)
    {
        if (handle is null) return false;

        immutable save = _rtLoading;
        _rtLoading = true;
        scope (exit) _rtLoading = save;

        // if it's a D library
        if (auto pdso = dsoForHandle(handle))
            decThreadRef(pdso, true);
        return .dlclose(handle) == 0;
    }
}

///////////////////////////////////////////////////////////////////////////////
// helper functions
///////////////////////////////////////////////////////////////////////////////

void initLocks() nothrow @nogc
{
    version (Shared)
        !pthread_mutex_init(&_handleToDSOMutex, null) || assert(0);
}

void finiLocks() nothrow @nogc
{
    version (Shared)
        !pthread_mutex_destroy(&_handleToDSOMutex) || assert(0);
}

void runModuleConstructors(DSO* pdso, bool runTlsCtors)
{
    pdso._moduleGroup.sortCtors();
    pdso._moduleGroup.runCtors();
    if (runTlsCtors) pdso._moduleGroup.runTlsCtors();
}

void runModuleDestructors(DSO* pdso, bool runTlsDtors)
{
    if (runTlsDtors) pdso._moduleGroup.runTlsDtors();
    pdso._moduleGroup.runDtors();
}

void registerGCRanges(DSO* pdso) nothrow @nogc
{
    foreach (rng; pdso._gcRanges)
        GC.addRange(rng.ptr, rng.length);
}

void unregisterGCRanges(DSO* pdso) nothrow @nogc
{
    foreach (rng; pdso._gcRanges)
        GC.removeRange(rng.ptr);
}

version (Shared) void runFinalizers(DSO* pdso)
{
    foreach (seg; pdso._codeSegments)
        GC.runFinalizers(seg);
}

void freeDSO(DSO* pdso) nothrow @nogc
{
    pdso._gcRanges.reset();
    version (Shared)
    {
        pdso._codeSegments.reset();
        pdso._deps.reset();
        pdso._handle = null;
    }
    .free(pdso);
}

version (Shared)
{
@nogc nothrow:
    const(char)* nameForDSO(in DSO* pdso)
    {
        Dl_info info = void;
        const success = dladdr(pdso._slot, &info) != 0;
        safeAssert(success, "Failed to get DSO info.");
        return info.dli_fname;
    }

    DSO* dsoForHandle(void* handle)
    {
        DSO* pdso;
        !pthread_mutex_lock(&_handleToDSOMutex) || assert(0);
        if (auto ppdso = handle in _handleToDSO)
            pdso = *ppdso;
        !pthread_mutex_unlock(&_handleToDSOMutex) || assert(0);
        return pdso;
    }

    void setDSOForHandle(DSO* pdso, void* handle)
    {
        !pthread_mutex_lock(&_handleToDSOMutex) || assert(0);
        safeAssert(handle !in _handleToDSO, "DSO already registered.");
        _handleToDSO[handle] = pdso;
        !pthread_mutex_unlock(&_handleToDSOMutex) || assert(0);
    }

    void unsetDSOForHandle(DSO* pdso, void* handle)
    {
        !pthread_mutex_lock(&_handleToDSOMutex) || assert(0);
        safeAssert(_handleToDSO[handle] == pdso, "Handle doesn't match registered DSO.");
        _handleToDSO.remove(handle);
        !pthread_mutex_unlock(&_handleToDSOMutex) || assert(0);
    }

    void getDependencies(in MachHeader info, ref Array!(DSO*) deps)
    {
        // FIXME: Not implemented yet.
    }

    void* handleForName(const char* name)
    {
        auto handle = .dlopen(name, RTLD_NOLOAD | RTLD_LAZY);
        if (handle !is null) .dlclose(handle); // drop reference count
        return handle;
    }
}

///////////////////////////////////////////////////////////////////////////////
// Mach-O program header iteration
///////////////////////////////////////////////////////////////////////////////

/************
 * Scan segments in the image header and store
 * the writeable data segments in *pdso.
 */

void scanSegments(in MachHeader info, DSO* pdso)
{
    foreach (e; dataSegs)
    {
        auto sect = getSection(info.header, info.slide, e.seg.ptr, e.sect.ptr);
        if (sect != null)
            pdso._gcRanges.insertBack((cast(void*)sect.ptr)[0 .. sect.length]);
    }

    version (Shared)
    {
        void[] text = getSection(info.header, info.slide, "__TEXT", "__text");
        if (!text)
            assert(0, "Failed to get text section.");
        pdso._codeSegments.insertBack(text);
    }
}

/**************************
 * Input:
 *      result where the output is to be written
 * Returns:
 *      true if found, and *result is filled in
 */

bool findImageHeaderForAddr(in void* addr, out MachHeader result)
{
    Dl_info info;
    if (dladdr(addr, &info) == 0)
        return false;

    foreach (i; 0 .. _dyld_image_count())
    {
        if (info.dli_fbase == _dyld_get_image_header(i))
        {
            result.header = cast(const(mach_header)*)info.dli_fbase;
            result.slide = _dyld_get_image_vmaddr_slide(i);
            return true;
        }
    }
    return false;
}

/**************************
 * Input:
 *      addr  an internal address of a DSO
 * Returns:
 *      the dlopen handle for that DSO or null if addr is not within a loaded DSO
 */
version (Shared) void* handleForAddr(void* addr) nothrow @nogc
{
    Dl_info info = void;
    if (dladdr(addr, &info) != 0)
        return handleForName(info.dli_fname);
    return null;
}

struct SegRef
{
    string seg;
    string sect;
}

static immutable SegRef[] dataSegs = [{SEG_DATA, SECT_DATA},
                                      {SEG_DATA, SECT_BSS},
                                      {SEG_DATA, SECT_COMMON}];

/**
 * Returns the section for the named section in the named segment
 * for the mach_header pointer passed, or null if not found.
 */
ubyte[] getSection(in mach_header* header, intptr_t slide,
                   in char* segmentName, in char* sectionName)
{
    version (D_LP64)
    {
        assert(header.magic == MH_MAGIC_64);
        auto sect = getsectbynamefromheader_64(cast(mach_header_64*)header,
                                            segmentName,
                                            sectionName);
    }
    else
    {
        assert(header.magic == MH_MAGIC);
        auto sect = getsectbynamefromheader(header,
                                            segmentName,
                                            sectionName);
    }

    if (sect !is null && sect.size > 0)
        return (cast(ubyte*)sect.addr + slide)[0 .. cast(size_t)sect.size];
    return null;
}
