/**
 * Written in the D programming language.
 * This module provides ELF-specific support for sections with shared libraries.
 *
 * Copyright: Copyright Martin Nowak 2012-2013.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 * Source: $(DRUNTIMESRC src/rt/_sections_linux.d)
 */

module rt.sections_elf_shared;

version (CRuntime_Glibc) enum SharedELF = true;
else version (FreeBSD) enum SharedELF = true;
else version (NetBSD) enum SharedELF = true;
else enum SharedELF = false;
static if (SharedELF):

// debug = PRINTF;
import core.memory;
import core.stdc.config;
import core.stdc.stdio;
import core.stdc.stdlib : calloc, exit, free, malloc, EXIT_FAILURE;
import core.stdc.string : strlen;
version (linux)
{
    import core.sys.linux.dlfcn;
    import core.sys.linux.elf;
    import core.sys.linux.link;
}
else version (FreeBSD)
{
    import core.sys.freebsd.dlfcn;
    import core.sys.freebsd.sys.elf;
    import core.sys.freebsd.sys.link_elf;
}
else version (NetBSD)
{
    import core.sys.netbsd.dlfcn;
    import core.sys.netbsd.sys.elf;
    import core.sys.netbsd.sys.link_elf;
}
else
{
    static assert(0, "unimplemented");
}
import core.sys.posix.pthread;
import rt.deh;
import rt.dmain2;
import rt.minfo;
import rt.util.container.array;
import rt.util.container.hashtab;

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

    @property ref inout(ModuleGroup) moduleGroup() inout nothrow @nogc
    {
        return _moduleGroup;
    }

    @property immutable(FuncTable)[] ehTables() const nothrow @nogc
    {
        return null;
    }

    @property inout(void[])[] gcRanges() inout nothrow @nogc
    {
        return _gcRanges[];
    }

private:

    invariant()
    {
        assert(_moduleGroup.modules.length);
        assert(_tlsMod || !_tlsSize);
    }

    ModuleGroup _moduleGroup;
    Array!(void[]) _gcRanges;
    size_t _tlsMod;
    size_t _tlsSize;

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


version (FreeBSD) private __gshared void* dummy_ref;
version (NetBSD) private __gshared void* dummy_ref;

/****
 * Gets called on program startup just before GC is initialized.
 */
void initSections() nothrow @nogc
{
    _isRuntimeInitialized = true;
    // reference symbol to support weak linkage
    version (FreeBSD) dummy_ref = &_d_dso_registry;
    version (NetBSD) dummy_ref = &_d_dso_registry;
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
    /***
     * Called once per thread; returns array of thread local storage ranges
     */
    Array!(ThreadDSO)* initTLSRanges() @nogc nothrow
    {
        return &_loadedDSOs;
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
        foreach (ref tdso; *tdsos)
            dg(tdso._tlsRange.ptr, tdso._tlsRange.ptr + tdso._tlsRange.length);
    }

    // interface for core.thread to inherit loaded libraries
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
                .dlopen(linkMapForHandle(tdso._pdso._handle).l_name, RTLD_LAZY) !is null || assert(0);
                (*res)[i]._addCnt = 1; // new array takes over the additional ref count
            }
        }
        return res;
    }

    void unpinLoadedLibraries(void* p) nothrow @nogc
    {
        auto pary = cast(Array!(ThreadDSO)*)p;
        // In case something failed we need to undo the pinning.
        foreach (ref tdso; *pary)
        {
            if (tdso._addCnt)
            {
                auto handle = tdso._pdso._handle;
                handle !is null || assert(0);
                .dlclose(handle);
            }
        }
        pary.reset();
        .free(pary);
    }

    // Called before TLS ctors are ran, copy over the loaded libraries
    // of the parent thread.
    void inheritLoadedLibraries(void* p) nothrow @nogc
    {
        assert(_loadedDSOs.empty);
        _loadedDSOs.swap(*cast(Array!(ThreadDSO)*)p);
        .free(p);
        foreach (ref dso; _loadedDSOs)
        {
            // the copied _tlsRange corresponds to parent thread
            dso.updateTLSRange();
        }
    }

    // Called after all TLS dtors ran, decrements all remaining dlopen refs.
    void cleanupLoadedLibraries() nothrow @nogc
    {
        foreach (ref tdso; _loadedDSOs)
        {
            if (tdso._addCnt == 0) continue;

            auto handle = tdso._pdso._handle;
            handle !is null || assert(0);
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
        return &_tlsRanges;
    }

    void finiTLSRanges(Array!(void[])* rngs) nothrow @nogc
    {
        rngs.reset();
    }

    void scanTLSRanges(Array!(void[])* rngs, scope ScanDG dg) nothrow
    {
        foreach (rng; *rngs)
            dg(rng.ptr, rng.ptr + rng.length);
    }
}

private:

// start of linked list for ModuleInfo references
version (FreeBSD) deprecated extern (C) __gshared void* _Dmodule_ref;
version (NetBSD) deprecated extern (C) __gshared void* _Dmodule_ref;

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
        void[] _tlsRange;
        alias _pdso this;
        // update the _tlsRange for the executing thread
        void updateTLSRange() nothrow @nogc
        {
            _tlsRange = getTLSRange(_pdso._tlsMod, _pdso._tlsSize);
        }
    }
    Array!(ThreadDSO) _loadedDSOs;

    /*
     * Set to true during rt_loadLibrary/rt_unloadLibrary calls.
     */
    bool _rtLoading;

    /*
     * Hash table to map link_map* to corresponding DSO*.
     * The hash table is protected by a Mutex.
     */
    __gshared pthread_mutex_t _handleToDSOMutex;
    __gshared HashTab!(void*, DSO*) _handleToDSO;

    /*
     * Section in executable that contains copy relocations.
     * Might be null when druntime is dynamically loaded by a C host.
     */
    __gshared const(void)[] _copyRelocSection;
}
else
{
    /*
     * Static DSOs loaded by the runtime linker. This includes the
     * executable. These can't be unloaded.
     */
    __gshared Array!(DSO*) _loadedDSOs;

    /*
     * Thread local array that contains TLS memory ranges for each
     * library initialized in this thread.
     */
    Array!(void[]) _tlsRanges;

    enum _rtLoading = false;
}

///////////////////////////////////////////////////////////////////////////////
// Compiler to runtime interface.
///////////////////////////////////////////////////////////////////////////////

import gcc.config;

/*
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
    data._version >= 1 || assert(0, "corrupt DSO data version");

    // no backlink => register
    if (*data._slot is null)
    {
        immutable firstDSO = _loadedDSOs.empty;
        if (firstDSO) initLocks();

        DSO* pdso = cast(DSO*).calloc(1, DSO.sizeof);
        assert(typeid(DSO).initializer().ptr is null);
        *data._slot = pdso; // store backlink in library record

        pdso._moduleGroup = ModuleGroup(toRange(data._minfo_beg, data._minfo_end));

        dl_phdr_info info = void;
        findDSOInfoForAddr(data._slot, &info) || assert(0);

        scanSegments(info, pdso);

        version (Shared)
        {
            auto handle = handleForAddr(data._slot);

            if (firstDSO)
            {
                /// Assert that the first loaded DSO is druntime itself. Use a
                /// local druntime symbol (rt_get_bss_start) to get the handle.
                assert(handleForAddr(data._slot) == handleForAddr(&rt_get_bss_start));
                _copyRelocSection = getCopyRelocSection();
            }
            checkModuleCollisions(info, pdso._moduleGroup.modules, _copyRelocSection);

            getDependencies(info, pdso._deps);
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
                auto tlsRng = getTLSRange(pdso._tlsMod, pdso._tlsSize);
                _loadedDSOs.insertBack(ThreadDSO(pdso, refCnt, addCnt, tlsRng));
            }
        }
        else
        {
            foreach (p; _loadedDSOs) assert(p !is pdso);
            _loadedDSOs.insertBack(pdso);
            _tlsRanges.insertBack(getTLSRange(pdso._tlsMod, pdso._tlsSize));
        }

        // don't initialize modules before rt_init was called (see Bugzilla 11378)
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
            pdso._handle = null;
        }
        else
        {
            // static DSOs are unloaded in reverse order
            assert(pdso._tlsSize == _tlsRanges.back.length);
            _tlsRanges.popBack();
            assert(pdso == _loadedDSOs.back);
            _loadedDSOs.popBack();
        }

        freeDSO(pdso);

        if (_loadedDSOs.empty) finiLocks(); // last DSO
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
            auto tlsRng = getTLSRange(pdso._tlsMod, pdso._tlsSize);
            _loadedDSOs.insertBack(ThreadDSO(pdso, refCnt, addCnt, tlsRng));
            pdso._moduleGroup.runTlsCtors();
        }
    }

    void decThreadRef(DSO* pdso, bool decAdd)
    {
        auto tdata = findThreadDSO(pdso);
        tdata !is null || assert(0);
        !decAdd || tdata._addCnt > 0 || assert(0, "Mismatching rt_unloadLibrary call.");

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
    version (Shared) pdso._codeSegments.reset();
    .free(pdso);
}

version (Shared)
{
@nogc nothrow:
    link_map* linkMapForHandle(void* handle) nothrow @nogc
    {
        link_map* map;
        dlinfo(handle, RTLD_DI_LINKMAP, &map) == 0 || assert(0);
        return map;
    }

     link_map* exeLinkMap(link_map* map) nothrow @nogc
     {
         assert(map);
         while (map.l_prev !is null)
             map = map.l_prev;
         return map;
     }

    DSO* dsoForHandle(void* handle) nothrow @nogc
    {
        DSO* pdso;
        !pthread_mutex_lock(&_handleToDSOMutex) || assert(0);
        if (auto ppdso = handle in _handleToDSO)
            pdso = *ppdso;
        !pthread_mutex_unlock(&_handleToDSOMutex) || assert(0);
        return pdso;
    }

    void setDSOForHandle(DSO* pdso, void* handle) nothrow @nogc
    {
        !pthread_mutex_lock(&_handleToDSOMutex) || assert(0);
        assert(handle !in _handleToDSO);
        _handleToDSO[handle] = pdso;
        !pthread_mutex_unlock(&_handleToDSOMutex) || assert(0);
    }

    void unsetDSOForHandle(DSO* pdso, void* handle) nothrow @nogc
    {
        !pthread_mutex_lock(&_handleToDSOMutex) || assert(0);
        assert(_handleToDSO[handle] == pdso);
        _handleToDSO.remove(handle);
        !pthread_mutex_unlock(&_handleToDSOMutex) || assert(0);
    }

    void getDependencies(in ref dl_phdr_info info, ref Array!(DSO*) deps) nothrow @nogc
    {
        // get the entries of the .dynamic section
        ElfW!"Dyn"[] dyns;
        foreach (ref phdr; info.dlpi_phdr[0 .. info.dlpi_phnum])
        {
            if (phdr.p_type == PT_DYNAMIC)
            {
                auto p = cast(ElfW!"Dyn"*)(info.dlpi_addr + (phdr.p_vaddr & ~(size_t.sizeof - 1)));
                dyns = p[0 .. phdr.p_memsz / ElfW!"Dyn".sizeof];
                break;
            }
        }
        // find the string table which contains the sonames
        const(char)* strtab;
        foreach (dyn; dyns)
        {
            if (dyn.d_tag == DT_STRTAB)
            {
                version (linux)
                    strtab = cast(const(char)*)dyn.d_un.d_ptr;
                else version (FreeBSD)
                    strtab = cast(const(char)*)(info.dlpi_addr + dyn.d_un.d_ptr); // relocate
                else version (NetBSD)
                    strtab = cast(const(char)*)(info.dlpi_addr + dyn.d_un.d_ptr); // relocate
                else
                    static assert(0, "unimplemented");
                break;
            }
        }
        foreach (dyn; dyns)
        {
            immutable tag = dyn.d_tag;
            if (!(tag == DT_NEEDED || tag == DT_AUXILIARY || tag == DT_FILTER))
                continue;

            // soname of the dependency
            auto name = strtab + dyn.d_un.d_val;
            // get handle without loading the library
            auto handle = handleForName(name);
            // the runtime linker has already loaded all dependencies
            if (handle is null) assert(0);
            // if it's a D library
            if (auto pdso = dsoForHandle(handle))
                deps.insertBack(pdso); // append it to the dependencies
        }
    }

    void* handleForName(const char* name) nothrow @nogc
    {
        auto handle = .dlopen(name, RTLD_NOLOAD | RTLD_LAZY);
        if (handle !is null) .dlclose(handle); // drop reference count
        return handle;
    }
}

///////////////////////////////////////////////////////////////////////////////
// Elf program header iteration
///////////////////////////////////////////////////////////////////////////////

/************
 * Scan segments in Linux dl_phdr_info struct and store
 * the TLS and writeable data segments in *pdso.
 */
void scanSegments(in ref dl_phdr_info info, DSO* pdso) nothrow @nogc
{
    foreach (ref phdr; info.dlpi_phdr[0 .. info.dlpi_phnum])
    {
        switch (phdr.p_type)
        {
        case PT_LOAD:
            if (phdr.p_flags & PF_W) // writeable data segment
            {
                auto beg = cast(void*)(info.dlpi_addr + (phdr.p_vaddr & ~(size_t.sizeof - 1)));
                pdso._gcRanges.insertBack(beg[0 .. phdr.p_memsz]);
            }
            version (Shared) if (phdr.p_flags & PF_X) // code segment
            {
                auto beg = cast(void*)(info.dlpi_addr + (phdr.p_vaddr & ~(size_t.sizeof - 1)));
                pdso._codeSegments.insertBack(beg[0 .. phdr.p_memsz]);
            }
            break;

        case PT_TLS: // TLS segment
            assert(!pdso._tlsSize); // is unique per DSO
            pdso._tlsMod = info.dlpi_tls_modid;
            pdso._tlsSize = phdr.p_memsz;
            break;

        default:
            break;
        }
    }
}

/**************************
 * Input:
 *      result  where the output is to be written; dl_phdr_info is a Linux struct
 * Returns:
 *      true if found, and *result is filled in
 * References:
 *      http://linux.die.net/man/3/dl_iterate_phdr
 */
version (linux) bool findDSOInfoForAddr(in void* addr, dl_phdr_info* result=null) nothrow @nogc
{
    static struct DG { const(void)* addr; dl_phdr_info* result; }

    extern(C) int callback(dl_phdr_info* info, size_t sz, void* arg) nothrow @nogc
    {
        auto p = cast(DG*)arg;
        if (findSegmentForAddr(*info, p.addr))
        {
            if (p.result !is null) *p.result = *info;
            return 1; // break;
        }
        return 0; // continue iteration
    }

    auto dg = DG(addr, result);

    /* Linux function that walks through the list of an application's shared objects and
     * calls 'callback' once for each object, until either all shared objects
     * have been processed or 'callback' returns a nonzero value.
     */
    return dl_iterate_phdr(&callback, &dg) != 0;
}
else version (FreeBSD) bool findDSOInfoForAddr(in void* addr, dl_phdr_info* result=null) nothrow @nogc
{
    return !!_rtld_addr_phdr(addr, result);
}
else version (NetBSD) bool findDSOInfoForAddr(in void* addr, dl_phdr_info* result=null) nothrow @nogc
{
    static struct DG { const(void)* addr; dl_phdr_info* result; }

    extern(C) int callback(dl_phdr_info* info, size_t sz, void* arg) nothrow @nogc
    {
        auto p = cast(DG*)arg;
        if (findSegmentForAddr(*info, p.addr))
        {
            if (p.result !is null) *p.result = *info;
            return 1; // break;
        }
        return 0; // continue iteration
    }
    auto dg = DG(addr, result);
    return dl_iterate_phdr(&callback, &dg) != 0;
}

/*********************************
 * Determine if 'addr' lies within shared object 'info'.
 * If so, return true and fill in 'result' with the corresponding ELF program header.
 */
bool findSegmentForAddr(in ref dl_phdr_info info, in void* addr, ElfW!"Phdr"* result=null) nothrow @nogc
{
    if (addr < cast(void*)info.dlpi_addr) // less than base address of object means quick reject
        return false;

    foreach (ref phdr; info.dlpi_phdr[0 .. info.dlpi_phnum])
    {
        auto beg = cast(void*)(info.dlpi_addr + phdr.p_vaddr);
        if (cast(size_t)(addr - beg) < phdr.p_memsz)
        {
            if (result !is null) *result = phdr;
            return true;
        }
    }
    return false;
}

version (linux) import core.sys.linux.errno : program_invocation_name;
// should be in core.sys.freebsd.stdlib
version (FreeBSD) extern(C) const(char)* getprogname() nothrow @nogc;
version (NetBSD) extern(C) const(char)* getprogname() nothrow @nogc;

@property const(char)* progname() nothrow @nogc
{
    version (linux) return program_invocation_name;
    version (FreeBSD) return getprogname();
    version (NetBSD) return getprogname();
}

const(char)[] dsoName(const char* dlpi_name) nothrow @nogc
{
    // the main executable doesn't have a name in its dlpi_name field
    const char* p = dlpi_name[0] != 0 ? dlpi_name : progname;
    return p[0 .. strlen(p)];
}

extern(C)
{
    void* rt_get_bss_start() @nogc nothrow;
    void* rt_get_end() @nogc nothrow;
}

/// get the BSS section of the executable to check for copy relocations
const(void)[] getCopyRelocSection() nothrow @nogc
{
    auto bss_start = rt_get_bss_start();
    auto bss_end = rt_get_end();
    immutable bss_size = bss_end - bss_start;

    /**
       Check whether __bss_start/_end both lie within the executable DSO.same DSO.

       When a C host program dynamically loads druntime, i.e. it isn't linked
       against, __bss_start/_end might be defined in different DSOs, b/c the
       linker creates those symbols only when they are used.
       But as there are no copy relocations when dynamically loading a shared
       library, we can simply return a null bss range in that case.
    */
    if (bss_size <= 0)
        return null;

    version (linux)
        enum ElfW!"Addr" exeBaseAddr = 0;
    else version (FreeBSD)
        enum ElfW!"Addr" exeBaseAddr = 0;
    else version (NetBSD)
        enum ElfW!"Addr" exeBaseAddr = 0;

    dl_phdr_info info = void;
    findDSOInfoForAddr(bss_start, &info) || assert(0);
    if (info.dlpi_addr != exeBaseAddr)
        return null;
    findDSOInfoForAddr(bss_end - 1, &info) || assert(0);
    if (info.dlpi_addr != exeBaseAddr)
        return null;

    return bss_start[0 .. bss_size];
}

/**
 * Check for module collisions. A module in a shared library collides
 * with an existing module if it's ModuleInfo is interposed (search
 * symbol interposition) by another DSO.  Therefor two modules with the
 * same name do not collide if their DSOs are in separate symbol resolution
 * chains.
 */
void checkModuleCollisions(in ref dl_phdr_info info, in immutable(ModuleInfo)*[] modules,
                           in void[] copyRelocSection) nothrow @nogc
in { assert(modules.length); }
body
{
    immutable(ModuleInfo)* conflicting;

    foreach (m; modules)
    {
        auto addr = cast(const(void*))m;
        if (cast(size_t)(addr - copyRelocSection.ptr) < copyRelocSection.length)
        {
            // Module is in .bss of the exe because it was copy relocated
        }
        else if (!findSegmentForAddr(info, addr))
        {
            // Module is in another DSO
            conflicting = m;
            break;
        }
    }

    if (conflicting !is null)
    {
        dl_phdr_info other=void;
        findDSOInfoForAddr(conflicting, &other) || assert(0);

        auto modname = conflicting.name;
        auto loading = dsoName(info.dlpi_name);
        auto existing = dsoName(other.dlpi_name);
        fprintf(stderr, "Fatal Error while loading '%.*s':\n\tThe module '%.*s' is already defined in '%.*s'.\n",
                cast(int)loading.length, loading.ptr,
                cast(int)modname.length, modname.ptr,
                cast(int)existing.length, existing.ptr);
        import core.stdc.stdlib : _Exit;
        _Exit(1);
    }
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

///////////////////////////////////////////////////////////////////////////////
// TLS module helper
///////////////////////////////////////////////////////////////////////////////


/*
 * Returns: the TLS memory range for a given module and the calling
 * thread or null if that module has no TLS.
 *
 * Note: This will cause the TLS memory to be eagerly allocated.
 */
struct tls_index
{
    version (CRuntime_Glibc)
    {
        // For x86_64, fields are of type uint64_t, this is important for x32
        // where tls_index would otherwise have the wrong size.
        // See https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/x86_64/dl-tls.h
        version (X86_64)
        {
            ulong ti_module;
            ulong ti_offset;
        }
        else
        {
            c_ulong ti_module;
            c_ulong ti_offset;
        }
    }
    else
    {
        size_t ti_module;
        size_t ti_offset;
    }
}

extern(C) void* __tls_get_addr(tls_index* ti) nothrow @nogc;

/* The dynamic thread vector (DTV) pointers may point 0x8000 past the start of
 * each TLS block. This is at least true for PowerPC and Mips platforms.
 * See: https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/powerpc/dl-tls.h;h=f7cf6f96ebfb505abfd2f02be0ad0e833107c0cd;hb=HEAD#l34
 *      https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/mips/dl-tls.h;h=93a6dc050cb144b9f68b96fb3199c60f5b1fcd18;hb=HEAD#l32
 *      https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/riscv/dl-tls.h;h=ab2d860314de94c18812bc894ff6b3f55368f20f;hb=HEAD#l32
 */
version (X86)
    enum TLS_DTV_OFFSET = 0x0;
else version (X86_64)
    enum TLS_DTV_OFFSET = 0x0;
else version (ARM)
    enum TLS_DTV_OFFSET = 0x0;
else version (AArch64)
    enum TLS_DTV_OFFSET = 0x0;
else version (RISCV32)
    enum TLS_DTV_OFFSET = 0x800;
else version (RISCV64)
    enum TLS_DTV_OFFSET = 0x800;
else version (HPPA)
    enum TLS_DTV_OFFSET = 0x0;
else version (SPARC)
    enum TLS_DTV_OFFSET = 0x0;
else version (SPARC64)
    enum TLS_DTV_OFFSET = 0x0;
else version (PPC)
    enum TLS_DTV_OFFSET = 0x8000;
else version (PPC64)
    enum TLS_DTV_OFFSET = 0x8000;
else version (MIPS32)
    enum TLS_DTV_OFFSET = 0x8000;
else version (MIPS64)
    enum TLS_DTV_OFFSET = 0x8000;
else
    static assert( false, "Platform not supported." );

void[] getTLSRange(size_t mod, size_t sz) nothrow @nogc
{
    if (mod == 0)
        return null;

    // base offset
    auto ti = tls_index(mod, 0);
    return (__tls_get_addr(&ti)-TLS_DTV_OFFSET)[0 .. sz];
}
