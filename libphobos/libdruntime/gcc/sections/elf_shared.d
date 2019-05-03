// ELF-specific support for sections with shared libraries.
// Copyright (C) 2019 Free Software Foundation, Inc.

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

module gcc.sections.elf_shared;

version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;
version (S390)    version = IBMZ_Any;
version (SystemZ) version = IBMZ_Any;

version (CRuntime_Glibc) enum SharedELF = true;
else version (CRuntime_Musl) enum SharedELF = true;
else version (FreeBSD) enum SharedELF = true;
else version (NetBSD) enum SharedELF = true;
else version (DragonFlyBSD) enum SharedELF = true;
else version (CRuntime_UClibc) enum SharedELF = true;
else version (Solaris) enum SharedELF = true;
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
else version (DragonFlyBSD)
{
    import core.sys.dragonflybsd.dlfcn;
    import core.sys.dragonflybsd.sys.elf;
    import core.sys.dragonflybsd.sys.link_elf;
}
else version (Solaris)
{
    import core.sys.solaris.dlfcn;
    import core.sys.solaris.link;
    import core.sys.solaris.sys.elf;
    import core.sys.solaris.sys.link;
}
else
{
    static assert(0, "unimplemented");
}
import core.sys.posix.pthread;
import gcc.builtins;
import gcc.config;
import rt.deh;
import rt.dmain2;
import rt.minfo;
import rt.util.container.array;
import rt.util.container.hashtab;

/****
 * Asserts the specified condition, independent from -release, by abort()ing.
 * Regular assertions throw an AssertError and thus require an initialized
 * GC, which isn't the case (yet or anymore) for the startup/shutdown code in
 * this module (called by CRT ctors/dtors etc.).
 */
private void safeAssert(bool condition, scope string msg, size_t line = __LINE__) @nogc nothrow @safe
{
    import core.internal.abort;
    condition || abort(msg, __FILE__, line);
}

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
        safeAssert(_moduleGroup.modules.length > 0, "No modules for DSO.");
        safeAssert(_tlsMod || !_tlsSize, "Inconsistent TLS fields for DSO.");
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

    // get the TLS range for the executing thread
    void[] tlsRange() const nothrow @nogc
    {
        return getTLSRange(_tlsMod, _tlsSize);
    }
}

/****
 * Boolean flag set to true while the runtime is initialized.
 */
__gshared bool _isRuntimeInitialized;


version (FreeBSD) private __gshared void* dummy_ref;
version (DragonFlyBSD) private __gshared void* dummy_ref;
version (NetBSD) private __gshared void* dummy_ref;
version (Solaris) private __gshared void* dummy_ref;

/****
 * Gets called on program startup just before GC is initialized.
 */
void initSections() nothrow @nogc
{
    _isRuntimeInitialized = true;
    // reference symbol to support weak linkage
    version (FreeBSD) dummy_ref = &_d_dso_registry;
    version (DragonFlyBSD) dummy_ref = &_d_dso_registry;
    version (NetBSD) dummy_ref = &_d_dso_registry;
    version (Solaris) dummy_ref = &_d_dso_registry;
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
        {
            import gcc.emutls;
            _d_emutls_scan(dg);
        }
        else
        {
            foreach (ref tdso; *tdsos)
                dg(tdso._tlsRange.ptr, tdso._tlsRange.ptr + tdso._tlsRange.length);
        }
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
                const success = .dlopen(linkMapForHandle(tdso._pdso._handle).l_name, RTLD_LAZY) !is null;
                safeAssert(success, "Failed to increment dlopen ref.");
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
                safeAssert(handle !is null, "Invalid library handle.");
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
        safeAssert(_loadedDSOs.empty, "DSOs have already been registered for this thread.");
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
        auto rngs = &_tlsRanges();
        if (rngs.empty)
        {
            foreach (ref pdso; _loadedDSOs)
                rngs.insertBack(pdso.tlsRange());
        }
        return rngs;
    }

    void finiTLSRanges(Array!(void[])* rngs) nothrow @nogc
    {
        rngs.reset();
    }

    void scanTLSRanges(Array!(void[])* rngs, scope ScanDG dg) nothrow
    {
        version (GNU_EMUTLS)
        {
            import gcc.emutls;
            _d_emutls_scan(dg);
        }
        else
        {
            foreach (rng; *rngs)
                dg(rng.ptr, rng.ptr + rng.length);
        }
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
        void[] _tlsRange;
        alias _pdso this;
        // update the _tlsRange for the executing thread
        void updateTLSRange() nothrow @nogc
        {
            _tlsRange = _pdso.tlsRange();
        }
    }
    @property ref Array!(ThreadDSO) _loadedDSOs() @nogc nothrow { static Array!(ThreadDSO) x; return x; }

    /*
     * Set to true during rt_loadLibrary/rt_unloadLibrary calls.
     */
    bool _rtLoading;

    /*
     * Hash table to map link_map* to corresponding DSO*.
     * The hash table is protected by a Mutex.
     */
    __gshared pthread_mutex_t _handleToDSOMutex;
    @property ref HashTab!(void*, DSO*) _handleToDSO() @nogc nothrow { __gshared HashTab!(void*, DSO*) x; return x; }

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
    @property ref Array!(DSO*) _loadedDSOs() @nogc nothrow { __gshared Array!(DSO*) x; return x; }

    /*
     * Thread local array that contains TLS memory ranges for each
     * library initialized in this thread.
     */
    @property ref Array!(void[]) _tlsRanges() @nogc nothrow { static Array!(void[]) x; return x; }

    enum _rtLoading = false;
}

///////////////////////////////////////////////////////////////////////////////
// Compiler to runtime interface.
///////////////////////////////////////////////////////////////////////////////

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
    safeAssert(data._version >= 1, "Incompatible compiler-generated DSO data version.");

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
        const headerFound = findDSOInfoForAddr(data._slot, &info);
        safeAssert(headerFound, "Failed to find image header.");

        scanSegments(info, pdso);

        version (Shared)
        {
            auto handle = handleForAddr(data._slot);

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
                _loadedDSOs.insertBack(ThreadDSO(pdso, refCnt, addCnt, pdso.tlsRange()));
            }
        }
        else
        {
            foreach (p; _loadedDSOs)
                safeAssert(p !is pdso, "DSO already registered.");
            _loadedDSOs.insertBack(pdso);
            _tlsRanges.insertBack(pdso.tlsRange());
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
            version (Shared)
            {
                safeAssert(_handleToDSO.empty, "_handleToDSO not in sync with _loadedDSOs.");
                _handleToDSO.reset();
            }
            finiLocks();
            version (GNU_EMUTLS)
            {
                import gcc.emutls;
                _d_emutls_destroy();
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
// Dynamic loading
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
            _loadedDSOs.insertBack(ThreadDSO(pdso, refCnt, addCnt, pdso.tlsRange()));
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
// Helper functions
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
    link_map* linkMapForHandle(void* handle)
    {
        link_map* map;
        const success = dlinfo(handle, RTLD_DI_LINKMAP, &map) == 0;
        safeAssert(success, "Failed to get DSO info.");
        return map;
    }

     link_map* exeLinkMap(link_map* map)
     {
         safeAssert(map !is null, "Invalid link_map.");
         while (map.l_prev !is null)
             map = map.l_prev;
         return map;
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

    void getDependencies(in ref dl_phdr_info info, ref Array!(DSO*) deps)
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
                version (CRuntime_Musl)
                    strtab = cast(const(char)*)(info.dlpi_addr + dyn.d_un.d_ptr); // relocate
                else version (linux)
                {
                    // This might change in future glibc releases (after 2.29) as dynamic sections
                    // are not required to be read-only on RISC-V. This was copy & pasted from MIPS
                    // while upstreaming RISC-V support. Otherwise MIPS is the only arch which sets
                    // in glibc: #define DL_RO_DYN_SECTION 1
                    version (RISCV_Any)
                        strtab = cast(const(char)*)(info.dlpi_addr + dyn.d_un.d_ptr); // relocate
                    else
                        strtab = cast(const(char)*)dyn.d_un.d_ptr;
                }
                else version (FreeBSD)
                    strtab = cast(const(char)*)(info.dlpi_addr + dyn.d_un.d_ptr); // relocate
                else version (NetBSD)
                    strtab = cast(const(char)*)(info.dlpi_addr + dyn.d_un.d_ptr); // relocate
                else version (DragonFlyBSD)
                    strtab = cast(const(char)*)(info.dlpi_addr + dyn.d_un.d_ptr); // relocate
                else version (Solaris)
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
            safeAssert(handle !is null, "Failed to get library handle.");
            // if it's a D library
            if (auto pdso = dsoForHandle(handle))
                deps.insertBack(pdso); // append it to the dependencies
        }
    }

    void* handleForName(const char* name)
    {
        auto handle = .dlopen(name, RTLD_NOLOAD | RTLD_LAZY);
        version (Solaris) { }
        else if (handle !is null) .dlclose(handle); // drop reference count
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
            version (GNU_EMUTLS)
            {
            }
            else
            {
                safeAssert(!pdso._tlsSize, "Multiple TLS segments in image header.");
                static if (OS_Have_Dlpi_Tls_Modid)
                {
                    pdso._tlsMod = info.dlpi_tls_modid;
                    pdso._tlsSize = phdr.p_memsz;
                }
                else version (Solaris)
                {
                    struct Rt_map
                    {
                        Link_map rt_public;
                        const char* rt_pathname;
                        c_ulong rt_padstart;
                        c_ulong rt_padimlen;
                        c_ulong rt_msize;
                        uint rt_flags;
                        uint rt_flags1;
                        c_ulong rt_tlsmodid;
                    }

                    Rt_map* map;
                    version (Shared)
                        dlinfo(handleForName(info.dlpi_name), RTLD_DI_LINKMAP, &map);
                    else
                        dlinfo(RTLD_SELF, RTLD_DI_LINKMAP, &map);
                    // Until Solaris 11.4, tlsmodid for the executable is 0.
                    // Let it start at 1 as the rest of the code expects.
                    pdso._tlsMod = map.rt_tlsmodid + 1;
                    pdso._tlsSize = phdr.p_memsz;
                }
                else
                {
                    pdso._tlsMod = 0;
                    pdso._tlsSize = 0;
                }
            }
            break;

        default:
            break;
        }
    }
}

/**************************
 * Input:
 *      result  where the output is to be written; dl_phdr_info is an OS struct
 * Returns:
 *      true if found, and *result is filled in
 * References:
 *      http://linux.die.net/man/3/dl_iterate_phdr
 */
bool findDSOInfoForAddr(in void* addr, dl_phdr_info* result=null) nothrow @nogc
{
    version (linux)        enum IterateManually = true;
    else version (NetBSD)  enum IterateManually = true;
    else version (Solaris) enum IterateManually = true;
    else                   enum IterateManually = false;

    static if (IterateManually)
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

        /* OS function that walks through the list of an application's shared objects and
         * calls 'callback' once for each object, until either all shared objects
         * have been processed or 'callback' returns a nonzero value.
         */
        return dl_iterate_phdr(&callback, &dg) != 0;
    }
    else version (FreeBSD)
    {
        return !!_rtld_addr_phdr(addr, result);
    }
    else version (DragonFlyBSD)
    {
        return !!_rtld_addr_phdr(addr, result);
    }
    else
        static assert(0, "unimplemented");
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
version (DragonFlyBSD) extern(C) const(char)* getprogname() nothrow @nogc;
version (NetBSD) extern(C) const(char)* getprogname() nothrow @nogc;
version (Solaris) extern(C) const(char)* getprogname() nothrow @nogc;

@property const(char)* progname() nothrow @nogc
{
    version (linux) return program_invocation_name;
    version (FreeBSD) return getprogname();
    version (DragonFlyBSD) return getprogname();
    version (NetBSD) return getprogname();
    version (Solaris) return getprogname();
}

const(char)[] dsoName(const char* dlpi_name) nothrow @nogc
{
    // the main executable doesn't have a name in its dlpi_name field
    const char* p = dlpi_name[0] != 0 ? dlpi_name : progname;
    return p[0 .. strlen(p)];
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
extern(C) void* __tls_get_addr_internal(tls_index* ti) nothrow @nogc;

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
else version (IBMZ_Any)
    enum TLS_DTV_OFFSET = 0x0;
else
    static assert( false, "Platform not supported." );

void[] getTLSRange(size_t mod, size_t sz) nothrow @nogc
{
    if (mod == 0)
        return null;

    version (GNU_EMUTLS)
        return null;    // Handled in scanTLSRanges().
    else
    {
        version (Solaris)
        {
            static if (!OS_Have_Dlpi_Tls_Modid)
                mod -= 1;
        }

        // base offset
        auto ti = tls_index(mod, 0);
        version (IBMZ_Any)
        {
            auto idx = cast(void *)__tls_get_addr_internal(&ti)
                + cast(ulong)__builtin_thread_pointer();
            return idx[0 .. sz];
        }
        else
            return (__tls_get_addr(&ti)-TLS_DTV_OFFSET)[0 .. sz];
    }
}
