/**
 * This module provides OS specific helper function for DLL support
 *
 * Copyright: Copyright Digital Mars 2010 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Rainer Schuetze
 * Source: $(DRUNTIMESRC src/core/sys/windows/_dll.d)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module core.sys.windows.dll;
version (Windows):

import core.sys.windows.winbase;
import core.sys.windows.winnt;
import core.stdc.string;
import core.runtime;

public import core.sys.windows.threadaux;

///////////////////////////////////////////////////////////////////
// support fixing implicit TLS for dynamically loaded DLLs on Windows XP

// in this special case, we have to treat _tlsstart and _tlsend as non-TLS variables
//  as they are used to simulate TLS when it is not set up under XP. In this case we must
//  not access tls_array[tls_index] as needed for thread local _tlsstart and _tlsend
extern (C)
{
        version (MinGW)
        {
            extern __gshared void* _tls_start;
            extern __gshared void* _tls_end;
            extern __gshared void* __xl_a;

            alias _tls_start _tlsstart;
            alias _tls_end   _tlsend;
            alias __xl_a     _tls_callbacks_a;
        }
        else version (Win32)
    {
        version (CRuntime_DigitalMars)
        {
            extern __gshared byte  _tlsstart;
            extern __gshared byte  _tlsend;
            extern __gshared void* _tls_callbacks_a;
        }
        else version (CRuntime_Microsoft)
        {
            extern __gshared byte  _tls_start;
            extern __gshared byte  _tls_end;
            extern __gshared void*  __xl_a;
            alias _tls_start _tlsstart;
            alias _tls_end   _tlsend;
            alias __xl_a     _tls_callbacks_a;
        }
        extern __gshared int   _tls_index;
    }
}

extern (C) // rt.minfo
{
    void rt_moduleTlsCtor();
    void rt_moduleTlsDtor();
}

private:
version (Win32)
{
struct dll_aux
{
    // don't let symbols leak into other modules
    struct LdrpTlsListEntry
    {
        LdrpTlsListEntry* next;
        LdrpTlsListEntry* prev;
        void* tlsstart;
        void* tlsend;
        void* ptr_tlsindex;
        void* callbacks;
        void* zerofill;
        int   tlsindex;
    }

    alias fnRtlAllocateHeap = extern(Windows)
    void* function(void* HeapHandle, uint Flags, size_t Size) nothrow;

    // find a code sequence and return the address after the sequence
    static void* findCodeSequence( void* adr, int len, ref ubyte[] pattern ) nothrow
    {
        if ( !adr )
            return null;

        ubyte* code = cast(ubyte*) adr;
        for ( int p = 0; p < len; p++ )
        {
            if ( code[ p .. p + pattern.length ] == pattern[ 0 .. $ ] )
            {
                ubyte* padr = code + p + pattern.length;
                return padr;
            }
        }
        return null;
    }

    // find a code sequence and return the (relative) address that follows
    static void* findCodeReference( void* adr, int len, ref ubyte[] pattern, bool relative ) nothrow
    {
        if ( !adr )
            return null;

        ubyte* padr = cast(ubyte*) findCodeSequence( adr, len, pattern );
        if ( padr )
        {
            if ( relative )
                return padr + 4 + *cast(int*) padr;
            return *cast(void**) padr;
        }
        return null;
    }

    // crawl through ntdll to find function _LdrpAllocateTls@0 and references
    //  to _LdrpNumberOfTlsEntries, _NtdllBaseTag and _LdrpTlsList
    // LdrInitializeThunk
    // -> _LdrpInitialize@12
    // -> _LdrpInitializeThread@4
    // -> _LdrpAllocateTls@0
    // -> je chunk
    //     _LdrpNumberOfTlsEntries - number of entries in TlsList
    //     _NtdllBaseTag           - tag used for RtlAllocateHeap
    //     _LdrpTlsList            - root of the double linked list with TlsList entries

    static __gshared int* pNtdllBaseTag; // remembered for reusage in addTlsData

    static __gshared ubyte[] jmp_LdrpInitialize = [ 0x33, 0xED, 0xE9 ]; // xor ebp,ebp; jmp _LdrpInitialize
    static __gshared ubyte[] jmp__LdrpInitialize = [ 0x5D, 0xE9 ]; // pop ebp; jmp __LdrpInitialize
    static __gshared ubyte[] jmp__LdrpInitialize_xp64 = [ 0x5D, 0x90, 0x90, 0x90, 0x90, 0x90 ]; // pop ebp; nop; nop; nop; nop; nop;
    static __gshared ubyte[] call_LdrpInitializeThread = [ 0xFF, 0x75, 0x08, 0xE8 ]; // push [ebp+8]; call _LdrpInitializeThread
    static __gshared ubyte[] call_LdrpAllocateTls = [ 0x00, 0x00, 0xE8 ]; // jne 0xc3; call _LdrpAllocateTls
    static __gshared ubyte[] call_LdrpAllocateTls_svr03 = [ 0x65, 0xfc, 0x00, 0xE8 ]; // and [ebp+fc], 0; call _LdrpAllocateTls
    static __gshared ubyte[] jne_LdrpAllocateTls = [ 0x0f, 0x85 ]; // jne body_LdrpAllocateTls
    static __gshared ubyte[] mov_LdrpNumberOfTlsEntries = [ 0x8B, 0x0D ]; // mov ecx, _LdrpNumberOfTlsEntries
    static __gshared ubyte[] mov_NtdllBaseTag = [ 0x51, 0x8B, 0x0D ]; // push ecx; mov ecx, _NtdllBaseTag
    static __gshared ubyte[] mov_NtdllBaseTag_srv03 = [ 0x50, 0xA1 ]; // push eax; mov eax, _NtdllBaseTag
    static __gshared ubyte[] mov_LdrpTlsList = [ 0x8B, 0x3D ]; // mov edi, _LdrpTlsList

    static LdrpTlsListEntry* addTlsListEntry( void** peb, void* tlsstart, void* tlsend, void* tls_callbacks_a, int* tlsindex ) nothrow
    {
        HANDLE hnd = GetModuleHandleA( "NTDLL" );
        assert( hnd, "cannot get module handle for ntdll" );
        ubyte* fn = cast(ubyte*) GetProcAddress( hnd, "LdrInitializeThunk" );
        assert( fn, "cannot find LdrInitializeThunk in ntdll" );

        void* pLdrpInitialize = findCodeReference( fn, 20, jmp_LdrpInitialize, true );
        void* p_LdrpInitialize = findCodeReference( pLdrpInitialize, 40, jmp__LdrpInitialize, true );
        if ( !p_LdrpInitialize )
            p_LdrpInitialize = findCodeSequence( pLdrpInitialize, 40, jmp__LdrpInitialize_xp64 );
        void* pLdrpInitializeThread = findCodeReference( p_LdrpInitialize, 200, call_LdrpInitializeThread, true );
        void* pLdrpAllocateTls = findCodeReference( pLdrpInitializeThread, 40, call_LdrpAllocateTls, true );
        if (!pLdrpAllocateTls)
            pLdrpAllocateTls = findCodeReference( pLdrpInitializeThread, 100, call_LdrpAllocateTls_svr03, true );
        void* pBodyAllocateTls = findCodeReference( pLdrpAllocateTls, 40, jne_LdrpAllocateTls, true );

        int* pLdrpNumberOfTlsEntries = cast(int*) findCodeReference( pBodyAllocateTls, 60, mov_LdrpNumberOfTlsEntries, false );
        pNtdllBaseTag = cast(int*) findCodeReference( pBodyAllocateTls, 30, mov_NtdllBaseTag, false );
        if (!pNtdllBaseTag)
            pNtdllBaseTag = cast(int*) findCodeReference( pBodyAllocateTls, 30, mov_NtdllBaseTag_srv03, false );
        LdrpTlsListEntry* pLdrpTlsList = cast(LdrpTlsListEntry*)findCodeReference( pBodyAllocateTls, 80, mov_LdrpTlsList, false );

        if ( !pLdrpNumberOfTlsEntries || !pNtdllBaseTag || !pLdrpTlsList )
            return null;

        fnRtlAllocateHeap fnAlloc = cast(fnRtlAllocateHeap) GetProcAddress( hnd, "RtlAllocateHeap" );
        if ( !fnAlloc )
            return null;

        // allocate new TlsList entry (adding 0xC0000 to the tag is obviously a flag also usesd by
        //  the nt-loader, could be the result of HEAP_MAKE_TAG_FLAGS(0,HEAP_NO_SERIALIZE|HEAP_GROWABLE)
        //  but this is not documented in the msdn entry for RtlAlloateHeap
        void* heap = peb[6];
        LdrpTlsListEntry* entry = cast(LdrpTlsListEntry*) (*fnAlloc)( heap, *pNtdllBaseTag | 0xc0000, LdrpTlsListEntry.sizeof );
        if ( !entry )
            return null;

        // fill entry
        entry.tlsstart = tlsstart;
        entry.tlsend = tlsend;
        entry.ptr_tlsindex = tlsindex;
        entry.callbacks = tls_callbacks_a;
        entry.zerofill = null;
        entry.tlsindex = *pLdrpNumberOfTlsEntries;

        // and add it to the end of TlsList
        *tlsindex = *pLdrpNumberOfTlsEntries;
        entry.next = pLdrpTlsList;
        entry.prev = pLdrpTlsList.prev;
        pLdrpTlsList.prev.next = entry;
        pLdrpTlsList.prev = entry;
        (*pLdrpNumberOfTlsEntries)++;

        return entry;
    }

    // reallocate TLS array and create a copy of the TLS data section
    static bool addTlsData( void** teb, void* tlsstart, void* tlsend, int tlsindex ) nothrow
    {
        HANDLE hnd = GetModuleHandleA( "NTDLL" );
        assert( hnd, "cannot get module handle for ntdll" );

        fnRtlAllocateHeap fnAlloc = cast(fnRtlAllocateHeap) GetProcAddress( hnd, "RtlAllocateHeap" );
        if ( !fnAlloc || !pNtdllBaseTag )
            return false;

        void** peb = cast(void**) teb[12];
        void* heap = peb[6];

        auto sz = tlsend - tlsstart;
        void* tlsdata = cast(void*) (*fnAlloc)( heap, *pNtdllBaseTag | 0xc0000, sz );
        if ( !tlsdata )
            return false;

        // no relocations! not even self-relocations. Windows does not do them.
        core.stdc.string.memcpy( tlsdata, tlsstart, sz );

        // create copy of tls pointer array
        void** array = cast(void**) (*fnAlloc)( heap, *pNtdllBaseTag | 0xc0000, (tlsindex + 1) * (void*).sizeof );
        if ( !array )
            return false;

        if ( tlsindex > 0 && teb[11] )
            core.stdc.string.memcpy( array, teb[11], tlsindex * (void*).sizeof);
        array[tlsindex] = tlsdata;
        teb[11] = cast(void*) array;

        // let the old array leak, in case a oncurrent thread is still relying on it
        return true;
    }

    alias bool BOOLEAN;

    struct UNICODE_STRING
    {
        short Length;
        short MaximumLength;
        wchar* Buffer;
    }

    struct LIST_ENTRY
    {
        LIST_ENTRY* next;
        LIST_ENTRY* prev;
    }

    // the following structures can be found here: http://undocumented.ntinternals.net/
    // perhaps this should be same as LDR_DATA_TABLE_ENTRY, which is introduced with PEB_LDR_DATA
    struct LDR_MODULE
    {
        LIST_ENTRY      InLoadOrderModuleList;
        LIST_ENTRY      InMemoryOrderModuleList;
        LIST_ENTRY      InInitializationOrderModuleList;
        PVOID           BaseAddress;
        PVOID           EntryPoint;
        SIZE_T          SizeOfImage;
        UNICODE_STRING  FullDllName;
        UNICODE_STRING  BaseDllName;
        ULONG           Flags;
        SHORT           LoadCount;
        SHORT           TlsIndex;
        LIST_ENTRY      HashTableEntry;
        ULONG           TimeDateStamp;
    }

    struct PEB_LDR_DATA
    {
        ULONG           Length;
        BOOLEAN         Initialized;
        PVOID           SsHandle;
        LIST_ENTRY      InLoadOrderModuleList;
        LIST_ENTRY      InMemoryOrderModuleList;
        LIST_ENTRY      InInitializationOrderModuleList;
    }

    static LDR_MODULE* findLdrModule( HINSTANCE hInstance, void** peb ) nothrow
    {
        PEB_LDR_DATA* ldrData = cast(PEB_LDR_DATA*) peb[3];
        LIST_ENTRY* root = &ldrData.InLoadOrderModuleList;
        for (LIST_ENTRY* entry = root.next; entry != root; entry = entry.next)
        {
            LDR_MODULE *ldrMod = cast(LDR_MODULE*) entry;
            if (ldrMod.BaseAddress == hInstance)
                return ldrMod;
        }
        return null;
    }

    static bool setDllTlsUsage( HINSTANCE hInstance, void** peb ) nothrow
    {
        LDR_MODULE *thisMod = findLdrModule( hInstance, peb );
        if ( !thisMod )
            return false;

        thisMod.TlsIndex = -1;  // uses TLS (not the index itself)
        thisMod.LoadCount = -1; // never unload
        return true;
    }
}
}

public:
/* *****************************************************
 * Fix implicit thread local storage for the case when a DLL is loaded
 * dynamically after process initialization.
 * The link time variables are passed to allow placing this function into
 * an RTL DLL itself.
 * The problem is described in Bugzilla 3342 and
 * http://www.nynaeve.net/?p=187, to quote from the latter:
 *
 * "When a DLL using implicit TLS is loaded, because the loader doesn't process the TLS
 *  directory, the _tls_index value is not initialized by the loader, nor is there space
 *  allocated for module's TLS data in the ThreadLocalStoragePointer arrays of running
 *  threads. The DLL continues to load, however, and things will appear to work... until the
 *  first access to a __declspec(thread) variable occurs, that is."
 *
 * _tls_index is initialized by the compiler to 0, so we can use this as a test.
 */
bool dll_fixTLS( HINSTANCE hInstance, void* tlsstart, void* tlsend, void* tls_callbacks_a, int* tlsindex ) nothrow
{
    version (Win64)
        return true;                // fixed
    else version (Win32)
    {
    /* If the OS has allocated a TLS slot for us, we don't have to do anything
     * tls_index 0 means: the OS has not done anything, or it has allocated slot 0
     * Vista and later Windows systems should do this correctly and not need
     * this function.
     */
    if ( *tlsindex != 0 )
        return true;

    void** peb;
    asm pure nothrow @nogc
    {
        mov EAX,FS:[0x30];
        mov peb, EAX;
    }
    dll_aux.LDR_MODULE *ldrMod = dll_aux.findLdrModule( hInstance, peb );
    if ( !ldrMod )
        return false; // not in module list, bail out
    if ( ldrMod.TlsIndex != 0 )
        return true;  // the OS has already setup TLS

    dll_aux.LdrpTlsListEntry* entry = dll_aux.addTlsListEntry( peb, tlsstart, tlsend, tls_callbacks_a, tlsindex );
    if ( !entry )
        return false;

    scope (failure) assert(0); // enforce nothrow, Bugzilla 13561

    if ( !enumProcessThreads(
        function (uint id, void* context) nothrow {
            dll_aux.LdrpTlsListEntry* entry = cast(dll_aux.LdrpTlsListEntry*) context;
            return dll_aux.addTlsData( getTEB( id ), entry.tlsstart, entry.tlsend, entry.tlsindex );
        }, entry ) )
        return false;

    ldrMod.TlsIndex = -1;  // flag TLS usage (not the index itself)
    ldrMod.LoadCount = -1; // prevent unloading of the DLL,
                           // since XP does not keep track of used TLS entries
    return true;
    }
}

// fixup TLS storage, initialize runtime and attach to threads
// to be called from DllMain with reason DLL_PROCESS_ATTACH
bool dll_process_attach( HINSTANCE hInstance, bool attach_threads,
                         void* tlsstart, void* tlsend, void* tls_callbacks_a, int* tlsindex )
{
    version (Win32)
    {
        if ( !dll_fixTLS( hInstance, tlsstart, tlsend, tls_callbacks_a, tlsindex ) )
            return false;
    }

    Runtime.initialize();

    if ( !attach_threads )
        return true;

    // attach to all other threads
    return enumProcessThreads(
        function (uint id, void* context) {
            if ( !thread_findByAddr( id ) )
            {
                // if the OS has not prepared TLS for us, don't attach to the thread
                if ( GetTlsDataAddress( id ) )
                {
                    thread_attachByAddr( id );
                    thread_moduleTlsCtor( id );
                }
            }
            return true;
        }, null );
}

// same as above, but only usable if druntime is linked statically
bool dll_process_attach( HINSTANCE hInstance, bool attach_threads = true )
{
    version (Win64)
    {
        return dll_process_attach( hInstance, attach_threads,
                                   null, null, null, null );
    }
    else version (Win32)
    {
        return dll_process_attach( hInstance, attach_threads,
                                   &_tlsstart, &_tlsend, &_tls_callbacks_a, &_tls_index );
    }
}

// to be called from DllMain with reason DLL_PROCESS_DETACH
void dll_process_detach( HINSTANCE hInstance, bool detach_threads = true )
{
    // detach from all other threads
    if ( detach_threads )
        enumProcessThreads(
            function (uint id, void* context) {
                if ( id != GetCurrentThreadId() && thread_findByAddr( id ) )
                {
                    thread_moduleTlsDtor( id );
                    thread_detachByAddr( id );
                }
                return true;
            }, null );

    Runtime.terminate();
}

/* Make sure that tlsCtorRun is itself a tls variable
 */
static bool tlsCtorRun;
static this() { tlsCtorRun = true; }
static ~this() { tlsCtorRun = false; }

// to be called from DllMain with reason DLL_THREAD_ATTACH
bool dll_thread_attach( bool attach_thread = true, bool initTls = true )
{
    // if the OS has not prepared TLS for us, don't attach to the thread
    //  (happened when running under x64 OS)
    if ( !GetTlsDataAddress( GetCurrentThreadId() ) )
        return false;
    if ( !thread_findByAddr( GetCurrentThreadId() ) )
    {
        // only attach to thread and initalize it if it is not in the thread list (so it's not created by "new Thread")
        if ( attach_thread )
            thread_attachThis();
        if ( initTls && !tlsCtorRun ) // avoid duplicate calls
            rt_moduleTlsCtor();
    }
    return true;
}

// to be called from DllMain with reason DLL_THREAD_DETACH
bool dll_thread_detach( bool detach_thread = true, bool exitTls = true )
{
    // if the OS has not prepared TLS for us, we did not attach to the thread
    if ( !GetTlsDataAddress( GetCurrentThreadId() ) )
         return false;
    if ( thread_findByAddr( GetCurrentThreadId() ) )
    {
        if ( exitTls && tlsCtorRun ) // avoid dtors to be run twice
            rt_moduleTlsDtor();
        if ( detach_thread )
            thread_detachThis();
    }
    return true;
}

/// A simple mixin to provide a $(D DllMain) which calls the necessary
/// runtime initialization and termination functions automatically.
///
/// Instead of writing a custom $(D DllMain), simply write:
///
/// ---
/// mixin SimpleDllMain;
/// ---
mixin template SimpleDllMain()
{
    import core.sys.windows.windef : HINSTANCE;

    extern(Windows)
    bool DllMain(HINSTANCE hInstance, uint ulReason, void* reserved)
    {
        import core.sys.windows.winnt;
        import core.sys.windows.dll :
            dll_process_attach, dll_process_detach,
            dll_thread_attach, dll_thread_detach;
        switch (ulReason)
        {
            default: assert(0);
            case DLL_PROCESS_ATTACH:
                return dll_process_attach( hInstance, true );

            case DLL_PROCESS_DETACH:
                dll_process_detach( hInstance, true );
                return true;

            case DLL_THREAD_ATTACH:
                return dll_thread_attach( true, true );

            case DLL_THREAD_DETACH:
                return dll_thread_detach( true, true );
        }
    }
}
