/**
 * This module provides OS specific helper function for threads support
 *
 * Copyright: Copyright Digital Mars 2010 - 2010.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Source:    $(DRUNTIMESRC core/sys/windows/_threadaux.d)
 * Authors:   Rainer Schuetze
 */

module core.sys.windows.threadaux;
version (Windows):

import core.sys.windows.basetsd/+ : HANDLE+/;
import core.sys.windows.winbase/+ : CloseHandle, GetCurrentThreadId, GetCurrentProcessId,
    GetModuleHandleA, GetProcAddress+/;
import core.sys.windows.windef/+ : BOOL, DWORD, FALSE, HRESULT+/;
import core.stdc.stdlib;

public import core.thread;

extern(Windows)
HANDLE OpenThread(DWORD dwDesiredAccess, BOOL bInheritHandle, DWORD dwThreadId) nothrow @nogc;

extern (C) extern __gshared int _tls_index;

extern (C) // rt.minfo
{
    void rt_moduleTlsCtor();
    void rt_moduleTlsDtor();
}

private:
///////////////////////////////////////////////////////////////////
struct thread_aux
{
    // don't let symbols leak into other modules

    enum SystemProcessInformation = 5;
    enum STATUS_INFO_LENGTH_MISMATCH = 0xc0000004;

    // structs subject to change according to MSDN, more info at http://undocumented.ntinternals.net
    // declarations according to http://processhacker.sourceforge.net/doc/ntexapi_8h_source.html
    // NOTE: the declarations assume default alignment for Win64 and contain some padding data
    struct UNICODE_STRING
    {
        short Length;
        short MaximumLength;
        wchar* Buffer;
    }
    // process or thread ID, documentation says it is a HANDLE, but it's actually the ID (a DWORD)
    alias size_t PTID;

    struct _SYSTEM_PROCESS_INFORMATION
    {
        int     NextEntryOffset; // When this entry is 0, there are no more processes to be read.
        int     NumberOfThreads;
        long    WorkingSetPrivateSize;
        uint    HardFaultCount;
        uint    NumberOfThreadsHighWatermark;
        ulong   CycleTime;
        long    CreateTime;
        long    UserTime;
        long    KernelTime;
        UNICODE_STRING      ImageName;
        int     BasePriority;
        PTID    /*Unique*/ProcessId;
        PTID    InheritedFromUniqueProcessId;
        uint    HandleCount;
        uint    SessionId;
        size_t  UniqueProcessKey;
        size_t  PeakVirtualSize;
        size_t  VirtualSize;
        uint    PageFaultCount;
        size_t  PeakWorkingSetSize;
        size_t  WorkingSetSize;
        size_t  QuotaPeakPagedPoolUsage;
        size_t  QuotaPagedPoolUsage;
        size_t  QuotaPeakNonPagedPoolUsage;
        size_t  QuotaNonPagedPoolUsage;
        size_t  PagefileUsage;
        size_t  PeakPagefileUsage;
        size_t  PrivatePageCount;
        long    ReadOperationCount;
        long    WriteOperationCount;
        long    OtherOperationCount;
        long    ReadTransferCount;
        long    WriteTransferCount;
        long    OtherTransferCount;

        // SYSTEM_THREAD_INFORMATION or SYSTEM_EXTENDED_THREAD_INFORMATION structures follow.
    }

    struct _SYSTEM_THREAD_INFORMATION
    {
        long    KernelTime;
        long    UserTime;
        long    CreateTime;
        uint    WaitTime;
        void*   StartAddress;
        PTID    ProcessId;
        PTID    ThreadId;
        int     Priority;
        int     BasePriority;
        uint    ContextSwitches;
        uint    ThreadState;
        int     WaitReason;
        int     reserved;
    }

    alias fnNtQuerySystemInformation = extern(Windows)
    HRESULT function( uint SystemInformationClass, void* info, uint infoLength, uint* ReturnLength ) nothrow @nogc;

    enum ThreadBasicInformation = 0;

    struct THREAD_BASIC_INFORMATION
    {
        int    ExitStatus;
        void** TebBaseAddress;
        PTID   ProcessId;
        PTID   ThreadId;
        size_t AffinityMask;
        int    Priority;
        int    BasePriority;
    }

    alias fnNtQueryInformationThread = extern(Windows)
    int function( HANDLE ThreadHandle, uint ThreadInformationClass, void* buf, uint size, uint* ReturnLength ) nothrow @nogc;

    enum SYNCHRONIZE = 0x00100000;
    enum THREAD_GET_CONTEXT = 8;
    enum THREAD_QUERY_INFORMATION = 0x40;
    enum THREAD_SUSPEND_RESUME = 2;

    ///////////////////////////////////////////////////////////////////
    // get the thread environment block (TEB) of the thread with the given handle
    static void** getTEB( HANDLE hnd ) nothrow @nogc
    {
        HANDLE nthnd = GetModuleHandleA( "NTDLL" );
        assert( nthnd, "cannot get module handle for ntdll" );
        fnNtQueryInformationThread fn = cast(fnNtQueryInformationThread) GetProcAddress( nthnd, "NtQueryInformationThread" );
        assert( fn, "cannot find NtQueryInformationThread in ntdll" );

        THREAD_BASIC_INFORMATION tbi;
        int Status = (*fn)(hnd, ThreadBasicInformation, &tbi, tbi.sizeof, null);
        assert(Status == 0);

        return tbi.TebBaseAddress;
    }

    // get the thread environment block (TEB) of the thread with the given identifier
    static void** getTEB( uint id ) nothrow @nogc
    {
        HANDLE hnd = OpenThread( THREAD_QUERY_INFORMATION, FALSE, id );
        assert( hnd, "OpenThread failed" );

        void** teb = getTEB( hnd );
        CloseHandle( hnd );
        return teb;
    }

    // get linear address of TEB of current thread
    static void** getTEB() nothrow @nogc
    {
        version (D_InlineAsm_X86)
        {
            asm pure nothrow @nogc
            {
                naked;
                mov EAX,FS:[0x18];
                ret;
            }
        }
        else version (D_InlineAsm_X86_64)
        {
            asm pure nothrow @nogc
            {
                naked;
                mov RAX,0x30;
                mov RAX,GS:[RAX]; // immediate value causes fixup
                ret;
            }
        }
        else version (GNU_InlineAsm)
        {
            void** teb;
            version (X86)
                asm pure nothrow @nogc { "movl %%fs:0x18, %0;" : "=r" (teb); }
            else version (X86_64)
                asm pure nothrow @nogc { "movq %%gs:0x30, %0;" : "=r" (teb); }
            else
                static assert(false);
            return teb;
        }
        else
        {
            static assert(false);
        }
    }

    // get the stack bottom (the top address) of the thread with the given handle
    static void* getThreadStackBottom( HANDLE hnd ) nothrow @nogc
    {
        void** teb = getTEB( hnd );
        return teb[1];
    }

    // get the stack bottom (the top address) of the thread with the given identifier
    static void* getThreadStackBottom( uint id ) nothrow @nogc
    {
        void** teb = getTEB( id );
        return teb[1];
    }

    // create a thread handle with full access to the thread with the given identifier
    static HANDLE OpenThreadHandle( uint id ) nothrow @nogc
    {
        return OpenThread( SYNCHRONIZE|THREAD_GET_CONTEXT|THREAD_QUERY_INFORMATION|THREAD_SUSPEND_RESUME, FALSE, id );
    }

    ///////////////////////////////////////////////////////////////////
    // enumerate threads of the given process calling the passed function on each thread
    // using function instead of delegate here to avoid allocating closure
    static bool enumProcessThreads( uint procid, bool function( uint id, void* context ) dg, void* context )
    {
        HANDLE hnd = GetModuleHandleA( "NTDLL" );
        fnNtQuerySystemInformation fn = cast(fnNtQuerySystemInformation) GetProcAddress( hnd, "NtQuerySystemInformation" );
        if ( !fn )
            return false;

        uint sz = 16384;
        uint retLength;
        HRESULT rc;
        char* buf;
        for ( ; ; )
        {
            buf = cast(char*) core.stdc.stdlib.malloc(sz);
            if (!buf)
                return false;
            rc = fn( SystemProcessInformation, buf, sz, &retLength );
            if ( rc != STATUS_INFO_LENGTH_MISMATCH )
                break;
            core.stdc.stdlib.free( buf );
            sz *= 2;
        }
        scope(exit) core.stdc.stdlib.free( buf );

        if (rc != 0)
            return false;

        auto pinfo = cast(_SYSTEM_PROCESS_INFORMATION*) buf;
        auto pend  = cast(_SYSTEM_PROCESS_INFORMATION*) (buf + retLength);
        for ( ; pinfo < pend; )
        {
            if ( pinfo.ProcessId == procid )
            {
                auto tinfo = cast(_SYSTEM_THREAD_INFORMATION*)(pinfo + 1);
                for ( int i = 0; i < pinfo.NumberOfThreads; i++, tinfo++ )
                    if ( tinfo.ProcessId == procid )
                        if ( !dg( cast(uint) tinfo.ThreadId, context ) ) // IDs are actually DWORDs
                            return false;
            }
            if ( pinfo.NextEntryOffset == 0 )
                break;
            pinfo = cast(_SYSTEM_PROCESS_INFORMATION*) (cast(char*) pinfo + pinfo.NextEntryOffset);
        }
        return true;
    }

    static bool enumProcessThreads( bool function( uint id, void* context ) dg, void* context )
    {
        return enumProcessThreads( GetCurrentProcessId(), dg, context );
    }

    // execute function on the TLS for the given thread
    alias extern(C) void function() externCVoidFunc;
    static void impersonate_thread( uint id, externCVoidFunc fn )
    {
        impersonate_thread(id, () => fn());
    }

    static void impersonate_thread( uint id, scope void delegate() dg)
    {
        if ( id == GetCurrentThreadId() )
        {
            dg();
            return;
        }

        // temporarily set current TLS array pointer to the array pointer of the referenced thread
        void** curteb = getTEB();
        void** teb    = getTEB( id );
        assert( teb && curteb );

        void** curtlsarray = cast(void**) curteb[11];
        void** tlsarray    = cast(void**) teb[11];
        if ( !curtlsarray || !tlsarray )
            return;

        curteb[11] = tlsarray;

        // swap out the TLS slots aswell
        version (Win64)
        {
            enum TEB_offset_TlsSlots = 0x1480;
            enum TEB_offset_TlsExpansionSlots = 0x1780;
        }
        else
        {
            enum TEB_offset_TlsSlots = 0xE10;
            enum TEB_offset_TlsExpansionSlots = 0xF94;
        }
        void* tlsSlotsAdr(void** teb) { return cast(void*) teb + TEB_offset_TlsSlots; }
        ref void* tlsExpansionSlots(void** teb) { return *cast(void**)(cast(void*) teb + TEB_offset_TlsExpansionSlots); }

        import core.stdc.string;
        void*[64] slots = void;
        memcpy(slots.ptr, tlsSlotsAdr(curteb), slots.sizeof);
        void* extraSlots = tlsExpansionSlots(curteb);

        memcpy(tlsSlotsAdr(curteb), tlsSlotsAdr(teb), slots.sizeof);
        tlsExpansionSlots(curteb) = tlsExpansionSlots(teb);

        dg();

        curteb[11] = curtlsarray;

        // copy the TLS slots back in case they have been changed in dg
        memcpy(tlsSlotsAdr(teb), tlsSlotsAdr(curteb), slots.sizeof);
        tlsExpansionSlots(teb) = tlsExpansionSlots(curteb);

        memcpy(tlsSlotsAdr(curteb), slots.ptr, slots.sizeof);
        tlsExpansionSlots(curteb) = extraSlots;
    }
}

public:
// forward as few symbols as possible into the "global" name space
alias thread_aux.getTEB getTEB;
alias thread_aux.getThreadStackBottom getThreadStackBottom;
alias thread_aux.OpenThreadHandle OpenThreadHandle;
alias thread_aux.enumProcessThreads enumProcessThreads;
alias thread_aux.impersonate_thread impersonate_thread;

// get the start of the TLS memory of the thread with the given handle
void* GetTlsDataAddress( HANDLE hnd ) nothrow
{
    if ( void** teb = getTEB( hnd ) )
        if ( void** tlsarray = cast(void**) teb[11] )
            return tlsarray[_tls_index];
    return null;
}

// get the start of the TLS memory of the thread with the given identifier
void* GetTlsDataAddress( uint id ) nothrow
{
    HANDLE hnd = OpenThread( thread_aux.THREAD_QUERY_INFORMATION, FALSE, id );
    assert( hnd, "OpenThread failed" );

    void* tls = GetTlsDataAddress( hnd );
    CloseHandle( hnd );
    return tls;
}

///////////////////////////////////////////////////////////////////
// run rt_moduleTlsCtor in the context of the given thread
void thread_moduleTlsCtor( uint id )
{
    thread_aux.impersonate_thread(id, &rt_moduleTlsCtor);
}

///////////////////////////////////////////////////////////////////
// run rt_moduleTlsDtor in the context of the given thread
void thread_moduleTlsDtor( uint id )
{
    thread_aux.impersonate_thread(id, &rt_moduleTlsDtor);
}
