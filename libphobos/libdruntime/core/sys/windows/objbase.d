/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_objbase.d)
 */
module core.sys.windows.objbase;
version (Windows):
nothrow:
pragma(lib, "ole32");

import core.sys.windows.cguid, core.sys.windows.objidl, core.sys.windows.unknwn, core.sys.windows.wtypes;
import core.sys.windows.basetyps, core.sys.windows.objfwd, core.sys.windows.rpcdce, core.sys.windows.winbase,
  core.sys.windows.windef;

// DAC: Not needed for D?
//MACRO #define LISet32(li, v) ((li).HighPart=(v)<0?-1:0, (li).LowPart=(v))
//MACRO #define ULISet32(li, v) ((li).HighPart=0, (li).LowPart=(v))

enum CLSCTX_ALL    = CLSCTX.CLSCTX_INPROC_SERVER|CLSCTX.CLSCTX_INPROC_HANDLER|CLSCTX.CLSCTX_LOCAL_SERVER;
enum CLSCTX_INPROC = CLSCTX.CLSCTX_INPROC_SERVER|CLSCTX.CLSCTX_INPROC_HANDLER;
enum CLSCTX_SERVER = CLSCTX.CLSCTX_INPROC_SERVER|CLSCTX.CLSCTX_LOCAL_SERVER|CLSCTX.CLSCTX_REMOTE_SERVER;
enum MARSHALINTERFACE_MIN=500;
enum CWCSTORAGENAME=32;

enum STGM_DIRECT           = 0;
enum STGM_TRANSACTED       = 0x10000L;
enum STGM_SIMPLE           = 0x8000000L;
enum STGM_READ             = 0;
enum STGM_WRITE            = 1;
enum STGM_READWRITE        = 2;
enum STGM_SHARE_DENY_NONE  = 0x40;
enum STGM_SHARE_DENY_READ  = 0x30;
enum STGM_SHARE_DENY_WRITE = 0x20;
enum STGM_SHARE_EXCLUSIVE  = 0x10;
enum STGM_PRIORITY         = 0x40000L;
enum STGM_DELETEONRELEASE  = 0x4000000;
enum STGM_NOSCRATCH        = 0x100000;
enum STGM_CREATE           = 0x1000;
enum STGM_CONVERT          = 0x20000;
enum STGM_NOSNAPSHOT       = 0x200000;
enum STGM_FAILIFTHERE      = 0;

enum ASYNC_MODE_COMPATIBILITY = 1;
enum ASYNC_MODE_DEFAULT       = 0;

enum STGTY_REPEAT = 256;
enum STG_TOEND = 0xFFFFFFFF;
enum STG_LAYOUT_SEQUENTIAL  = 0;
enum STG_LAYOUT_INTERLEAVED = 1;

enum COM_RIGHTS_EXECUTE            = 1;
enum COM_RIGHTS_SAFE_FOR_SCRIPTING = 2;

enum STGOPTIONS_VERSION = 2;

enum STGFMT {
    STGFMT_STORAGE = 0,
    STGFMT_FILE = 3,
    STGFMT_ANY = 4,
    STGFMT_DOCFILE = 5
}

struct STGOPTIONS {
    USHORT usVersion;
    USHORT reserved;
    ULONG ulSectorSize;
const(WCHAR)* pwcsTemplateFile;
}

enum REGCLS {
    REGCLS_SINGLEUSE = 0,
    REGCLS_MULTIPLEUSE = 1,
    REGCLS_MULTI_SEPARATE = 2
}

/*
BOOL IsEqualGUID(GUID rguid1, GUID rguid2) {
    return rguid1 == rguid2;
}
*/

extern (Windows) BOOL IsEqualGUID(
  REFGUID rguid1,
  REFGUID rguid2
);

alias IsEqualGUID IsEqualIID;
alias IsEqualGUID IsEqualCLSID;

enum COINIT {
    COINIT_APARTMENTTHREADED = 2,
    COINIT_MULTITHREADED     = 0,
    COINIT_DISABLE_OLE1DDE   = 4,
    COINIT_SPEED_OVER_MEMORY = 8
}

enum STDMSHLFLAGS {
    SMEXF_SERVER  = 1,
    SMEXF_HANDLER
}

extern(Windows) {
    alias HRESULT function(REFCLSID, REFIID, PVOID*) LPFNGETCLASSOBJECT;
    alias HRESULT function() LPFNCANUNLOADNOW;

    DWORD CoBuildVersion();
    HRESULT CoInitialize(PVOID);
    HRESULT CoInitializeEx(LPVOID, DWORD);
    void CoUninitialize();
    HRESULT CoGetMalloc(DWORD, LPMALLOC*);
    DWORD CoGetCurrentProcess();
    HRESULT CoRegisterMallocSpy(LPMALLOCSPY);
    HRESULT CoRevokeMallocSpy();
    HRESULT CoCreateStandardMalloc(DWORD, IMalloc*);
    //#ifdef DBG
    ULONG DebugCoGetRpcFault();
    void DebugCoSetRpcFault(ULONG);
    //#endif
    HRESULT CoGetClassObject(REFCLSID, DWORD, COSERVERINFO*, REFIID, PVOID*);
    HRESULT CoRegisterClassObject(REFCLSID, LPUNKNOWN, DWORD, DWORD, PDWORD);
    HRESULT CoRevokeClassObject(DWORD);
    HRESULT CoGetMarshalSizeMax(ULONG*, REFIID, LPUNKNOWN, DWORD, PVOID, DWORD);
    HRESULT CoMarshalInterface(LPSTREAM, REFIID, LPUNKNOWN, DWORD, PVOID, DWORD);
    HRESULT CoUnmarshalInterface(LPSTREAM, REFIID, PVOID*);
    HRESULT CoMarshalHresult(LPSTREAM, HRESULT);
    HRESULT CoUnmarshalHresult(LPSTREAM, HRESULT*);
    HRESULT CoReleaseMarshalData(LPSTREAM);
    HRESULT CoDisconnectObject(LPUNKNOWN, DWORD);
    HRESULT CoLockObjectExternal(LPUNKNOWN, BOOL, BOOL);
    HRESULT CoGetStandardMarshal(REFIID, LPUNKNOWN, DWORD, PVOID, DWORD, LPMARSHAL*);
    HRESULT CoGetStdMarshalEx(LPUNKNOWN, DWORD, LPUNKNOWN*);
    BOOL CoIsHandlerConnected(LPUNKNOWN);
    BOOL CoHasStrongExternalConnections(LPUNKNOWN);
    HRESULT CoMarshalInterThreadInterfaceInStream(REFIID, LPUNKNOWN, LPSTREAM*);
    HRESULT CoGetInterfaceAndReleaseStream(LPSTREAM, REFIID, PVOID*);
    HRESULT CoCreateFreeThreadedMarshaler(LPUNKNOWN, LPUNKNOWN*);
    HINSTANCE CoLoadLibrary(LPOLESTR, BOOL);
    void CoFreeLibrary(HINSTANCE);
    void CoFreeAllLibraries();
    void CoFreeUnusedLibraries();
    HRESULT CoCreateInstance(REFCLSID, LPUNKNOWN, DWORD, REFIID, PVOID*);
    HRESULT CoCreateInstanceEx(REFCLSID, IUnknown, DWORD, COSERVERINFO*, DWORD, MULTI_QI*);
    HRESULT StringFromCLSID(REFCLSID, LPOLESTR*);
    HRESULT CLSIDFromString(LPOLESTR, LPCLSID);
    HRESULT StringFromIID(REFIID, LPOLESTR*);
    HRESULT IIDFromString(LPOLESTR, LPIID);
    BOOL CoIsOle1Class(REFCLSID);
    HRESULT ProgIDFromCLSID(REFCLSID, LPOLESTR*);
    HRESULT CLSIDFromProgID(LPCOLESTR, LPCLSID);
    int StringFromGUID2(REFGUID, LPOLESTR, int);
    HRESULT CoCreateGuid(GUID*);
    BOOL CoFileTimeToDosDateTime(FILETIME*, LPWORD, LPWORD);
    BOOL CoDosDateTimeToFileTime(WORD, WORD, FILETIME*);
    HRESULT CoFileTimeNow(FILETIME*);
    HRESULT CoRegisterMessageFilter(LPMESSAGEFILTER, LPMESSAGEFILTER*);
    HRESULT CoGetTreatAsClass(REFCLSID, LPCLSID);
    HRESULT CoTreatAsClass(REFCLSID, REFCLSID);
    HRESULT DllGetClassObject(REFCLSID, REFIID, PVOID*);
    HRESULT DllCanUnloadNow();
    PVOID CoTaskMemAlloc(SIZE_T);
    PVOID CoTaskMemRealloc(PVOID, SIZE_T);
    void CoTaskMemFree(PVOID);
    HRESULT CreateDataAdviseHolder(LPDATAADVISEHOLDER*);
    HRESULT CreateDataCache(LPUNKNOWN, REFCLSID, REFIID, PVOID*);
    HRESULT StgCreateDocfile(const(OLECHAR)*, DWORD, DWORD, IStorage*);
    HRESULT StgCreateDocfileOnILockBytes(ILockBytes, DWORD, DWORD, IStorage*);
    HRESULT StgOpenStorage(const(OLECHAR)*, IStorage, DWORD, SNB, DWORD, IStorage*);
    HRESULT StgOpenStorageOnILockBytes(ILockBytes, IStorage, DWORD, SNB, DWORD, IStorage*);
    HRESULT StgIsStorageFile(const(OLECHAR)*);
    HRESULT StgIsStorageILockBytes(ILockBytes);
    HRESULT StgSetTimes(OLECHAR *, FILETIME *, FILETIME *, FILETIME *);
    HRESULT StgCreateStorageEx(const(WCHAR)*, DWORD, DWORD, DWORD, STGOPTIONS*, void*, REFIID, void**);
    HRESULT StgOpenStorageEx(const(WCHAR)*, DWORD, DWORD, DWORD, STGOPTIONS*, void*, REFIID, void**);
    HRESULT BindMoniker(LPMONIKER, DWORD, REFIID, PVOID*);
    HRESULT CoGetObject(LPCWSTR, BIND_OPTS*, REFIID, void**);
    HRESULT MkParseDisplayName(LPBC, LPCOLESTR, ULONG*, LPMONIKER*);
    HRESULT MonikerRelativePathTo(LPMONIKER, LPMONIKER, LPMONIKER*, BOOL);
    HRESULT MonikerCommonPrefixWith(LPMONIKER, LPMONIKER, LPMONIKER*);
    HRESULT CreateBindCtx(DWORD, LPBC*);
    HRESULT CreateGenericComposite(LPMONIKER, LPMONIKER, LPMONIKER*);
    HRESULT GetClassFile (LPCOLESTR, CLSID*);
    HRESULT CreateFileMoniker(LPCOLESTR, LPMONIKER*);
    HRESULT CreateItemMoniker(LPCOLESTR, LPCOLESTR, LPMONIKER*);
    HRESULT CreateAntiMoniker(LPMONIKER*);
    HRESULT CreatePointerMoniker(LPUNKNOWN, LPMONIKER*);
    HRESULT GetRunningObjectTable(DWORD, LPRUNNINGOBJECTTABLE*);
    HRESULT CoInitializeSecurity(PSECURITY_DESCRIPTOR, LONG, SOLE_AUTHENTICATION_SERVICE*, void*, DWORD, DWORD, void*, DWORD, void*);
    HRESULT CoGetCallContext(REFIID, void**);
    HRESULT CoQueryProxyBlanket(IUnknown*, DWORD*, DWORD*, OLECHAR**, DWORD*, DWORD*, RPC_AUTH_IDENTITY_HANDLE*, DWORD*);
    HRESULT CoSetProxyBlanket(IUnknown*, DWORD, DWORD, OLECHAR*, DWORD, DWORD, RPC_AUTH_IDENTITY_HANDLE, DWORD);
    HRESULT CoCopyProxy(IUnknown*, IUnknown**);
    HRESULT CoQueryClientBlanket(DWORD*, DWORD*, OLECHAR**, DWORD*, DWORD*, RPC_AUTHZ_HANDLE*, DWORD*);
    HRESULT CoImpersonateClient();
    HRESULT CoRevertToSelf();
    HRESULT CoQueryAuthenticationServices(DWORD*, SOLE_AUTHENTICATION_SERVICE**);
    HRESULT CoSwitchCallContext(IUnknown*, IUnknown**);
    HRESULT CoGetInstanceFromFile(COSERVERINFO*, CLSID*, IUnknown*, DWORD, DWORD, OLECHAR*, DWORD, MULTI_QI*);
    HRESULT CoGetInstanceFromIStorage(COSERVERINFO*, CLSID*, IUnknown*, DWORD, IStorage*, DWORD, MULTI_QI*);
    ULONG CoAddRefServerProcess();
    ULONG CoReleaseServerProcess();
    HRESULT CoResumeClassObjects();
    HRESULT CoSuspendClassObjects();
    HRESULT CoGetPSClsid(REFIID, CLSID*);
    HRESULT CoRegisterPSClsid(REFIID, REFCLSID);
}
