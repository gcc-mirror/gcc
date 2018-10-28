/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_unknwn.d)
 */
module core.sys.windows.unknwn;
version (Windows):

import core.sys.windows.objfwd, core.sys.windows.windef, core.sys.windows.wtypes;
private import core.sys.windows.basetyps;

extern (Windows) {
    void* MIDL_user_allocate(size_t);
    void MIDL_user_free(void*);
}


extern (Windows) {

    interface IUnknown {
        HRESULT QueryInterface(IID* riid, void** pvObject);
        ULONG AddRef();
        ULONG Release();
    }

    alias IUnknown LPUNKNOWN;

    interface IClassFactory : IUnknown {
        HRESULT CreateInstance(IUnknown UnkOuter, IID* riid, void** pvObject);
        HRESULT LockServer(BOOL fLock);
    }
    alias IClassFactory LPCLASSFACTORY;

    /+
    // These do not seem to be necessary (or desirable) for D.
    HRESULT IUnknown_QueryInterface_Proxy(IUnknown,REFIID,void**);
    ULONG IUnknown_AddRef_Proxy(IUnknown);
    ULONG IUnknown_Release_Proxy(IUnknown);
    HRESULT IClassFactory_RemoteCreateInstance_Proxy(IClassFactory,REFIID,IUnknown*);
    HRESULT IClassFactory_RemoteLockServer_Proxy(IClassFactory,BOOL);
    HRESULT IClassFactory_CreateInstance_Proxy(IClassFactory,IUnknown,REFIID,void**);
    HRESULT IClassFactory_CreateInstance_Stub(IClassFactory,REFIID,IUnknown*);
    HRESULT IClassFactory_LockServer_Proxy(IClassFactory,BOOL);
    HRESULT IClassFactory_LockServer_Stub(IClassFactory,BOOL);

    void IUnknown_QueryInterface_Stub(LPRPCSTUBBUFFER,LPRPCCHANNELBUFFER,PRPC_MESSAGE,PDWORD);
    void IUnknown_AddRef_Stub(LPRPCSTUBBUFFER,LPRPCCHANNELBUFFER,PRPC_MESSAGE,PDWORD);
    void IUnknown_Release_Stub(LPRPCSTUBBUFFER,LPRPCCHANNELBUFFER,PRPC_MESSAGE,PDWORD);
    void IClassFactory_RemoteCreateInstance_Stub(LPRPCSTUBBUFFER,LPRPCCHANNELBUFFER,PRPC_MESSAGE,PDWORD);
    void IClassFactory_RemoteLockServer_Stub(LPRPCSTUBBUFFER,LPRPCCHANNELBUFFER,PRPC_MESSAGE,PDWORD);
    +/
}
