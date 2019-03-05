module core.sys.windows.com;
version (Windows):

pragma(lib,"uuid");

import core.atomic;
import core.sys.windows.windows;
//import std.string;

public import core.sys.windows.basetyps : GUID, IID, CLSID;

public import core.sys.windows.objbase :
    CLSCTX_INPROC, CLSCTX_ALL, CLSCTX_SERVER,
    COINIT,
    CoBuildVersion, StringFromGUID2,
    CoInitialize, CoInitializeEx, CoUninitialize, CoGetCurrentProcess,
    CoCreateInstance,
    CoFreeLibrary, CoFreeAllLibraries, CoFreeUnusedLibraries;

public import core.sys.windows.ole2ver : rmm, rup;

public import core.sys.windows.unknwn : IUnknown, IClassFactory;

public import core.sys.windows.winerror :
    S_OK,
    S_FALSE,
    NOERROR,
    E_NOTIMPL,
    E_NOINTERFACE,
    E_POINTER,
    E_ABORT,
    E_FAIL,
    E_HANDLE,
    CLASS_E_NOAGGREGATION,
    E_OUTOFMEMORY,
    E_INVALIDARG,
    E_UNEXPECTED,
    RPC_E_CHANGED_MODE;

public import core.sys.windows.wtypes :
    OLECHAR, LPOLESTR, LPCOLESTR;

alias CLSCTX_INPROC_SERVER     = core.sys.windows.wtypes.CLSCTX.CLSCTX_INPROC_SERVER    ;
alias CLSCTX_INPROC_HANDLER    = core.sys.windows.wtypes.CLSCTX.CLSCTX_INPROC_HANDLER   ;
alias CLSCTX_LOCAL_SERVER      = core.sys.windows.wtypes.CLSCTX.CLSCTX_LOCAL_SERVER     ;
alias CLSCTX_INPROC_SERVER16   = core.sys.windows.wtypes.CLSCTX.CLSCTX_INPROC_SERVER16  ;
alias CLSCTX_REMOTE_SERVER     = core.sys.windows.wtypes.CLSCTX.CLSCTX_REMOTE_SERVER    ;
alias CLSCTX_INPROC_HANDLER16  = core.sys.windows.wtypes.CLSCTX.CLSCTX_INPROC_HANDLER16 ;
alias CLSCTX_INPROC_SERVERX86  = core.sys.windows.wtypes.CLSCTX.CLSCTX_INPROC_SERVERX86 ;
alias CLSCTX_INPROC_HANDLERX86 = core.sys.windows.wtypes.CLSCTX.CLSCTX_INPROC_HANDLERX86;

alias COINIT_APARTMENTTHREADED   = COINIT.COINIT_APARTMENTTHREADED;
alias COINIT_MULTITHREADED       = COINIT.COINIT_MULTITHREADED    ;
alias COINIT_DISABLE_OLE1DDE     = COINIT.COINIT_DISABLE_OLE1DDE  ;
alias COINIT_SPEED_OVER_MEMORY   = COINIT.COINIT_SPEED_OVER_MEMORY;

public import core.sys.windows.uuid;

extern (System)
{

class ComObject : IUnknown
{
extern (System):
    HRESULT QueryInterface(const(IID)* riid, void** ppv)
    {
        if (*riid == IID_IUnknown)
        {
            *ppv = cast(void*)cast(IUnknown)this;
            AddRef();
            return S_OK;
        }
        else
        {   *ppv = null;
            return E_NOINTERFACE;
        }
    }

    ULONG AddRef()
    {
        return atomicOp!"+="(*cast(shared)&count, 1);
    }

    ULONG Release()
    {
        LONG lRef = atomicOp!"-="(*cast(shared)&count, 1);
        if (lRef == 0)
        {
            // free object

            // If we delete this object, then the postinvariant called upon
            // return from Release() will fail.
            // Just let the GC reap it.
            //delete this;

            return 0;
        }
        return cast(ULONG)lRef;
    }

    LONG count = 0;             // object reference count
}

}
