/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_richole.d)
 */
module core.sys.windows.richole;
version (Windows):

private import core.sys.windows.objfwd, core.sys.windows.objidl, core.sys.windows.ole2, core.sys.windows.unknwn,
  core.sys.windows.windef;
private import core.sys.windows.richedit; // for CHARRANGE

//align(4):

enum ULONG
    REO_GETOBJ_NO_INTERFACES = 0,
    REO_GETOBJ_POLEOBJ = 1,
    REO_GETOBJ_PSTG = 2,
    REO_GETOBJ_POLESITE = 4,
    REO_GETOBJ_ALL_INTERFACES = 7,
    REO_CP_SELECTION = -1,
    REO_IOB_SELECTION = -1,
    REO_IOB_USE_CP = -2,
    REO_NULL = 0,
    REO_READWRITEMASK = 0x3F,
    REO_DONTNEEDPALETTE = 32,
    REO_BLANK = 16,
    REO_DYNAMICSIZE = 8,
    REO_INVERTEDSELECT = 4,
    REO_BELOWBASELINE = 2,
    REO_RESIZABLE = 1,
    REO_LINK = 0x80000000,
    REO_STATIC = 0x40000000,
    REO_SELECTED = 0x08000000,
    REO_OPEN = 0x4000000,
    REO_INPLACEACTIVE = 0x2000000,
    REO_HILITED = 0x1000000,
    REO_LINKAVAILABLE = 0x800000,
    REO_GETMETAFILE = 0x400000;

enum {
    RECO_PASTE = 0,
    RECO_DROP,
    RECO_COPY,
    RECO_CUT,
    RECO_DRAG // = 4
}

extern (C) extern const GUID
    IID_IRichEditOle,
    IID_IRichEditOleCallback;

struct REOBJECT {
    DWORD           cbStruct = REOBJECT.sizeof;
    LONG            cp;
    CLSID           clsid;
    LPOLEOBJECT     poleobj;
    LPSTORAGE       pstg;
    LPOLECLIENTSITE polesite;
    SIZEL           sizel;
    DWORD           dvaspect;
    DWORD           dwFlags;
    DWORD           dwUser;
}

interface IRichEditOle : IUnknown {
    HRESULT GetClientSite(LPOLECLIENTSITE*);
    LONG GetObjectCount();
    LONG GetLinkCount();
    HRESULT GetObject(LONG, REOBJECT*, DWORD);
    HRESULT InsertObject(REOBJECT*);
    HRESULT ConvertObject(LONG, REFCLSID, LPCSTR);
    HRESULT ActivateAs(REFCLSID, REFCLSID);
    HRESULT SetHostNames(LPCSTR, LPCSTR);
    HRESULT SetLinkAvailable(LONG, BOOL);
    HRESULT SetDvaspect(LONG, DWORD);
    HRESULT HandsOffStorage(LONG);
    HRESULT SaveCompleted(LONG, LPSTORAGE);
    HRESULT InPlaceDeactivate();
    HRESULT ContextSensitiveHelp(BOOL);
    HRESULT GetClipboardData(CHARRANGE*, DWORD, LPDATAOBJECT*);
    HRESULT ImportDataObject(LPDATAOBJECT, CLIPFORMAT, HGLOBAL);
};
alias IRichEditOle LPRICHEDITOLE;

interface IRichEditOleCallback : IUnknown {
    HRESULT GetNewStorage(LPSTORAGE*);
    HRESULT GetInPlaceContext(LPOLEINPLACEFRAME*, LPOLEINPLACEUIWINDOW*, LPOLEINPLACEFRAMEINFO);
    HRESULT ShowContainerUI(BOOL);
    HRESULT QueryInsertObject(LPCLSID, LPSTORAGE, LONG);
    HRESULT DeleteObject(LPOLEOBJECT);
    HRESULT QueryAcceptData(LPDATAOBJECT, CLIPFORMAT*, DWORD, BOOL, HGLOBAL);
    HRESULT ContextSensitiveHelp(BOOL);
    HRESULT GetClipboardData(CHARRANGE*, DWORD, LPDATAOBJECT*);
    HRESULT GetDragDropEffect(BOOL, DWORD, PDWORD);
    HRESULT GetContextMenu(WORD, LPOLEOBJECT, CHARRANGE*, HMENU*);
};
alias IRichEditOleCallback LPRICHEDITOLECALLBACK;
