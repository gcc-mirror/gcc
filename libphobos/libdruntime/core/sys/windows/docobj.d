/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_docobj.d)
 */
module core.sys.windows.docobj;
version (Windows):

private import core.sys.windows.basetyps, core.sys.windows.oaidl, core.sys.windows.objidl, core.sys.windows.oleidl,
  core.sys.windows.unknwn, core.sys.windows.windef, core.sys.windows.wtypes;

// FIXME: remove inherited methods from interface definitions

enum {
    OLECMDERR_E_UNKNOWNGROUP = -2147221244,
    OLECMDERR_E_CANCELED     = -2147221245,
    OLECMDERR_E_NOHELP       = -2147221246,
    OLECMDERR_E_DISABLED     = -2147221247,
    OLECMDERR_E_NOTSUPPORTED = -2147221248
}

enum OLECMDID {
    OLECMDID_OPEN = 1,
    OLECMDID_NEW = 2,
    OLECMDID_SAVE = 3,
    OLECMDID_SAVEAS = 4,
    OLECMDID_SAVECOPYAS = 5,
    OLECMDID_PRINT = 6,
    OLECMDID_PRINTPREVIEW = 7,
    OLECMDID_PAGESETUP = 8,
    OLECMDID_SPELL = 9,
    OLECMDID_PROPERTIES = 10,
    OLECMDID_CUT = 11,
    OLECMDID_COPY = 12,
    OLECMDID_PASTE = 13,
    OLECMDID_PASTESPECIAL = 14,
    OLECMDID_UNDO = 15,
    OLECMDID_REDO = 16,
    OLECMDID_SELECTALL = 17,
    OLECMDID_CLEARSELECTION = 18,
    OLECMDID_ZOOM = 19,
    OLECMDID_GETZOOMRANGE = 20,
    OLECMDID_UPDATECOMMANDS = 21,
    OLECMDID_REFRESH = 22,
    OLECMDID_STOP = 23,
    OLECMDID_HIDETOOLBARS = 24,
    OLECMDID_SETPROGRESSMAX = 25,
    OLECMDID_SETPROGRESSPOS = 26,
    OLECMDID_SETPROGRESSTEXT = 27,
    OLECMDID_SETTITLE = 28,
    OLECMDID_SETDOWNLOADSTATE = 29,
    OLECMDID_STOPDOWNLOAD = 30
}

enum OLECMDF {
    OLECMDF_SUPPORTED = 1,
    OLECMDF_ENABLED = 2,
    OLECMDF_LATCHED = 4,
    OLECMDF_NINCHED = 8
}

enum OLECMDEXECOPT {
    OLECMDEXECOPT_DODEFAULT = 0,
    OLECMDEXECOPT_PROMPTUSER = 1,
    OLECMDEXECOPT_DONTPROMPTUSER = 2,
    OLECMDEXECOPT_SHOWHELP = 3
}

struct OLECMDTEXT {
    DWORD cmdtextf;
    ULONG cwActual;
    ULONG cwBuf;
    wchar[1] rgwz = 0;
}

struct OLECMD {
    ULONG cmdID;
    DWORD cmdf;
}

alias IOleInPlaceSite LPOLEINPLACESITE;
alias IEnumOleDocumentViews LPENUMOLEDOCUMENTVIEWS;

extern (C) extern const IID
    IID_IContinueCallback,
    IID_IEnumOleDocumentViews,
    IID_IPrint,
    IID_IOleDocumentView,
    IID_IOleDocument,
    IID_IOleCommandTarget,
    IID_IOleDocumentSite;


interface IOleDocumentView : IUnknown {
    HRESULT SetInPlaceSite(LPOLEINPLACESITE);
    HRESULT GetInPlaceSite(LPOLEINPLACESITE*);
    HRESULT GetDocument(IUnknown*);
    HRESULT SetRect(LPRECT);
    HRESULT GetRect(LPRECT);
    HRESULT SetRectComplex(LPRECT, LPRECT, LPRECT, LPRECT);
    HRESULT Show(BOOL);
    HRESULT UIActivate(BOOL);
    HRESULT Open();
    HRESULT Close(DWORD);
    HRESULT SaveViewState(IStream);
    HRESULT ApplyViewState(IStream);
    HRESULT Clone(LPOLEINPLACESITE, IOleDocumentView*);
}

interface IEnumOleDocumentViews : IUnknown {
      HRESULT Next(ULONG, IOleDocumentView, ULONG*);
      HRESULT Skip(ULONG);
      HRESULT Reset();
      HRESULT Clone(IEnumOleDocumentViews*);
}

interface IOleDocument : IUnknown {
    HRESULT CreateView(LPOLEINPLACESITE, IStream, DWORD, IOleDocumentView*);
    HRESULT GetDocMiscStatus(DWORD*);
    HRESULT EnumViews(LPENUMOLEDOCUMENTVIEWS*, IOleDocumentView*);
}

interface IOleCommandTarget : IUnknown {
    HRESULT QueryStatus(const(GUID)*, ULONG, OLECMD*, OLECMDTEXT*);
    HRESULT Exec(const(GUID)*, DWORD, DWORD, VARIANTARG*, VARIANTARG*);
}

interface IOleDocumentSite : IUnknown {
    HRESULT ActivateMe(IOleDocumentView);
}
