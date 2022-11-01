/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_oaidl.d)
 */
module core.sys.windows.oaidl;
version (Windows):

import core.sys.windows.basetyps, core.sys.windows.unknwn, core.sys.windows.windef, core.sys.windows.wtypes;

enum DISPID_UNKNOWN = -1;
enum DISPID_VALUE = 0;
enum DISPID_PROPERTYPUT = -3;
enum DISPID_NEWENUM = -4;
enum DISPID_EVALUATE = -5;
enum DISPID_CONSTRUCTOR = -6;
enum DISPID_DESTRUCTOR = -7;
enum DISPID_COLLECT = -8;

enum FADF_AUTO = 1;
enum FADF_STATIC = 2;
enum FADF_EMBEDDED = 4;
enum FADF_FIXEDSIZE = 16;
enum FADF_RECORD = 32;
enum FADF_HAVEIID = 64;
enum FADF_HAVEVARTYPE = 128;
enum FADF_BSTR = 256;
enum FADF_UNKNOWN = 512;
enum FADF_DISPATCH = 1024;
enum FADF_VARIANT = 2048;
enum FADF_RESERVED = 0xf0e8;
enum FADF_DATADELETED = 0x1000;
enum FADF_CREATEVECTOR = 0x2000;

enum PARAMFLAG_NONE = 0;
enum PARAMFLAG_FIN = 1;
enum PARAMFLAG_FOUT = 2;
enum PARAMFLAG_FLCID = 4;
enum PARAMFLAG_FRETVAL = 8;
enum PARAMFLAG_FOPT = 16;
enum PARAMFLAG_FHASDEFAULT = 32;
enum PARAMFLAG_FHASCUSTDATA = 64;

enum IDLFLAG_NONE = PARAMFLAG_NONE;
enum IDLFLAG_FIN = PARAMFLAG_FIN;
enum IDLFLAG_FOUT = PARAMFLAG_FOUT;
enum IDLFLAG_FLCID = PARAMFLAG_FLCID;
enum IDLFLAG_FRETVAL = PARAMFLAG_FRETVAL;

enum IMPLTYPEFLAG_FDEFAULT       = 1;
enum IMPLTYPEFLAG_FSOURCE        = 2;
enum IMPLTYPEFLAG_FRESTRICTED    = 4;
enum IMPLTYPEFLAG_FDEFAULTVTABLE = 8;


enum SYSKIND {
    SYS_WIN16,
    SYS_WIN32,
    SYS_MAC
}

enum LIBFLAGS {
    LIBFLAG_FRESTRICTED   = 1,
    LIBFLAG_FCONTROL      = 2,
    LIBFLAG_FHIDDEN       = 4,
    LIBFLAG_FHASDISKIMAGE = 8
}

struct TLIBATTR {
    GUID guid;
    LCID lcid;
    SYSKIND syskind;
    WORD wMajorVerNum;
    WORD wMinorVerNum;
    WORD wLibFlags;
}
alias TLIBATTR* LPTLIBATTR;

alias CY CURRENCY;

struct SAFEARRAYBOUND {
    ULONG cElements;
    LONG lLbound;
}
alias SAFEARRAYBOUND* LPSAFEARRAYBOUND;

struct SAFEARR_BSTR {
    ULONG Size;
    wireBSTR* aBstr;
}

struct SAFEARR_UNKNOWN {
    ULONG Size;
    IUnknown* apUnknown;
}

struct SAFEARR_DISPATCH {
    ULONG Size;
    LPDISPATCH* apDispatch;
}

struct SAFEARR_VARIANT {
    ULONG Size;
    _wireVARIANT* aVariant;
}

enum SF_TYPE {
    SF_ERROR=VARENUM.VT_ERROR,
    SF_I1=VARENUM.VT_I1,
    SF_I2=VARENUM.VT_I2,
    SF_I4=VARENUM.VT_I4,
    SF_I8=VARENUM.VT_I8,
    SF_BSTR=VARENUM.VT_BSTR,
    SF_UNKNOWN=VARENUM.VT_UNKNOWN,
    SF_DISPATCH=VARENUM.VT_DISPATCH,
    SF_VARIANT=VARENUM.VT_VARIANT
}

struct _wireBRECORD {
    ULONG fFlags;
    ULONG clSize;
    LPRECORDINFO* pRecInfo;
    byte* pRecord;
}
alias _wireBRECORD* wireBRECORD;

struct SAFEARR_BRECORD {
    ULONG Size;
    wireBRECORD* aRecord;
}

struct SAFEARR_HAVEIID {
    ULONG Size;
    IUnknown* apUnknown;
    IID iid;
}

struct SAFEARRAYUNION {
    ULONG sfType;
    union _u {
        SAFEARR_BSTR BstrStr;
        SAFEARR_UNKNOWN UnknownStr;
        SAFEARR_DISPATCH DispatchStr;
        SAFEARR_VARIANT VariantStr;
        SAFEARR_BRECORD RecordStr;
        SAFEARR_HAVEIID HaveIidStr;
        BYTE_SIZEDARR ByteStr;
        WORD_SIZEDARR WordStr;
        DWORD_SIZEDARR LongStr;
        HYPER_SIZEDARR HyperStr;
    }
    _u u;
}

struct _wireSAFEARRAY {
    USHORT cDims;
    USHORT fFeatures;
    ULONG cbElements;
    ULONG cLocks;
    SAFEARRAYUNION uArrayStructs;
    SAFEARRAYBOUND[1] rgsabound;
}
alias _wireSAFEARRAY* wireSAFEARRAY;

alias wireSAFEARRAY* wirePSAFEARRAY;

struct SAFEARRAY {
    USHORT cDims;
    USHORT fFeatures;
    ULONG cbElements;
    ULONG cLocks;
    PVOID pvData;
    SAFEARRAYBOUND[1] rgsabound;
}
alias SAFEARRAY* LPSAFEARRAY;

struct VARIANT {
    union {
        struct {
            VARTYPE vt;
            WORD wReserved1;
            WORD wReserved2;
            WORD wReserved3;
            union {
                int lVal;
                LONGLONG llVal;
                ubyte bVal;
                short iVal;
                float fltVal;
                double dblVal;
                VARIANT_BOOL  boolVal;
                SCODE scode;
                CY cyVal;
                DATE date;
                BSTR bstrVal;
                IUnknown punkVal;
                IDispatch pdispVal;
                SAFEARRAY* parray;
                ubyte* pbVal;
                short* piVal;
                int* plVal;
                LONGLONG* pllVal;
                float* pfltVal;
                double* pdblVal;
                VARIANT_BOOL* pboolVal;
                _VARIANT_BOOL*  pbool;
                SCODE* pscode;
                CY* pcyVal;
                DATE* pdate;
                BSTR* pbstrVal;
                IUnknown* ppunkVal;
                IDispatch* ppdispVal;
                SAFEARRAY** pparray;
                VARIANT* pvarVal;
                void* byref;
                CHAR cVal;
                USHORT uiVal;
                ULONG ulVal;
                ULONGLONG ullVal;
                INT intVal;
                UINT uintVal;
                DECIMAL* pdecVal;
                CHAR*  pcVal;
                USHORT*  puiVal;
                ULONG*  pulVal;
                ULONGLONG* pullVal;
                INT*  pintVal;
                UINT*  puintVal;
                struct {
                    PVOID pvRecord;
                    IRecordInfo pRecInfo;
                }
            }
        }
        DECIMAL decVal;
    }
}
alias VARIANT* LPVARIANT;

alias VARIANT VARIANTARG;
alias VARIANT* LPVARIANTARG;

struct _wireVARIANT {
    DWORD clSize;
    DWORD rpcReserved;
    USHORT vt;
    USHORT wReserved1;
    USHORT wReserved2;
    USHORT wReserved3;
    union {
        LONG lVal;
        LONGLONG llVal;
        BYTE bVal;
        SHORT iVal;
        FLOAT fltVal;
        DOUBLE dblVal;
        VARIANT_BOOL boolVal;
        SCODE scode;
        CY cyVal;
        DATE date;
        wireBSTR bstrVal;
        IUnknown punkVal;
        LPDISPATCH pdispVal;
        wirePSAFEARRAY parray;
        wireBRECORD brecVal;
        BYTE* pbVal;
        SHORT* piVal;
        LONG* plVal;
        LONGLONG* pllVal;
        FLOAT* pfltVal;
        DOUBLE* pdblVal;
        VARIANT_BOOL* pboolVal;
        SCODE* pscode;
        CY* pcyVal;
        DATE* pdate;
        wireBSTR* pbstrVal;
        IUnknown* ppunkVal;
        LPDISPATCH* ppdispVal;
        wirePSAFEARRAY* pparray;
        wireVARIANT* pvarVal;
        CHAR cVal;
        USHORT uiVal;
        ULONG ulVal;
        ULONGLONG ullVal;
        INT intVal;
        UINT uintVal;
        DECIMAL decVal;
        DECIMAL* pdecVal;
        CHAR* pcVal;
        USHORT* puiVal;
        ULONG* pulVal;
        ULONGLONG* pullVal;
        INT* pintVal;
        UINT* puintVal;
    }
}
alias _wireVARIANT* wireVARIANT;

alias LONG DISPID;
alias DISPID MEMBERID;
alias DWORD HREFTYPE;

enum TYPEKIND {
    TKIND_ENUM, TKIND_RECORD, TKIND_MODULE, TKIND_INTERFACE, TKIND_DISPATCH,
    TKIND_COCLASS, TKIND_ALIAS, TKIND_UNION, TKIND_MAX
}

struct TYPEDESC {
    union {
        TYPEDESC* lptdesc;
        ARRAYDESC* lpadesc;
        HREFTYPE hreftype;
    }
    VARTYPE vt;
}

struct ARRAYDESC {
    TYPEDESC tdescElem;
    USHORT cDims;
    SAFEARRAYBOUND[1] rgbounds;
}

struct PARAMDESCEX {
    ULONG cBytes;
    VARIANTARG varDefaultValue;
}
alias PARAMDESCEX* LPPARAMDESCEX;

struct PARAMDESC {
    LPPARAMDESCEX pparamdescex;
    USHORT wParamFlags;
}
alias PARAMDESC* LPPARAMDESC;

struct IDLDESC {
    ULONG_PTR dwReserved;
    USHORT wIDLFlags;
}
alias IDLDESC* LPIDLDESC;

struct ELEMDESC {
    TYPEDESC tdesc;
    union {
        IDLDESC idldesc;
        PARAMDESC paramdesc;
    }
}
alias ELEMDESC* LPELEMDESC;

struct TYPEATTR {
    GUID guid;
    LCID lcid;
    DWORD dwReserved;
    MEMBERID memidConstructor;
    MEMBERID memidDestructor;
    LPOLESTR lpstrSchema;
    ULONG cbSizeInstance;
    TYPEKIND typekind;
    WORD cFuncs;
    WORD cVars;
    WORD cImplTypes;
    WORD cbSizeVft;
    WORD cbAlignment;
    WORD wTypeFlags;
    WORD wMajorVerNum;
    WORD wMinorVerNum;
    TYPEDESC tdescAlias;
    IDLDESC idldescType;
}
alias TYPEATTR* LPTYPEATTR;

struct DISPPARAMS {
    VARIANTARG* rgvarg;
    DISPID* rgdispidNamedArgs;
    UINT cArgs;
    UINT cNamedArgs;
}

struct EXCEPINFO {
    WORD wCode;
    WORD wReserved;
    BSTR bstrSource;
    BSTR bstrDescription;
    BSTR bstrHelpFile;
    DWORD dwHelpContext;
    PVOID pvReserved;
    extern (Windows) {
    HRESULT function (EXCEPINFO* ) pfnDeferredFillIn;
    }
    SCODE scode;
}
alias EXCEPINFO* LPEXCEPINFO;

enum CALLCONV {
    CC_FASTCALL,
    CC_CDECL,
    CC_MSCPASCAL,
    CC_PASCAL=CC_MSCPASCAL,
    CC_MACPASCAL,
    CC_STDCALL,
    CC_FPFASTCALL,
    CC_SYSCALL,
    CC_MPWCDECL,
    CC_MPWPASCAL,
    CC_MAX=CC_MPWPASCAL
}

enum FUNCKIND {
    FUNC_VIRTUAL,
    FUNC_PUREVIRTUAL,
    FUNC_NONVIRTUAL,
    FUNC_STATIC,
    FUNC_DISPATCH
}

enum INVOKEKIND {
    INVOKE_FUNC           = 1,
    INVOKE_PROPERTYGET    = 2,
    INVOKE_PROPERTYPUT    = 4,
    INVOKE_PROPERTYPUTREF = 8
}

struct FUNCDESC {
    MEMBERID memid;
    SCODE* lprgscode;
    ELEMDESC* lprgelemdescParam;
    FUNCKIND funckind;
    INVOKEKIND invkind;
    CALLCONV callconv;
    SHORT cParams;
    SHORT cParamsOpt;
    SHORT oVft;
    SHORT cScodes;
    ELEMDESC elemdescFunc;
    WORD wFuncFlags;
}
alias FUNCDESC* LPFUNCDESC;

enum VARKIND {
    VAR_PERINSTANCE, VAR_STATIC, VAR_CONST, VAR_DISPATCH
}

struct VARDESC {
    MEMBERID memid;
    LPOLESTR lpstrSchema;
    union {
        ULONG oInst;
        VARIANT* lpvarValue;
    }
    ELEMDESC elemdescVar;
    WORD wVarFlags;
    VARKIND varkind;
}
alias VARDESC* LPVARDESC;

enum TYPEFLAGS {
    TYPEFLAG_FAPPOBJECT     = 1,
    TYPEFLAG_FCANCREATE     = 2,
    TYPEFLAG_FLICENSED      = 4,
    TYPEFLAG_FPREDECLID     = 8,
    TYPEFLAG_FHIDDEN        = 16,
    TYPEFLAG_FCONTROL       = 32,
    TYPEFLAG_FDUAL          = 64,
    TYPEFLAG_FNONEXTENSIBLE = 128,
    TYPEFLAG_FOLEAUTOMATION = 256,
    TYPEFLAG_FRESTRICTED    = 512,
    TYPEFLAG_FAGGREGATABLE  = 1024,
    TYPEFLAG_FREPLACEABLE   = 2048,
    TYPEFLAG_FDISPATCHABLE  = 4096,
    TYPEFLAG_FREVERSEBIND   = 8192
}

enum FUNCFLAGS {
    FUNCFLAG_FRESTRICTED = 1,
    FUNCFLAG_FSOURCE = 2,
    FUNCFLAG_FBINDABLE = 4,
    FUNCFLAG_FREQUESTEDIT = 8,
    FUNCFLAG_FDISPLAYBIND = 16,
    FUNCFLAG_FDEFAULTBIND = 32,
    FUNCFLAG_FHIDDEN = 64,
    FUNCFLAG_FUSESGETLASTERROR = 128,
    FUNCFLAG_FDEFAULTCOLLELEM = 256,
    FUNCFLAG_FUIDEFAULT = 512,
    FUNCFLAG_FNONBROWSABLE = 1024,
    FUNCFLAG_FREPLACEABLE = 2048,
    FUNCFLAG_FIMMEDIATEBIND = 4096
}

enum VARFLAGS {
    VARFLAG_FREADONLY = 1,
    VARFLAG_FSOURCE = 2,
    VARFLAG_FBINDABLE = 4,
    VARFLAG_FREQUESTEDIT = 8,
    VARFLAG_FDISPLAYBIND = 16,
    VARFLAG_FDEFAULTBIND = 32,
    VARFLAG_FHIDDEN = 64,
    VARFLAG_FRESTRICTED = 128,
    VARFLAG_FDEFAULTCOLLELEM = 256,
    VARFLAG_FUIDEFAULT = 512,
    VARFLAG_FNONBROWSABLE = 1024,
    VARFLAG_FREPLACEABLE = 2048,
    VARFLAG_FIMMEDIATEBIND = 4096
}

struct CLEANLOCALSTORAGE {
    IUnknown pInterface;
    PVOID pStorage;
    DWORD flags;
}

struct CUSTDATAITEM {
    GUID guid;
    VARIANTARG varValue;
}
alias CUSTDATAITEM* LPCUSTDATAITEM;

struct CUSTDATA {
    DWORD cCustData;
    LPCUSTDATAITEM prgCustData;
}
alias CUSTDATA* LPCUSTDATA;

enum DESCKIND {
    DESCKIND_NONE = 0,
    DESCKIND_FUNCDESC = DESCKIND_NONE+1,
    DESCKIND_VARDESC = DESCKIND_FUNCDESC+1,
    DESCKIND_TYPECOMP = DESCKIND_VARDESC+1,
    DESCKIND_IMPLICITAPPOBJ = DESCKIND_TYPECOMP+1,
    DESCKIND_MAX = DESCKIND_IMPLICITAPPOBJ+1
}

union BINDPTR {
    LPFUNCDESC lpfuncdesc;
    LPVARDESC lpvardesc;
    LPTYPECOMP lptcomp;
}
alias BINDPTR* LPBINDPTR;

interface IDispatch : IUnknown {
    HRESULT GetTypeInfoCount(UINT*);
    HRESULT GetTypeInfo(UINT, LCID, LPTYPEINFO*);
    HRESULT GetIDsOfNames(REFIID, LPOLESTR*, UINT, LCID, DISPID*);
    HRESULT Invoke(DISPID, REFIID, LCID, WORD, DISPPARAMS*, VARIANT*, EXCEPINFO*, UINT*);
}
alias IDispatch LPDISPATCH;

interface IEnumVARIANT : IUnknown {
    HRESULT Next(ULONG, VARIANT*, ULONG*);
    HRESULT Skip(ULONG);
    HRESULT Reset();
    HRESULT Clone(IEnumVARIANT*);
}
alias IEnumVARIANT LPENUMVARIANT;

interface ITypeComp : IUnknown {
    HRESULT Bind(LPOLESTR, ULONG, WORD, LPTYPEINFO*, DESCKIND*, LPBINDPTR);
    HRESULT BindType(LPOLESTR, ULONG, LPTYPEINFO*, LPTYPECOMP*);
}
alias ITypeComp LPTYPECOMP;

interface ITypeInfo : IUnknown {
    HRESULT GetTypeAttr(LPTYPEATTR*);
    HRESULT GetTypeComp(LPTYPECOMP*);
    HRESULT GetFuncDesc(UINT, LPFUNCDESC*);
    HRESULT GetVarDesc(UINT, LPVARDESC*);
    HRESULT GetNames(MEMBERID, BSTR*, UINT, UINT*);
    HRESULT GetRefTypeOfImplType(UINT, HREFTYPE*);
    HRESULT GetImplTypeFlags(UINT, INT*);
    HRESULT GetIDsOfNames(LPOLESTR*, UINT, MEMBERID*);
    HRESULT Invoke(PVOID, MEMBERID, WORD, DISPPARAMS*, VARIANT*, EXCEPINFO*,
      UINT*);
    HRESULT GetDocumentation(MEMBERID, BSTR*, BSTR*, DWORD*, BSTR*);
    HRESULT GetDllEntry(MEMBERID, INVOKEKIND, BSTR*, BSTR*, WORD*);
    HRESULT GetRefTypeInfo(HREFTYPE, LPTYPEINFO*);
    HRESULT AddressOfMember(MEMBERID, INVOKEKIND, PVOID*);
    HRESULT CreateInstance(LPUNKNOWN, REFIID, PVOID*);
    HRESULT GetMops(MEMBERID, BSTR*);
    HRESULT GetContainingTypeLib(LPTYPELIB*, UINT*);
    void ReleaseTypeAttr(LPTYPEATTR);
    void ReleaseFuncDesc(LPFUNCDESC);
    void ReleaseVarDesc(LPVARDESC);
}
alias ITypeInfo LPTYPEINFO;

interface ITypeInfo2 : ITypeInfo {
    HRESULT GetTypeKind(TYPEKIND*);
    HRESULT GetTypeFlags(ULONG*);
    HRESULT GetFuncIndexOfMemId(MEMBERID, INVOKEKIND, UINT*);
    HRESULT GetVarIndexOfMemId(MEMBERID, UINT*);
    HRESULT GetCustData(REFGUID, VARIANT*);
    HRESULT GetFuncCustData(UINT, REFGUID, VARIANT*);
    HRESULT GetParamCustData(UINT, UINT, REFGUID, VARIANT*);
    HRESULT GetVarCustData(UINT, REFGUID, VARIANT*);
    HRESULT GetImplTypeCustData(UINT, REFGUID, VARIANT*);
    HRESULT GetDocumentation2(MEMBERID, LCID, BSTR*, DWORD*, BSTR*);
    HRESULT GetAllCustData(CUSTDATA*);
    HRESULT GetAllFuncCustData(UINT, CUSTDATA*);
    HRESULT GetAllParamCustData(UINT, UINT, CUSTDATA*);
    HRESULT GetAllVarCustData(UINT, CUSTDATA*);
    HRESULT GetAllImplTypeCustData(UINT, CUSTDATA*);
}
alias ITypeInfo2 LPTYPEINFO2;

interface ITypeLib : IUnknown {
    UINT GetTypeInfoCount();
    HRESULT GetTypeInfo(UINT, ITypeInfo*);
    HRESULT GetTypeInfoType(UINT, TYPEKIND*);
    HRESULT GetTypeInfoOfGuid(REFGUID, ITypeInfo*);
    HRESULT GetLibAttr(TLIBATTR**);
    HRESULT GetTypeComp(ITypeComp);
    HRESULT GetDocumentation(INT, BSTR*, BSTR*, DWORD*, BSTR*);
    HRESULT IsName(LPOLESTR, ULONG, BOOL*);
    HRESULT FindName(LPOLESTR, ULONG, ITypeInfo*, MEMBERID*, USHORT*);
    void ReleaseTLibAttr(TLIBATTR*);
}
alias ITypeLib LPTYPELIB;

interface ITypeLib2 : ITypeLib {
    HRESULT GetCustData(REFGUID, VARIANT*);
    HRESULT GetLibStatistics(ULONG*, ULONG*);
    HRESULT GetDocumentation2(INT, LCID, BSTR*, DWORD*, BSTR*);
    HRESULT GetAllCustData(CUSTDATA*);
}
alias ITypeLib2 LPTYPELIB2;

interface IErrorInfo : IUnknown {
    HRESULT GetGUID(GUID*);
    HRESULT GetSource(BSTR*);
    HRESULT GetDescription(BSTR*);
    HRESULT GetHelpFile(BSTR*);
    HRESULT GetHelpContext(DWORD*);
}
alias IErrorInfo LPERRORINFO;

interface ICreateErrorInfo : IUnknown {
    HRESULT SetGUID(REFGUID);
    HRESULT SetSource(LPOLESTR);
    HRESULT SetDescription(LPOLESTR);
    HRESULT SetHelpFile(LPOLESTR);
    HRESULT SetHelpContext(DWORD);
}
alias ICreateErrorInfo LPCREATEERRORINFO;

interface ISupportErrorInfo : IUnknown {
    HRESULT InterfaceSupportsErrorInfo(REFIID);
}
alias ISupportErrorInfo LPSUPPORTERRORINFO;

interface IRecordInfo : IUnknown {
    HRESULT RecordInit(PVOID);
    HRESULT RecordClear(PVOID);
    HRESULT RecordCopy(PVOID, PVOID);
    HRESULT GetGuid(GUID*);
    HRESULT GetName(BSTR*);
    HRESULT GetSize(ULONG*);
    HRESULT GetTypeInfo(ITypeInfo*);
    HRESULT GetField(PVOID, LPCOLESTR, VARIANT*);
    HRESULT GetFieldNoCopy(PVOID, LPCOLESTR, VARIANT*, PVOID*);
    HRESULT PutField (ULONG, PVOID, LPCOLESTR, VARIANT*);
    HRESULT PutFieldNoCopy(ULONG, PVOID, LPCOLESTR, VARIANT*);
    HRESULT GetFieldNames(ULONG*, BSTR*);
    BOOL IsMatchingType();
    PVOID RecordCreate();
    HRESULT RecordCreateCopy(PVOID, PVOID*);
    HRESULT RecordDestroy (PVOID);
}
alias IRecordInfo LPRECORDINFO;

interface ITypeMarshal : IUnknown {
    HRESULT Size(PVOID, DWORD, PVOID, ULONG*);
    HRESULT Marshal(PVOID, DWORD, PVOID, ULONG, BYTE*, ULONG*);
    HRESULT Unmarshal(PVOID, DWORD, ULONG, BYTE*, ULONG*);
    HRESULT Free(PVOID);
}
