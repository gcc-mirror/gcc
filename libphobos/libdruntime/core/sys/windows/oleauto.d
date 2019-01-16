/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_oleauto.d)
 */
module core.sys.windows.oleauto;
version (Windows):
pragma(lib, "oleaut32");

import core.sys.windows.oaidl;
private import core.sys.windows.basetyps, core.sys.windows.unknwn, core.sys.windows.windef, core.sys.windows.wtypes;
private import core.sys.windows.winbase; // for SYSTEMTIME

align(8):
enum STDOLE_MAJORVERNUM = 1;
enum STDOLE_MINORVERNUM = 0;
enum STDOLE_LCID = 0;

enum VARIANT_NOVALUEPROP = 0x01;
enum VARIANT_ALPHABOOL = 0x02;
enum VARIANT_NOUSEOVERRIDE = 0x04;
enum VARIANT_LOCALBOOL = 0x08;

enum VAR_TIMEVALUEONLY = 0x0001;
enum VAR_DATEVALUEONLY = 0x0002;
enum VAR_VALIDDATE = 0x0004;
enum VAR_CALENDAR_HIJRI = 0x0008;
enum VAR_LOCALBOOL = 0x0010;
enum VAR_FORMAT_NOSUBSTITUTE = 0x0020;
enum VAR_FOURDIGITYEARS = 0x0040;
enum VAR_CALENDAR_THAI = 0x0080;
enum VAR_CALENDAR_GREGORIAN = 0x0100;

enum MEMBERID_NIL = DISPID_UNKNOWN;
enum ID_DEFAULTINST =  -2;
enum DISPATCH_METHOD = 1;
enum DISPATCH_PROPERTYGET = 2;
enum DISPATCH_PROPERTYPUT = 4;
enum DISPATCH_PROPERTYPUTREF = 8;

//ULONG LHashValOfName(LCID l, OLECHAR* n) { return LHashValOfNameSys(SYSKIND.SYS_WIN32, l, n); }

// DAC: These aren't in the 2003 SDK.
//MACRO #define WHashValOfLHashVal(h) ((unsigned short)(0x0000ffff&(h)))
//MACRO #define IsHashValCompatible(h1, h2) ((BOOL)((0x00ff0000&(h1))==(0x00ff0000&(h2))))

enum {
    ACTIVEOBJECT_STRONG = 0,
    ACTIVEOBJECT_WEAK   = 1
}

// DAC: These seem to be irrelevant for D.
//#define V_UNION(X, Y) ((X)->Y)
//#define V_VT(X) ((X)->vt)
//#define V_BOOL(X) V_UNION(X, boolVal)
//#define V_ISBYREF(X) (V_VT(X)&VT_BYREF)
//#define V_ISARRAY(X) (V_VT(X)&VT_ARRAY)
//#define V_ISVECTOR(X) (V_VT(X)&VT_VECTOR)
//#define V_NONE(X) V_I2(X)
//#define V_UI1(X) V_UNION(X, bVal)
//#define V_UI1REF(X) V_UNION(X, pbVal)
//#define V_I2(X) V_UNION(X, iVal)
//#define V_UI2(X) V_UNION(X, uiVal)
//#define V_I2REF(X) V_UNION(X, piVal)
//#define V_I4(X) V_UNION(X, lVal)
//#define V_UI4(X) V_UNION(X, ulVal)
//#define V_I4REF(X) V_UNION(X, plVal)
//#define V_UI4REF(X) V_UNION(X, pulVal)
//#define V_I8(X) V_UNION(X, llVal)
//#define V_UI8(X) V_UNION(X, ullVal)
//#define V_I8REF(X) V_UNION(X, pllVal)
//#define V_UI8REF(X) V_UNION(X, pullVal)
//#define V_R4(X) V_UNION(X, fltVal)
//#define V_R4REF(X) V_UNION(X, pfltVal)
//#define V_R8(X) V_UNION(X, dblVal)
//#define V_R8REF(X) V_UNION(X, pdblVal)
//#define V_CY(X) V_UNION(X, cyVal)
//#define V_CYREF(X) V_UNION(X, pcyVal)
//#define V_DATE(X) V_UNION(X, date)
//#define V_DATEREF(X) V_UNION(X, pdate)
//#define V_BSTR(X) V_UNION(X, bstrVal)
//#define V_BSTRREF(X) V_UNION(X, pbstrVal)
//#define V_DISPATCH(X) V_UNION(X, pdispVal)
//#define V_DISPATCHREF(X) V_UNION(X, ppdispVal)
//#define V_ERROR(X) V_UNION(X, scode)
//#define V_ERRORREF(X) V_UNION(X, pscode)
//#define V_BOOLREF(X) V_UNION(X, pboolVal)
//#define V_UNKNOWN(X) V_UNION(X, punkVal)
//#define V_UNKNOWNREF(X) V_UNION(X, ppunkVal)
//#define V_VARIANTREF(X) V_UNION(X, pvarVal)
//#define V_LPSTR(X) V_UNION(X, pszVal)
//#define V_LPSTRREF(X) V_UNION(X, ppszVal)
//#define V_LPWSTR(X) V_UNION(X, pwszVal)
//#define V_LPWSTRREF(X) V_UNION(X, ppwszVal)
//#define V_FILETIME(X) V_UNION(X, filetime)
//#define V_FILETIMEREF(X) V_UNION(X, pfiletime)
//#define V_BLOB(X) V_UNION(X, blob)
//#define V_UUID(X) V_UNION(X, puuid)
//#define V_CLSID(X) V_UNION(X, puuid)
//#define V_ARRAY(X) V_UNION(X, parray)
//#define V_ARRAYREF(X) V_UNION(X, pparray)
//#define V_BYREF(X) V_UNION(X, byref)
//#define V_DECIMAL(X) ((X)->decVal)
//#define V_DECIMALREF(X) V_UNION(X, pdecVal)
//#define V_I1(X) V_UNION(X, cVal)

//#ifdef _WIN64
//#define V_INT_PTR(X) V_I8(X)
//#define V_UINT_PTR(X) V_UI8(X)
//#define V_INT_PTRREF(X) V_I8REF(X)
//#define V_UINT_PTRREF(X) V_UI8REF(X)
//#else
//#define V_INT_PTR(X) V_I4(X)
//#define V_UINT_PTR(X) V_UI4(X)
//#define V_INT_PTRREF(X) V_I4REF(X)
//#define V_UINT_PTRREF(X) V_UI4REF(X)
//#endif

enum {
    VARCMP_LT = 0,
    VARCMP_EQ,
    VARCMP_GT,
    VARCMP_NULL // = 3
}

enum LOCALE_USE_NLS = 0x10000000;

enum VARIANT_NOUSEROVERRIDE     = 0x04;
enum VARIANT_CALENDAR_HIJRI     = 0x08;
enum VARIANT_CALENDAR_THAI      = 0x20;
enum VARIANT_CALENDAR_GREGORIAN = 0x40;
enum VARIANT_USE_NLS            = 0x80;

enum NUMPRS_LEADING_WHITE  = 0x00001;
enum NUMPRS_TRAILING_WHITE = 0x00002;
enum NUMPRS_LEADING_PLUS   = 0x00004;
enum NUMPRS_TRAILING_PLUS  = 0x00008;
enum NUMPRS_LEADING_MINUS  = 0x00010;
enum NUMPRS_TRAILING_MINUS = 0x00020;
enum NUMPRS_HEX_OCT        = 0x00040;
enum NUMPRS_PARENS         = 0x00080;
enum NUMPRS_DECIMAL        = 0x00100;
enum NUMPRS_THOUSANDS      = 0x00200;
enum NUMPRS_CURRENCY       = 0x00400;
enum NUMPRS_EXPONENT       = 0x00800;
enum NUMPRS_USE_ALL        = 0x01000;
enum NUMPRS_STD            = 0x01FFF;
enum NUMPRS_NEG            = 0x10000;
enum NUMPRS_INEXACT        = 0x20000;

enum VTBIT_I1 = 1 << VARENUM.VT_I1;
enum VTBIT_UI1 = 1 << VARENUM.VT_UI1;
enum VTBIT_I2 = 1 << VARENUM.VT_I2;
enum VTBIT_UI2 = 1 << VARENUM.VT_UI2;
enum VTBIT_I4 = 1 << VARENUM.VT_I4;
enum VTBIT_UI4 = 1 << VARENUM.VT_UI4;
enum VTBIT_I8 = 1 << VARENUM.VT_I8;
enum VTBIT_UI8 = 1 << VARENUM.VT_UI8;
enum VTBIT_R4 = 1 << VARENUM.VT_R4;
enum VTBIT_R8 = 1 << VARENUM.VT_R8;
enum VTBIT_CY = 1 << VARENUM.VT_CY;
enum VTBIT_DECIMAL = 1 << VARENUM.VT_DECIMAL;


enum REGKIND{
    REGKIND_DEFAULT,
    REGKIND_REGISTER,
    REGKIND_NONE
}

struct PARAMDATA{
    OLECHAR* szName;
    VARTYPE vt;
}
alias PARAMDATA* LPPARAMDATA;

struct METHODDATA{
    OLECHAR* szName;
    PARAMDATA* ppdata;
    DISPID dispid;
    UINT iMeth;
    CALLCONV cc;
    UINT cArgs;
    WORD wFlags;
    VARTYPE vtReturn;
}
alias METHODDATA* LPMETHODDATA;

struct INTERFACEDATA{
    METHODDATA* pmethdata;
    UINT cMembers;
}
alias INTERFACEDATA* LPINTERFACEDATA;

struct UDATE {
    SYSTEMTIME st;
    USHORT wDayOfYear;
}

struct NUMPARSE {
    int cDig;
    uint dwInFlags;
    uint dwOutFlags;
    int cchUsed;
    int nBaseShift;
    int nPwr10;
}


// DAC: In MinGW, these were declared but not defined in oaidl.
// The SDK docs suggest they belong in this file instead.

deprecated {  // not actually deprecated, but they aren't converted yet.
              // (will need to reinstate CreateTypeLib as well)
    interface ICreateTypeInfo {};
    interface ICreateTypeInfo2 {};
    interface ICreateTypeLib {};
    interface ICreateTypeLib2 {};

    alias ICreateTypeInfo LPCREATETYPEINFO;
    alias ICreateTypeInfo2 LPCREATETYPEINFO2;
    alias ICreateTypeLib LPCREATETYPELIB;
    alias ICreateTypeLib2 LPCREATETYPELIB2;
}

extern (Windows) {
    BSTR SysAllocString(const(OLECHAR)*);
    int SysReAllocString(BSTR*, const(OLECHAR)*);
    BSTR SysAllocStringLen(const(OLECHAR)*, uint);
    int SysReAllocStringLen(BSTR*, const(OLECHAR)*, uint);
    void SysFreeString(BSTR);
    uint SysStringLen(BSTR);
    uint SysStringByteLen(BSTR);
    BSTR SysAllocStringByteLen(const(char)*, uint);
    int DosDateTimeToVariantTime(ushort, ushort, double*);
    int VariantTimeToDosDateTime(double, ushort*, ushort*);
    int VariantTimeToSystemTime(double, LPSYSTEMTIME);
    int SystemTimeToVariantTime(LPSYSTEMTIME, double*);
    HRESULT VarDateFromUdate(UDATE*, ULONG, DATE*);
    HRESULT VarDateFromUdateEx(UDATE*, LCID, ULONG, DATE*);
    HRESULT VarUdateFromDate(DATE, ULONG, UDATE*);
    HRESULT SafeArrayAllocDescriptor(uint, SAFEARRAY**);
    HRESULT SafeArrayAllocData(SAFEARRAY*);
    SAFEARRAY* SafeArrayCreate(VARTYPE, uint, SAFEARRAYBOUND*);
    HRESULT SafeArrayDestroyDescriptor(SAFEARRAY*);
    HRESULT SafeArrayDestroyData(SAFEARRAY*);
    HRESULT SafeArrayDestroy(SAFEARRAY*);
    HRESULT SafeArrayRedim(SAFEARRAY*, SAFEARRAYBOUND*);
    uint SafeArrayGetDim(SAFEARRAY*);
    uint SafeArrayGetElemsize(SAFEARRAY*);
    HRESULT SafeArrayGetUBound(SAFEARRAY*, uint, int*);
    HRESULT SafeArrayGetLBound(SAFEARRAY*, uint, int*);
    HRESULT SafeArrayLock(SAFEARRAY*);
    HRESULT SafeArrayUnlock(SAFEARRAY*);
    HRESULT SafeArrayAccessData(SAFEARRAY*, void**);
    HRESULT SafeArrayUnaccessData(SAFEARRAY*);
    HRESULT SafeArrayGetElement(SAFEARRAY*, int*, void*);
    HRESULT SafeArrayPutElement(SAFEARRAY*, int*, void*);
    HRESULT SafeArrayCopy(SAFEARRAY*, SAFEARRAY**);
    HRESULT SafeArrayPtrOfIndex(SAFEARRAY*, int*, void**);
    SAFEARRAY* SafeArrayCreateVector(VARTYPE, LONG, ULONG);
    SAFEARRAY* SafeArrayCreateVectorEx(VARTYPE, LONG, ULONG, LPVOID);
    HRESULT SafeArrayAllocDescriptorEx(VARTYPE, UINT, SAFEARRAY**);
    HRESULT SafeArrayGetVartype(SAFEARRAY*, VARTYPE*);
    HRESULT SafeArraySetRecordInfo(SAFEARRAY*, IRecordInfo);
    HRESULT SafeArrayGetRecordInfo(SAFEARRAY*, IRecordInfo*);
    HRESULT SafeArraySetIID(SAFEARRAY*, REFGUID);
    HRESULT SafeArrayGetIID(SAFEARRAY*, GUID*);
    void VariantInit(VARIANTARG*);
    HRESULT VariantClear(VARIANTARG*);
    HRESULT VariantCopy(VARIANTARG*, VARIANTARG*);
    HRESULT VariantCopyInd(VARIANT*, VARIANTARG*);
    HRESULT VariantChangeType(VARIANTARG*, VARIANTARG*, ushort, VARTYPE);
    HRESULT VariantChangeTypeEx(VARIANTARG*, VARIANTARG*, LCID, ushort, VARTYPE);
    HRESULT VarUI1FromI2(short, ubyte*);
    HRESULT VarUI1FromI4(int, ubyte*);
    HRESULT VarUI1FromR4(float, ubyte*);
    HRESULT VarUI1FromR8(double, ubyte*);
    HRESULT VarUI1FromCy(CY, ubyte*);
    HRESULT VarUI1FromDate(DATE, ubyte*);
    HRESULT VarUI1FromStr(OLECHAR*, LCID, uint, ubyte*);
    HRESULT VarUI1FromDisp(LPDISPATCH, LCID, ubyte*);
    HRESULT VarUI1FromBool(VARIANT_BOOL, ubyte*);
    HRESULT VarI2FromUI1(ubyte, short*);
    HRESULT VarI2FromI4(int, short*);
    HRESULT VarI2FromR4(float, short*);
    HRESULT VarI2FromR8(double, short*);
    HRESULT VarI2FromCy(CY cyIn, short*);
    HRESULT VarI2FromDate(DATE, short*);
    HRESULT VarI2FromStr(OLECHAR*, LCID, uint, short*);
    HRESULT VarI2FromDisp(LPDISPATCH, LCID, short*);
    HRESULT VarI2FromBool(VARIANT_BOOL, short*);
    HRESULT VarI4FromUI1(ubyte, int*);
    HRESULT VarI4FromI2(short, int*);
    HRESULT VarI4FromR4(float, int*);
    HRESULT VarI4FromR8(double, int*);
    HRESULT VarI4FromCy(CY, int*);
    HRESULT VarI4FromDate(DATE, int*);
    HRESULT VarI4FromStr(OLECHAR*, LCID, uint, int*);
    HRESULT VarI4FromDisp(LPDISPATCH, LCID, int*);
    HRESULT VarI4FromBool(VARIANT_BOOL, int*);
    HRESULT VarR4FromUI1(ubyte, float*);
    HRESULT VarR4FromI2(short, float*);
    HRESULT VarR4FromI4(int, float*);
    HRESULT VarR4FromR8(double, float*);
    HRESULT VarR4FromCy(CY, float*);
    HRESULT VarR4FromDate(DATE, float*);
    HRESULT VarR4FromStr(OLECHAR*, LCID, uint, float*);
    HRESULT VarR4FromDisp(LPDISPATCH, LCID, float*);
    HRESULT VarR4FromBool(VARIANT_BOOL, float*);
    HRESULT VarR8FromUI1(ubyte, double*);
    HRESULT VarR8FromI2(short, double*);
    HRESULT VarR8FromI4(int, double*);
    HRESULT VarR8FromR4(float, double*);
    HRESULT VarR8FromCy(CY, double*);
    HRESULT VarR8FromDate(DATE, double*);
    HRESULT VarR8FromStr(OLECHAR*, LCID, uint, double*);
    HRESULT VarR8FromDisp(LPDISPATCH, LCID, double*);
    HRESULT VarR8FromBool(VARIANT_BOOL, double*);
    HRESULT VarR8FromDec(DECIMAL*, double*);
    HRESULT VarDateFromUI1(ubyte, DATE*);
    HRESULT VarDateFromI2(short, DATE*);
    HRESULT VarDateFromI4(int, DATE*);
    HRESULT VarDateFromR4(float, DATE*);
    HRESULT VarDateFromR8(double, DATE*);
    HRESULT VarDateFromCy(CY, DATE*);
    HRESULT VarDateFromStr(OLECHAR*, LCID, uint, DATE*);
    HRESULT VarDateFromDisp(LPDISPATCH, LCID, DATE*);
    HRESULT VarDateFromBool(VARIANT_BOOL, DATE*);
    HRESULT VarCyFromUI1(ubyte, CY*);
    HRESULT VarCyFromI2(short, CY*);
    HRESULT VarCyFromI4(int, CY*);
    HRESULT VarCyFromR4(float, CY*);
    HRESULT VarCyFromR8(double, CY*);
    HRESULT VarCyFromDate(DATE, CY*);
    HRESULT VarCyFromStr(OLECHAR*, LCID, uint, CY*);
    HRESULT VarCyFromDisp(LPDISPATCH, LCID, CY*);
    HRESULT VarCyFromBool(VARIANT_BOOL, CY*);
    HRESULT VarBstrFromUI1(ubyte, LCID, uint, BSTR*);
    HRESULT VarBstrFromI2(short, LCID, uint, BSTR*);
    HRESULT VarBstrFromI4(int, LCID, uint, BSTR*);
    HRESULT VarBstrFromR4(float, LCID, uint, BSTR*);
    HRESULT VarBstrFromR8(double, LCID, uint, BSTR*);
    HRESULT VarBstrFromCy(CY, LCID, uint, BSTR*);
    HRESULT VarBstrFromDate(DATE, LCID, uint, BSTR*);
    HRESULT VarBstrFromDisp(LPDISPATCH, LCID, uint, BSTR*);
    HRESULT VarBstrFromBool(VARIANT_BOOL, LCID, uint, BSTR*);
    HRESULT VarBoolFromUI1(ubyte, VARIANT_BOOL*);
    HRESULT VarBoolFromI2(short, VARIANT_BOOL*);
    HRESULT VarBoolFromI4(int, VARIANT_BOOL*);
    HRESULT VarBoolFromR4(float, VARIANT_BOOL*);
    HRESULT VarBoolFromR8(double, VARIANT_BOOL*);
    HRESULT VarBoolFromDate(DATE, VARIANT_BOOL*);
    HRESULT VarBoolFromCy(CY, VARIANT_BOOL*);
    HRESULT VarBoolFromStr(OLECHAR*, LCID, uint, VARIANT_BOOL*);
    HRESULT VarBoolFromDisp(LPDISPATCH, LCID, VARIANT_BOOL*);
    HRESULT VarDecFromR8(double, DECIMAL*);
    ULONG LHashValOfNameSysA(SYSKIND, LCID, const(char)*);
    ULONG LHashValOfNameSys(SYSKIND, LCID, const(OLECHAR)*);
    HRESULT LoadTypeLib(const(OLECHAR)*, LPTYPELIB*);
    HRESULT LoadTypeLibEx(LPCOLESTR, REGKIND, LPTYPELIB*);
    HRESULT LoadRegTypeLib(REFGUID, WORD, WORD, LCID, LPTYPELIB*);
    HRESULT QueryPathOfRegTypeLib(REFGUID, ushort, ushort, LCID, LPBSTR);
    HRESULT RegisterTypeLib(LPTYPELIB, OLECHAR*, OLECHAR*);
    HRESULT UnRegisterTypeLib(REFGUID, WORD, WORD, LCID, SYSKIND);
    // not actually deprecated, but depends on unconverted ICreateTypeLib
    deprecated HRESULT CreateTypeLib(SYSKIND, const(OLECHAR)*, LPCREATETYPELIB*);
    HRESULT DispGetParam(DISPPARAMS*, UINT, VARTYPE, VARIANT*, UINT*);
    HRESULT DispGetIDsOfNames(LPTYPEINFO, OLECHAR**, UINT, DISPID*);
    HRESULT DispInvoke(void*, LPTYPEINFO, DISPID, WORD, DISPPARAMS*, VARIANT*, EXCEPINFO*, UINT*);
    HRESULT CreateDispTypeInfo(INTERFACEDATA*, LCID, LPTYPEINFO*);
    HRESULT CreateStdDispatch(IUnknown, void*, LPTYPEINFO, IUnknown*);
    HRESULT RegisterActiveObject(IUnknown, REFCLSID, DWORD, DWORD*);
    HRESULT RevokeActiveObject(DWORD, void*);
    HRESULT GetActiveObject(REFCLSID, void*, IUnknown*);
    HRESULT SetErrorInfo(uint, LPERRORINFO);
    HRESULT GetErrorInfo(uint, LPERRORINFO*);
    HRESULT CreateErrorInfo(LPCREATEERRORINFO*);
    uint OaBuildVersion();
    HRESULT VectorFromBstr (BSTR, SAFEARRAY**);
    HRESULT BstrFromVector (SAFEARRAY*, BSTR*);
    HRESULT VarParseNumFromStr(OLECHAR*, LCID, ULONG, NUMPARSE*, BYTE*);
    HRESULT VarNumFromParseNum(NUMPARSE*, BYTE*, ULONG, VARIANT*);

    HRESULT VarAdd(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarSub(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarMul(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarDiv(LPVARIANT, LPVARIANT, LPVARIANT);

    HRESULT VarUI1FromI2(SHORT, BYTE*);
    HRESULT VarUI1FromI4(LONG, BYTE*);
    HRESULT VarUI1FromI8(LONG64, BYTE*);
    HRESULT VarUI1FromR4(FLOAT, BYTE*);
    HRESULT VarUI1FromR8(DOUBLE, BYTE*);
    HRESULT VarUI1FromDate(DATE, BYTE*);
    HRESULT VarUI1FromBool(VARIANT_BOOL, BYTE*);
    HRESULT VarUI1FromI1(byte, BYTE*);
    HRESULT VarUI1FromUI2(USHORT, BYTE*);
    HRESULT VarUI1FromUI4(ULONG, BYTE*);
    HRESULT VarUI1FromUI8(ULONG64, BYTE*);
    HRESULT VarUI1FromStr(OLECHAR*, LCID, ULONG, BYTE*);
    HRESULT VarUI1FromCy(CY, BYTE*);
    HRESULT VarUI1FromDec(DECIMAL*, BYTE*);
    HRESULT VarUI1FromDisp(IDispatch, LCID, BYTE*);

    HRESULT VarI2FromUI1(BYTE, SHORT*);
    HRESULT VarI2FromI4(LONG, SHORT*);
    HRESULT VarI2FromI8(LONG64, SHORT*);
    HRESULT VarI2FromR4(FLOAT, SHORT*);
    HRESULT VarI2FromR8(DOUBLE, SHORT*);
    HRESULT VarI2FromDate(DATE, SHORT*);
    HRESULT VarI2FromBool(VARIANT_BOOL, SHORT*);
    HRESULT VarI2FromI1(byte, SHORT*);
    HRESULT VarI2FromUI2(USHORT, SHORT*);
    HRESULT VarI2FromUI4(ULONG, SHORT*);
    HRESULT VarI2FromUI8(ULONG64, SHORT*);
    HRESULT VarI2FromStr(OLECHAR*, LCID, ULONG, SHORT*);
    HRESULT VarI2FromCy(CY, SHORT*);
    HRESULT VarI2FromDec(DECIMAL*, SHORT*);
    HRESULT VarI2FromDisp(IDispatch, LCID, SHORT*);

    HRESULT VarI4FromUI1(BYTE, LONG*);
    HRESULT VarI4FromI2(SHORT, LONG*);
    HRESULT VarI4FromI8(LONG64, LONG*);
    HRESULT VarI4FromR4(FLOAT, LONG*);
    HRESULT VarI4FromR8(DOUBLE, LONG*);
    HRESULT VarI4FromDate(DATE, LONG*);
    HRESULT VarI4FromBool(VARIANT_BOOL, LONG*);
    HRESULT VarI4FromI1(byte, LONG*);
    HRESULT VarI4FromUI2(USHORT, LONG*);
    HRESULT VarI4FromUI4(ULONG, LONG*);
    HRESULT VarI4FromUI8(ULONG64, LONG*);
    HRESULT VarI4FromStr(OLECHAR*, LCID, ULONG, LONG*);
    HRESULT VarI4FromCy(CY, LONG*);
    HRESULT VarI4FromDec(DECIMAL*, LONG*);
    HRESULT VarI4FromDisp(IDispatch, LCID, LONG*);

    HRESULT VarI8FromUI1(BYTE, LONG64*);
    HRESULT VarI8FromI2(SHORT, LONG64*);
    HRESULT VarI8FromI4(LONG, LONG64*);
    HRESULT VarI8FromR4(FLOAT, LONG64*);
    HRESULT VarI8FromR8(DOUBLE, LONG64*);
    HRESULT VarI8FromDate(DATE, LONG64*);
    HRESULT VarI8FromStr(OLECHAR*, LCID, ULONG, LONG64*);
    HRESULT VarI8FromBool(VARIANT_BOOL, LONG64*);
    HRESULT VarI8FromI1(byte, LONG64*);
    HRESULT VarI8FromUI2(USHORT, LONG64*);
    HRESULT VarI8FromUI4(ULONG, LONG64*);
    HRESULT VarI8FromUI8(ULONG64, LONG64*);
    HRESULT VarI8FromDec(DECIMAL* pdecIn, LONG64*);
    HRESULT VarI8FromInt(INT intIn, LONG64*);
    HRESULT VarI8FromCy(CY, LONG64*);
    HRESULT VarI8FromDisp(IDispatch, LCID, LONG64*);

    HRESULT VarR4FromUI1(BYTE, FLOAT*);
    HRESULT VarR4FromI2(SHORT, FLOAT*);
    HRESULT VarR4FromI4(LONG, FLOAT*);
    HRESULT VarR4FromI8(LONG64, FLOAT*);
    HRESULT VarR4FromR8(DOUBLE, FLOAT*);
    HRESULT VarR4FromDate(DATE, FLOAT*);
    HRESULT VarR4FromBool(VARIANT_BOOL, FLOAT*);
    HRESULT VarR4FromI1(byte, FLOAT*);
    HRESULT VarR4FromUI2(USHORT, FLOAT*);
    HRESULT VarR4FromUI4(ULONG, FLOAT*);
    HRESULT VarR4FromUI8(ULONG64, FLOAT*);
    HRESULT VarR4FromStr(OLECHAR*, LCID, ULONG, FLOAT*);
    HRESULT VarR4FromCy(CY, FLOAT*);
    HRESULT VarR4FromDec(DECIMAL*, FLOAT*);
    HRESULT VarR4FromDisp(IDispatch, LCID, FLOAT*);

    HRESULT VarR8FromUI1(BYTE, double*);
    HRESULT VarR8FromI2(SHORT, double*);
    HRESULT VarR8FromI4(LONG, double*);
    HRESULT VarR8FromI8(LONG64, double*);
    HRESULT VarR8FromR4(FLOAT, double*);
    HRESULT VarR8FromDate(DATE, double*);
    HRESULT VarR8FromBool(VARIANT_BOOL, double*);
    HRESULT VarR8FromI1(byte, double*);
    HRESULT VarR8FromUI2(USHORT, double*);
    HRESULT VarR8FromUI4(ULONG, double*);
    HRESULT VarR8FromUI8(ULONG64, double*);
    HRESULT VarR8FromStr(OLECHAR*, LCID, ULONG, double*);
    HRESULT VarR8FromCy(CY, double*);
    HRESULT VarR8FromDec(DECIMAL*, double*);
    HRESULT VarR8FromDisp(IDispatch, LCID, double*);

    HRESULT VarDateFromUI1(BYTE, DATE*);
    HRESULT VarDateFromI2(SHORT, DATE*);
    HRESULT VarDateFromI4(LONG, DATE*);
    HRESULT VarDateFromI8(LONG64, DATE*);
    HRESULT VarDateFromR4(FLOAT, DATE*);
    HRESULT VarDateFromR8(DOUBLE, DATE*);
    HRESULT VarDateFromStr(OLECHAR*, LCID, ULONG, DATE*);
    HRESULT VarDateFromI1(byte, DATE*);
    HRESULT VarDateFromUI2(USHORT, DATE*);
    HRESULT VarDateFromUI4(ULONG, DATE*);
    HRESULT VarDateFromUI8(ULONG64, DATE*);
    HRESULT VarDateFromBool(VARIANT_BOOL, DATE*);
    HRESULT VarDateFromCy(CY, DATE*);
    HRESULT VarDateFromDec(DECIMAL*, DATE*);
    HRESULT VarDateFromDisp(IDispatch, LCID, DATE*);

    HRESULT VarCyFromUI1(BYTE, CY*);
    HRESULT VarCyFromI2(SHORT sIn, CY*);
    HRESULT VarCyFromI4(LONG, CY*);
    HRESULT VarCyFromI8(LONG64, CY*);
    HRESULT VarCyFromR4(FLOAT, CY*);
    HRESULT VarCyFromR8(DOUBLE, CY*);
    HRESULT VarCyFromDate(DATE, CY*);
    HRESULT VarCyFromStr(OLECHAR*, LCID, ULONG, CY*);
    HRESULT VarCyFromBool(VARIANT_BOOL, CY*);
    HRESULT VarCyFromI1(byte, CY*);
    HRESULT VarCyFromUI2(USHORT, CY*);
    HRESULT VarCyFromUI4(ULONG, CY*);
    HRESULT VarCyFromUI8(ULONG64, CY*);
    HRESULT VarCyFromDec(DECIMAL*, CY*);
    HRESULT VarCyFromStr(OLECHAR*, LCID, ULONG, CY*);
    HRESULT VarCyFromDisp(IDispatch, LCID, CY*);

    HRESULT VarBstrFromUI1(BYTE, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromI2(SHORT, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromI4(LONG, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromI8(LONG64, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromR4(FLOAT, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromR8(DOUBLE, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromDate(DATE, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromBool(VARIANT_BOOL, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromI1(byte, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromUI2(USHORT, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromUI8(ULONG64, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromUI4(ULONG, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromCy(CY, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromDec(DECIMAL*, LCID, ULONG, BSTR*);
    HRESULT VarBstrFromDisp(IDispatch, LCID, ULONG, BSTR*);

    HRESULT VarBoolFromUI1(BYTE, VARIANT_BOOL*);
    HRESULT VarBoolFromI2(SHORT, VARIANT_BOOL*);
    HRESULT VarBoolFromI4(LONG, VARIANT_BOOL*);
    HRESULT VarBoolFromI8(LONG64, VARIANT_BOOL*);
    HRESULT VarBoolFromR4(FLOAT, VARIANT_BOOL*);
    HRESULT VarBoolFromR8(DOUBLE, VARIANT_BOOL*);
    HRESULT VarBoolFromDate(DATE, VARIANT_BOOL*);
    HRESULT VarBoolFromStr(OLECHAR*, LCID, ULONG, VARIANT_BOOL*);
    HRESULT VarBoolFromI1(byte, VARIANT_BOOL*);
    HRESULT VarBoolFromUI2(USHORT, VARIANT_BOOL*);
    HRESULT VarBoolFromUI4(ULONG, VARIANT_BOOL*);
    HRESULT VarBoolFromUI8(ULONG64, VARIANT_BOOL*);
    HRESULT VarBoolFromCy(CY, VARIANT_BOOL*);
    HRESULT VarBoolFromDec(DECIMAL*, VARIANT_BOOL*);
    HRESULT VarBoolFromDisp(IDispatch, LCID, VARIANT_BOOL*);

    HRESULT VarI1FromUI1(BYTE, byte*);
    HRESULT VarI1FromI2(SHORT, byte*);
    HRESULT VarI1FromI4(LONG, byte*);
    HRESULT VarI1FromI8(LONG64, byte*);
    HRESULT VarI1FromR4(FLOAT, byte*);
    HRESULT VarI1FromR8(DOUBLE, byte*);
    HRESULT VarI1FromDate(DATE, byte*);
    HRESULT VarI1FromStr(OLECHAR*, LCID, ULONG, byte*);
    HRESULT VarI1FromBool(VARIANT_BOOL, byte*);
    HRESULT VarI1FromUI2(USHORT, byte*);
    HRESULT VarI1FromUI4(ULONG, byte*);
    HRESULT VarI1FromUI8(ULONG64, byte*);
    HRESULT VarI1FromCy(CY, byte*);
    HRESULT VarI1FromDec(DECIMAL*, byte*);
    HRESULT VarI1FromDisp(IDispatch, LCID, byte*);

    HRESULT VarUI2FromUI1(BYTE, USHORT*);
    HRESULT VarUI2FromI2(SHORT, USHORT*);
    HRESULT VarUI2FromI4(LONG, USHORT*);
    HRESULT VarUI2FromI8(LONG64, USHORT*);
    HRESULT VarUI2FromR4(FLOAT, USHORT*);
    HRESULT VarUI2FromR8(DOUBLE, USHORT*);
    HRESULT VarUI2FromDate(DATE, USHORT*);
    HRESULT VarUI2FromStr(OLECHAR*, LCID, ULONG, USHORT*);
    HRESULT VarUI2FromBool(VARIANT_BOOL, USHORT*);
    HRESULT VarUI2FromI1(byte, USHORT*);
    HRESULT VarUI2FromUI4(ULONG, USHORT*);
    HRESULT VarUI2FromUI8(ULONG64, USHORT*);
    HRESULT VarUI2FromCy(CY, USHORT*);
    HRESULT VarUI2FromDec(DECIMAL*, USHORT*);
    HRESULT VarUI2FromDisp(IDispatch, LCID, USHORT*);

    HRESULT VarUI4FromStr(OLECHAR*, LCID, ULONG, ULONG*);
    HRESULT VarUI4FromUI1(BYTE, ULONG*);
    HRESULT VarUI4FromI2(SHORT, ULONG*);
    HRESULT VarUI4FromI4(LONG, ULONG*);
    HRESULT VarUI4FromI8(LONG64, ULONG*);
    HRESULT VarUI4FromR4(FLOAT, ULONG*);
    HRESULT VarUI4FromR8(DOUBLE, ULONG*);
    HRESULT VarUI4FromDate(DATE, ULONG*);
    HRESULT VarUI4FromBool(VARIANT_BOOL, ULONG*);
    HRESULT VarUI4FromI1(byte, ULONG*);
    HRESULT VarUI4FromUI2(USHORT, ULONG*);
    HRESULT VarUI4FromUI8(ULONG64, ULONG*);
    HRESULT VarUI4FromCy(CY, ULONG*);
    HRESULT VarUI4FromDec(DECIMAL*, ULONG*);
    HRESULT VarUI4FromDisp(IDispatch, LCID, ULONG*);

    HRESULT VarUI8FromUI1(BYTE, ULONG64*);
    HRESULT VarUI8FromI2(SHORT, ULONG64*);
    HRESULT VarUI8FromI4(LONG, ULONG64*);
    HRESULT VarUI8FromI8(LONG64, ULONG64*);
    HRESULT VarUI8FromR4(FLOAT, ULONG64*);
    HRESULT VarUI8FromR8(DOUBLE, ULONG64*);
    HRESULT VarUI8FromDate(DATE, ULONG64*);
    HRESULT VarUI8FromStr(OLECHAR*, LCID, ULONG, ULONG64*);
    HRESULT VarUI8FromBool(VARIANT_BOOL, ULONG64*);
    HRESULT VarUI8FromI1(byte, ULONG64*);
    HRESULT VarUI8FromUI2(USHORT, ULONG64*);
    HRESULT VarUI8FromUI4(ULONG, ULONG64*);
    HRESULT VarUI8FromDec(DECIMAL*, ULONG64*);
    HRESULT VarUI8FromInt(INT, ULONG64*);
    HRESULT VarUI8FromCy(CY, ULONG64*);
    HRESULT VarUI8FromDisp(IDispatch, LCID, ULONG64*);

    HRESULT VarDecFromUI1(BYTE, DECIMAL*);
    HRESULT VarDecFromI2(SHORT, DECIMAL*);
    HRESULT VarDecFromI4(LONG, DECIMAL*);
    HRESULT VarDecFromI8(LONG64, DECIMAL*);
    HRESULT VarDecFromR4(FLOAT, DECIMAL*);
    HRESULT VarDecFromR8(DOUBLE, DECIMAL*);
    HRESULT VarDecFromDate(DATE, DECIMAL*);
    HRESULT VarDecFromStr(OLECHAR*, LCID, ULONG, DECIMAL*);
    HRESULT VarDecFromBool(VARIANT_BOOL, DECIMAL*);
    HRESULT VarDecFromI1(byte, DECIMAL*);
    HRESULT VarDecFromUI2(USHORT, DECIMAL*);
    HRESULT VarDecFromUI4(ULONG, DECIMAL*);
    HRESULT VarDecFromUI8(ULONG64, DECIMAL*);
    HRESULT VarDecFromCy(CY, DECIMAL*);
    HRESULT VarDecFromDisp(IDispatch, LCID, DECIMAL*);

    HRESULT VarDecNeg(const(DECIMAL)*, DECIMAL*);
    HRESULT VarR4CmpR8(float, double);
    HRESULT VarR8Pow(double, double, double*);
    HRESULT VarR8Round(double, int, double*);
    HRESULT VarDecAbs(const(DECIMAL)*, DECIMAL*);
    HRESULT VarDecAdd(const(DECIMAL)*, const(DECIMAL)*, DECIMAL*);
    HRESULT VarDecCmp(const(DECIMAL)*, const(DECIMAL)*);
    HRESULT VarDecCmpR8(const(DECIMAL)*, DOUBLE);
    HRESULT VarDecDiv(const(DECIMAL)*, const(DECIMAL)*, DECIMAL*);
    HRESULT VarDecFix(const(DECIMAL)*, DECIMAL*);
    HRESULT VarDecInt(const(DECIMAL)*, DECIMAL*);
    HRESULT VarDecMul(const(DECIMAL)*, const(DECIMAL)*, DECIMAL*);
    HRESULT VarDecRound(const(DECIMAL)*, int, DECIMAL*);
    HRESULT VarDecSub(const(DECIMAL)*, const(DECIMAL)*, DECIMAL*);
    HRESULT VarCyAbs(CY, CY*);
    HRESULT VarCyAdd(CY, CY, CY*);
    HRESULT VarCyCmp(CY, CY);
    HRESULT VarCyCmpR8(CY, DOUBLE);
    HRESULT VarCyFix(CY, CY*);
    HRESULT VarCyInt(CY, CY*);
    HRESULT VarCyMul(CY, CY, CY*);
    HRESULT VarCyMulI4(CY, LONG, CY*);
    HRESULT VarCyMulI8(CY, LONG64, CY*);
    HRESULT VarCyNeg(CY, CY*);
    HRESULT VarCyRound(CY, INT, CY*);
    HRESULT VarCySub(CY, CY, CY*);
    HRESULT VarAdd(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarAnd(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarCat(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarDiv(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarEqv(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarIdiv(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarImp(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarMod(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarMul(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarOr(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarPow(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarSub(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarXor(LPVARIANT, LPVARIANT, LPVARIANT);
    HRESULT VarAbs(LPVARIANT, LPVARIANT);
    HRESULT VarFix(LPVARIANT, LPVARIANT);
    HRESULT VarInt(LPVARIANT, LPVARIANT);
    HRESULT VarNeg(LPVARIANT, LPVARIANT);
    HRESULT VarNot(LPVARIANT, LPVARIANT);
    HRESULT VarRound(LPVARIANT, int, LPVARIANT);
    HRESULT VarCmp(LPVARIANT, LPVARIANT, LCID, ULONG);
    HRESULT VarBstrCmp(BSTR, BSTR, LCID, ULONG);
    HRESULT VarBstrCat(BSTR, BSTR, BSTR*);
}
