/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_comcat.d)
 */
module core.sys.windows.comcat;
version (Windows):

import core.sys.windows.ole2;
private import core.sys.windows.basetyps, core.sys.windows.cguid, core.sys.windows.objbase, core.sys.windows.unknwn,
  core.sys.windows.windef, core.sys.windows.wtypes;

alias IEnumGUID LPENUMGUID;

interface IEnumGUID : IUnknown {
    HRESULT Next(ULONG, GUID*, ULONG*);
    HRESULT Skip(ULONG);
    HRESULT Reset();
    HRESULT Clone(LPENUMGUID*);
}

alias GUID CATID;
alias REFGUID REFCATID;
alias GUID_NULL CATID_NULL;
alias IsEqualGUID IsEqualCATID;

struct CATEGORYINFO {
    CATID        catid;
    LCID         lcid;
    OLECHAR[128] szDescription = 0;
}
alias CATEGORYINFO* LPCATEGORYINFO;

alias IEnumGUID IEnumCATID;
alias LPENUMGUID LPENUMCATID;
alias IID_IEnumGUID IID_IEnumCATID;

alias IEnumGUID IEnumCLSID;
alias LPENUMGUID LPENUMCLSID;
alias IID_IEnumGUID IID_IEnumCLSID;

interface ICatInformation : IUnknown {
    HRESULT EnumCategories(LCID, LPENUMCATEGORYINFO*);
    HRESULT GetCategoryDesc(REFCATID, LCID, PWCHAR*);
    HRESULT EnumClassesOfCategories(ULONG, CATID*, ULONG, CATID*,
      LPENUMCLSID*);
    HRESULT IsClassOfCategories(REFCLSID, ULONG, CATID*, ULONG, CATID*);
    HRESULT EnumImplCategoriesOfClass(REFCLSID, LPENUMCATID*);
    HRESULT EnumReqCategoriesOfClass(REFCLSID, LPENUMCATID*);
}
alias ICatInformation LPCATINFORMATION;

interface ICatRegister : IUnknown {
    HRESULT RegisterCategories(ULONG, CATEGORYINFO*);
    HRESULT UnRegisterCategories(ULONG, CATID*);
    HRESULT RegisterClassImplCategories(REFCLSID, ULONG, CATID*);
    HRESULT UnRegisterClassImplCategories(REFCLSID, ULONG, CATID*);
    HRESULT RegisterClassReqCategories(REFCLSID, ULONG, CATID*);
    HRESULT UnRegisterClassReqCategories(REFCLSID, ULONG, CATID*);
}
alias ICatRegister LPCATREGISTER;

interface IEnumCATEGORYINFO : IUnknown {
    HRESULT Next(ULONG, CATEGORYINFO*, ULONG*);
    HRESULT Skip(ULONG);
    HRESULT Reset();
    HRESULT Clone(LPENUMCATEGORYINFO*);
}
alias IEnumCATEGORYINFO LPENUMCATEGORYINFO;
