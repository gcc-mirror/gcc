/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 3.10
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_aclui.d)
 */
module core.sys.windows.aclui;
version (Windows):
pragma(lib, "aclui");

import core.sys.windows.w32api;
/*
static assert (_WIN32_WINNT >= 0x500,
    "core.sys.windows.aclui is available only if version Windows2000, WindowsXP, Windows2003 "
    "or WindowsVista is set");
*/
import core.sys.windows.accctrl, core.sys.windows.commctrl, core.sys.windows.objbase;
import core.sys.windows.basetyps, core.sys.windows.prsht, core.sys.windows.unknwn, core.sys.windows.windef,
  core.sys.windows.winuser;


struct SI_OBJECT_INFO {
    DWORD     dwFlags;
    HINSTANCE hInstance;
    LPWSTR    pszServerName;
    LPWSTR    pszObjectName;
    LPWSTR    pszPageTitle;
    GUID      guidObjectType;
}
alias SI_OBJECT_INFO* PSI_OBJECT_INFO;

// values for SI_OBJECT_INFO.dwFlags
enum DWORD
    SI_EDIT_PERMS               = 0x00000000,
    SI_EDIT_OWNER               = 0x00000001,
    SI_EDIT_AUDITS              = 0x00000002,
    SI_CONTAINER                = 0x00000004,
    SI_READONLY                 = 0x00000008,
    SI_ADVANCED                 = 0x00000010,
    SI_RESET                    = 0x00000020,
    SI_OWNER_READONLY           = 0x00000040,
    SI_EDIT_PROPERTIES          = 0x00000080,
    SI_OWNER_RECURSE            = 0x00000100,
    SI_NO_ACL_PROTECT           = 0x00000200,
    SI_NO_TREE_APPLY            = 0x00000400,
    SI_PAGE_TITLE               = 0x00000800,
    SI_SERVER_IS_DC             = 0x00001000,
    SI_RESET_DACL_TREE          = 0x00004000,
    SI_RESET_SACL_TREE          = 0x00008000,
    SI_OBJECT_GUID              = 0x00010000,
    SI_EDIT_EFFECTIVE           = 0x00020000,
    SI_RESET_DACL               = 0x00040000,
    SI_RESET_SACL               = 0x00080000,
    SI_RESET_OWNER              = 0x00100000,
    SI_NO_ADDITIONAL_PERMISSION = 0x00200000,
    SI_MAY_WRITE                = 0x10000000,
    SI_EDIT_ALL                 = SI_EDIT_PERMS | SI_EDIT_OWNER
                                  | SI_EDIT_AUDITS;

struct SI_ACCESS {
const(GUID)* pguid;
    ACCESS_MASK  mask;
    LPCWSTR      pszName;
    DWORD        dwFlags;
}
alias SI_ACCESS* PSI_ACCESS;

// values for SI_ACCESS.dwFlags
enum DWORD
    SI_ACCESS_SPECIFIC  = 0x00010000,
    SI_ACCESS_GENERAL   = 0x00020000,
    SI_ACCESS_CONTAINER = 0x00040000,
    SI_ACCESS_PROPERTY  = 0x00080000;


struct SI_INHERIT_TYPE {
const(GUID)* pguid;
    ULONG        dwFlags;
    LPCWSTR      pszName;
}
alias SI_INHERIT_TYPE* PSI_INHERIT_TYPE;

/* values for SI_INHERIT_TYPE.dwFlags
   INHERIT_ONLY_ACE, CONTAINER_INHERIT_ACE, OBJECT_INHERIT_ACE
   defined elsewhere */

enum SI_PAGE_TYPE {
    SI_PAGE_PERM,
    SI_PAGE_ADVPERM,
    SI_PAGE_AUDIT,
    SI_PAGE_OWNER
}

enum uint PSPCB_SI_INITDIALOG = WM_USER + 1;

interface ISecurityInformation : IUnknown {
    HRESULT GetObjectInformation(PSI_OBJECT_INFO);
    HRESULT GetSecurity(SECURITY_INFORMATION, PSECURITY_DESCRIPTOR*, BOOL);
    HRESULT SetSecurity(SECURITY_INFORMATION, PSECURITY_DESCRIPTOR);
    HRESULT GetAccessRights(const(GUID)*, DWORD, PSI_ACCESS*, ULONG*, ULONG*);
    HRESULT MapGeneric(const(GUID)*, UCHAR*, ACCESS_MASK*);
    HRESULT GetInheritTypes(PSI_INHERIT_TYPE*, ULONG*);
    HRESULT PropertySheetPageCallback(HWND, UINT, SI_PAGE_TYPE);
}
alias ISecurityInformation LPSECURITYINFO;

/* Comment from MinGW
 * TODO: ISecurityInformation2, IEffectivePermission, ISecurityObjectTypeInfo
 */

// FIXME: linkage attribute?
extern (C) /+DECLSPEC_IMPORT+/ extern const IID IID_ISecurityInformation;

extern (Windows) {
    HPROPSHEETPAGE CreateSecurityPage(LPSECURITYINFO psi);
    BOOL EditSecurity(HWND hwndOwner, LPSECURITYINFO psi);
}
