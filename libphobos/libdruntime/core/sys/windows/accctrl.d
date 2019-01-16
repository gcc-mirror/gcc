/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_accctrl.d)
 */
module core.sys.windows.accctrl;
version (Windows):

version (ANSI) {} else version = Unicode;

private import core.sys.windows.basetyps, core.sys.windows.w32api, core.sys.windows.winbase, core.sys.windows.windef;

// FIXME: check types and grouping of constants
// FIXME: check Windows version support

alias LocalFree AccFree;

enum uint
    ACTRL_RESERVED            = 0x00000000,
    ACTRL_ACCESS_PROTECTED    = 0x00000001,
    ACTRL_ACCESS_ALLOWED      = 0x00000001,
    ACTRL_ACCESS_DENIED       = 0x00000002,
    ACTRL_AUDIT_SUCCESS       = 0x00000004,
    ACTRL_AUDIT_FAILURE       = 0x00000008,
    ACTRL_SYSTEM_ACCESS       = 0x04000000,
    ACTRL_DELETE              = 0x08000000,
    ACTRL_READ_CONTROL        = 0x10000000,
    ACTRL_CHANGE_ACCESS       = 0x20000000,
    ACTRL_CHANGE_OWNER        = 0x40000000,
    ACTRL_SYNCHRONIZE         = 0x80000000,
    ACTRL_STD_RIGHTS_ALL      = 0xf8000000;

enum uint
    ACTRL_FILE_READ           = 0x00000001,
    ACTRL_FILE_WRITE          = 0x00000002,
    ACTRL_FILE_APPEND         = 0x00000004,
    ACTRL_FILE_READ_PROP      = 0x00000008,
    ACTRL_FILE_WRITE_PROP     = 0x00000010,
    ACTRL_FILE_EXECUTE        = 0x00000020,
    ACTRL_FILE_READ_ATTRIB    = 0x00000080,
    ACTRL_FILE_WRITE_ATTRIB   = 0x00000100,
    ACTRL_FILE_CREATE_PIPE    = 0x00000200;

enum uint
    ACTRL_DIR_LIST            = 0x00000001,
    ACTRL_DIR_CREATE_OBJECT   = 0x00000002,
    ACTRL_DIR_CREATE_CHILD    = 0x00000004,
    ACTRL_DIR_DELETE_CHILD    = 0x00000040,
    ACTRL_DIR_TRAVERSE        = 0x00000020;

enum uint
    ACTRL_KERNEL_TERMINATE    = 0x00000001,
    ACTRL_KERNEL_THREAD       = 0x00000002,
    ACTRL_KERNEL_VM           = 0x00000004,
    ACTRL_KERNEL_VM_READ      = 0x00000008,
    ACTRL_KERNEL_VM_WRITE     = 0x00000010,
    ACTRL_KERNEL_DUP_HANDLE   = 0x00000020,
    ACTRL_KERNEL_PROCESS      = 0x00000040,
    ACTRL_KERNEL_SET_INFO     = 0x00000080,
    ACTRL_KERNEL_GET_INFO     = 0x00000100,
    ACTRL_KERNEL_CONTROL      = 0x00000200,
    ACTRL_KERNEL_ALERT        = 0x00000400,
    ACTRL_KERNEL_GET_CONTEXT  = 0x00000800,
    ACTRL_KERNEL_SET_CONTEXT  = 0x00001000,
    ACTRL_KERNEL_TOKEN        = 0x00002000,
    ACTRL_KERNEL_IMPERSONATE  = 0x00004000,
    ACTRL_KERNEL_DIMPERSONATE = 0x00008000;

enum uint
    ACTRL_PRINT_SADMIN        = 0x00000001,
    ACTRL_PRINT_SLIST         = 0x00000002,
    ACTRL_PRINT_PADMIN        = 0x00000004,
    ACTRL_PRINT_PUSE          = 0x00000008,
    ACTRL_PRINT_JADMIN        = 0x00000010;

enum uint
    ACTRL_SVC_GET_INFO        = 0x00000001,
    ACTRL_SVC_SET_INFO        = 0x00000002,
    ACTRL_SVC_STATUS          = 0x00000004,
    ACTRL_SVC_LIST            = 0x00000008,
    ACTRL_SVC_START           = 0x00000010,
    ACTRL_SVC_STOP            = 0x00000020,
    ACTRL_SVC_PAUSE           = 0x00000040,
    ACTRL_SVC_INTERROGATE     = 0x00000080,
    ACTRL_SVC_UCONTROL        = 0x00000100;

enum uint
    ACTRL_REG_QUERY           = 0x00000001,
    ACTRL_REG_SET             = 0x00000002,
    ACTRL_REG_CREATE_CHILD    = 0x00000004,
    ACTRL_REG_LIST            = 0x00000008,
    ACTRL_REG_NOTIFY          = 0x00000010,
    ACTRL_REG_LINK            = 0x00000020;

enum uint
    ACTRL_WIN_CLIPBRD         = 0x00000001,
    ACTRL_WIN_GLOBAL_ATOMS    = 0x00000002,
    ACTRL_WIN_CREATE          = 0x00000004,
    ACTRL_WIN_LIST_DESK       = 0x00000008,
    ACTRL_WIN_LIST            = 0x00000010,
    ACTRL_WIN_READ_ATTRIBS    = 0x00000020,
    ACTRL_WIN_WRITE_ATTRIBS   = 0x00000040,
    ACTRL_WIN_SCREEN          = 0x00000080,
    ACTRL_WIN_EXIT            = 0x00000100;

enum : uint {
    ACTRL_ACCESS_NO_OPTIONS              = 0x00000000,
    ACTRL_ACCESS_SUPPORTS_OBJECT_ENTRIES = 0x00000001
}

const TCHAR[] ACCCTRL_DEFAULT_PROVIDER = "Windows NT Access Provider";

enum uint
    TRUSTEE_ACCESS_ALLOWED    = 0x00000001,
    TRUSTEE_ACCESS_READ       = 0x00000002,
    TRUSTEE_ACCESS_WRITE      = 0x00000004,
    TRUSTEE_ACCESS_EXPLICIT   = 0x00000001,
    TRUSTEE_ACCESS_READ_WRITE = 0x00000006,
    TRUSTEE_ACCESS_ALL        = 0xFFFFFFFF;

enum uint
    NO_INHERITANCE                     = 0x0,
    SUB_OBJECTS_ONLY_INHERIT           = 0x1,
    SUB_CONTAINERS_ONLY_INHERIT        = 0x2,
    SUB_CONTAINERS_AND_OBJECTS_INHERIT = 0x3,
    INHERIT_NO_PROPAGATE               = 0x4,
    INHERIT_ONLY                       = 0x8,
    INHERITED_ACCESS_ENTRY             = 0x10,
    INHERITED_PARENT                   = 0x10000000,
    INHERITED_GRANDPARENT              = 0x20000000;

alias ULONG INHERIT_FLAGS, ACCESS_RIGHTS;
alias ULONG* PINHERIT_FLAGS, PACCESS_RIGHTS;

enum ACCESS_MODE {
    NOT_USED_ACCESS,
    GRANT_ACCESS,
    SET_ACCESS,
    DENY_ACCESS,
    REVOKE_ACCESS,
    SET_AUDIT_SUCCESS,
    SET_AUDIT_FAILURE
}

enum SE_OBJECT_TYPE {
    SE_UNKNOWN_OBJECT_TYPE,
    SE_FILE_OBJECT,
    SE_SERVICE,
    SE_PRINTER,
    SE_REGISTRY_KEY,
    SE_LMSHARE,
    SE_KERNEL_OBJECT,
    SE_WINDOW_OBJECT,
    SE_DS_OBJECT,
    SE_DS_OBJECT_ALL,
    SE_PROVIDER_DEFINED_OBJECT,
    SE_WMIGUID_OBJECT,
    SE_REGISTRY_WOW64_32KEY
}

enum TRUSTEE_TYPE {
    TRUSTEE_IS_UNKNOWN,
    TRUSTEE_IS_USER,
    TRUSTEE_IS_GROUP,
    TRUSTEE_IS_DOMAIN,
    TRUSTEE_IS_ALIAS,
    TRUSTEE_IS_WELL_KNOWN_GROUP,
    TRUSTEE_IS_DELETED,
    TRUSTEE_IS_INVALID,
    TRUSTEE_IS_COMPUTER
}

enum TRUSTEE_FORM {
    TRUSTEE_IS_SID,
    TRUSTEE_IS_NAME,
    TRUSTEE_BAD_FORM,
    TRUSTEE_IS_OBJECTS_AND_SID,
    TRUSTEE_IS_OBJECTS_AND_NAME
}

enum MULTIPLE_TRUSTEE_OPERATION {
    NO_MULTIPLE_TRUSTEE,
    TRUSTEE_IS_IMPERSONATE
}

struct TRUSTEE_A {
    TRUSTEE_A*                 pMultipleTrustee;
    MULTIPLE_TRUSTEE_OPERATION MultipleTrusteeOperation;
    TRUSTEE_FORM               TrusteeForm;
    TRUSTEE_TYPE               TrusteeType;
    LPSTR                      ptstrName;
}
alias TRUSTEE_A TRUSTEEA;
alias TRUSTEE_A* PTRUSTEE_A, PTRUSTEEA;

struct TRUSTEE_W {
    TRUSTEE_W*                 pMultipleTrustee;
    MULTIPLE_TRUSTEE_OPERATION MultipleTrusteeOperation;
    TRUSTEE_FORM               TrusteeForm;
    TRUSTEE_TYPE               TrusteeType;
    LPWSTR                     ptstrName;
}
alias TRUSTEE_W TRUSTEEW;
alias TRUSTEEW* PTRUSTEE_W, PTRUSTEEW;

struct ACTRL_ACCESS_ENTRYA {
    TRUSTEE_A     Trustee;
    ULONG         fAccessFlags;
    ACCESS_RIGHTS Access;
    ACCESS_RIGHTS ProvSpecificAccess;
    INHERIT_FLAGS Inheritance;
    LPCSTR        lpInheritProperty;
}
alias ACTRL_ACCESS_ENTRYA* PACTRL_ACCESS_ENTRYA;

struct ACTRL_ACCESS_ENTRYW {
    TRUSTEE_W     Trustee;
    ULONG         fAccessFlags;
    ACCESS_RIGHTS Access;
    ACCESS_RIGHTS ProvSpecificAccess;
    INHERIT_FLAGS Inheritance;
    LPCWSTR       lpInheritProperty;
}
alias ACTRL_ACCESS_ENTRYW* PACTRL_ACCESS_ENTRYW;

struct ACTRL_ACCESS_ENTRY_LISTA {
    ULONG                cEntries;
    ACTRL_ACCESS_ENTRYA* pAccessList;
}
alias ACTRL_ACCESS_ENTRY_LISTA* PACTRL_ACCESS_ENTRY_LISTA;

struct ACTRL_ACCESS_ENTRY_LISTW {
    ULONG                cEntries;
    ACTRL_ACCESS_ENTRYW* pAccessList;
}
alias ACTRL_ACCESS_ENTRY_LISTW* PACTRL_ACCESS_ENTRY_LISTW;

struct ACTRL_PROPERTY_ENTRYA {
    LPCSTR                    lpProperty;
    PACTRL_ACCESS_ENTRY_LISTA pAccessEntryList;
    ULONG                     fListFlags;
}
alias ACTRL_PROPERTY_ENTRYA* PACTRL_PROPERTY_ENTRYA;

struct ACTRL_PROPERTY_ENTRYW {
    LPCWSTR                   lpProperty;
    PACTRL_ACCESS_ENTRY_LISTW pAccessEntryList;
    ULONG                     fListFlags;
}
alias ACTRL_PROPERTY_ENTRYW* PACTRL_PROPERTY_ENTRYW;

struct ACTRL_ACCESSA {
    ULONG                  cEntries;
    PACTRL_PROPERTY_ENTRYA pPropertyAccessList;
}
alias ACTRL_ACCESSA ACTRL_AUDITA;
alias ACTRL_ACCESSA* PACTRL_ACCESSA, PACTRL_AUDITA;

struct ACTRL_ACCESSW {
    ULONG                  cEntries;
    PACTRL_PROPERTY_ENTRYW pPropertyAccessList;
}
alias ACTRL_ACCESSW ACTRL_AUDITW;
alias ACTRL_ACCESSW* PACTRL_ACCESSW, PACTRL_AUDITW;

struct TRUSTEE_ACCESSA {
    LPSTR         lpProperty;
    ACCESS_RIGHTS Access;
    ULONG         fAccessFlags;
    ULONG         fReturnedAccess;
}
alias TRUSTEE_ACCESSA* PTRUSTEE_ACCESSA;

struct TRUSTEE_ACCESSW {
    LPWSTR        lpProperty;
    ACCESS_RIGHTS Access;
    ULONG         fAccessFlags;
    ULONG         fReturnedAccess;
}
alias TRUSTEE_ACCESSW* PTRUSTEE_ACCESSW;

struct ACTRL_OVERLAPPED {
    union {
        PVOID Provider;
        ULONG Reserved1;
    }
    ULONG     Reserved2;
    HANDLE    hEvent;
}
alias ACTRL_OVERLAPPED* PACTRL_OVERLAPPED;

struct ACTRL_ACCESS_INFOA {
    ULONG fAccessPermission;
    LPSTR lpAccessPermissionName;
}
alias ACTRL_ACCESS_INFOA* PACTRL_ACCESS_INFOA;

struct ACTRL_ACCESS_INFOW {
    ULONG  fAccessPermission;
    LPWSTR lpAccessPermissionName;
}
alias ACTRL_ACCESS_INFOW* PACTRL_ACCESS_INFOW;

struct ACTRL_CONTROL_INFOA {
    LPSTR lpControlId;
    LPSTR lpControlName;
}
alias ACTRL_CONTROL_INFOA* PACTRL_CONTROL_INFOA;

struct ACTRL_CONTROL_INFOW {
    LPWSTR lpControlId;
    LPWSTR lpControlName;
}
alias ACTRL_CONTROL_INFOW* PACTRL_CONTROL_INFOW;

struct EXPLICIT_ACCESS_A {
    DWORD       grfAccessPermissions;
    ACCESS_MODE grfAccessMode;
    DWORD       grfInheritance;
    TRUSTEE_A   Trustee;
}
alias EXPLICIT_ACCESS_A EXPLICIT_ACCESSA;
alias EXPLICIT_ACCESS_A* PEXPLICIT_ACCESS_A, PEXPLICIT_ACCESSA;

struct EXPLICIT_ACCESS_W {
    DWORD       grfAccessPermissions;
    ACCESS_MODE grfAccessMode;
    DWORD       grfInheritance;
    TRUSTEE_W   Trustee;
}
alias EXPLICIT_ACCESS_W EXPLICIT_ACCESSW;
alias EXPLICIT_ACCESS_W* PEXPLICIT_ACCESS_W, PEXPLICIT_ACCESSW;

struct OBJECTS_AND_SID {
    DWORD ObjectsPresent;
    GUID  ObjectTypeGuid;
    GUID  InheritedObjectTypeGuid;
    SID*  pSid;
}
alias OBJECTS_AND_SID* POBJECTS_AND_SID;

struct OBJECTS_AND_NAME_A {
    DWORD          ObjectsPresent;
    SE_OBJECT_TYPE ObjectType;
    LPSTR          ObjectTypeName;
    LPSTR          InheritedObjectTypeName;
    LPSTR          ptstrName;
}
alias OBJECTS_AND_NAME_A* POBJECTS_AND_NAME_A;

struct OBJECTS_AND_NAME_W {
    DWORD          ObjectsPresent;
    SE_OBJECT_TYPE ObjectType;
    LPWSTR         ObjectTypeName;
    LPWSTR         InheritedObjectTypeName;
    LPWSTR         ptstrName;
}
alias OBJECTS_AND_NAME_W* POBJECTS_AND_NAME_W;

static if (_WIN32_WINNT >= 0x501) {
    struct INHERITED_FROMA {
        LONG  GenerationGap;
        LPSTR AncestorName;
    }
    alias INHERITED_FROMA* PINHERITED_FROMA;

    struct INHERITED_FROMW {
        LONG   GenerationGap;
        LPWSTR AncestorName;
    }
    alias INHERITED_FROMW* PINHERITED_FROMW;
}

version (Unicode) {
    alias TRUSTEEW TRUSTEE;
    alias ACTRL_ACCESSW ACTRL_ACCESS;
    alias ACTRL_ACCESS_ENTRY_LISTW ACTRL_ACCESS_ENTRY_LIST;
    alias ACTRL_ACCESS_INFOW ACTRL_ACCESS_INFO;
    alias ACTRL_ACCESS_ENTRYW ACTRL_ACCESS_ENTRY;
    alias ACTRL_AUDITW ACTRL_AUDIT;
    alias ACTRL_CONTROL_INFOW ACTRL_CONTROL_INFO;
    alias EXPLICIT_ACCESSW EXPLICIT_ACCESS;
    alias TRUSTEE_ACCESSW TRUSTEE_ACCESS;
    alias OBJECTS_AND_NAME_W OBJECTS_AND_NAME_;
    static if (_WIN32_WINNT >= 0x501) {
        alias INHERITED_FROMW INHERITED_FROM;
    }
} else {
    alias TRUSTEEA TRUSTEE;
    alias ACTRL_ACCESSA ACTRL_ACCESS;
    alias ACTRL_ACCESS_ENTRY_LISTA ACTRL_ACCESS_ENTRY_LIST;
    alias ACTRL_ACCESS_INFOA ACTRL_ACCESS_INFO;
    alias ACTRL_ACCESS_ENTRYA ACTRL_ACCESS_ENTRY;
    alias ACTRL_AUDITA ACTRL_AUDIT;
    alias ACTRL_CONTROL_INFOA ACTRL_CONTROL_INFO;
    alias EXPLICIT_ACCESSA EXPLICIT_ACCESS;
    alias TRUSTEE_ACCESSA TRUSTEE_ACCESS;
    alias OBJECTS_AND_NAME_A OBJECTS_AND_NAME_;
    static if (_WIN32_WINNT >= 0x501) {
        alias INHERITED_FROMA INHERITED_FROM;
    }
}

alias TRUSTEE TRUSTEE_;
alias TRUSTEE* PTRUSTEE, PTRUSTEE_;
alias ACTRL_ACCESS* PACTRL_ACCESS;
alias ACTRL_ACCESS_ENTRY_LIST* PACTRL_ACCESS_ENTRY_LIST;
alias ACTRL_ACCESS_INFO* PACTRL_ACCESS_INFO;
alias ACTRL_ACCESS_ENTRY* PACTRL_ACCESS_ENTRY;
alias ACTRL_AUDIT* PACTRL_AUDIT;
alias ACTRL_CONTROL_INFO* PACTRL_CONTROL_INFO;
alias EXPLICIT_ACCESS EXPLICIT_ACCESS_;
alias EXPLICIT_ACCESS* PEXPLICIT_ACCESS, PEXPLICIT_ACCESS_;
alias TRUSTEE_ACCESS* PTRUSTEE_ACCESS;
alias OBJECTS_AND_NAME_* POBJECTS_AND_NAME_;
static if (_WIN32_WINNT >= 0x501) {
    alias INHERITED_FROM* PINHERITED_FROM;
}
