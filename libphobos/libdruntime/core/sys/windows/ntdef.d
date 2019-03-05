/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_ntdef.d)
 */
module core.sys.windows.ntdef;
version (Windows):

private import core.sys.windows.basetsd, core.sys.windows.subauth, core.sys.windows.windef, core.sys.windows.winnt;

enum uint
    OBJ_INHERIT          = 0x0002,
    OBJ_PERMANENT        = 0x0010,
    OBJ_EXCLUSIVE        = 0x0020,
    OBJ_CASE_INSENSITIVE = 0x0040,
    OBJ_OPENIF           = 0x0080,
    OBJ_OPENLINK         = 0x0100,
    OBJ_VALID_ATTRIBUTES = 0x01F2;

void InitializeObjectAttributes(OBJECT_ATTRIBUTES* p, UNICODE_STRING* n,
      uint a, HANDLE r, void* s) {
    with (*p) {
        Length = OBJECT_ATTRIBUTES.sizeof;
        RootDirectory = r;
        Attributes = a;
        ObjectName = n;
        SecurityDescriptor = s;
        SecurityQualityOfService = null;
    }
}

bool NT_SUCCESS(int x) { return x >= 0; }

/*  In MinGW, NTSTATUS, UNICODE_STRING, STRING and their associated pointer
 *  type aliases are defined in ntdef.h, ntsecapi.h and subauth.h, each of
 *  which checks that none of the others is already included.
 */
alias int  NTSTATUS;
alias int* PNTSTATUS;

struct UNICODE_STRING {
    USHORT Length;
    USHORT MaximumLength;
    PWSTR  Buffer;
}
alias UNICODE_STRING*        PUNICODE_STRING;
alias const(UNICODE_STRING)* PCUNICODE_STRING;

struct STRING {
    USHORT Length;
    USHORT MaximumLength;
    PCHAR  Buffer;
}
alias STRING  ANSI_STRING, OEM_STRING;
alias STRING* PSTRING, PANSI_STRING, POEM_STRING;

alias LARGE_INTEGER  PHYSICAL_ADDRESS;
alias LARGE_INTEGER* PPHYSICAL_ADDRESS;

enum SECTION_INHERIT {
    ViewShare = 1,
    ViewUnmap
}

/*  In MinGW, this is defined in ntdef.h and ntsecapi.h, each of which checks
 *  that the other isn't already included.
 */
struct OBJECT_ATTRIBUTES {
    ULONG           Length = OBJECT_ATTRIBUTES.sizeof;
    HANDLE          RootDirectory;
    PUNICODE_STRING ObjectName;
    ULONG           Attributes;
    PVOID           SecurityDescriptor;
    PVOID           SecurityQualityOfService;
}
alias OBJECT_ATTRIBUTES* POBJECT_ATTRIBUTES;
