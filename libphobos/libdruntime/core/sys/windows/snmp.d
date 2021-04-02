/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_snmp.d)
 */
module core.sys.windows.snmp;
version (Windows):
@system:

import core.sys.windows.basetsd /+: HANDLE+/;
import core.sys.windows.windef /+: BOOL, BYTE, DWORD, INT, LONG, UINT, ULONG+/;
import core.sys.windows.winnt /+: LPSTR, LPVOID, ULARGE_INTEGER, VOID+/;

// These are not documented on MSDN
enum {
    DEFAULT_SNMP_PORT_UDP     =   161,
    DEFAULT_SNMP_PORT_IPX     = 36879,
    DEFAULT_SNMPTRAP_PORT_UDP =   162,
    DEFAULT_SNMPTRAP_PORT_IPX = 36880
}

enum : BYTE {
    ASN_UNIVERSAL                 = 0x00,
    ASN_PRIMITIVE                 = 0x00,
    ASN_CONSTRUCTOR               = 0x20,
    ASN_APPLICATION               = 0x40,
    ASN_CONTEXT                   = 0x80,
    ASN_PRIVATE                   = 0xC0,

    SNMP_PDU_GET                  = ASN_CONTEXT | ASN_CONSTRUCTOR,
    SNMP_PDU_GETNEXT,
    SNMP_PDU_RESPONSE,
    SNMP_PDU_SET,
    SNMP_PDU_GETBULK,          // = ASN_CONTEXT | ASN_CONSTRUCTOR | 4
    SNMP_PDU_V1TRAP               = ASN_CONTEXT | ASN_CONSTRUCTOR | 4,
    SNMP_PDU_INFORM               = ASN_CONTEXT | ASN_CONSTRUCTOR | 6,
    SNMP_PDU_TRAP,
    SNMP_PDU_REPORT,
    ASN_INTEGER                   = ASN_UNIVERSAL | ASN_PRIMITIVE | 2,
    ASN_BITS,
    ASN_OCTETSTRING,
    ASN_NULL,
    ASN_OBJECTIDENTIFIER,      // = ASN_UNIVERSAL | ASN_PRIMITIVE | 6
    ASN_INTEGER32                 = ASN_INTEGER,
    ASN_SEQUENCE                  = ASN_UNIVERSAL | ASN_CONSTRUCTOR | 0x10,
    ASN_SEQUENCEOF                = ASN_SEQUENCE,
    ASN_IPADDRESS                 = ASN_APPLICATION | ASN_PRIMITIVE,
    ASN_COUNTER32,
    ASN_GAUGE32,
    ASN_TIMETICKS,
    ASN_OPAQUE,                // = ASN_APPLICATION | ASN_PRIMITIVE | 4
    ASN_COUNTER64                 = ASN_APPLICATION | ASN_PRIMITIVE | 6,
    ASN_UNSIGNED32,            // = ASN_APPLICATION | ASN_PRIMITIVE | 7
    SNMP_EXCEPTION_NOSUCHOBJECT   = ASN_CONTEXT | ASN_PRIMITIVE,
    SNMP_EXCEPTION_NOSUCHINSTANCE,
    SNMP_EXCEPTION_ENDOFMIBVIEW,
    SNMP_EXTENSION_GET            = SNMP_PDU_GET,
    SNMP_EXTENSION_GET_NEXT       = SNMP_PDU_GETNEXT,
    SNMP_EXTENSION_GET_BULK       = SNMP_PDU_GETBULK,
    SNMP_EXTENSION_SET_TEST       = ASN_PRIVATE | ASN_CONSTRUCTOR,
    SNMP_EXTENSION_SET_COMMIT     = SNMP_PDU_SET,
    SNMP_EXTENSION_SET_UNDO       = ASN_PRIVATE | ASN_CONSTRUCTOR | 1,
    SNMP_EXTENSION_SET_CLEANUP
}


enum : AsnInteger {
    SNMP_ERRORSTATUS_NOERROR,
    SNMP_ERRORSTATUS_TOOBIG,
    SNMP_ERRORSTATUS_NOSUCHNAME,
    SNMP_ERRORSTATUS_BADVALUE,
    SNMP_ERRORSTATUS_READONLY,
    SNMP_ERRORSTATUS_GENERR,
    SNMP_ERRORSTATUS_NOACCESS,
    SNMP_ERRORSTATUS_WRONGTYPE,
    SNMP_ERRORSTATUS_WRONGLENGTH,
    SNMP_ERRORSTATUS_WRONGENCODING,
    SNMP_ERRORSTATUS_WRONGVALUE,
    SNMP_ERRORSTATUS_NOCREATION,
    SNMP_ERRORSTATUS_INCONSISTENTVALUE,
    SNMP_ERRORSTATUS_RESOURCEUNAVAILABLE,
    SNMP_ERRORSTATUS_COMMITFAILED,
    SNMP_ERRORSTATUS_UNDOFAILED,
    SNMP_ERRORSTATUS_AUTHORIZATIONERROR,
    SNMP_ERRORSTATUS_NOTWRITABLE,
    SNMP_ERRORSTATUS_INCONSISTENTNAME
}

enum : AsnInteger {
    SNMP_GENERICTRAP_COLDSTART,
    SNMP_GENERICTRAP_WARMSTART,
    SNMP_GENERICTRAP_LINKDOWN,
    SNMP_GENERICTRAP_LINKUP,
    SNMP_GENERICTRAP_AUTHFAILURE,
    SNMP_GENERICTRAP_EGPNEIGHLOSS,
    SNMP_GENERICTRAP_ENTERSPECIFIC
}

// These are not documented on MSDN
enum {
    SNMP_ACCESS_NONE,
    SNMP_ACCESS_NOTIFY,
    SNMP_ACCESS_READ_ONLY,
    SNMP_ACCESS_READ_WRITE,
    SNMP_ACCESS_READ_CREATE
}

enum : BOOL {
    SNMPAPI_ERROR   = false,
    SNMPAPI_NOERROR = true
}

enum : INT {
    SNMP_LOG_SILENT,
    SNMP_LOG_FATAL,
    SNMP_LOG_ERROR,
    SNMP_LOG_WARNING,
    SNMP_LOG_TRACE,
    SNMP_LOG_VERBOSE
}

enum INT
    SNMP_OUTPUT_TO_CONSOLE  = 1,
    SNMP_OUTPUT_TO_LOGFILE  = 2,
    SNMP_OUTPUT_TO_EVENTLOG = 4,
    SNMP_OUTPUT_TO_DEBUGGER = 8;

enum size_t SNMP_MAX_OID_LEN = 128;

enum : DWORD {
    SNMP_MEM_ALLOC_ERROR          =  1,
    SNMP_BERAPI_INVALID_LENGTH    = 10,
    SNMP_BERAPI_INVALID_TAG,
    SNMP_BERAPI_OVERFLOW,
    SNMP_BERAPI_SHORT_BUFFER,
    SNMP_BERAPI_INVALID_OBJELEM,
    SNMP_PDUAPI_UNRECOGNIZED_PDU  = 20,
    SNMP_PDUAPI_INVALID_ES,
    SNMP_PDUAPI_INVALID_GT,
    SNMP_AUTHAPI_INVALID_VERSION  = 30,
    SNMP_AUTHAPI_INVALID_MSG_TYPE,
    SNMP_AUTHAPI_TRIV_AUTH_FAILED,
}

alias INT SNMPAPI;
alias LONG AsnInteger32;
alias ULONG AsnUnsigned32, AsnCounter32, AsnGauge32, AsnTimeticks;
alias ULARGE_INTEGER AsnCounter64;

align (4):

struct AsnOctetString {
align (4):
    BYTE* stream;
    UINT  length;
    BOOL  dynamic;
}
alias AsnOctetString AsnBits, AsnSequence, AsnImplicitSequence,
  AsnIPAddress, AsnNetworkAddress, AsnDisplayString, AsnOpaque;

struct AsnObjectIdentifier {
align (4):
    UINT  idLength;
    UINT* ids;
}
alias AsnObjectIdentifier AsnObjectName;

struct AsnAny {
align (4):
    BYTE      asnType;
    union _asnValue {
        AsnInteger32        number;
        AsnUnsigned32       unsigned32;
        AsnCounter64        counter64;
        AsnOctetString      string;
        AsnBits             bits;
        AsnObjectIdentifier object;
        AsnSequence         sequence;
        AsnIPAddress        address;
        AsnCounter32        counter;
        AsnGauge32          gauge;
        AsnTimeticks        ticks;
        AsnOpaque           arbitrary;
    }
    _asnValue asnValue;
}
alias AsnAny AsnObjectSyntax;

struct SnmpVarBind {
align (4):
    AsnObjectName   name;
    AsnObjectSyntax value;
}

struct SnmpVarBindList {
align (4):
    SnmpVarBind* list;
    UINT         len;
}

extern (Windows) {
    VOID SnmpExtensionClose();
    BOOL SnmpExtensionInit(DWORD, HANDLE*, AsnObjectIdentifier*);
    BOOL SnmpExtensionInitEx(AsnObjectIdentifier*);
    BOOL SnmpExtensionMonitor(LPVOID);
    BOOL SnmpExtensionQuery(BYTE, SnmpVarBindList*, AsnInteger32*,
      AsnInteger32*);
    BOOL SnmpExtensionQueryEx(DWORD, DWORD, SnmpVarBindList*, AsnOctetString*,
      AsnInteger32*, AsnInteger32*);
    BOOL SnmpExtensionTrap(AsnObjectIdentifier*, AsnInteger32*, AsnInteger32*,
      AsnTimeticks*, SnmpVarBindList*);
    DWORD SnmpSvcGetUptime();
    VOID SnmpSvcSetLogLevel(INT);
    VOID SnmpSvcSetLogType(INT);
    SNMPAPI SnmpUtilAsnAnyCpy(AsnAny*, AsnAny*);
    VOID SnmpUtilAsnAnyFree(AsnAny*);
    VOID SnmpUtilDbgPrint(INT, LPSTR, ...);
    LPSTR SnmpUtilIdsToA(UINT*, UINT);
    LPVOID SnmpUtilMemAlloc(UINT);
    VOID SnmpUtilMemFree(LPVOID);
    LPVOID SnmpUtilMemReAlloc(LPVOID, UINT);
    SNMPAPI SnmpUtilOctetsCmp(AsnOctetString*, AsnOctetString*);
    SNMPAPI SnmpUtilOctetsCpy(AsnOctetString*, AsnOctetString*);
    VOID SnmpUtilOctetsFree(AsnOctetString*);
    SNMPAPI SnmpUtilOctetsNCmp(AsnOctetString*, AsnOctetString*, UINT);
    SNMPAPI SnmpUtilOidAppend(AsnObjectIdentifier*, AsnObjectIdentifier*);
    SNMPAPI SnmpUtilOidCmp(AsnObjectIdentifier*, AsnObjectIdentifier*);
    SNMPAPI SnmpUtilOidCpy(AsnObjectIdentifier*, AsnObjectIdentifier*);
    VOID SnmpUtilOidFree(AsnObjectIdentifier*);
    SNMPAPI SnmpUtilOidNCmp(AsnObjectIdentifier*, AsnObjectIdentifier*, UINT);
    LPSTR SnmpUtilOidToA(AsnObjectIdentifier*);
    VOID SnmpUtilPrintAsnAny(AsnAny*);
    VOID SnmpUtilPrintOid(AsnObjectIdentifier*);
    SNMPAPI SnmpUtilVarBindCpy(SnmpVarBind*, SnmpVarBind*);
    SNMPAPI SnmpUtilVarBindListCpy(SnmpVarBindList*, SnmpVarBindList*);
    VOID SnmpUtilVarBindFree(SnmpVarBind*);
    VOID SnmpUtilVarBindListFree(SnmpVarBindList*);
}

alias SnmpUtilMemAlloc SNMP_malloc;
alias SnmpUtilMemFree SNMP_free;
alias SnmpUtilMemReAlloc SNMP_realloc;
alias SnmpUtilMemAlloc SNMP_DBG_malloc;
alias SnmpUtilMemFree SNMP_DBG_free;
alias SnmpUtilMemReAlloc SNMP_DBG_realloc;
alias SnmpUtilOidAppend SNMP_oidappend;
alias SnmpUtilOidCmp SNMP_oidcmp;
alias SnmpUtilOidCpy SNMP_oidcpy;
alias SnmpUtilOidFree SNMP_oidfree;
alias SnmpUtilOidNCmp SNMP_oidncmp;
alias SnmpUtilPrintAsnAny SNMP_printany;
alias SnmpUtilVarBindCpy SNMP_CopyVarBind;
alias SnmpUtilVarBindListCpy SNMP_CopyVarBindList;
alias SnmpUtilVarBindFree SNMP_FreeVarBind;
alias SnmpUtilVarBindListFree SNMP_FreeVarBindList;
alias ASN_IPADDRESS ASN_RFC1155_IPADDRESS;
alias ASN_COUNTER32 ASN_RFC1155_COUNTER;
alias ASN_GAUGE32 ASN_RFC1155_GAUGE;
alias ASN_TIMETICKS ASN_RFC1155_TIMETICKS;
alias ASN_OPAQUE ASN_RFC1155_OPAQUE;
alias ASN_OCTETSTRING ASN_RFC1213_DISPSTRING;
alias SNMP_PDU_GET ASN_RFC1157_GETREQUEST;
alias SNMP_PDU_GETNEXT ASN_RFC1157_GETNEXTREQUEST;
alias SNMP_PDU_RESPONSE ASN_RFC1157_GETRESPONSE;
alias SNMP_PDU_SET ASN_RFC1157_SETREQUEST;
alias SNMP_PDU_V1TRAP ASN_RFC1157_TRAP;
alias ASN_CONTEXT ASN_CONTEXTSPECIFIC;
alias ASN_PRIMITIVE ASN_PRIMATIVE;
alias SnmpVarBindList RFC1157VarBindList;
alias SnmpVarBind RFC1157VarBind;
alias AsnInteger32 AsnInteger;
alias AsnCounter32 AsnCounter;
alias AsnGauge32 AsnGauge;
