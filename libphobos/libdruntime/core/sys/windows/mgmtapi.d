/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Stewart Gordon
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_mgmtapi.d)
 */
module core.sys.windows.mgmtapi;
version (Windows):
@system:

import core.sys.windows.snmp;
import core.sys.windows.windef;

enum {
    SNMP_MGMTAPI_TIMEOUT = 40,
    SNMP_MGMTAPI_SELECT_FDERRORS,
    SNMP_MGMTAPI_TRAP_ERRORS,
    SNMP_MGMTAPI_TRAP_DUPINIT,
    SNMP_MGMTAPI_NOTRAPS,
    SNMP_MGMTAPI_AGAIN,
    SNMP_MGMTAPI_INVALID_CTL,
    SNMP_MGMTAPI_INVALID_SESSION,
    SNMP_MGMTAPI_INVALID_BUFFER // = 48
}

enum MGMCTL_SETAGENTPORT = 1;

alias PVOID LPSNMP_MGR_SESSION;

extern (Windows) {
    BOOL SnmpMgrClose(LPSNMP_MGR_SESSION);
    BOOL SnmpMgrCtl(LPSNMP_MGR_SESSION, DWORD, LPVOID, DWORD, LPVOID, DWORD,
      LPDWORD);
    BOOL SnmpMgrGetTrap(AsnObjectIdentifier*, AsnNetworkAddress*,
      AsnInteger*, AsnInteger*, AsnTimeticks*, SnmpVarBindList*);
    BOOL SnmpMgrGetTrapEx(AsnObjectIdentifier*, AsnNetworkAddress*,
      AsnNetworkAddress*, AsnInteger*, AsnInteger*, AsnOctetString*,
      AsnTimeticks*, SnmpVarBindList*);
    BOOL SnmpMgrOidToStr(AsnObjectIdentifier*, LPSTR*);
    LPSNMP_MGR_SESSION SnmpMgrOpen(LPSTR, LPSTR, INT, INT);
    INT SnmpMgrRequest(LPSNMP_MGR_SESSION, BYTE, SnmpVarBindList*,
      AsnInteger*, AsnInteger*);
    BOOL SnmpMgrStrToOid(LPSTR, AsnObjectIdentifier*);
    BOOL SnmpMgrTrapListen(HANDLE*);
}
