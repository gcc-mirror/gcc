/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_lmsname.d)
 */
module core.sys.windows.lmsname;
version (Windows):

private import core.sys.windows.windef;

const TCHAR[]
    SERVICE_WORKSTATION      = "LanmanWorkstation",
    SERVICE_LM20_WORKSTATION = "WORKSTATION",
    WORKSTATION_DISPLAY_NAME = "Workstation",
    SERVICE_SERVER           = "LanmanServer",
    SERVICE_LM20_SERVER      = "SERVER",
    SERVER_DISPLAY_NAME      = "Server",
    SERVICE_BROWSER          = "BROWSER",
    SERVICE_LM20_BROWSER     = SERVICE_BROWSER,
    SERVICE_MESSENGER        = "MESSENGER",
    SERVICE_LM20_MESSENGER   = SERVICE_MESSENGER,
    SERVICE_NETRUN           = "NETRUN",
    SERVICE_LM20_NETRUN      = SERVICE_NETRUN,
    SERVICE_SPOOLER          = "SPOOLER",
    SERVICE_LM20_SPOOLER     = SERVICE_SPOOLER,
    SERVICE_ALERTER          = "ALERTER",
    SERVICE_LM20_ALERTER     = SERVICE_ALERTER,
    SERVICE_NETLOGON         = "NETLOGON",
    SERVICE_LM20_NETLOGON    = SERVICE_NETLOGON,
    SERVICE_NETPOPUP         = "NETPOPUP",
    SERVICE_LM20_NETPOPUP    = SERVICE_NETPOPUP,
    SERVICE_SQLSERVER        = "SQLSERVER",
    SERVICE_LM20_SQLSERVER   = SERVICE_SQLSERVER,
    SERVICE_REPL             = "REPLICATOR",
    SERVICE_LM20_REPL        = SERVICE_REPL,
    SERVICE_RIPL             = "REMOTEBOOT",
    SERVICE_LM20_RIPL        = SERVICE_RIPL,
    SERVICE_TIMESOURCE       = "TIMESOURCE",
    SERVICE_LM20_TIMESOURCE  = SERVICE_TIMESOURCE,
    SERVICE_AFP              = "AFP",
    SERVICE_LM20_AFP         = SERVICE_AFP,
    SERVICE_UPS              = "UPS",
    SERVICE_LM20_UPS         = SERVICE_UPS,
    SERVICE_XACTSRV          = "XACTSRV",
    SERVICE_LM20_XACTSRV     = SERVICE_XACTSRV,
    SERVICE_TCPIP            = "TCPIP",
    SERVICE_LM20_TCPIP       = SERVICE_TCPIP,
    SERVICE_NBT              = "NBT",
    SERVICE_LM20_NBT         = SERVICE_NBT,
    SERVICE_LMHOSTS          = "LMHOSTS",
    SERVICE_LM20_LMHOSTS     = SERVICE_LMHOSTS,
    SERVICE_TELNET           = "Telnet",
    SERVICE_LM20_TELNET      = SERVICE_TELNET,
    SERVICE_SCHEDULE         = "Schedule",
    SERVICE_LM20_SCHEDULE    = SERVICE_SCHEDULE,
    SERVICE_NTLMSSP          = "NtLmSsp",
    SERVICE_DHCP             = "DHCP",
    SERVICE_LM20_DHCP        = SERVICE_DHCP,
    SERVICE_NWSAP            = "NwSapAgent",
    SERVICE_LM20_NWSAP       = SERVICE_NWSAP,
    NWSAP_DISPLAY_NAME       = "NW Sap Agent",
    SERVICE_NWCS             = "NWCWorkstation";
