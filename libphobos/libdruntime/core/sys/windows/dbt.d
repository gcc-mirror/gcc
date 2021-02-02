/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * Authors: Vladimir Vlasov
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_dbt.d)
 */
module core.sys.windows.dbt;
version (Windows):
@system:

version (ANSI) {} else version = Unicode;

import core.sys.windows.w32api, core.sys.windows.windef;
import core.sys.windows.basetyps; // for GUID

// FIXME: clean up Windows version support

enum : DWORD {
    DBT_NO_DISK_SPACE           = 0x47,
    DBT_CONFIGMGPRIVATE         = 0x7FFF,
    DBT_DEVICEARRIVAL           = 0x8000,
    DBT_DEVICEQUERYREMOVE       = 0x8001,
    DBT_DEVICEQUERYREMOVEFAILED = 0x8002,
    DBT_DEVICEREMOVEPENDING     = 0x8003,
    DBT_DEVICEREMOVECOMPLETE    = 0x8004,
    DBT_DEVICETYPESPECIFIC      = 0x8005,
    DBT_DEVTYP_OEM              = 0,
    DBT_DEVTYP_DEVNODE,
    DBT_DEVTYP_VOLUME,
    DBT_DEVTYP_PORT,
    DBT_DEVTYP_NET,
    DBT_DEVTYP_DEVICEINTERFACE,
    DBT_DEVTYP_HANDLE        // = 6
}

enum : DWORD {
    DBT_APPYBEGIN,
    DBT_APPYEND,
    DBT_DEVNODES_CHANGED     = 7,
    DBT_QUERYCHANGECONFIG    = 0x17,
    DBT_CONFIGCHANGED        = 0x18,
    DBT_CONFIGCHANGECANCELED = 0x19,
    DBT_MONITORCHANGE        = 0x1B,
    DBT_SHELLLOGGEDON        = 32,
    DBT_CONFIGMGAPI32        = 34,
    DBT_VXDINITCOMPLETE      = 35,
    DBT_VOLLOCKQUERYLOCK     = 0x8041,
    DBT_VOLLOCKLOCKTAKEN     = 0x8042,
    DBT_VOLLOCKLOCKFAILED    = 0x8043,
    DBT_VOLLOCKQUERYUNLOCK   = 0x8044,
    DBT_VOLLOCKLOCKRELEASED  = 0x8045,
    DBT_VOLLOCKUNLOCKFAILED  = 0x8046,
    DBT_USERDEFINED          = 0xFFFF
}

enum : WORD {
    DBTF_MEDIA = 1,
    DBTF_NET   = 2
}

enum : DWORD {
    BSM_ALLCOMPONENTS      = 0,
    BSM_APPLICATIONS       = 8,
    BSM_ALLDESKTOPS        = 16,
    BSM_INSTALLABLEDRIVERS = 4,
    BSM_NETDRIVER          = 2,
    BSM_VXDS               = 1,
    BSF_FLUSHDISK          = 0x00000004,
    BSF_FORCEIFHUNG        = 0x00000020,
    BSF_IGNORECURRENTTASK  = 0x00000002,
    BSF_NOHANG             = 0x00000008,
    BSF_NOTIMEOUTIFNOTHUNG = 0x00000040,
    BSF_POSTMESSAGE        = 0x00000010,
    BSF_QUERY              = 0x00000001,
    BSF_MSGSRV32ISOK_BIT   = 31,
    BSF_MSGSRV32ISOK       = 0x80000000
}

//static if (_WIN32_WINNT >= 0x500) {
    enum : DWORD {
        BSF_ALLOWSFW          = 0x00000080,
        BSF_SENDNOTIFYMESSAGE = 0x00000100
    }
//}

static if (_WIN32_WINNT >= 0x501) {
    enum : DWORD {
        BSF_LUID        = 0x00000400,
        BSF_RETURNHDESK = 0x00000200
    }
}

struct DEV_BROADCAST_HDR {
    DWORD dbch_size = DEV_BROADCAST_HDR.sizeof;
    DWORD dbch_devicetype;
    DWORD dbch_reserved;
}
alias DEV_BROADCAST_HDR* PDEV_BROADCAST_HDR;

struct DEV_BROADCAST_OEM {
    DWORD dbco_size = DEV_BROADCAST_OEM.sizeof;
    DWORD dbco_devicetype;
    DWORD dbco_reserved;
    DWORD dbco_identifier;
    DWORD dbco_suppfunc;
}
alias DEV_BROADCAST_OEM* PDEV_BROADCAST_OEM;

struct DEV_BROADCAST_PORT_A {
    DWORD dbcp_size = DEV_BROADCAST_PORT_A.sizeof;
    DWORD dbcp_devicetype;
    DWORD dbcp_reserved;
    char  _dbcp_name = 0;
    char* dbcp_name() return { return &_dbcp_name; }
}
alias DEV_BROADCAST_PORT_A* PDEV_BROADCAST_PORT_A;

struct DEV_BROADCAST_PORT_W {
    DWORD  dbcp_size = DEV_BROADCAST_PORT_W.sizeof;
    DWORD  dbcp_devicetype;
    DWORD  dbcp_reserved;
    WCHAR  _dbcp_name = 0;
    WCHAR* dbcp_name() return { return &_dbcp_name; }
}
alias DEV_BROADCAST_PORT_W* PDEV_BROADCAST_PORT_W;

struct DEV_BROADCAST_USERDEFINED {
    DEV_BROADCAST_HDR dbud_dbh;
    char  _dbud_szName = 0;
    char* dbud_szName() return { return &_dbud_szName; }
}

struct DEV_BROADCAST_VOLUME {
    DWORD dbcv_size = DEV_BROADCAST_VOLUME.sizeof;
    DWORD dbcv_devicetype;
    DWORD dbcv_reserved;
    DWORD dbcv_unitmask;
    WORD  dbcv_flags;
}
alias DEV_BROADCAST_VOLUME* PDEV_BROADCAST_VOLUME;

version (Unicode) {
    alias DEV_BROADCAST_PORT_W DEV_BROADCAST_PORT;
} else {
    alias DEV_BROADCAST_PORT_A DEV_BROADCAST_PORT;
}
alias DEV_BROADCAST_PORT* PDEV_BROADCAST_PORT;

//static if (_WIN32_WINNT >= 0x500) {
    struct DEV_BROADCAST_DEVICEINTERFACE_A {
        DWORD dbcc_size = DEV_BROADCAST_DEVICEINTERFACE_A.sizeof;
        DWORD dbcc_devicetype;
        DWORD dbcc_reserved;
        GUID  dbcc_classguid;
        char  _dbcc_name;
        char* dbcc_name() return { return &_dbcc_name; }
    }
    alias DEV_BROADCAST_DEVICEINTERFACE_A* PDEV_BROADCAST_DEVICEINTERFACE_A;

    struct DEV_BROADCAST_DEVICEINTERFACE_W {
        DWORD  dbcc_size = DEV_BROADCAST_DEVICEINTERFACE_W.sizeof;
        DWORD  dbcc_devicetype;
        DWORD  dbcc_reserved;
        GUID   dbcc_classguid;
        WCHAR  _dbcc_name = 0;
        WCHAR* dbcc_name() return { return &_dbcc_name; }
    }
    alias DEV_BROADCAST_DEVICEINTERFACE_W* PDEV_BROADCAST_DEVICEINTERFACE_W;

    version (Unicode) {
        alias DEV_BROADCAST_DEVICEINTERFACE_W DEV_BROADCAST_DEVICEINTERFACE;
    } else {
        alias DEV_BROADCAST_DEVICEINTERFACE_A DEV_BROADCAST_DEVICEINTERFACE;
    }
    alias DEV_BROADCAST_DEVICEINTERFACE* PDEV_BROADCAST_DEVICEINTERFACE;

    struct DEV_BROADCAST_HANDLE {
        DWORD  dbch_size = DEV_BROADCAST_HANDLE.sizeof;
        DWORD  dbch_devicetype;
        DWORD  dbch_reserved;
        HANDLE dbch_handle;
        DWORD  dbch_hdevnotify;
        GUID   dbch_eventguid;
        LONG   dbch_nameoffset;
        BYTE   _dbch_data;
        BYTE*  dbch_data() return { return &_dbch_data; }
    }
    alias DEV_BROADCAST_HANDLE* PDEV_BROADCAST_HANDLE;
//}
