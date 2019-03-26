/**
 * Windows API header module
 *
 * Translated from MinGW API for MS-Windows 3.10
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_winbase.d)
 */
module core.sys.windows.winbase;
version (Windows):

version (ANSI) {} else version = Unicode;
pragma(lib, "kernel32");

/**
Translation Notes:
The following macros are obsolete, and have no effect.

LockSegment(w), MakeProcInstance(p, i), UnlockResource(h), UnlockSegment(w)
FreeModule(m), FreeProcInstance(p), GetFreeSpace(w), DefineHandleTable(w)
SetSwapAreaSize(w), LimitEmsPages(n), Yield()

// These are not required for DMD.

//FIXME:
// #ifndef UNDER_CE
    int WinMain(HINSTANCE, HINSTANCE, LPSTR, int);
#else
    int WinMain(HINSTANCE, HINSTANCE, LPWSTR, int);
#endif
int wWinMain(HINSTANCE, HINSTANCE, LPWSTR, int);

*/

import core.sys.windows.windef, core.sys.windows.winver;
private import core.sys.windows.basetyps, core.sys.windows.w32api, core.sys.windows.winnt;

// FIXME:
//alias void va_list;
import core.stdc.stdarg : va_list;


// COMMPROP structure, used by GetCommProperties()
// -----------------------------------------------

// Communications provider type
enum : DWORD {
    PST_UNSPECIFIED,
    PST_RS232,
    PST_PARALLELPORT,
    PST_RS422,
    PST_RS423,
    PST_RS449,
    PST_MODEM,      // =      6
    PST_FAX            = 0x0021,
    PST_SCANNER        = 0x0022,
    PST_NETWORK_BRIDGE = 0x0100,
    PST_LAT            = 0x0101,
    PST_TCPIP_TELNET   = 0x0102,
    PST_X25            = 0x0103
}

// Max baud rate
enum : DWORD {
    BAUD_075    = 0x00000001,
    BAUD_110    = 0x00000002,
    BAUD_134_5  = 0x00000004,
    BAUD_150    = 0x00000008,
    BAUD_300    = 0x00000010,
    BAUD_600    = 0x00000020,
    BAUD_1200   = 0x00000040,
    BAUD_1800   = 0x00000080,
    BAUD_2400   = 0x00000100,
    BAUD_4800   = 0x00000200,
    BAUD_7200   = 0x00000400,
    BAUD_9600   = 0x00000800,
    BAUD_14400  = 0x00001000,
    BAUD_19200  = 0x00002000,
    BAUD_38400  = 0x00004000,
    BAUD_56K    = 0x00008000,
    BAUD_128K   = 0x00010000,
    BAUD_115200 = 0x00020000,
    BAUD_57600  = 0x00040000,
    BAUD_USER   = 0x10000000
}

// Comm capabilities
enum : DWORD {
    PCF_DTRDSR        = 0x0001,
    PCF_RTSCTS        = 0x0002,
    PCF_RLSD          = 0x0004,
    PCF_PARITY_CHECK  = 0x0008,
    PCF_XONXOFF       = 0x0010,
    PCF_SETXCHAR      = 0x0020,
    PCF_TOTALTIMEOUTS = 0x0040,
    PCF_INTTIMEOUTS   = 0x0080,
    PCF_SPECIALCHARS  = 0x0100,
    PCF_16BITMODE     = 0x0200
}

enum  : DWORD {
    SP_PARITY       = 1,
    SP_BAUD         = 2,
    SP_DATABITS     = 4,
    SP_STOPBITS     = 8,
    SP_HANDSHAKING  = 16,
    SP_PARITY_CHECK = 32,
    SP_RLSD         = 64
}

enum : DWORD {
    DATABITS_5   = 1,
    DATABITS_6   = 2,
    DATABITS_7   = 4,
    DATABITS_8   = 8,
    DATABITS_16  = 16,
    DATABITS_16X = 32
}

enum : WORD {
    STOPBITS_10  = 0x0001,
    STOPBITS_15  = 0x0002,
    STOPBITS_20  = 0x0004,
    PARITY_NONE  = 0x0100,
    PARITY_ODD   = 0x0200,
    PARITY_EVEN  = 0x0400,
    PARITY_MARK  = 0x0800,
    PARITY_SPACE = 0x1000
}

// used by dwServiceMask
enum SP_SERIALCOMM = 1;

struct COMMPROP {
    WORD  wPacketLength;
    WORD  wPacketVersion;
    DWORD dwServiceMask;
    DWORD dwReserved1;
    DWORD dwMaxTxQueue;
    DWORD dwMaxRxQueue;
    DWORD dwMaxBaud;
    DWORD dwProvSubType;
    DWORD dwProvCapabilities;
    DWORD dwSettableParams;
    DWORD dwSettableBaud;
    WORD  wSettableData;
    WORD  wSettableStopParity;
    DWORD dwCurrentTxQueue;
    DWORD dwCurrentRxQueue;
    DWORD dwProvSpec1;
    DWORD dwProvSpec2;
    WCHAR _wcProvChar = 0;

    WCHAR* wcProvChar() return { return &_wcProvChar; }
}
alias COMMPROP* LPCOMMPROP;

// ----------

// for DEBUG_EVENT
enum : DWORD {
    EXCEPTION_DEBUG_EVENT = 1,
    CREATE_THREAD_DEBUG_EVENT,
    CREATE_PROCESS_DEBUG_EVENT,
    EXIT_THREAD_DEBUG_EVENT,
    EXIT_PROCESS_DEBUG_EVENT,
    LOAD_DLL_DEBUG_EVENT,
    UNLOAD_DLL_DEBUG_EVENT,
    OUTPUT_DEBUG_STRING_EVENT,
    RIP_EVENT
}

enum HFILE HFILE_ERROR = cast(HFILE) (-1);

// for SetFilePointer()
enum : DWORD {
    FILE_BEGIN   = 0,
    FILE_CURRENT = 1,
    FILE_END     = 2
}
enum DWORD INVALID_SET_FILE_POINTER = -1;


// for OpenFile()
deprecated enum : UINT {
    OF_READ             = 0,
    OF_WRITE            = 0x0001,
    OF_READWRITE        = 0x0002,
    OF_SHARE_COMPAT     = 0,
    OF_SHARE_EXCLUSIVE  = 0x0010,
    OF_SHARE_DENY_WRITE = 0x0020,
    OF_SHARE_DENY_READ  = 0x0030,
    OF_SHARE_DENY_NONE  = 0x0040,
    OF_PARSE            = 0x0100,
    OF_DELETE           = 0x0200,
    OF_VERIFY           = 0x0400,
    OF_CANCEL           = 0x0800,
    OF_CREATE           = 0x1000,
    OF_PROMPT           = 0x2000,
    OF_EXIST            = 0x4000,
    OF_REOPEN           = 0x8000
}

enum : DWORD {
    NMPWAIT_NOWAIT           =  1,
    NMPWAIT_WAIT_FOREVER     = -1,
    NMPWAIT_USE_DEFAULT_WAIT =  0
}

// for ClearCommError()
enum DWORD
    CE_RXOVER   = 0x0001,
    CE_OVERRUN  = 0x0002,
    CE_RXPARITY = 0x0004,
    CE_FRAME    = 0x0008,
    CE_BREAK    = 0x0010,
    CE_TXFULL   = 0x0100,
    CE_PTO      = 0x0200,
    CE_IOE      = 0x0400,
    CE_DNS      = 0x0800,
    CE_OOP      = 0x1000,
    CE_MODE     = 0x8000;

// for CopyProgressRoutine callback.
enum : DWORD {
    PROGRESS_CONTINUE = 0,
    PROGRESS_CANCEL   = 1,
    PROGRESS_STOP     = 2,
    PROGRESS_QUIET    = 3
}

enum : DWORD {
    CALLBACK_CHUNK_FINISHED = 0,
    CALLBACK_STREAM_SWITCH  = 1
}

// CopyFileEx()
enum : DWORD {
    COPY_FILE_FAIL_IF_EXISTS = 1,
    COPY_FILE_RESTARTABLE    = 2
}

enum : DWORD {
    FILE_MAP_COPY       = 1,
    FILE_MAP_WRITE      = 2,
    FILE_MAP_READ       = 4,
    FILE_MAP_ALL_ACCESS = 0x000F001F
}

enum : DWORD {
    MUTEX_ALL_ACCESS       = 0x001f0001,
    MUTEX_MODIFY_STATE     = 0x00000001,
    SEMAPHORE_ALL_ACCESS   = 0x001f0003,
    SEMAPHORE_MODIFY_STATE = 0x00000002,
    EVENT_ALL_ACCESS       = 0x001f0003,
    EVENT_MODIFY_STATE     = 0x00000002
}

// CreateNamedPipe()
enum : DWORD {
    PIPE_ACCESS_INBOUND  = 1,
    PIPE_ACCESS_OUTBOUND = 2,
    PIPE_ACCESS_DUPLEX   = 3
}

enum DWORD
    PIPE_TYPE_BYTE        = 0,
    PIPE_TYPE_MESSAGE     = 4,
    PIPE_READMODE_BYTE    = 0,
    PIPE_READMODE_MESSAGE = 2,
    PIPE_WAIT             = 0,
    PIPE_NOWAIT           = 1;

// GetNamedPipeInfo()
enum DWORD
    PIPE_CLIENT_END  = 0,
    PIPE_SERVER_END  = 1;

enum DWORD PIPE_UNLIMITED_INSTANCES = 255;

// dwCreationFlags for CreateProcess() and CreateProcessAsUser()
enum : DWORD {
    DEBUG_PROCESS               = 0x00000001,
    DEBUG_ONLY_THIS_PROCESS     = 0x00000002,
    CREATE_SUSPENDED            = 0x00000004,
    DETACHED_PROCESS            = 0x00000008,
    CREATE_NEW_CONSOLE          = 0x00000010,
    NORMAL_PRIORITY_CLASS       = 0x00000020,
    IDLE_PRIORITY_CLASS         = 0x00000040,
    HIGH_PRIORITY_CLASS         = 0x00000080,
    REALTIME_PRIORITY_CLASS     = 0x00000100,
    CREATE_NEW_PROCESS_GROUP    = 0x00000200,
    CREATE_UNICODE_ENVIRONMENT  = 0x00000400,
    CREATE_SEPARATE_WOW_VDM     = 0x00000800,
    CREATE_SHARED_WOW_VDM       = 0x00001000,
    CREATE_FORCEDOS             = 0x00002000,
    BELOW_NORMAL_PRIORITY_CLASS = 0x00004000,
    ABOVE_NORMAL_PRIORITY_CLASS = 0x00008000,
    CREATE_BREAKAWAY_FROM_JOB   = 0x01000000,
    CREATE_WITH_USERPROFILE     = 0x02000000,
    CREATE_DEFAULT_ERROR_MODE   = 0x04000000,
    CREATE_NO_WINDOW            = 0x08000000,
    PROFILE_USER                = 0x10000000,
    PROFILE_KERNEL              = 0x20000000,
    PROFILE_SERVER              = 0x40000000
}

enum DWORD CONSOLE_TEXTMODE_BUFFER = 1;

// CreateFile()
enum : DWORD {
    CREATE_NEW = 1,
    CREATE_ALWAYS,
    OPEN_EXISTING,
    OPEN_ALWAYS,
    TRUNCATE_EXISTING
}

// CreateFile()
enum DWORD
    FILE_FLAG_WRITE_THROUGH      = 0x80000000,
    FILE_FLAG_OVERLAPPED         = 0x40000000,
    FILE_FLAG_NO_BUFFERING       = 0x20000000,
    FILE_FLAG_RANDOM_ACCESS      = 0x10000000,
    FILE_FLAG_SEQUENTIAL_SCAN    = 0x08000000,
    FILE_FLAG_DELETE_ON_CLOSE    = 0x04000000,
    FILE_FLAG_BACKUP_SEMANTICS   = 0x02000000,
    FILE_FLAG_POSIX_SEMANTICS    = 0x01000000,
    FILE_FLAG_OPEN_REPARSE_POINT = 0x00200000,
    FILE_FLAG_OPEN_NO_RECALL     = 0x00100000;

static if (_WIN32_WINNT >= 0x500) {
enum DWORD FILE_FLAG_FIRST_PIPE_INSTANCE = 0x00080000;
}

// for CreateFile()
enum DWORD
    SECURITY_ANONYMOUS        = SECURITY_IMPERSONATION_LEVEL.SecurityAnonymous<<16,
    SECURITY_IDENTIFICATION   = SECURITY_IMPERSONATION_LEVEL.SecurityIdentification<<16,
    SECURITY_IMPERSONATION    = SECURITY_IMPERSONATION_LEVEL.SecurityImpersonation<<16,
    SECURITY_DELEGATION       = SECURITY_IMPERSONATION_LEVEL.SecurityDelegation<<16,
    SECURITY_CONTEXT_TRACKING = 0x00040000,
    SECURITY_EFFECTIVE_ONLY   = 0x00080000,
    SECURITY_SQOS_PRESENT     = 0x00100000,
    SECURITY_VALID_SQOS_FLAGS = 0x001F0000;


// Thread exit code
enum DWORD STILL_ACTIVE = 0x103;

/*  ??? The only documentation of this seems to be about Windows CE and to
 *  state what _doesn't_ support it.
 */
enum DWORD FIND_FIRST_EX_CASE_SENSITIVE = 1;

// GetBinaryType()
enum : DWORD {
    SCS_32BIT_BINARY = 0,
    SCS_DOS_BINARY,
    SCS_WOW_BINARY,
    SCS_PIF_BINARY,
    SCS_POSIX_BINARY,
    SCS_OS216_BINARY
}

enum size_t
    MAX_COMPUTERNAME_LENGTH = 15,
    HW_PROFILE_GUIDLEN      = 39,
    MAX_PROFILE_LEN         = 80;

// HW_PROFILE_INFO
enum DWORD
    DOCKINFO_UNDOCKED      = 1,
    DOCKINFO_DOCKED        = 2,
    DOCKINFO_USER_SUPPLIED = 4,
    DOCKINFO_USER_UNDOCKED = DOCKINFO_USER_SUPPLIED | DOCKINFO_UNDOCKED,
    DOCKINFO_USER_DOCKED   = DOCKINFO_USER_SUPPLIED | DOCKINFO_DOCKED;

// DriveType(), RealDriveType()
enum : int {
    DRIVE_UNKNOWN = 0,
    DRIVE_NO_ROOT_DIR,
    DRIVE_REMOVABLE,
    DRIVE_FIXED,
    DRIVE_REMOTE,
    DRIVE_CDROM,
    DRIVE_RAMDISK
}

// GetFileType()
enum : DWORD {
    FILE_TYPE_UNKNOWN = 0,
    FILE_TYPE_DISK,
    FILE_TYPE_CHAR,
    FILE_TYPE_PIPE,
    FILE_TYPE_REMOTE = 0x8000
}

// Get/SetHandleInformation()
enum DWORD
    HANDLE_FLAG_INHERIT            = 0x01,
    HANDLE_FLAG_PROTECT_FROM_CLOSE = 0x02;

enum : DWORD {
    STD_INPUT_HANDLE  = 0xFFFFFFF6,
    STD_OUTPUT_HANDLE = 0xFFFFFFF5,
    STD_ERROR_HANDLE  = 0xFFFFFFF4
}

enum HANDLE INVALID_HANDLE_VALUE = cast(HANDLE) (-1);

enum : DWORD {
    GET_TAPE_MEDIA_INFORMATION = 0,
    GET_TAPE_DRIVE_INFORMATION = 1
}

enum : DWORD {
    SET_TAPE_MEDIA_INFORMATION = 0,
    SET_TAPE_DRIVE_INFORMATION = 1
}

// SetThreadPriority()/GetThreadPriority()
enum : int {
    THREAD_PRIORITY_IDLE          = -15,
    THREAD_PRIORITY_LOWEST        =  -2,
    THREAD_PRIORITY_BELOW_NORMAL  =  -1,
    THREAD_PRIORITY_NORMAL        =   0,
    THREAD_PRIORITY_ABOVE_NORMAL  =   1,
    THREAD_PRIORITY_HIGHEST       =   2,
    THREAD_PRIORITY_TIME_CRITICAL =  15,
    THREAD_PRIORITY_ERROR_RETURN  = 2147483647
}

enum : DWORD {
    TIME_ZONE_ID_UNKNOWN,
    TIME_ZONE_ID_STANDARD,
    TIME_ZONE_ID_DAYLIGHT,
    TIME_ZONE_ID_INVALID = 0xFFFFFFFF
}

enum DWORD
    FS_CASE_SENSITIVE         =     1,
    FS_CASE_IS_PRESERVED      =     2,
    FS_UNICODE_STORED_ON_DISK =     4,
    FS_PERSISTENT_ACLS        =     8,
    FS_FILE_COMPRESSION       =    16,
    FS_VOL_IS_COMPRESSED      = 32768;

// Flags for GlobalAlloc
enum UINT
    GMEM_FIXED       = 0,
    GMEM_MOVEABLE    = 0x0002,
    GMEM_ZEROINIT    = 0x0040,
    GPTR             = 0x0040,
    GHND             = 0x0042,
    GMEM_MODIFY      = 0x0080,  // used only for GlobalRealloc
    GMEM_VALID_FLAGS = 0x7F72;

/+  // Obselete flags (Win16 only)
    GMEM_NOCOMPACT=16;
    GMEM_NODISCARD=32;
    GMEM_DISCARDABLE=256;
    GMEM_NOT_BANKED=4096;
    GMEM_LOWER=4096;
    GMEM_SHARE=8192;
    GMEM_DDESHARE=8192;

    GMEM_LOCKCOUNT=255;

// for GlobalFlags()
    GMEM_DISCARDED      = 16384;
    GMEM_INVALID_HANDLE = 32768;

    GMEM_NOTIFY         = 16384;
+/

enum UINT
    LMEM_FIXED          = 0,
    LMEM_MOVEABLE       = 0x0002,
    LMEM_NONZEROLPTR    = 0,
    NONZEROLPTR         = 0,
    LMEM_NONZEROLHND    = 0x0002,
    NONZEROLHND         = 0x0002,
    LMEM_DISCARDABLE    = 0x0F00,
    LMEM_NOCOMPACT      = 0x0010,
    LMEM_NODISCARD      = 0x0020,
    LMEM_ZEROINIT       = 0x0040,
    LPTR                = 0x0040,
    LHND                = 0x0042,
    LMEM_MODIFY         = 0x0080,
    LMEM_LOCKCOUNT      = 0x00FF,
    LMEM_DISCARDED      = 0x4000,
    LMEM_INVALID_HANDLE = 0x8000;



// used in EXCEPTION_RECORD
enum : DWORD {
    STATUS_WAIT_0                      = 0,
    STATUS_ABANDONED_WAIT_0            = 0x00000080,
    STATUS_USER_APC                    = 0x000000C0,
    STATUS_TIMEOUT                     = 0x00000102,
    STATUS_PENDING                     = 0x00000103,

    STATUS_SEGMENT_NOTIFICATION        = 0x40000005,
    STATUS_GUARD_PAGE_VIOLATION        = 0x80000001,
    STATUS_DATATYPE_MISALIGNMENT       = 0x80000002,
    STATUS_BREAKPOINT                  = 0x80000003,
    STATUS_SINGLE_STEP                 = 0x80000004,

    STATUS_ACCESS_VIOLATION            = 0xC0000005,
    STATUS_IN_PAGE_ERROR               = 0xC0000006,
    STATUS_INVALID_HANDLE              = 0xC0000008,

    STATUS_NO_MEMORY                   = 0xC0000017,
    STATUS_ILLEGAL_INSTRUCTION         = 0xC000001D,
    STATUS_NONCONTINUABLE_EXCEPTION    = 0xC0000025,
    STATUS_INVALID_DISPOSITION         = 0xC0000026,
    STATUS_ARRAY_BOUNDS_EXCEEDED       = 0xC000008C,
    STATUS_FLOAT_DENORMAL_OPERAND      = 0xC000008D,
    STATUS_FLOAT_DIVIDE_BY_ZERO        = 0xC000008E,
    STATUS_FLOAT_INEXACT_RESULT        = 0xC000008F,
    STATUS_FLOAT_INVALID_OPERATION     = 0xC0000090,
    STATUS_FLOAT_OVERFLOW              = 0xC0000091,
    STATUS_FLOAT_STACK_CHECK           = 0xC0000092,
    STATUS_FLOAT_UNDERFLOW             = 0xC0000093,
    STATUS_INTEGER_DIVIDE_BY_ZERO      = 0xC0000094,
    STATUS_INTEGER_OVERFLOW            = 0xC0000095,
    STATUS_PRIVILEGED_INSTRUCTION      = 0xC0000096,
    STATUS_STACK_OVERFLOW              = 0xC00000FD,
    STATUS_CONTROL_C_EXIT              = 0xC000013A,
    STATUS_DLL_INIT_FAILED             = 0xC0000142,
    STATUS_DLL_INIT_FAILED_LOGOFF      = 0xC000026B,

    CONTROL_C_EXIT                     = STATUS_CONTROL_C_EXIT,

    EXCEPTION_ACCESS_VIOLATION         = STATUS_ACCESS_VIOLATION,
    EXCEPTION_DATATYPE_MISALIGNMENT    = STATUS_DATATYPE_MISALIGNMENT,
    EXCEPTION_BREAKPOINT               = STATUS_BREAKPOINT,
    EXCEPTION_SINGLE_STEP              = STATUS_SINGLE_STEP,
    EXCEPTION_ARRAY_BOUNDS_EXCEEDED    = STATUS_ARRAY_BOUNDS_EXCEEDED,
    EXCEPTION_FLT_DENORMAL_OPERAND     = STATUS_FLOAT_DENORMAL_OPERAND,
    EXCEPTION_FLT_DIVIDE_BY_ZERO       = STATUS_FLOAT_DIVIDE_BY_ZERO,
    EXCEPTION_FLT_INEXACT_RESULT       = STATUS_FLOAT_INEXACT_RESULT,
    EXCEPTION_FLT_INVALID_OPERATION    = STATUS_FLOAT_INVALID_OPERATION,
    EXCEPTION_FLT_OVERFLOW             = STATUS_FLOAT_OVERFLOW,
    EXCEPTION_FLT_STACK_CHECK          = STATUS_FLOAT_STACK_CHECK,
    EXCEPTION_FLT_UNDERFLOW            = STATUS_FLOAT_UNDERFLOW,
    EXCEPTION_INT_DIVIDE_BY_ZERO       = STATUS_INTEGER_DIVIDE_BY_ZERO,
    EXCEPTION_INT_OVERFLOW             = STATUS_INTEGER_OVERFLOW,
    EXCEPTION_PRIV_INSTRUCTION         = STATUS_PRIVILEGED_INSTRUCTION,
    EXCEPTION_IN_PAGE_ERROR            = STATUS_IN_PAGE_ERROR,
    EXCEPTION_ILLEGAL_INSTRUCTION      = STATUS_ILLEGAL_INSTRUCTION,
    EXCEPTION_NONCONTINUABLE_EXCEPTION = STATUS_NONCONTINUABLE_EXCEPTION,
    EXCEPTION_STACK_OVERFLOW           = STATUS_STACK_OVERFLOW,
    EXCEPTION_INVALID_DISPOSITION      = STATUS_INVALID_DISPOSITION,
    EXCEPTION_GUARD_PAGE               = STATUS_GUARD_PAGE_VIOLATION,
    EXCEPTION_INVALID_HANDLE           = STATUS_INVALID_HANDLE
}

// for PROCESS_HEAP_ENTRY
enum WORD
    PROCESS_HEAP_REGION            =  1,
    PROCESS_HEAP_UNCOMMITTED_RANGE =  2,
    PROCESS_HEAP_ENTRY_BUSY        =  4,
    PROCESS_HEAP_ENTRY_MOVEABLE    = 16,
    PROCESS_HEAP_ENTRY_DDESHARE    = 32;

// for LoadLibraryEx()
enum DWORD
    DONT_RESOLVE_DLL_REFERENCES   = 0x01, // not for WinME and earlier
    LOAD_LIBRARY_AS_DATAFILE      = 0x02,
    LOAD_WITH_ALTERED_SEARCH_PATH = 0x08,
    LOAD_IGNORE_CODE_AUTHZ_LEVEL  = 0x10; // only for XP and later

// for LockFile()
enum DWORD
    LOCKFILE_FAIL_IMMEDIATELY = 1,
    LOCKFILE_EXCLUSIVE_LOCK   = 2;

enum MAXIMUM_WAIT_OBJECTS  = 64;
enum MAXIMUM_SUSPEND_COUNT = 0x7F;

enum WAIT_OBJECT_0    = 0;
enum WAIT_ABANDONED_0 = 128;

//const WAIT_TIMEOUT=258;  // also in winerror.h

enum : DWORD {
    WAIT_IO_COMPLETION = 0x000000C0,
    WAIT_ABANDONED     = 0x00000080,
    WAIT_FAILED        = 0xFFFFFFFF
}

// PurgeComm()
enum DWORD
    PURGE_TXABORT = 1,
    PURGE_RXABORT = 2,
    PURGE_TXCLEAR = 4,
    PURGE_RXCLEAR = 8;

// ReadEventLog()
enum DWORD
    EVENTLOG_SEQUENTIAL_READ = 1,
    EVENTLOG_SEEK_READ       = 2,
    EVENTLOG_FORWARDS_READ   = 4,
    EVENTLOG_BACKWARDS_READ  = 8;

// ReportEvent()
enum : WORD {
    EVENTLOG_SUCCESS          = 0,
    EVENTLOG_ERROR_TYPE       = 1,
    EVENTLOG_WARNING_TYPE     = 2,
    EVENTLOG_INFORMATION_TYPE = 4,
    EVENTLOG_AUDIT_SUCCESS    = 8,
    EVENTLOG_AUDIT_FAILURE    = 16
}

// FormatMessage()
enum DWORD
    FORMAT_MESSAGE_ALLOCATE_BUFFER = 0x0100,
    FORMAT_MESSAGE_IGNORE_INSERTS  = 0x0200,
    FORMAT_MESSAGE_FROM_STRING     = 0x0400,
    FORMAT_MESSAGE_FROM_HMODULE    = 0x0800,
    FORMAT_MESSAGE_FROM_SYSTEM     = 0x1000,
    FORMAT_MESSAGE_ARGUMENT_ARRAY  = 0x2000;

enum DWORD FORMAT_MESSAGE_MAX_WIDTH_MASK = 255;

// also in ddk/ntapi.h
// To restore default error mode, call SetErrorMode(0)
enum {
    SEM_FAILCRITICALERRORS     = 0x0001,
    SEM_NOGPFAULTERRORBOX      = 0x0002,
    SEM_NOALIGNMENTFAULTEXCEPT = 0x0004,
    SEM_NOOPENFILEERRORBOX     = 0x8000
}
// end ntapi.h

enum {
    SLE_ERROR = 1,
    SLE_MINORERROR,
    SLE_WARNING
}

enum SHUTDOWN_NORETRY = 1;

// Return type for exception filters.
enum : LONG {
    EXCEPTION_EXECUTE_HANDLER    =  1,
    EXCEPTION_CONTINUE_EXECUTION = -1,
    EXCEPTION_CONTINUE_SEARCH    =  0
}

enum  : ATOM {
    MAXINTATOM   = 0xC000,
    INVALID_ATOM = 0
}

enum IGNORE   = 0;
enum INFINITE = 0xFFFFFFFF;

// EscapeCommFunction()
enum {
    SETXOFF    = 1,
    SETXON,
    SETRTS,
    CLRRTS,
    SETDTR,
    CLRDTR, // = 6
    SETBREAK   = 8,
    CLRBREAK   = 9
}


// for SetCommMask()
enum DWORD
    EV_RXCHAR   = 0x0001,
    EV_RXFLAG   = 0x0002,
    EV_TXEMPTY  = 0x0004,
    EV_CTS      = 0x0008,
    EV_DSR      = 0x0010,
    EV_RLSD     = 0x0020,
    EV_BREAK    = 0x0040,
    EV_ERR      = 0x0080,
    EV_RING     = 0x0100,
    EV_PERR     = 0x0200,
    EV_RX80FULL = 0x0400,
    EV_EVENT1   = 0x0800,
    EV_EVENT2   = 0x1000;

// GetCommModemStatus()
enum DWORD
    MS_CTS_ON  = 0x0010,
    MS_DSR_ON  = 0x0020,
    MS_RING_ON = 0x0040,
    MS_RLSD_ON = 0x0080;


// DCB
enum : BYTE {
    NOPARITY = 0,
    ODDPARITY,
    EVENPARITY,
    MARKPARITY,
    SPACEPARITY
}
// DCB
enum : BYTE {
    ONESTOPBIT = 0,
    ONE5STOPBITS,
    TWOSTOPBITS
}
// DCB
enum : DWORD {
    CBR_110    =    110,
    CBR_300    =    300,
    CBR_600    =    600,
    CBR_1200   =   1200,
    CBR_2400   =   2400,
    CBR_4800   =   4800,
    CBR_9600   =   9600,
    CBR_14400  =  14400,
    CBR_19200  =  19200,
    CBR_38400  =  38400,
    CBR_56000  =  56000,
    CBR_57600  =  57600,
    CBR_115200 = 115200,
    CBR_128000 = 128000,
    CBR_256000 = 256000
}
// DCB, 2-bit bitfield
enum {
    DTR_CONTROL_DISABLE = 0,
    DTR_CONTROL_ENABLE,
    DTR_CONTROL_HANDSHAKE
}

// DCB, 2-bit bitfield
enum {
    RTS_CONTROL_DISABLE = 0,
    RTS_CONTROL_ENABLE,
    RTS_CONTROL_HANDSHAKE,
    RTS_CONTROL_TOGGLE,
}

// WIN32_STREAM_ID
enum : DWORD {
    BACKUP_INVALID = 0,
    BACKUP_DATA,
    BACKUP_EA_DATA,
    BACKUP_SECURITY_DATA,
    BACKUP_ALTERNATE_DATA,
    BACKUP_LINK,
    BACKUP_PROPERTY_DATA,
    BACKUP_OBJECT_ID,
    BACKUP_REPARSE_DATA,
    BACKUP_SPARSE_BLOCK
}

// WIN32_STREAM_ID
enum : DWORD {
    STREAM_NORMAL_ATTRIBUTE    = 0,
    STREAM_MODIFIED_WHEN_READ  = 1,
    STREAM_CONTAINS_SECURITY   = 2,
    STREAM_CONTAINS_PROPERTIES = 4
}

// STARTUPINFO
enum DWORD
    STARTF_USESHOWWINDOW    = 0x0001,
    STARTF_USESIZE          = 0x0002,
    STARTF_USEPOSITION      = 0x0004,
    STARTF_USECOUNTCHARS    = 0x0008,
    STARTF_USEFILLATTRIBUTE = 0x0010,
    STARTF_RUNFULLSCREEN    = 0x0020,
    STARTF_FORCEONFEEDBACK  = 0x0040,
    STARTF_FORCEOFFFEEDBACK = 0x0080,
    STARTF_USESTDHANDLES    = 0x0100,
    STARTF_USEHOTKEY        = 0x0200;

// ???
enum {
    TC_NORMAL  = 0,
    TC_HARDERR = 1,
    TC_GP_TRAP = 2,
    TC_SIGNAL  = 3
}

/+ These seem to be Windows CE-specific
enum {
    AC_LINE_OFFLINE      = 0,
    AC_LINE_ONLINE       = 1,
    AC_LINE_BACKUP_POWER = 2,
    AC_LINE_UNKNOWN      = 255
}

enum {
    BATTERY_FLAG_HIGH          = 1,
    BATTERY_FLAG_LOW           = 2,
    BATTERY_FLAG_CRITICAL      = 4,
    BATTERY_FLAG_CHARGING      = 8,
    BATTERY_FLAG_NO_BATTERY    = 128,
    BATTERY_FLAG_UNKNOWN       = 255,
    BATTERY_PERCENTAGE_UNKNOWN = 255,
    BATTERY_LIFE_UNKNOWN       = 0xFFFFFFFF
}
+/

// ???
enum HINSTANCE_ERROR = 32;

// returned from GetFileSize()
enum DWORD INVALID_FILE_SIZE = 0xFFFFFFFF;

enum DWORD TLS_OUT_OF_INDEXES = 0xFFFFFFFF;

// GetWriteWatch()
enum DWORD WRITE_WATCH_FLAG_RESET = 1;

// for LogonUser()
enum : DWORD {
    LOGON32_LOGON_INTERACTIVE = 2,
    LOGON32_LOGON_NETWORK     = 3,
    LOGON32_LOGON_BATCH       = 4,
    LOGON32_LOGON_SERVICE     = 5,
    LOGON32_LOGON_UNLOCK      = 7
}

// for LogonUser()
enum : DWORD {
    LOGON32_PROVIDER_DEFAULT,
    LOGON32_PROVIDER_WINNT35,
    LOGON32_PROVIDER_WINNT40,
    LOGON32_PROVIDER_WINNT50
}

// for MoveFileEx()
enum DWORD
    MOVEFILE_REPLACE_EXISTING   = 1,
    MOVEFILE_COPY_ALLOWED       = 2,
    MOVEFILE_DELAY_UNTIL_REBOOT = 4,
    MOVEFILE_WRITE_THROUGH      = 8;

// DefineDosDevice()
enum DWORD
    DDD_RAW_TARGET_PATH       = 1,
    DDD_REMOVE_DEFINITION     = 2,
    DDD_EXACT_MATCH_ON_REMOVE = 4;

static if (_WIN32_WINNT >= 0x500) {
    enum : DWORD {
        LOGON32_LOGON_NETWORK_CLEARTEXT = 8,
        LOGON32_LOGON_NEW_CREDENTIALS   = 9
    }

    // ReplaceFile()
enum DWORD
        REPLACEFILE_WRITE_THROUGH       = 1,
        REPLACEFILE_IGNORE_MERGE_ERRORS = 2;
}

static if (_WIN32_WINNT >= 0x501) {
enum DWORD
        GET_MODULE_HANDLE_EX_FLAG_PIN                = 1,
        GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT = 2,
        GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS       = 4;

    // for ACTCTX
enum DWORD
        ACTCTX_FLAG_PROCESSOR_ARCHITECTURE_VALID = 0x01,
        ACTCTX_FLAG_LANGID_VALID                 = 0x02,
        ACTCTX_FLAG_ASSEMBLY_DIRECTORY_VALID     = 0x04,
        ACTCTX_FLAG_RESOURCE_NAME_VALID          = 0x08,
        ACTCTX_FLAG_SET_PROCESS_DEFAULT          = 0x10,
        ACTCTX_FLAG_APPLICATION_NAME_VALID       = 0x20,
        ACTCTX_FLAG_HMODULE_VALID                = 0x80;

    // DeactivateActCtx()
enum DWORD DEACTIVATE_ACTCTX_FLAG_FORCE_EARLY_DEACTIVATION = 1;
    // FindActCtxSectionString()
enum DWORD FIND_ACTCTX_SECTION_KEY_RETURN_HACTCTX          = 1;
    // QueryActCtxW()
enum DWORD
        QUERY_ACTCTX_FLAG_USE_ACTIVE_ACTCTX             = 0x04,
        QUERY_ACTCTX_FLAG_ACTCTX_IS_HMODULE             = 0x08,
        QUERY_ACTCTX_FLAG_ACTCTX_IS_ADDRESS             = 0x10;

    enum {
        LOGON_WITH_PROFILE        = 1,
        LOGON_NETCREDENTIALS_ONLY
    }
}

// ----

struct FILETIME {
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
}
alias FILETIME* PFILETIME, LPFILETIME;

struct BY_HANDLE_FILE_INFORMATION {
    DWORD    dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD    dwVolumeSerialNumber;
    DWORD    nFileSizeHigh;
    DWORD    nFileSizeLow;
    DWORD    nNumberOfLinks;
    DWORD    nFileIndexHigh;
    DWORD    nFileIndexLow;
}
alias BY_HANDLE_FILE_INFORMATION* LPBY_HANDLE_FILE_INFORMATION;

struct DCB {
    DWORD DCBlength = DCB.sizeof;
    DWORD BaudRate;
/+
    DWORD fBinary:1;              // Binary Mode (skip EOF check)
    DWORD fParity:1;              // Enable parity checking
    DWORD fOutxCtsFlow:1;         // CTS handshaking on output
    DWORD fOutxDsrFlow:1;         // DSR handshaking on output
    DWORD fDtrControl:2;          // DTR Flow control
    DWORD fDsrSensitivity:1;      // DSR Sensitivity
    DWORD fTXContinueOnXoff:1;    // Continue TX when Xoff sent
    DWORD fOutX:1;                // Enable output X-ON/X-OFF
    DWORD fInX:1;                 // Enable input X-ON/X-OFF
    DWORD fErrorChar:1;           // Enable Err Replacement
    DWORD fNull:1;                // Enable Null stripping
    DWORD fRtsControl:2;          // Rts Flow control
    DWORD fAbortOnError:1;        // Abort all reads and writes on Error
    DWORD fDummy2:17;             // Reserved
+/
    uint _bf;
    bool fBinary(bool f)           { _bf = (_bf & ~0x0001) | f; return f; }
    bool fParity(bool f)           { _bf = (_bf & ~0x0002) | (f<<1); return f; }
    bool fOutxCtsFlow(bool f)      { _bf = (_bf & ~0x0004) | (f<<2); return f; }
    bool fOutxDsrFlow(bool f)      { _bf = (_bf & ~0x0008) | (f<<3); return f; }
    byte fDtrControl(byte x)       { _bf = (_bf & ~0x0030) | (x<<4); return cast(byte)(x & 3); }
    bool fDsrSensitivity(bool f)   { _bf = (_bf & ~0x0040) | (f<<6); return f; }
    bool fTXContinueOnXoff(bool f) { _bf = (_bf & ~0x0080) | (f<<7); return f; }
    bool fOutX(bool f)             { _bf = (_bf & ~0x0100) | (f<<8); return f; }
    bool fInX(bool f)              { _bf = (_bf & ~0x0200) | (f<<9); return f; }
    bool fErrorChar(bool f)        { _bf = (_bf & ~0x0400) | (f<<10); return f; }
    bool fNull(bool f)             { _bf = (_bf & ~0x0800) | (f<<11); return f; }
    byte fRtsControl(byte x)       { _bf = (_bf & ~0x3000) | (x<<12); return cast(byte)(x & 3); }
    bool fAbortOnError(bool f)     { _bf = (_bf & ~0x4000) | (f<<14); return f; }

    bool fBinary()           { return cast(bool) (_bf & 1); }
    bool fParity()           { return cast(bool) (_bf & 2); }
    bool fOutxCtsFlow()      { return cast(bool) (_bf & 4); }
    bool fOutxDsrFlow()      { return cast(bool) (_bf & 8); }
    byte fDtrControl()       { return cast(byte) ((_bf & (32+16))>>4); }
    bool fDsrSensitivity()   { return cast(bool) (_bf & 64); }
    bool fTXContinueOnXoff() { return cast(bool) (_bf & 128); }
    bool fOutX()             { return cast(bool) (_bf & 256); }
    bool fInX()              { return cast(bool) (_bf & 512); }
    bool fErrorChar()        { return cast(bool) (_bf & 1024); }
    bool fNull()             { return cast(bool) (_bf & 2048); }
    byte fRtsControl()       { return cast(byte) ((_bf & (4096+8192))>>12); }
    bool fAbortOnError()     { return cast(bool) (_bf & 16384); }

    WORD wReserved;
    WORD XonLim;
    WORD XoffLim;
    BYTE ByteSize;
    BYTE Parity;
    BYTE StopBits;
    char XonChar = 0;
    char XoffChar = 0;
    char ErrorChar = 0;
    char EofChar = 0;
    char EvtChar = 0;
    WORD wReserved1;
}
alias DCB* LPDCB;

struct COMMCONFIG {
    DWORD dwSize = COMMCONFIG.sizeof;
    WORD  wVersion;
    WORD  wReserved;
    DCB   dcb;
    DWORD dwProviderSubType;
    DWORD dwProviderOffset;
    DWORD dwProviderSize;
    WCHAR _wcProviderData = 0;

    WCHAR* wcProviderData() return { return &_wcProviderData; }
}
alias COMMCONFIG* LPCOMMCONFIG;

struct COMMTIMEOUTS {
    DWORD ReadIntervalTimeout;
    DWORD ReadTotalTimeoutMultiplier;
    DWORD ReadTotalTimeoutConstant;
    DWORD WriteTotalTimeoutMultiplier;
    DWORD WriteTotalTimeoutConstant;
}
alias COMMTIMEOUTS* LPCOMMTIMEOUTS;

struct COMSTAT {
/+
    DWORD fCtsHold:1;
    DWORD fDsrHold:1;
    DWORD fRlsdHold:1;
    DWORD fXoffHold:1;
    DWORD fXoffSent:1;
    DWORD fEof:1;
    DWORD fTxim:1;
    DWORD fReserved:25;
+/
    DWORD _bf;
    bool fCtsHold(bool f)  { _bf = (_bf & ~1) | f; return f; }
    bool fDsrHold(bool f)  { _bf = (_bf & ~2) | (f<<1); return f; }
    bool fRlsdHold(bool f) { _bf = (_bf & ~4) | (f<<2); return f; }
    bool fXoffHold(bool f) { _bf = (_bf & ~8) | (f<<3); return f; }
    bool fXoffSent(bool f) { _bf = (_bf & ~16) | (f<<4); return f; }
    bool fEof(bool f)      { _bf = (_bf & ~32) | (f<<5); return f; }
    bool fTxim(bool f)     { _bf = (_bf & ~64) | (f<<6); return f; }

    bool fCtsHold()  { return cast(bool) (_bf & 1); }
    bool fDsrHold()  { return cast(bool) (_bf & 2); }
    bool fRlsdHold() { return cast(bool) (_bf & 4); }
    bool fXoffHold() { return cast(bool) (_bf & 8); }
    bool fXoffSent() { return cast(bool) (_bf & 16); }
    bool fEof()      { return cast(bool) (_bf & 32); }
    bool fTxim()     { return cast(bool) (_bf & 64); }

    DWORD cbInQue;
    DWORD cbOutQue;
}
alias COMSTAT* LPCOMSTAT;

struct CREATE_PROCESS_DEBUG_INFO {
    HANDLE hFile;
    HANDLE hProcess;
    HANDLE hThread;
    LPVOID lpBaseOfImage;
    DWORD  dwDebugInfoFileOffset;
    DWORD  nDebugInfoSize;
    LPVOID lpThreadLocalBase;
    LPTHREAD_START_ROUTINE lpStartAddress;
    LPVOID lpImageName;
    WORD   fUnicode;
}
alias CREATE_PROCESS_DEBUG_INFO* LPCREATE_PROCESS_DEBUG_INFO;

struct CREATE_THREAD_DEBUG_INFO {
    HANDLE hThread;
    LPVOID lpThreadLocalBase;
    LPTHREAD_START_ROUTINE lpStartAddress;
}
alias CREATE_THREAD_DEBUG_INFO* LPCREATE_THREAD_DEBUG_INFO;

struct EXCEPTION_DEBUG_INFO {
    EXCEPTION_RECORD ExceptionRecord;
    DWORD            dwFirstChance;
}
alias EXCEPTION_DEBUG_INFO* LPEXCEPTION_DEBUG_INFO;

struct EXIT_THREAD_DEBUG_INFO {
    DWORD dwExitCode;
}
alias EXIT_THREAD_DEBUG_INFO* LPEXIT_THREAD_DEBUG_INFO;

struct EXIT_PROCESS_DEBUG_INFO {
    DWORD dwExitCode;
}
alias EXIT_PROCESS_DEBUG_INFO* LPEXIT_PROCESS_DEBUG_INFO;

struct LOAD_DLL_DEBUG_INFO {
    HANDLE hFile;
    LPVOID lpBaseOfDll;
    DWORD  dwDebugInfoFileOffset;
    DWORD  nDebugInfoSize;
    LPVOID lpImageName;
    WORD   fUnicode;
}
alias LOAD_DLL_DEBUG_INFO* LPLOAD_DLL_DEBUG_INFO;

struct UNLOAD_DLL_DEBUG_INFO {
    LPVOID lpBaseOfDll;
}
alias UNLOAD_DLL_DEBUG_INFO* LPUNLOAD_DLL_DEBUG_INFO;

struct OUTPUT_DEBUG_STRING_INFO {
    LPSTR lpDebugStringData;
    WORD  fUnicode;
    WORD  nDebugStringLength;
}
alias OUTPUT_DEBUG_STRING_INFO* LPOUTPUT_DEBUG_STRING_INFO;

struct RIP_INFO {
    DWORD dwError;
    DWORD dwType;
}
alias RIP_INFO* LPRIP_INFO;

struct DEBUG_EVENT {
    DWORD dwDebugEventCode;
    DWORD dwProcessId;
    DWORD dwThreadId;
    union {
        EXCEPTION_DEBUG_INFO      Exception;
        CREATE_THREAD_DEBUG_INFO  CreateThread;
        CREATE_PROCESS_DEBUG_INFO CreateProcessInfo;
        EXIT_THREAD_DEBUG_INFO    ExitThread;
        EXIT_PROCESS_DEBUG_INFO   ExitProcess;
        LOAD_DLL_DEBUG_INFO       LoadDll;
        UNLOAD_DLL_DEBUG_INFO     UnloadDll;
        OUTPUT_DEBUG_STRING_INFO  DebugString;
        RIP_INFO                  RipInfo;
    }
}
alias DEBUG_EVENT* LPDEBUG_EVENT;

struct OVERLAPPED {
    ULONG_PTR Internal;
    ULONG_PTR InternalHigh;
    union {
        struct {
            DWORD     Offset;
            DWORD     OffsetHigh;
        }
        PVOID     Pointer;
    }
    HANDLE    hEvent;
}
alias OVERLAPPED* POVERLAPPED, LPOVERLAPPED;

struct STARTUPINFOA {
    DWORD  cb = STARTUPINFOA.sizeof;
    LPSTR  lpReserved;
    LPSTR  lpDesktop;
    LPSTR  lpTitle;
    DWORD  dwX;
    DWORD  dwY;
    DWORD  dwXSize;
    DWORD  dwYSize;
    DWORD  dwXCountChars;
    DWORD  dwYCountChars;
    DWORD  dwFillAttribute;
    DWORD  dwFlags;
    WORD   wShowWindow;
    WORD   cbReserved2;
    PBYTE  lpReserved2;
    HANDLE hStdInput;
    HANDLE hStdOutput;
    HANDLE hStdError;
}
alias STARTUPINFOA* LPSTARTUPINFOA;

struct STARTUPINFOW {
    DWORD  cb = STARTUPINFOW.sizeof;
    LPWSTR lpReserved;
    LPWSTR lpDesktop;
    LPWSTR lpTitle;
    DWORD  dwX;
    DWORD  dwY;
    DWORD  dwXSize;
    DWORD  dwYSize;
    DWORD  dwXCountChars;
    DWORD  dwYCountChars;
    DWORD  dwFillAttribute;
    DWORD  dwFlags;
    WORD   wShowWindow;
    WORD   cbReserved2;
    PBYTE  lpReserved2;
    HANDLE hStdInput;
    HANDLE hStdOutput;
    HANDLE hStdError;
}
alias STARTUPINFOW STARTUPINFO_W;
alias STARTUPINFOW* LPSTARTUPINFOW, LPSTARTUPINFO_W;

struct PROCESS_INFORMATION {
    HANDLE hProcess;
    HANDLE hThread;
    DWORD  dwProcessId;
    DWORD  dwThreadId;
}
alias PROCESS_INFORMATION* PPROCESS_INFORMATION, LPPROCESS_INFORMATION;

/*
struct CRITICAL_SECTION_DEBUG {
    WORD              Type;
    WORD              CreatorBackTraceIndex;
    CRITICAL_SECTION* CriticalSection;
    LIST_ENTRY        ProcessLocksList;
    DWORD             EntryCount;
    DWORD             ContentionCount;
    DWORD[2]          Spare;
}
alias CRITICAL_SECTION_DEBUG* PCRITICAL_SECTION_DEBUG;

struct CRITICAL_SECTION {
    PCRITICAL_SECTION_DEBUG DebugInfo;
    LONG   LockCount;
    LONG   RecursionCount;
    HANDLE OwningThread;
    HANDLE LockSemaphore;
    DWORD  SpinCount;
}
alias CRITICAL_SECTION* PCRITICAL_SECTION, LPCRITICAL_SECTION;
*/

alias CRITICAL_SECTION_DEBUG = RTL_CRITICAL_SECTION_DEBUG;
alias CRITICAL_SECTION_DEBUG* PCRITICAL_SECTION_DEBUG;

alias CRITICAL_SECTION = RTL_CRITICAL_SECTION;
alias CRITICAL_SECTION* PCRITICAL_SECTION, LPCRITICAL_SECTION;

struct SYSTEMTIME {
    WORD wYear;
    WORD wMonth;
    WORD wDayOfWeek;
    WORD wDay;
    WORD wHour;
    WORD wMinute;
    WORD wSecond;
    WORD wMilliseconds;
}
alias SYSTEMTIME* LPSYSTEMTIME;

struct WIN32_FILE_ATTRIBUTE_DATA {
    DWORD    dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD    nFileSizeHigh;
    DWORD    nFileSizeLow;
}
alias WIN32_FILE_ATTRIBUTE_DATA* LPWIN32_FILE_ATTRIBUTE_DATA;

struct WIN32_FIND_DATAA {
    DWORD          dwFileAttributes;
    FILETIME       ftCreationTime;
    FILETIME       ftLastAccessTime;
    FILETIME       ftLastWriteTime;
    DWORD          nFileSizeHigh;
    DWORD          nFileSizeLow;
// #ifdef _WIN32_WCE
//  DWORD dwOID;
// #else
    DWORD          dwReserved0;
    DWORD          dwReserved1;
// #endif
    CHAR[MAX_PATH] cFileName = 0;
// #ifndef _WIN32_WCE
    CHAR[14]       cAlternateFileName = 0;
// #endif
}
alias WIN32_FIND_DATAA* PWIN32_FIND_DATAA, LPWIN32_FIND_DATAA;

struct WIN32_FIND_DATAW {
    DWORD           dwFileAttributes;
    FILETIME        ftCreationTime;
    FILETIME        ftLastAccessTime;
    FILETIME        ftLastWriteTime;
    DWORD           nFileSizeHigh;
    DWORD           nFileSizeLow;
// #ifdef _WIN32_WCE
//  DWORD dwOID;
// #else
    DWORD           dwReserved0;
    DWORD           dwReserved1;
// #endif
    WCHAR[MAX_PATH] cFileName = 0;
// #ifndef _WIN32_WCE
    WCHAR[14]       cAlternateFileName = 0;
// #endif
}
alias WIN32_FIND_DATAW* PWIN32_FIND_DATAW, LPWIN32_FIND_DATAW;

struct WIN32_STREAM_ID {
    DWORD         dwStreamId;
    DWORD         dwStreamAttributes;
    LARGE_INTEGER Size;
    DWORD         dwStreamNameSize;
    WCHAR         _cStreamName = 0;

    WCHAR* cStreamName() return { return &_cStreamName; }
}
alias WIN32_STREAM_ID* LPWIN32_STREAM_ID;

enum FINDEX_INFO_LEVELS {
    FindExInfoStandard,
    FindExInfoMaxInfoLevel
}

enum FINDEX_SEARCH_OPS {
    FindExSearchNameMatch,
    FindExSearchLimitToDirectories,
    FindExSearchLimitToDevices,
    FindExSearchMaxSearchOp
}

enum ACL_INFORMATION_CLASS {
    AclRevisionInformation = 1,
    AclSizeInformation
}

struct HW_PROFILE_INFOA {
    DWORD dwDockInfo;
    CHAR[HW_PROFILE_GUIDLEN] szHwProfileGuid = 0;
    CHAR[MAX_PROFILE_LEN]    szHwProfileName = 0;
}
alias HW_PROFILE_INFOA* LPHW_PROFILE_INFOA;

struct HW_PROFILE_INFOW {
    DWORD dwDockInfo;
    WCHAR[HW_PROFILE_GUIDLEN] szHwProfileGuid = 0;
    WCHAR[MAX_PROFILE_LEN]    szHwProfileName = 0;
}
alias HW_PROFILE_INFOW* LPHW_PROFILE_INFOW;

/*  ??? MSDN documents this only for Windows CE/Mobile, but it's used by
 *  GetFileAttributesEx, which is in desktop Windows.
 */
enum GET_FILEEX_INFO_LEVELS {
    GetFileExInfoStandard,
    GetFileExMaxInfoLevel
}

struct SYSTEM_INFO {
    union {
        DWORD dwOemId;
        struct {
            WORD wProcessorArchitecture;
            WORD wReserved;
        }
    }
    DWORD dwPageSize;
    PVOID lpMinimumApplicationAddress;
    PVOID lpMaximumApplicationAddress;
    DWORD_PTR dwActiveProcessorMask;
    DWORD dwNumberOfProcessors;
    DWORD dwProcessorType;
    DWORD dwAllocationGranularity;
    WORD  wProcessorLevel;
    WORD  wProcessorRevision;
}
alias SYSTEM_INFO* LPSYSTEM_INFO;

static if (_WIN32_WINNT >= 0x500) {
    struct SYSTEM_POWER_STATUS {
        BYTE ACLineStatus;
        BYTE BatteryFlag;
        BYTE BatteryLifePercent;
        BYTE Reserved1;
        DWORD BatteryLifeTime;
        DWORD BatteryFullLifeTime;
    }
    alias SYSTEM_POWER_STATUS* LPSYSTEM_POWER_STATUS;
}

struct TIME_ZONE_INFORMATION {
    LONG       Bias;
    WCHAR[32]  StandardName = 0;
    SYSTEMTIME StandardDate;
    LONG       StandardBias;
    WCHAR[32]  DaylightName = 0;
    SYSTEMTIME DaylightDate;
    LONG       DaylightBias;
}
alias TIME_ZONE_INFORMATION* LPTIME_ZONE_INFORMATION;

// Does not exist in Windows headers, only MSDN
// documentation (for TIME_ZONE_INFORMATION).
// Provided solely for compatibility with the old
// core.sys.windows.windows
struct REG_TZI_FORMAT {
    LONG Bias;
    LONG StandardBias;
    LONG DaylightBias;
    SYSTEMTIME StandardDate;
    SYSTEMTIME DaylightDate;
}

// MSDN documents this, possibly erroneously, as Win2000+.
struct MEMORYSTATUS {
    DWORD dwLength;
    DWORD dwMemoryLoad;
    SIZE_T dwTotalPhys;
    SIZE_T dwAvailPhys;
    SIZE_T dwTotalPageFile;
    SIZE_T dwAvailPageFile;
    SIZE_T dwTotalVirtual;
    SIZE_T dwAvailVirtual;
}
alias MEMORYSTATUS* LPMEMORYSTATUS;

static if (_WIN32_WINNT >= 0x500) {
    struct MEMORYSTATUSEX {
        DWORD     dwLength;
        DWORD     dwMemoryLoad;
        DWORDLONG ullTotalPhys;
        DWORDLONG ullAvailPhys;
        DWORDLONG ullTotalPageFile;
        DWORDLONG ullAvailPageFile;
        DWORDLONG ullTotalVirtual;
        DWORDLONG ullAvailVirtual;
        DWORDLONG ullAvailExtendedVirtual;
    }
    alias MEMORYSTATUSEX* LPMEMORYSTATUSEX;
}

struct LDT_ENTRY {
    WORD LimitLow;
    WORD BaseLow;
    struct {
        BYTE BaseMid;
        BYTE Flags1;
        BYTE Flags2;
        BYTE BaseHi;

        byte Type(byte f)        { Flags1 = cast(BYTE) ((Flags1 & 0xE0) | f); return cast(byte)(f & 0x1F); }
        byte Dpl(byte f)         { Flags1 = cast(BYTE) ((Flags1 & 0x9F) | (f<<5)); return cast(byte)(f & 3); }
        bool Pres(bool f)        { Flags1 = cast(BYTE) ((Flags1 & 0x7F) | (f<<7)); return f; }

        byte LimitHi(byte f)     { Flags2 = cast(BYTE) ((Flags2 & 0xF0) | (f&0x0F)); return cast(byte)(f & 0x0F); }
        bool Sys(bool f)         { Flags2 = cast(BYTE) ((Flags2 & 0xEF) | (f<<4)); return f; }
        // Next bit is reserved
        bool Default_Big(bool f) { Flags2 = cast(BYTE) ((Flags2 & 0xBF) | (f<<6)); return f; }
        bool Granularity(bool f) { Flags2 = cast(BYTE) ((Flags2 & 0x7F) | (f<<7)); return f; }

        byte Type()        { return cast(byte) (Flags1 & 0x1F); }
        byte Dpl()         { return cast(byte) ((Flags1 & 0x60)>>5); }
        bool Pres()        { return cast(bool) (Flags1 & 0x80); }

        byte LimitHi()     { return cast(byte) (Flags2 & 0x0F); }
        bool Sys()         { return cast(bool) (Flags2 & 0x10); }
        bool Default_Big() { return cast(bool) (Flags2 & 0x40); }
        bool Granularity() { return cast(bool) (Flags2 & 0x80); }
    }
/+
    union  HighWord {
        struct Bytes {
            BYTE BaseMid;
            BYTE Flags1;
            BYTE Flags2;
            BYTE BaseHi;
        }
    struct Bits {
        DWORD BaseMid:8;
        DWORD Type:5;
        DWORD Dpl:2;
        DWORD Pres:1;
        DWORD LimitHi:4;
        DWORD Sys:1;
        DWORD Reserved_0:1;
        DWORD Default_Big:1;
        DWORD Granularity:1;
        DWORD BaseHi:8;
    }
    }
+/
}
alias LDT_ENTRY* PLDT_ENTRY, LPLDT_ENTRY;

/*  As with the other memory management functions and structures, MSDN's
 *  Windows version info shall be taken with a cup of salt.
 */
struct PROCESS_HEAP_ENTRY {
    PVOID lpData;
    DWORD cbData;
    BYTE  cbOverhead;
    BYTE  iRegionIndex;
    WORD  wFlags;
    union {
        struct _Block {
            HANDLE   hMem;
            DWORD[3] dwReserved;
        }
        _Block Block;
        struct _Region {
            DWORD    dwCommittedSize;
            DWORD    dwUnCommittedSize;
            LPVOID   lpFirstBlock;
            LPVOID   lpLastBlock;
        }
        _Region Region;
    }
}
alias PROCESS_HEAP_ENTRY* LPPROCESS_HEAP_ENTRY;

struct OFSTRUCT {
    BYTE      cBytes = OFSTRUCT.sizeof;
    BYTE      fFixedDisk;
    WORD      nErrCode;
    WORD      Reserved1;
    WORD      Reserved2;
    CHAR[128] szPathName = 0; // const OFS_MAXPATHNAME = 128;
}
alias OFSTRUCT* LPOFSTRUCT, POFSTRUCT;

/*  ??? MSDN documents this only for Windows CE, but it's used by
 *  ImageGetCertificateData, which is in desktop Windows.
 */
struct WIN_CERTIFICATE {
    DWORD dwLength;
    WORD  wRevision;
    WORD  wCertificateType;
    BYTE  _bCertificate;

    BYTE* bCertificate() return { return &_bCertificate; }
}
alias WIN_CERTIFICATE* LPWIN_CERTIFICATE;

static if (_WIN32_WINNT >= 0x500) {
    enum COMPUTER_NAME_FORMAT {
        ComputerNameNetBIOS,
        ComputerNameDnsHostname,
        ComputerNameDnsDomain,
        ComputerNameDnsFullyQualified,
        ComputerNamePhysicalNetBIOS,
        ComputerNamePhysicalDnsHostname,
        ComputerNamePhysicalDnsDomain,
        ComputerNamePhysicalDnsFullyQualified,
        ComputerNameMax
    }
}

static if (_WIN32_WINNT >= 0x501) {
    struct ACTCTXA {
        ULONG cbSize = this.sizeof;
        DWORD dwFlags;
        LPCSTR lpSource;
        USHORT wProcessorArchitecture;
        LANGID wLangId;
        LPCSTR lpAssemblyDirectory;
        LPCSTR lpResourceName;
        LPCSTR lpApplicationName;
        HMODULE hModule;
    }
    alias ACTCTXA*        PACTCTXA;
    alias const(ACTCTXA)* PCACTCTXA;

    struct ACTCTXW {
        ULONG cbSize = this.sizeof;
        DWORD dwFlags;
        LPCWSTR lpSource;
        USHORT wProcessorArchitecture;
        LANGID wLangId;
        LPCWSTR lpAssemblyDirectory;
        LPCWSTR lpResourceName;
        LPCWSTR lpApplicationName;
        HMODULE hModule;
    }
    alias ACTCTXW*        PACTCTXW;
    alias const(ACTCTXW)* PCACTCTXW;

    struct ACTCTX_SECTION_KEYED_DATA {
        ULONG cbSize = this.sizeof;
        ULONG ulDataFormatVersion;
        PVOID lpData;
        ULONG ulLength;
        PVOID lpSectionGlobalData;
        ULONG ulSectionGlobalDataLength;
        PVOID lpSectionBase;
        ULONG ulSectionTotalLength;
        HANDLE hActCtx;
        HANDLE ulAssemblyRosterIndex;
    }
    alias ACTCTX_SECTION_KEYED_DATA*        PACTCTX_SECTION_KEYED_DATA;
    alias const(ACTCTX_SECTION_KEYED_DATA)* PCACTCTX_SECTION_KEYED_DATA;

    enum MEMORY_RESOURCE_NOTIFICATION_TYPE {
        LowMemoryResourceNotification,
        HighMemoryResourceNotification
    }

} // (_WIN32_WINNT >= 0x501)

static if (_WIN32_WINNT >= 0x410) {
    /*  apparently used only by SetThreadExecutionState (Win2000+)
     *  and DDK functions (version compatibility not established)
     */
    alias DWORD EXECUTION_STATE;
}

// Callbacks
extern (Windows) {
    alias DWORD function(LPVOID) LPTHREAD_START_ROUTINE;
    alias DWORD function(LARGE_INTEGER, LARGE_INTEGER, LARGE_INTEGER, LARGE_INTEGER,
        DWORD, DWORD, HANDLE, HANDLE, LPVOID)  LPPROGRESS_ROUTINE;
    alias void function(PVOID) LPFIBER_START_ROUTINE;

    alias BOOL function(HMODULE, LPCSTR, LPCSTR, WORD, LONG_PTR) ENUMRESLANGPROCA;
    alias BOOL function(HMODULE, LPCWSTR, LPCWSTR, WORD, LONG_PTR) ENUMRESLANGPROCW;
    alias BOOL function(HMODULE, LPCSTR, LPSTR, LONG_PTR) ENUMRESNAMEPROCA;
    alias BOOL function(HMODULE, LPCWSTR, LPWSTR, LONG_PTR) ENUMRESNAMEPROCW;
    alias BOOL function(HMODULE, LPSTR, LONG_PTR) ENUMRESTYPEPROCA;
    alias BOOL function(HMODULE, LPWSTR, LONG_PTR) ENUMRESTYPEPROCW;
    alias void function(DWORD, DWORD, LPOVERLAPPED) LPOVERLAPPED_COMPLETION_ROUTINE;
    alias LONG function(LPEXCEPTION_POINTERS) PTOP_LEVEL_EXCEPTION_FILTER;
    alias PTOP_LEVEL_EXCEPTION_FILTER LPTOP_LEVEL_EXCEPTION_FILTER;

    alias void function(ULONG_PTR) PAPCFUNC;
    alias void function(PVOID, DWORD, DWORD) PTIMERAPCROUTINE;

    static if (_WIN32_WINNT >= 0x500) {
        alias void function(PVOID, BOOLEAN) WAITORTIMERCALLBACK;
    }
}

LPTSTR MAKEINTATOM()(ushort i) {
    return cast(LPTSTR) cast(size_t) i;
}

extern (Windows) nothrow @nogc {
    // The following Win16 functions are obselete in Win32.
    int _hread(HFILE, LPVOID, int);
    int _hwrite(HFILE, LPCSTR, int);
    HFILE _lclose(HFILE);
    HFILE _lcreat(LPCSTR, int);
    LONG _llseek(HFILE, LONG, int);
    HFILE _lopen(LPCSTR, int);
    UINT _lread(HFILE, LPVOID, UINT);
    UINT _lwrite(HFILE, LPCSTR, UINT);
    SIZE_T GlobalCompact(DWORD);
    VOID GlobalFix(HGLOBAL);

    // MSDN contradicts itself on GlobalFlags:
    // "This function is provided only for compatibility with 16-bit versions of Windows."
    // but also requires Windows 2000 or above
    UINT GlobalFlags(HGLOBAL);
    VOID GlobalUnfix(HGLOBAL);
    BOOL GlobalUnWire(HGLOBAL);
    PVOID GlobalWire(HGLOBAL);
    SIZE_T LocalCompact(UINT);
    UINT LocalFlags(HLOCAL);
    SIZE_T LocalShrink(HLOCAL, UINT);

    /+
    //--------------------------------------
    // These functions are problematic

    version (UseNtoSKernel) {}else {
        /* CAREFUL: These are exported from ntoskrnl.exe and declared in winddk.h
           as __fastcall functions, but are  exported from kernel32.dll as __stdcall */
        static if (_WIN32_WINNT >= 0x501) {
         VOID InitializeSListHead(PSLIST_HEADER);
        }
        LONG InterlockedCompareExchange(LPLONG, LONG, LONG);
        // PVOID WINAPI InterlockedCompareExchangePointer(PVOID*, PVOID, PVOID);
        (PVOID)InterlockedCompareExchange((LPLONG)(d)    (PVOID)InterlockedCompareExchange((LPLONG)(d), (LONG)(e), (LONG)(c))
        LONG InterlockedDecrement(LPLONG);
        LONG InterlockedExchange(LPLONG, LONG);
        // PVOID WINAPI InterlockedExchangePointer(PVOID*, PVOID);
        (PVOID)InterlockedExchange((LPLONG)((PVOID)InterlockedExchange((LPLONG)(t), (LONG)(v))
        LONG InterlockedExchangeAdd(LPLONG, LONG);

        static if (_WIN32_WINNT >= 0x501) {
        PSLIST_ENTRY InterlockedFlushSList(PSLIST_HEADER);
        }
        LONG InterlockedIncrement(LPLONG);
        static if (_WIN32_WINNT >= 0x501) {
        PSLIST_ENTRY InterlockedPopEntrySList(PSLIST_HEADER);
        PSLIST_ENTRY InterlockedPushEntrySList(PSLIST_HEADER, PSLIST_ENTRY);
        }
    } // #endif //  __USE_NTOSKRNL__
    //--------------------------------------
    +/

    LONG InterlockedIncrement(LPLONG lpAddend);
    LONG InterlockedDecrement(LPLONG lpAddend);
    LONG InterlockedExchange(LPLONG Target, LONG Value);
    LONG InterlockedExchangeAdd(LPLONG Addend, LONG Value);
    LONG InterlockedCompareExchange(LONG *Destination, LONG Exchange, LONG Comperand);

    ATOM AddAtomA(LPCSTR);
    ATOM AddAtomW(LPCWSTR);
    BOOL AreFileApisANSI();
    BOOL Beep(DWORD, DWORD);
    HANDLE BeginUpdateResourceA(LPCSTR, BOOL);
    HANDLE BeginUpdateResourceW(LPCWSTR, BOOL);
    BOOL BuildCommDCBA(LPCSTR, LPDCB);
    BOOL BuildCommDCBW(LPCWSTR, LPDCB);
    BOOL BuildCommDCBAndTimeoutsA(LPCSTR, LPDCB, LPCOMMTIMEOUTS);
    BOOL BuildCommDCBAndTimeoutsW(LPCWSTR, LPDCB, LPCOMMTIMEOUTS);
    BOOL CallNamedPipeA(LPCSTR, PVOID, DWORD, PVOID, DWORD, PDWORD, DWORD);
    BOOL CallNamedPipeW(LPCWSTR, PVOID, DWORD, PVOID, DWORD, PDWORD, DWORD);
    BOOL CancelDeviceWakeupRequest(HANDLE);
    BOOL CheckTokenMembership(HANDLE, PSID, PBOOL);
    BOOL ClearCommBreak(HANDLE);
    BOOL ClearCommError(HANDLE, PDWORD, LPCOMSTAT);
    BOOL CloseHandle(HANDLE) @trusted;
    BOOL CommConfigDialogA(LPCSTR, HWND, LPCOMMCONFIG);
    BOOL CommConfigDialogW(LPCWSTR, HWND, LPCOMMCONFIG);
    LONG CompareFileTime(const(FILETIME)*, const(FILETIME)*);
    BOOL ContinueDebugEvent(DWORD, DWORD, DWORD);
    BOOL CopyFileA(LPCSTR, LPCSTR, BOOL);
    BOOL CopyFileW(LPCWSTR, LPCWSTR, BOOL);
    BOOL CopyFileExA(LPCSTR, LPCSTR, LPPROGRESS_ROUTINE, LPVOID, LPBOOL, DWORD);
    BOOL CopyFileExW(LPCWSTR, LPCWSTR, LPPROGRESS_ROUTINE, LPVOID, LPBOOL, DWORD);

    /+ FIXME
    alias memmove RtlMoveMemory;
    alias memcpy RtlCopyMemory;

    void RtlFillMemory(PVOID dest, SIZE_T len, BYTE fill) {
        memset(dest, fill, len);
    }

    void RtlZeroMemory(PVOID dest, SIZE_T len) {
        RtlFillMemory(dest, len, 0);
    }

    alias RtlMoveMemory MoveMemory;
    alias RtlCopyMemory CopyMemory;
    alias RtlFillMemory FillMemory;
    alias RtlZeroMemory ZeroMemory;
    +/
    BOOL CreateDirectoryA(LPCSTR, LPSECURITY_ATTRIBUTES);
    BOOL CreateDirectoryW(LPCWSTR, LPSECURITY_ATTRIBUTES);
    BOOL CreateDirectoryExA(LPCSTR, LPCSTR, LPSECURITY_ATTRIBUTES);
    BOOL CreateDirectoryExW(LPCWSTR, LPCWSTR, LPSECURITY_ATTRIBUTES);
    HANDLE CreateEventA(LPSECURITY_ATTRIBUTES, BOOL, BOOL, LPCSTR);
    HANDLE CreateEventW(LPSECURITY_ATTRIBUTES, BOOL, BOOL, LPCWSTR);
    HANDLE CreateFileA(LPCSTR, DWORD, DWORD, LPSECURITY_ATTRIBUTES, DWORD, DWORD, HANDLE);
    HANDLE CreateFileW(LPCWSTR, DWORD, DWORD, LPSECURITY_ATTRIBUTES, DWORD, DWORD, HANDLE);
    HANDLE CreateIoCompletionPort(HANDLE, HANDLE, ULONG_PTR, DWORD);
    HANDLE CreateMailslotA(LPCSTR, DWORD, DWORD, LPSECURITY_ATTRIBUTES);
    HANDLE CreateMailslotW(LPCWSTR, DWORD, DWORD, LPSECURITY_ATTRIBUTES);
    HANDLE CreateMutexA(LPSECURITY_ATTRIBUTES, BOOL, LPCSTR);
    HANDLE CreateMutexW(LPSECURITY_ATTRIBUTES, BOOL, LPCWSTR);
    BOOL CreatePipe(PHANDLE, PHANDLE, LPSECURITY_ATTRIBUTES, DWORD);
    BOOL CreateProcessA(LPCSTR, LPSTR, LPSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES, BOOL, DWORD, PVOID, LPCSTR, LPSTARTUPINFOA, LPPROCESS_INFORMATION);
    BOOL CreateProcessW(LPCWSTR, LPWSTR, LPSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES, BOOL, DWORD, PVOID, LPCWSTR, LPSTARTUPINFOW, LPPROCESS_INFORMATION);
    HANDLE CreateSemaphoreA(LPSECURITY_ATTRIBUTES, LONG, LONG, LPCSTR) @trusted;
    HANDLE CreateSemaphoreW(LPSECURITY_ATTRIBUTES, LONG, LONG, LPCWSTR) @trusted;
    HANDLE CreateThread(LPSECURITY_ATTRIBUTES, SIZE_T, LPTHREAD_START_ROUTINE, PVOID, DWORD, PDWORD);
    BOOL DebugActiveProcess(DWORD);
    void DebugBreak();
    ATOM DeleteAtom(ATOM);
    void DeleteCriticalSection(PCRITICAL_SECTION);
    BOOL DeleteFileA(LPCSTR);
    BOOL DeleteFileW(LPCWSTR);
    BOOL DisableThreadLibraryCalls(HMODULE);
    BOOL DosDateTimeToFileTime(WORD, WORD, LPFILETIME);
    BOOL DuplicateHandle(HANDLE, HANDLE, HANDLE, PHANDLE, DWORD, BOOL, DWORD);
    BOOL EndUpdateResourceA(HANDLE, BOOL);
    BOOL EndUpdateResourceW(HANDLE, BOOL);
    void EnterCriticalSection(LPCRITICAL_SECTION);
    void EnterCriticalSection(shared(CRITICAL_SECTION)*);
    BOOL EnumResourceLanguagesA(HMODULE, LPCSTR, LPCSTR, ENUMRESLANGPROC, LONG_PTR);
    BOOL EnumResourceLanguagesW(HMODULE, LPCWSTR, LPCWSTR, ENUMRESLANGPROC, LONG_PTR);
    BOOL EnumResourceNamesA(HMODULE, LPCSTR, ENUMRESNAMEPROC, LONG_PTR);
    BOOL EnumResourceNamesW(HMODULE, LPCWSTR, ENUMRESNAMEPROC, LONG_PTR);
    BOOL EnumResourceTypesA(HMODULE, ENUMRESTYPEPROC, LONG_PTR);
    BOOL EnumResourceTypesW(HMODULE, ENUMRESTYPEPROC, LONG_PTR);
    BOOL EscapeCommFunction(HANDLE, DWORD);
    void ExitProcess(UINT); // Never returns
    void ExitThread(DWORD); // Never returns
    DWORD ExpandEnvironmentStringsA(LPCSTR, LPSTR, DWORD);
    DWORD ExpandEnvironmentStringsW(LPCWSTR, LPWSTR, DWORD);
    void FatalAppExitA(UINT, LPCSTR);
    void FatalAppExitW(UINT, LPCWSTR);
    void FatalExit(int);
    BOOL FileTimeToDosDateTime(const(FILETIME)*, LPWORD, LPWORD);
    BOOL FileTimeToLocalFileTime(const(FILETIME)*, LPFILETIME);
    BOOL FileTimeToSystemTime(const(FILETIME)*, LPSYSTEMTIME);
    ATOM FindAtomA(LPCSTR);
    ATOM FindAtomW(LPCWSTR);
    BOOL FindClose(HANDLE);
    BOOL FindCloseChangeNotification(HANDLE);
    HANDLE FindFirstChangeNotificationA(LPCSTR, BOOL, DWORD);
    HANDLE FindFirstChangeNotificationW(LPCWSTR, BOOL, DWORD);
    HANDLE FindFirstFileA(LPCSTR, LPWIN32_FIND_DATAA);
    HANDLE FindFirstFileW(LPCWSTR, LPWIN32_FIND_DATAW);
    BOOL FindNextChangeNotification(HANDLE);
    BOOL FindNextFileA(HANDLE, LPWIN32_FIND_DATAA);
    BOOL FindNextFileW(HANDLE, LPWIN32_FIND_DATAW);
    HRSRC FindResourceA(HMODULE, LPCSTR, LPCSTR);
    HRSRC FindResourceW(HINSTANCE, LPCWSTR, LPCWSTR);
    HRSRC FindResourceExA(HINSTANCE, LPCSTR, LPCSTR, WORD);
    HRSRC FindResourceExW(HINSTANCE, LPCWSTR, LPCWSTR, WORD);
    BOOL FlushFileBuffers(HANDLE);
    BOOL FlushInstructionCache(HANDLE, PCVOID, SIZE_T);
    DWORD FormatMessageA(DWORD, PCVOID, DWORD, DWORD, LPSTR, DWORD, va_list*);
    DWORD FormatMessageW(DWORD, PCVOID, DWORD, DWORD, LPWSTR, DWORD, va_list*);
    BOOL FreeEnvironmentStringsA(LPSTR);
    BOOL FreeEnvironmentStringsW(LPWSTR);
    BOOL FreeLibrary(HMODULE);
    void FreeLibraryAndExitThread(HMODULE, DWORD); // never returns
    BOOL FreeResource(HGLOBAL);
    UINT GetAtomNameA(ATOM, LPSTR, int);
    UINT GetAtomNameW(ATOM, LPWSTR, int);
    LPSTR GetCommandLineA();
    LPWSTR GetCommandLineW();
    BOOL GetCommConfig(HANDLE, LPCOMMCONFIG, PDWORD);
    BOOL GetCommMask(HANDLE, PDWORD);
    BOOL GetCommModemStatus(HANDLE, PDWORD);
    BOOL GetCommProperties(HANDLE, LPCOMMPROP);
    BOOL GetCommState(HANDLE, LPDCB);
    BOOL GetCommTimeouts(HANDLE, LPCOMMTIMEOUTS);
    BOOL GetComputerNameA(LPSTR, PDWORD);
    BOOL GetComputerNameW(LPWSTR, PDWORD);
    DWORD GetCurrentDirectoryA(DWORD, LPSTR);
    DWORD GetCurrentDirectoryW(DWORD, LPWSTR);
    HANDLE GetCurrentProcess();
    DWORD GetCurrentProcessId();
    HANDLE GetCurrentThread();
/* In MinGW:
#ifdef _WIN32_WCE
extern DWORD GetCurrentThreadId(void);
#else
WINBASEAPI DWORD WINAPI GetCurrentThreadId(void);
#endif
*/
    DWORD GetCurrentThreadId();

    alias GetTickCount GetCurrentTime;

    BOOL GetDefaultCommConfigA(LPCSTR, LPCOMMCONFIG, PDWORD);
    BOOL GetDefaultCommConfigW(LPCWSTR, LPCOMMCONFIG, PDWORD);
    BOOL GetDiskFreeSpaceA(LPCSTR, PDWORD, PDWORD, PDWORD, PDWORD);
    BOOL GetDiskFreeSpaceW(LPCWSTR, PDWORD, PDWORD, PDWORD, PDWORD);
    BOOL GetDiskFreeSpaceExA(LPCSTR, PULARGE_INTEGER, PULARGE_INTEGER, PULARGE_INTEGER);
    BOOL GetDiskFreeSpaceExW(LPCWSTR, PULARGE_INTEGER, PULARGE_INTEGER, PULARGE_INTEGER);
    UINT GetDriveTypeA(LPCSTR);
    UINT GetDriveTypeW(LPCWSTR);
    LPSTR GetEnvironmentStringsA();
    LPWSTR GetEnvironmentStringsW();
    DWORD GetEnvironmentVariableA(LPCSTR, LPSTR, DWORD);
    DWORD GetEnvironmentVariableW(LPCWSTR, LPWSTR, DWORD);
    BOOL GetExitCodeProcess(HANDLE, PDWORD);
    BOOL GetExitCodeThread(HANDLE, PDWORD);
    DWORD GetFileAttributesA(LPCSTR);
    DWORD GetFileAttributesW(LPCWSTR);
    BOOL GetFileInformationByHandle(HANDLE, LPBY_HANDLE_FILE_INFORMATION);
    DWORD GetFileSize(HANDLE, PDWORD);
    BOOL GetFileTime(HANDLE, LPFILETIME, LPFILETIME, LPFILETIME);
    DWORD GetFileType(HANDLE);
    DWORD GetFullPathNameA(LPCSTR, DWORD, LPSTR, LPSTR*);
    DWORD GetFullPathNameW(LPCWSTR, DWORD, LPWSTR, LPWSTR*);
    DWORD GetLastError() @trusted;
    void GetLocalTime(LPSYSTEMTIME);
    DWORD GetLogicalDrives();
    DWORD GetLogicalDriveStringsA(DWORD, LPSTR);
    DWORD GetLogicalDriveStringsW(DWORD, LPWSTR);
    BOOL GetMailslotInfo(HANDLE, PDWORD, PDWORD, PDWORD, PDWORD);
    DWORD GetModuleFileNameA(HINSTANCE, LPSTR, DWORD);
    DWORD GetModuleFileNameW(HINSTANCE, LPWSTR, DWORD);
    HMODULE GetModuleHandleA(LPCSTR);
    HMODULE GetModuleHandleW(LPCWSTR);
    BOOL GetNamedPipeHandleStateA(HANDLE, PDWORD, PDWORD, PDWORD, PDWORD, LPSTR, DWORD);
    BOOL GetNamedPipeHandleStateW(HANDLE, PDWORD, PDWORD, PDWORD, PDWORD, LPWSTR, DWORD);
    BOOL GetNamedPipeInfo(HANDLE, PDWORD, PDWORD, PDWORD, PDWORD);
    BOOL GetOverlappedResult(HANDLE, LPOVERLAPPED, PDWORD, BOOL);
    DWORD GetPriorityClass(HANDLE);
    UINT GetPrivateProfileIntA(LPCSTR, LPCSTR, INT, LPCSTR);
    UINT GetPrivateProfileIntW(LPCWSTR, LPCWSTR, INT, LPCWSTR);
    DWORD GetPrivateProfileSectionA(LPCSTR, LPSTR, DWORD, LPCSTR);
    DWORD GetPrivateProfileSectionW(LPCWSTR, LPWSTR, DWORD, LPCWSTR);
    DWORD GetPrivateProfileSectionNamesA(LPSTR, DWORD, LPCSTR);
    DWORD GetPrivateProfileSectionNamesW(LPWSTR, DWORD, LPCWSTR);
    DWORD GetPrivateProfileStringA(LPCSTR, LPCSTR, LPCSTR, LPSTR, DWORD, LPCSTR);
    DWORD GetPrivateProfileStringW(LPCWSTR, LPCWSTR, LPCWSTR, LPWSTR, DWORD, LPCWSTR);
    BOOL GetPrivateProfileStructA(LPCSTR, LPCSTR, LPVOID, UINT, LPCSTR);
    BOOL GetPrivateProfileStructW(LPCWSTR, LPCWSTR, LPVOID, UINT, LPCWSTR);
    FARPROC GetProcAddress(HMODULE, LPCSTR); // 1st param wrongly HINSTANCE in MinGW
    BOOL GetProcessAffinityMask(HANDLE, PDWORD_PTR, PDWORD_PTR);
    DWORD GetProcessVersion(DWORD);
    UINT GetProfileIntA(LPCSTR, LPCSTR, INT);
    UINT GetProfileIntW(LPCWSTR, LPCWSTR, INT);
    DWORD GetProfileSectionA(LPCSTR, LPSTR, DWORD);
    DWORD GetProfileSectionW(LPCWSTR, LPWSTR, DWORD);
    DWORD GetProfileStringA(LPCSTR, LPCSTR, LPCSTR, LPSTR, DWORD);
    DWORD GetProfileStringW(LPCWSTR, LPCWSTR, LPCWSTR, LPWSTR, DWORD);
    DWORD GetShortPathNameA(LPCSTR, LPSTR, DWORD);
    DWORD GetShortPathNameW(LPCWSTR, LPWSTR, DWORD);
    VOID GetStartupInfoA(LPSTARTUPINFOA);
    VOID GetStartupInfoW(LPSTARTUPINFOW);
    HANDLE GetStdHandle(DWORD);
    UINT GetSystemDirectoryA(LPSTR, UINT);
    UINT GetSystemDirectoryW(LPWSTR, UINT);
    VOID GetSystemInfo(LPSYSTEM_INFO);
    VOID GetSystemTime(LPSYSTEMTIME);
    BOOL GetSystemTimeAdjustment(PDWORD, PDWORD, PBOOL);
    void GetSystemTimeAsFileTime(LPFILETIME);
    UINT GetTempFileNameA(LPCSTR, LPCSTR, UINT, LPSTR);
    UINT GetTempFileNameW(LPCWSTR, LPCWSTR, UINT, LPWSTR);
    DWORD GetTempPathA(DWORD, LPSTR);
    DWORD GetTempPathW(DWORD, LPWSTR);
    BOOL GetThreadContext(HANDLE, LPCONTEXT);
    int GetThreadPriority(HANDLE);
    BOOL GetThreadSelectorEntry(HANDLE, DWORD, LPLDT_ENTRY);
    DWORD GetTickCount();
    DWORD GetTimeZoneInformation(LPTIME_ZONE_INFORMATION);
    BOOL GetUserNameA (LPSTR, PDWORD);
    BOOL GetUserNameW(LPWSTR, PDWORD);
    DWORD GetVersion();
    BOOL GetVersionExA(LPOSVERSIONINFOA);
    BOOL GetVersionExW(LPOSVERSIONINFOW);
    BOOL GetVolumeInformationA(LPCSTR, LPSTR, DWORD, PDWORD, PDWORD, PDWORD, LPSTR, DWORD);
    BOOL GetVolumeInformationW(LPCWSTR, LPWSTR, DWORD, PDWORD, PDWORD, PDWORD, LPWSTR, DWORD);
    UINT GetWindowsDirectoryA(LPSTR, UINT);
    UINT GetWindowsDirectoryW(LPWSTR, UINT);
    DWORD GetWindowThreadProcessId(HWND, PDWORD);
    ATOM GlobalAddAtomA(LPCSTR);
    ATOM GlobalAddAtomW(LPCWSTR);
    ATOM GlobalDeleteAtom(ATOM);
    ATOM GlobalFindAtomA(LPCSTR);
    ATOM GlobalFindAtomW(LPCWSTR);
    UINT GlobalGetAtomNameA(ATOM, LPSTR, int);
    UINT GlobalGetAtomNameW(ATOM, LPWSTR, int);

    bool HasOverlappedIoCompleted(LPOVERLAPPED lpOverlapped) {
        return lpOverlapped.Internal != STATUS_PENDING;
    }

    BOOL InitAtomTable(DWORD);
    VOID InitializeCriticalSection(LPCRITICAL_SECTION) @trusted;
    /*  ??? The next two are allegedly obsolete and "supported only for
     *  backward compatibility with the 16-bit Windows API".  Yet the
     *  replacements IsBadReadPtr and IsBadWritePtr are apparently Win2000+
     *  only.  Where's the mistake?
     */
    BOOL IsBadHugeReadPtr(PCVOID, UINT_PTR);
    BOOL IsBadHugeWritePtr(PVOID, UINT_PTR);
    BOOL IsBadReadPtr(PCVOID, UINT_PTR);
    BOOL IsBadStringPtrA(LPCSTR, UINT_PTR);
    BOOL IsBadStringPtrW(LPCWSTR, UINT_PTR);
    BOOL IsBadWritePtr(PVOID, UINT_PTR);
    void LeaveCriticalSection(LPCRITICAL_SECTION);
    void LeaveCriticalSection(shared(CRITICAL_SECTION)*);
    HINSTANCE LoadLibraryA(LPCSTR);
    HINSTANCE LoadLibraryW(LPCWSTR);
    HINSTANCE LoadLibraryExA(LPCSTR, HANDLE, DWORD);
    HINSTANCE LoadLibraryExW(LPCWSTR, HANDLE, DWORD);
    DWORD LoadModule(LPCSTR, PVOID);
    HGLOBAL LoadResource(HINSTANCE, HRSRC);
    BOOL LocalFileTimeToFileTime(const(FILETIME)*, LPFILETIME);
    BOOL LockFile(HANDLE, DWORD, DWORD, DWORD, DWORD);
    PVOID LockResource(HGLOBAL);

    LPSTR lstrcatA(LPSTR, LPCSTR);
    LPWSTR lstrcatW(LPWSTR, LPCWSTR);
    int lstrcmpA(LPCSTR, LPCSTR);
    int lstrcmpiA(LPCSTR, LPCSTR);
    int lstrcmpiW(LPCWSTR, LPCWSTR);
    int lstrcmpW(LPCWSTR, LPCWSTR);
    LPSTR lstrcpyA(LPSTR, LPCSTR);
    LPSTR lstrcpynA(LPSTR, LPCSTR, int);
    LPWSTR lstrcpynW(LPWSTR, LPCWSTR, int);
    LPWSTR lstrcpyW(LPWSTR, LPCWSTR);
    int lstrlenA(LPCSTR);
    int lstrlenW(LPCWSTR);

    BOOL MoveFileA(LPCSTR, LPCSTR);
    BOOL MoveFileW(LPCWSTR, LPCWSTR);
    int MulDiv(int, int, int);
    HANDLE OpenEventA(DWORD, BOOL, LPCSTR);
    HANDLE OpenEventW(DWORD, BOOL, LPCWSTR);
    deprecated HFILE OpenFile(LPCSTR, LPOFSTRUCT, UINT);
    HANDLE OpenMutexA(DWORD, BOOL, LPCSTR);
    HANDLE OpenMutexW(DWORD, BOOL, LPCWSTR);
    HANDLE OpenProcess(DWORD, BOOL, DWORD);
    HANDLE OpenSemaphoreA(DWORD, BOOL, LPCSTR);
    HANDLE OpenSemaphoreW(DWORD, BOOL, LPCWSTR);
    void OutputDebugStringA(LPCSTR);
    void OutputDebugStringW(LPCWSTR);
    BOOL PeekNamedPipe(HANDLE, PVOID, DWORD, PDWORD, PDWORD, PDWORD);
    BOOL PulseEvent(HANDLE);
    BOOL PurgeComm(HANDLE, DWORD);
    BOOL QueryPerformanceCounter(PLARGE_INTEGER);
    BOOL QueryPerformanceFrequency(PLARGE_INTEGER);
    DWORD QueueUserAPC(PAPCFUNC, HANDLE, ULONG_PTR);
    void RaiseException(DWORD, DWORD, DWORD, const(ULONG_PTR)*);
    BOOL ReadFile(HANDLE, PVOID, DWORD, PDWORD, LPOVERLAPPED);
    BOOL ReadFileEx(HANDLE, PVOID, DWORD, LPOVERLAPPED, LPOVERLAPPED_COMPLETION_ROUTINE);
    BOOL ReadProcessMemory(HANDLE, PCVOID, PVOID, SIZE_T, SIZE_T*);
    BOOL ReleaseMutex(HANDLE);
    BOOL ReleaseSemaphore(HANDLE, LONG, LPLONG);
    BOOL RemoveDirectoryA(LPCSTR);
    BOOL RemoveDirectoryW(LPCWSTR);
/* In MinGW:
#ifdef _WIN32_WCE
extern BOOL ResetEvent(HANDLE);
#else
WINBASEAPI BOOL WINAPI ResetEvent(HANDLE);
#endif
*/
    BOOL ResetEvent(HANDLE);
    DWORD ResumeThread(HANDLE);
    DWORD SearchPathA(LPCSTR, LPCSTR, LPCSTR, DWORD, LPSTR, LPSTR*);
    DWORD SearchPathW(LPCWSTR, LPCWSTR, LPCWSTR, DWORD, LPWSTR, LPWSTR*);
    BOOL SetCommBreak(HANDLE);
    BOOL SetCommConfig(HANDLE, LPCOMMCONFIG, DWORD);
    BOOL SetCommMask(HANDLE, DWORD);
    BOOL SetCommState(HANDLE, LPDCB);
    BOOL SetCommTimeouts(HANDLE, LPCOMMTIMEOUTS);
    BOOL SetComputerNameA(LPCSTR);
    BOOL SetComputerNameW(LPCWSTR);
    BOOL SetCurrentDirectoryA(LPCSTR);
    BOOL SetCurrentDirectoryW(LPCWSTR);
    BOOL SetDefaultCommConfigA(LPCSTR, LPCOMMCONFIG, DWORD);
    BOOL SetDefaultCommConfigW(LPCWSTR, LPCOMMCONFIG, DWORD);
    BOOL SetEndOfFile(HANDLE);
    BOOL SetEnvironmentVariableA(LPCSTR, LPCSTR);
    BOOL SetEnvironmentVariableW(LPCWSTR, LPCWSTR);
    UINT SetErrorMode(UINT);
/* In MinGW:
#ifdef _WIN32_WCE
extern BOOL SetEvent(HANDLE);
#else
WINBASEAPI BOOL WINAPI SetEvent(HANDLE);
#endif
*/
    BOOL SetEvent(HANDLE);
    VOID SetFileApisToANSI();
    VOID SetFileApisToOEM();
    BOOL SetFileAttributesA(LPCSTR, DWORD);
    BOOL SetFileAttributesW(LPCWSTR, DWORD);
    DWORD SetFilePointer(HANDLE, LONG, PLONG, DWORD);
    BOOL SetFileTime(HANDLE, const(FILETIME)*, const(FILETIME)*, const(FILETIME)*);
    deprecated UINT SetHandleCount(UINT);
    void SetLastError(DWORD);
    void SetLastErrorEx(DWORD, DWORD);
    BOOL SetLocalTime(const(SYSTEMTIME)*);
    BOOL SetMailslotInfo(HANDLE, DWORD);
    BOOL SetNamedPipeHandleState(HANDLE, PDWORD, PDWORD, PDWORD);
    BOOL SetPriorityClass(HANDLE, DWORD);
    BOOL SetStdHandle(DWORD, HANDLE);
    BOOL SetSystemTime(const(SYSTEMTIME)*);
    DWORD_PTR SetThreadAffinityMask(HANDLE, DWORD_PTR);
    BOOL SetThreadContext(HANDLE, const(CONTEXT)*);
    BOOL SetThreadPriority(HANDLE, int);
    BOOL SetTimeZoneInformation(const(TIME_ZONE_INFORMATION)*);
    LPTOP_LEVEL_EXCEPTION_FILTER SetUnhandledExceptionFilter(LPTOP_LEVEL_EXCEPTION_FILTER);
    BOOL SetupComm(HANDLE, DWORD, DWORD);
    BOOL SetVolumeLabelA(LPCSTR, LPCSTR);
    BOOL SetVolumeLabelW(LPCWSTR, LPCWSTR);

    DWORD SizeofResource(HINSTANCE, HRSRC);
    void Sleep(DWORD);
    DWORD SleepEx(DWORD, BOOL);
    DWORD SuspendThread(HANDLE);
    BOOL SystemTimeToFileTime(const(SYSTEMTIME)*, LPFILETIME);
    BOOL TerminateProcess(HANDLE, UINT);
    BOOL TerminateThread(HANDLE, DWORD);
    DWORD TlsAlloc();
    BOOL TlsFree(DWORD);
    PVOID TlsGetValue(DWORD);
    BOOL TlsSetValue(DWORD, PVOID);
    BOOL TransactNamedPipe(HANDLE, PVOID, DWORD, PVOID, DWORD, PDWORD, LPOVERLAPPED);
    BOOL TransmitCommChar(HANDLE, char);
    LONG UnhandledExceptionFilter(LPEXCEPTION_POINTERS);
    BOOL UnlockFile(HANDLE, DWORD, DWORD, DWORD, DWORD);
    BOOL WaitCommEvent(HANDLE, PDWORD, LPOVERLAPPED);
    BOOL WaitForDebugEvent(LPDEBUG_EVENT, DWORD);
    DWORD WaitForMultipleObjects(DWORD, const(HANDLE)*, BOOL, DWORD);
    DWORD WaitForMultipleObjectsEx(DWORD, const(HANDLE)*, BOOL, DWORD, BOOL);
    DWORD WaitForSingleObject(HANDLE, DWORD);
    DWORD WaitForSingleObjectEx(HANDLE, DWORD, BOOL);
    BOOL WaitNamedPipeA(LPCSTR, DWORD);
    BOOL WaitNamedPipeW(LPCWSTR, DWORD);
    // undocumented on MSDN
    BOOL WinLoadTrustProvider(GUID*);
    BOOL WriteFile(HANDLE, PCVOID, DWORD, PDWORD, LPOVERLAPPED);
    BOOL WriteFileEx(HANDLE, PCVOID, DWORD, LPOVERLAPPED, LPOVERLAPPED_COMPLETION_ROUTINE);
    BOOL WritePrivateProfileSectionA(LPCSTR, LPCSTR, LPCSTR);
    BOOL WritePrivateProfileSectionW(LPCWSTR, LPCWSTR, LPCWSTR);
    BOOL WritePrivateProfileStringA(LPCSTR, LPCSTR, LPCSTR, LPCSTR);
    BOOL WritePrivateProfileStringW(LPCWSTR, LPCWSTR, LPCWSTR, LPCWSTR);
    BOOL WritePrivateProfileStructA(LPCSTR, LPCSTR, LPVOID, UINT, LPCSTR);
    BOOL WritePrivateProfileStructW(LPCWSTR, LPCWSTR, LPVOID, UINT, LPCWSTR);
    BOOL WriteProcessMemory(HANDLE, LPVOID, LPCVOID, SIZE_T, SIZE_T*);
    BOOL WriteProfileSectionA(LPCSTR, LPCSTR);
    BOOL WriteProfileSectionW(LPCWSTR, LPCWSTR);
    BOOL WriteProfileStringA(LPCSTR, LPCSTR, LPCSTR);
    BOOL WriteProfileStringW(LPCWSTR, LPCWSTR, LPCWSTR);

    /*  Memory allocation functions.
     *  MSDN documents these erroneously as Win2000+; thus it is uncertain what
     *  version compatibility they really have.
     */
    HGLOBAL GlobalAlloc(UINT, SIZE_T);
    HGLOBAL GlobalDiscard(HGLOBAL);
    HGLOBAL GlobalFree(HGLOBAL);
    HGLOBAL GlobalHandle(PCVOID);
    LPVOID GlobalLock(HGLOBAL);
    VOID GlobalMemoryStatus(LPMEMORYSTATUS);
    HGLOBAL GlobalReAlloc(HGLOBAL, SIZE_T, UINT);
    SIZE_T GlobalSize(HGLOBAL);
    BOOL GlobalUnlock(HGLOBAL);
    PVOID HeapAlloc(HANDLE, DWORD, SIZE_T);
    SIZE_T HeapCompact(HANDLE, DWORD);
    HANDLE HeapCreate(DWORD, SIZE_T, SIZE_T);
    BOOL HeapDestroy(HANDLE);
    BOOL HeapFree(HANDLE, DWORD, PVOID);
    BOOL HeapLock(HANDLE);
    PVOID HeapReAlloc(HANDLE, DWORD, PVOID, SIZE_T);
    SIZE_T HeapSize(HANDLE, DWORD, PCVOID);
    BOOL HeapUnlock(HANDLE);
    BOOL HeapValidate(HANDLE, DWORD, PCVOID);
    BOOL HeapWalk(HANDLE, LPPROCESS_HEAP_ENTRY);
    HLOCAL LocalAlloc(UINT, SIZE_T);
    HLOCAL LocalDiscard(HLOCAL);
    HLOCAL LocalFree(HLOCAL);
    HLOCAL LocalHandle(LPCVOID);
    PVOID LocalLock(HLOCAL);
    HLOCAL LocalReAlloc(HLOCAL, SIZE_T, UINT);
    SIZE_T LocalSize(HLOCAL);
    BOOL LocalUnlock(HLOCAL);
    PVOID VirtualAlloc(PVOID, SIZE_T, DWORD, DWORD);
    PVOID VirtualAllocEx(HANDLE, PVOID, SIZE_T, DWORD, DWORD);
    BOOL VirtualFree(PVOID, SIZE_T, DWORD);
    BOOL VirtualFreeEx(HANDLE, PVOID, SIZE_T, DWORD);
    BOOL VirtualLock(PVOID, SIZE_T);
    BOOL VirtualProtect(PVOID, SIZE_T, DWORD, PDWORD);
    BOOL VirtualProtectEx(HANDLE, PVOID, SIZE_T, DWORD, PDWORD);
    SIZE_T VirtualQuery(LPCVOID, PMEMORY_BASIC_INFORMATION, SIZE_T);
    SIZE_T VirtualQueryEx(HANDLE, LPCVOID, PMEMORY_BASIC_INFORMATION, SIZE_T);
    BOOL VirtualUnlock(PVOID, SIZE_T);
// not in MinGW 4.0 - ???
    static if (_WIN32_WINNT >= 0x600) {
        BOOL CancelIoEx(HANDLE, LPOVERLAPPED);
    }

    BOOL CancelIo(HANDLE);
    BOOL CancelWaitableTimer(HANDLE);
    PVOID ConvertThreadToFiber(PVOID);
    LPVOID CreateFiber(SIZE_T, LPFIBER_START_ROUTINE, LPVOID);
    HANDLE CreateWaitableTimerA(LPSECURITY_ATTRIBUTES, BOOL, LPCSTR);
    HANDLE CreateWaitableTimerW(LPSECURITY_ATTRIBUTES, BOOL, LPCWSTR);
    void DeleteFiber(PVOID);
    BOOL GetFileAttributesExA(LPCSTR, GET_FILEEX_INFO_LEVELS, PVOID);
    BOOL GetFileAttributesExW(LPCWSTR, GET_FILEEX_INFO_LEVELS, PVOID);
    DWORD GetLongPathNameA(LPCSTR, LPSTR, DWORD);
    DWORD GetLongPathNameW(LPCWSTR, LPWSTR, DWORD);
    BOOL InitializeCriticalSectionAndSpinCount(LPCRITICAL_SECTION, DWORD);
    BOOL IsDebuggerPresent();
    HANDLE OpenWaitableTimerA(DWORD, BOOL, LPCSTR);
    HANDLE OpenWaitableTimerW(DWORD, BOOL, LPCWSTR);
    DWORD QueryDosDeviceA(LPCSTR, LPSTR, DWORD);
    DWORD QueryDosDeviceW(LPCWSTR, LPWSTR, DWORD);
    BOOL SetWaitableTimer(HANDLE, const(LARGE_INTEGER)*, LONG, PTIMERAPCROUTINE, PVOID, BOOL);
    void SwitchToFiber(PVOID);

    static if (_WIN32_WINNT >= 0x500) {
        HANDLE OpenThread(DWORD, BOOL, DWORD);
    }

    BOOL AccessCheck(PSECURITY_DESCRIPTOR, HANDLE, DWORD, PGENERIC_MAPPING, PPRIVILEGE_SET, PDWORD, PDWORD, PBOOL);
    BOOL AccessCheckAndAuditAlarmA(LPCSTR, LPVOID, LPSTR, LPSTR, PSECURITY_DESCRIPTOR, DWORD, PGENERIC_MAPPING, BOOL, PDWORD, PBOOL, PBOOL);
    BOOL AccessCheckAndAuditAlarmW(LPCWSTR, LPVOID, LPWSTR, LPWSTR, PSECURITY_DESCRIPTOR, DWORD, PGENERIC_MAPPING, BOOL, PDWORD, PBOOL, PBOOL);
    BOOL AddAccessAllowedAce(PACL, DWORD, DWORD, PSID);
    BOOL AddAccessDeniedAce(PACL, DWORD, DWORD, PSID);
    BOOL AddAce(PACL, DWORD, DWORD, PVOID, DWORD);
    BOOL AddAuditAccessAce(PACL, DWORD, DWORD, PSID, BOOL, BOOL);
    BOOL AdjustTokenGroups(HANDLE, BOOL, PTOKEN_GROUPS, DWORD, PTOKEN_GROUPS, PDWORD);
    BOOL AdjustTokenPrivileges(HANDLE, BOOL, PTOKEN_PRIVILEGES, DWORD, PTOKEN_PRIVILEGES, PDWORD);
    BOOL AllocateAndInitializeSid(PSID_IDENTIFIER_AUTHORITY, BYTE, DWORD, DWORD, DWORD, DWORD, DWORD, DWORD, DWORD, DWORD, PSID*);
    BOOL AllocateLocallyUniqueId(PLUID);
    BOOL AreAllAccessesGranted(DWORD, DWORD);
    BOOL AreAnyAccessesGranted(DWORD, DWORD);
    BOOL BackupEventLogA(HANDLE, LPCSTR);
    BOOL BackupEventLogW(HANDLE, LPCWSTR);
    BOOL BackupRead(HANDLE, LPBYTE, DWORD, LPDWORD, BOOL, BOOL, LPVOID*);
    BOOL BackupSeek(HANDLE, DWORD, DWORD, LPDWORD, LPDWORD, LPVOID*);
    BOOL BackupWrite(HANDLE, LPBYTE, DWORD, LPDWORD, BOOL, BOOL, LPVOID*);
    BOOL ClearEventLogA(HANDLE, LPCSTR);
    BOOL ClearEventLogW(HANDLE, LPCWSTR);
    BOOL CloseEventLog(HANDLE);
    BOOL ConnectNamedPipe(HANDLE, LPOVERLAPPED);
    BOOL CopySid(DWORD, PSID, PSID);
    HANDLE CreateNamedPipeA(LPCSTR, DWORD, DWORD, DWORD, DWORD, DWORD, DWORD, LPSECURITY_ATTRIBUTES);
    HANDLE CreateNamedPipeW(LPCWSTR, DWORD, DWORD, DWORD, DWORD, DWORD, DWORD, LPSECURITY_ATTRIBUTES);
    BOOL CreatePrivateObjectSecurity(PSECURITY_DESCRIPTOR, PSECURITY_DESCRIPTOR, PSECURITY_DESCRIPTOR*, BOOL, HANDLE, PGENERIC_MAPPING);
    BOOL CreateProcessAsUserA(HANDLE, LPCSTR, LPSTR, LPSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES, BOOL, DWORD, PVOID, LPCSTR, LPSTARTUPINFOA, LPPROCESS_INFORMATION);
    BOOL CreateProcessAsUserW(HANDLE, LPCWSTR, LPWSTR, LPSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES, BOOL, DWORD, PVOID, LPCWSTR, LPSTARTUPINFOW, LPPROCESS_INFORMATION);
    HANDLE CreateRemoteThread(HANDLE, LPSECURITY_ATTRIBUTES, SIZE_T, LPTHREAD_START_ROUTINE, LPVOID, DWORD, LPDWORD);
    DWORD CreateTapePartition(HANDLE, DWORD, DWORD, DWORD);
    BOOL DefineDosDeviceA(DWORD, LPCSTR, LPCSTR);
    BOOL DefineDosDeviceW(DWORD, LPCWSTR, LPCWSTR);
    BOOL DeleteAce(PACL, DWORD);
    BOOL DeregisterEventSource(HANDLE);
    BOOL DestroyPrivateObjectSecurity(PSECURITY_DESCRIPTOR*);
    BOOL DeviceIoControl(HANDLE, DWORD, PVOID, DWORD, PVOID, DWORD, PDWORD, POVERLAPPED);
    BOOL DisconnectNamedPipe(HANDLE);
    BOOL DuplicateToken(HANDLE, SECURITY_IMPERSONATION_LEVEL, PHANDLE);
    BOOL DuplicateTokenEx(HANDLE, DWORD, LPSECURITY_ATTRIBUTES, SECURITY_IMPERSONATION_LEVEL, TOKEN_TYPE, PHANDLE);
    BOOL EqualPrefixSid(PSID, PSID);
    BOOL EqualSid(PSID, PSID);
    DWORD EraseTape(HANDLE, DWORD, BOOL);
    HANDLE FindFirstFileExA(LPCSTR, FINDEX_INFO_LEVELS, PVOID, FINDEX_SEARCH_OPS, PVOID, DWORD);
    HANDLE FindFirstFileExW(LPCWSTR, FINDEX_INFO_LEVELS, PVOID, FINDEX_SEARCH_OPS, PVOID, DWORD);
    BOOL FindFirstFreeAce(PACL, PVOID*);
    PVOID FreeSid(PSID);
    BOOL GetAce(PACL, DWORD, LPVOID*);
    BOOL GetAclInformation(PACL, PVOID, DWORD, ACL_INFORMATION_CLASS);
    BOOL GetBinaryTypeA(LPCSTR, PDWORD);
    BOOL GetBinaryTypeW(LPCWSTR, PDWORD);
    DWORD GetCompressedFileSizeA(LPCSTR, PDWORD);
    DWORD GetCompressedFileSizeW(LPCWSTR, PDWORD);
    BOOL GetCurrentHwProfileA(LPHW_PROFILE_INFOA);
    BOOL GetCurrentHwProfileW(LPHW_PROFILE_INFOW);
    BOOL GetFileSecurityA(LPCSTR, SECURITY_INFORMATION, PSECURITY_DESCRIPTOR, DWORD, PDWORD);
    BOOL GetFileSecurityW(LPCWSTR, SECURITY_INFORMATION, PSECURITY_DESCRIPTOR, DWORD, PDWORD);
    BOOL GetHandleInformation(HANDLE, PDWORD);
    BOOL GetKernelObjectSecurity(HANDLE, SECURITY_INFORMATION, PSECURITY_DESCRIPTOR, DWORD, PDWORD);
    DWORD GetLengthSid(PSID);
    BOOL GetNumberOfEventLogRecords(HANDLE, PDWORD);
    BOOL GetOldestEventLogRecord(HANDLE, PDWORD);
    BOOL GetPrivateObjectSecurity(PSECURITY_DESCRIPTOR, SECURITY_INFORMATION, PSECURITY_DESCRIPTOR, DWORD, PDWORD);
    BOOL GetProcessPriorityBoost(HANDLE, PBOOL);
    BOOL GetProcessShutdownParameters(PDWORD, PDWORD);
    BOOL GetProcessTimes(HANDLE, LPFILETIME, LPFILETIME, LPFILETIME, LPFILETIME);
    HWINSTA GetProcessWindowStation();
    BOOL GetProcessWorkingSetSize(HANDLE, PSIZE_T, PSIZE_T);
    BOOL GetQueuedCompletionStatus(HANDLE, PDWORD, PULONG_PTR, LPOVERLAPPED*, DWORD);
    BOOL GetSecurityDescriptorControl(PSECURITY_DESCRIPTOR, PSECURITY_DESCRIPTOR_CONTROL, PDWORD);
    BOOL GetSecurityDescriptorDacl(PSECURITY_DESCRIPTOR, LPBOOL, PACL*, LPBOOL);
    BOOL GetSecurityDescriptorGroup(PSECURITY_DESCRIPTOR, PSID*, LPBOOL);
    DWORD GetSecurityDescriptorLength(PSECURITY_DESCRIPTOR);
    BOOL GetSecurityDescriptorOwner(PSECURITY_DESCRIPTOR, PSID*, LPBOOL);
    BOOL GetSecurityDescriptorSacl(PSECURITY_DESCRIPTOR, LPBOOL, PACL*, LPBOOL);
    PSID_IDENTIFIER_AUTHORITY GetSidIdentifierAuthority(PSID);
    DWORD GetSidLengthRequired(UCHAR);
    PDWORD GetSidSubAuthority(PSID, DWORD);
    PUCHAR GetSidSubAuthorityCount(PSID);
    DWORD GetTapeParameters(HANDLE, DWORD, PDWORD, PVOID);
    DWORD GetTapePosition(HANDLE, DWORD, PDWORD, PDWORD, PDWORD);
    DWORD GetTapeStatus(HANDLE);
    BOOL GetThreadPriorityBoost(HANDLE, PBOOL);
    BOOL GetThreadTimes(HANDLE, LPFILETIME, LPFILETIME, LPFILETIME, LPFILETIME);
    BOOL GetTokenInformation(HANDLE, TOKEN_INFORMATION_CLASS, PVOID, DWORD, PDWORD);
    BOOL ImpersonateLoggedOnUser(HANDLE);
    BOOL ImpersonateNamedPipeClient(HANDLE);
    BOOL ImpersonateSelf(SECURITY_IMPERSONATION_LEVEL);
    BOOL InitializeAcl(PACL, DWORD, DWORD);
    DWORD SetCriticalSectionSpinCount(LPCRITICAL_SECTION, DWORD);
    BOOL InitializeSecurityDescriptor(PSECURITY_DESCRIPTOR, DWORD);
    BOOL InitializeSid(PSID, PSID_IDENTIFIER_AUTHORITY, BYTE);
    BOOL IsProcessorFeaturePresent(DWORD);
    BOOL IsTextUnicode(PCVOID, int, LPINT);
    BOOL IsValidAcl(PACL);
    BOOL IsValidSecurityDescriptor(PSECURITY_DESCRIPTOR);
    BOOL IsValidSid(PSID);
    BOOL LockFileEx(HANDLE, DWORD, DWORD, DWORD, DWORD, LPOVERLAPPED);
    BOOL LogonUserA(LPSTR, LPSTR, LPSTR, DWORD, DWORD, PHANDLE);
    BOOL LogonUserW(LPWSTR, LPWSTR, LPWSTR, DWORD, DWORD, PHANDLE);
    BOOL LookupAccountNameA(LPCSTR, LPCSTR, PSID, PDWORD, LPSTR, PDWORD, PSID_NAME_USE);
    BOOL LookupAccountNameW(LPCWSTR, LPCWSTR, PSID, PDWORD, LPWSTR, PDWORD, PSID_NAME_USE);
    BOOL LookupAccountSidA(LPCSTR, PSID, LPSTR, PDWORD, LPSTR, PDWORD, PSID_NAME_USE);
    BOOL LookupAccountSidW(LPCWSTR, PSID, LPWSTR, PDWORD, LPWSTR, PDWORD, PSID_NAME_USE);
    BOOL LookupPrivilegeDisplayNameA(LPCSTR, LPCSTR, LPSTR, PDWORD, PDWORD);
    BOOL LookupPrivilegeDisplayNameW(LPCWSTR, LPCWSTR, LPWSTR, PDWORD, PDWORD);
    BOOL LookupPrivilegeNameA(LPCSTR, PLUID, LPSTR, PDWORD);
    BOOL LookupPrivilegeNameW(LPCWSTR, PLUID, LPWSTR, PDWORD);
    BOOL LookupPrivilegeValueA(LPCSTR, LPCSTR, PLUID);
    BOOL LookupPrivilegeValueW(LPCWSTR, LPCWSTR, PLUID);
    BOOL MakeAbsoluteSD(PSECURITY_DESCRIPTOR, PSECURITY_DESCRIPTOR, PDWORD, PACL, PDWORD, PACL, PDWORD, PSID, PDWORD, PSID, PDWORD);
    BOOL MakeSelfRelativeSD(PSECURITY_DESCRIPTOR, PSECURITY_DESCRIPTOR, PDWORD);
    VOID MapGenericMask(PDWORD, PGENERIC_MAPPING);
    BOOL MoveFileExA(LPCSTR, LPCSTR, DWORD);
    BOOL MoveFileExW(LPCWSTR, LPCWSTR, DWORD);
    BOOL NotifyChangeEventLog(HANDLE, HANDLE);
    BOOL ObjectCloseAuditAlarmA(LPCSTR, PVOID, BOOL);
    BOOL ObjectCloseAuditAlarmW(LPCWSTR, PVOID, BOOL);
    BOOL ObjectDeleteAuditAlarmA(LPCSTR, PVOID, BOOL);
    BOOL ObjectDeleteAuditAlarmW(LPCWSTR, PVOID, BOOL);
    BOOL ObjectOpenAuditAlarmA(LPCSTR, PVOID, LPSTR, LPSTR, PSECURITY_DESCRIPTOR, HANDLE, DWORD, DWORD, PPRIVILEGE_SET, BOOL, BOOL, PBOOL);
    BOOL ObjectOpenAuditAlarmW(LPCWSTR, PVOID, LPWSTR, LPWSTR, PSECURITY_DESCRIPTOR, HANDLE, DWORD, DWORD, PPRIVILEGE_SET, BOOL, BOOL, PBOOL);
    BOOL ObjectPrivilegeAuditAlarmA(LPCSTR, PVOID, HANDLE, DWORD, PPRIVILEGE_SET, BOOL);
    BOOL ObjectPrivilegeAuditAlarmW(LPCWSTR, PVOID, HANDLE, DWORD, PPRIVILEGE_SET, BOOL);
    HANDLE OpenBackupEventLogA(LPCSTR, LPCSTR);
    HANDLE OpenBackupEventLogW(LPCWSTR, LPCWSTR);
    HANDLE OpenEventLogA(LPCSTR, LPCSTR);
    HANDLE OpenEventLogW(LPCWSTR, LPCWSTR);
    BOOL OpenProcessToken(HANDLE, DWORD, PHANDLE);
    BOOL OpenThreadToken(HANDLE, DWORD, BOOL, PHANDLE);
    BOOL PostQueuedCompletionStatus(HANDLE, DWORD, ULONG_PTR, LPOVERLAPPED);
    DWORD PrepareTape(HANDLE, DWORD, BOOL);
    BOOL PrivilegeCheck(HANDLE, PPRIVILEGE_SET, PBOOL);
    BOOL PrivilegedServiceAuditAlarmA(LPCSTR, LPCSTR, HANDLE, PPRIVILEGE_SET, BOOL);
    BOOL PrivilegedServiceAuditAlarmW(LPCWSTR, LPCWSTR, HANDLE, PPRIVILEGE_SET, BOOL);
    BOOL ReadDirectoryChangesW(HANDLE, PVOID, DWORD, BOOL, DWORD, PDWORD, LPOVERLAPPED, LPOVERLAPPED_COMPLETION_ROUTINE);
    BOOL ReadEventLogA(HANDLE, DWORD, DWORD, PVOID, DWORD, DWORD*, DWORD*);
    BOOL ReadEventLogW(HANDLE, DWORD, DWORD, PVOID, DWORD, DWORD*, DWORD*);
    BOOL ReadFileScatter(HANDLE, FILE_SEGMENT_ELEMENT*, DWORD, LPDWORD, LPOVERLAPPED);
    HANDLE RegisterEventSourceA (LPCSTR, LPCSTR);
    HANDLE RegisterEventSourceW(LPCWSTR, LPCWSTR);
    BOOL ReportEventA(HANDLE, WORD, WORD, DWORD, PSID, WORD, DWORD, LPCSTR*, PVOID);
    BOOL ReportEventW(HANDLE, WORD, WORD, DWORD, PSID, WORD, DWORD, LPCWSTR*, PVOID);
    BOOL RevertToSelf();
    BOOL SetAclInformation(PACL, PVOID, DWORD, ACL_INFORMATION_CLASS);
    BOOL SetFileSecurityA(LPCSTR, SECURITY_INFORMATION, PSECURITY_DESCRIPTOR);
    BOOL SetFileSecurityW(LPCWSTR, SECURITY_INFORMATION, PSECURITY_DESCRIPTOR);
    BOOL SetHandleInformation(HANDLE, DWORD, DWORD);
    BOOL SetKernelObjectSecurity(HANDLE, SECURITY_INFORMATION, PSECURITY_DESCRIPTOR);
    BOOL SetPrivateObjectSecurity(SECURITY_INFORMATION, PSECURITY_DESCRIPTOR, PSECURITY_DESCRIPTOR*, PGENERIC_MAPPING, HANDLE);
    BOOL SetProcessAffinityMask(HANDLE, DWORD_PTR);
    BOOL SetProcessPriorityBoost(HANDLE, BOOL);
    BOOL SetProcessShutdownParameters(DWORD, DWORD);
    BOOL SetProcessWorkingSetSize(HANDLE, SIZE_T, SIZE_T);
    BOOL SetSecurityDescriptorDacl(PSECURITY_DESCRIPTOR, BOOL, PACL, BOOL);
    BOOL SetSecurityDescriptorGroup(PSECURITY_DESCRIPTOR, PSID, BOOL);
    BOOL SetSecurityDescriptorOwner(PSECURITY_DESCRIPTOR, PSID, BOOL);
    BOOL SetSecurityDescriptorSacl(PSECURITY_DESCRIPTOR, BOOL, PACL, BOOL);
    BOOL SetSystemTimeAdjustment(DWORD, BOOL);
    DWORD SetTapeParameters(HANDLE, DWORD, PVOID);
    DWORD SetTapePosition(HANDLE, DWORD, DWORD, DWORD, DWORD, BOOL);
    BOOL SetThreadPriorityBoost(HANDLE, BOOL);
    BOOL SetThreadToken(PHANDLE, HANDLE);
    BOOL SetTokenInformation(HANDLE, TOKEN_INFORMATION_CLASS, PVOID, DWORD);
    DWORD SignalObjectAndWait(HANDLE, HANDLE, DWORD, BOOL);
    BOOL SwitchToThread();
    BOOL SystemTimeToTzSpecificLocalTime(LPTIME_ZONE_INFORMATION, LPSYSTEMTIME, LPSYSTEMTIME);
    BOOL TzSpecificLocalTimeToSystemTime(LPTIME_ZONE_INFORMATION, LPSYSTEMTIME, LPSYSTEMTIME);
    BOOL TryEnterCriticalSection(LPCRITICAL_SECTION);
    BOOL TryEnterCriticalSection(shared(CRITICAL_SECTION)*);
    BOOL UnlockFileEx(HANDLE, DWORD, DWORD, DWORD, LPOVERLAPPED);
    BOOL UpdateResourceA(HANDLE, LPCSTR, LPCSTR, WORD, PVOID, DWORD);
    BOOL UpdateResourceW(HANDLE, LPCWSTR, LPCWSTR, WORD, PVOID, DWORD);
    BOOL WriteFileGather(HANDLE, FILE_SEGMENT_ELEMENT*, DWORD, LPDWORD, LPOVERLAPPED);
    DWORD WriteTapemark(HANDLE, DWORD, DWORD, BOOL);

    static if (_WIN32_WINNT >= 0x500) {
        BOOL AddAccessAllowedAceEx(PACL, DWORD, DWORD, DWORD, PSID);
        BOOL AddAccessDeniedAceEx(PACL, DWORD, DWORD, DWORD, PSID);
        PVOID AddVectoredExceptionHandler(ULONG, PVECTORED_EXCEPTION_HANDLER);
        BOOL AllocateUserPhysicalPages(HANDLE, PULONG_PTR, PULONG_PTR);
        BOOL AssignProcessToJobObject(HANDLE, HANDLE);
        BOOL ChangeTimerQueueTimer(HANDLE,HANDLE,ULONG,ULONG);
        LPVOID CreateFiberEx(SIZE_T, SIZE_T, DWORD, LPFIBER_START_ROUTINE, LPVOID);
        HANDLE CreateFileMappingA(HANDLE, LPSECURITY_ATTRIBUTES, DWORD, DWORD, DWORD, LPCSTR);
        HANDLE CreateFileMappingW(HANDLE, LPSECURITY_ATTRIBUTES, DWORD, DWORD, DWORD, LPCWSTR);
        BOOL CreateHardLinkA(LPCSTR, LPCSTR, LPSECURITY_ATTRIBUTES);
        BOOL CreateHardLinkW(LPCWSTR, LPCWSTR, LPSECURITY_ATTRIBUTES);
        HANDLE CreateJobObjectA(LPSECURITY_ATTRIBUTES, LPCSTR);
        HANDLE CreateJobObjectW(LPSECURITY_ATTRIBUTES, LPCWSTR);
        BOOL CreateProcessWithLogonW(LPCWSTR, LPCWSTR, LPCWSTR, DWORD, LPCWSTR, LPWSTR, DWORD, LPVOID, LPCWSTR, LPSTARTUPINFOW, LPPROCESS_INFORMATION);
        HANDLE CreateTimerQueue();
        BOOL CreateTimerQueueTimer(PHANDLE, HANDLE, WAITORTIMERCALLBACK, PVOID, DWORD, DWORD, ULONG);
        BOOL DeleteTimerQueue(HANDLE);
        BOOL DeleteTimerQueueEx(HANDLE, HANDLE);
        BOOL DeleteTimerQueueTimer(HANDLE, HANDLE, HANDLE);
        BOOL DeleteVolumeMountPointA(LPCSTR);
        BOOL DeleteVolumeMountPointW(LPCWSTR);
        BOOL DnsHostnameToComputerNameA(LPCSTR, LPSTR, LPDWORD);
        BOOL DnsHostnameToComputerNameW(LPCWSTR, LPWSTR, LPDWORD);
        BOOL EncryptFileA(LPCSTR);
        BOOL EncryptFileW(LPCWSTR);
        BOOL FileEncryptionStatusA(LPCSTR, LPDWORD);
        BOOL FileEncryptionStatusW(LPCWSTR, LPDWORD);
        HANDLE FindFirstVolumeA(LPCSTR, DWORD);
        HANDLE FindFirstVolumeMountPointA(LPSTR, LPSTR, DWORD);
        HANDLE FindFirstVolumeMountPointW(LPWSTR, LPWSTR, DWORD);
        HANDLE FindFirstVolumeW(LPCWSTR, DWORD);
        BOOL FindNextVolumeA(HANDLE, LPCSTR, DWORD);
        BOOL FindNextVolumeW(HANDLE, LPWSTR, DWORD);
        BOOL FindNextVolumeMountPointA(HANDLE, LPSTR, DWORD);
        BOOL FindNextVolumeMountPointW(HANDLE, LPWSTR, DWORD);
        BOOL FindVolumeClose(HANDLE);
        BOOL FindVolumeMountPointClose(HANDLE);
        BOOL FlushViewOfFile(PCVOID, SIZE_T);
        BOOL FreeUserPhysicalPages(HANDLE, PULONG_PTR, PULONG_PTR);
        BOOL GetComputerNameExA(COMPUTER_NAME_FORMAT, LPSTR, LPDWORD);
        BOOL GetComputerNameExW(COMPUTER_NAME_FORMAT, LPWSTR, LPDWORD);
        BOOL GetFileSizeEx(HANDLE, PLARGE_INTEGER);
        BOOL GetModuleHandleExA(DWORD, LPCSTR, HMODULE*);
        BOOL GetModuleHandleExW(DWORD, LPCWSTR, HMODULE*);
        HANDLE GetProcessHeap();
        DWORD GetProcessHeaps(DWORD, PHANDLE);
        BOOL GetProcessIoCounters(HANDLE, PIO_COUNTERS);
        BOOL GetSystemPowerStatus(LPSYSTEM_POWER_STATUS);
        UINT GetSystemWindowsDirectoryA(LPSTR, UINT);
        UINT GetSystemWindowsDirectoryW(LPWSTR, UINT);
        BOOL GetVolumeNameForVolumeMountPointA(LPCSTR, LPSTR, DWORD);
        BOOL GetVolumeNameForVolumeMountPointW(LPCWSTR, LPWSTR, DWORD);
        BOOL GetVolumePathNameA(LPCSTR, LPSTR, DWORD);
        BOOL GetVolumePathNameW(LPCWSTR, LPWSTR, DWORD);
        BOOL GlobalMemoryStatusEx(LPMEMORYSTATUSEX);
        BOOL IsBadCodePtr(FARPROC);
        BOOL IsSystemResumeAutomatic();
        BOOL MapUserPhysicalPages(PVOID, ULONG_PTR, PULONG_PTR);
        BOOL MapUserPhysicalPagesScatter(PVOID*, ULONG_PTR, PULONG_PTR);
        PVOID MapViewOfFile(HANDLE, DWORD, DWORD, DWORD, SIZE_T);
        PVOID MapViewOfFileEx(HANDLE, DWORD, DWORD, DWORD, SIZE_T, PVOID);
        HANDLE OpenFileMappingA(DWORD, BOOL, LPCSTR);
        HANDLE OpenFileMappingW(DWORD, BOOL, LPCWSTR);
        BOOL ProcessIdToSessionId(DWORD, DWORD*);
        BOOL QueryInformationJobObject(HANDLE, JOBOBJECTINFOCLASS, LPVOID, DWORD, LPDWORD);
        ULONG RemoveVectoredExceptionHandler(PVOID);
        BOOL ReplaceFileA(LPCSTR, LPCSTR, LPCSTR, DWORD, LPVOID, LPVOID);
        BOOL ReplaceFileW(LPCWSTR, LPCWSTR, LPCWSTR, DWORD, LPVOID, LPVOID);
        BOOL SetComputerNameExA(COMPUTER_NAME_FORMAT, LPCSTR);
        BOOL SetComputerNameExW(COMPUTER_NAME_FORMAT, LPCWSTR);
        BOOL SetFilePointerEx(HANDLE, LARGE_INTEGER, PLARGE_INTEGER, DWORD);
        BOOL SetInformationJobObject(HANDLE, JOBOBJECTINFOCLASS, LPVOID, DWORD);
        BOOL SetSecurityDescriptorControl(PSECURITY_DESCRIPTOR, SECURITY_DESCRIPTOR_CONTROL, SECURITY_DESCRIPTOR_CONTROL);
        BOOL SetSystemPowerState(BOOL, BOOL);
        EXECUTION_STATE SetThreadExecutionState(EXECUTION_STATE);
        DWORD SetThreadIdealProcessor(HANDLE, DWORD);
        BOOL SetVolumeMountPointA(LPCSTR, LPCSTR);
        BOOL SetVolumeMountPointW(LPCWSTR, LPCWSTR);
        BOOL TerminateJobObject(HANDLE, UINT);
        BOOL UnmapViewOfFile(PCVOID);
        BOOL UnregisterWait(HANDLE);
        BOOL UnregisterWaitEx(HANDLE, HANDLE);
        BOOL VerifyVersionInfoA(LPOSVERSIONINFOEXA, DWORD, DWORDLONG);
        BOOL VerifyVersionInfoW(LPOSVERSIONINFOEXW, DWORD, DWORDLONG);
    }

    static if (_WIN32_WINNT >= 0x501) {
        BOOL ActivateActCtx(HANDLE, ULONG_PTR*);
        void AddRefActCtx(HANDLE);
        BOOL CheckNameLegalDOS8Dot3A(LPCSTR, LPSTR, DWORD, PBOOL, PBOOL);
        BOOL CheckNameLegalDOS8Dot3W(LPCWSTR, LPSTR, DWORD, PBOOL, PBOOL);
        BOOL CheckRemoteDebuggerPresent(HANDLE, PBOOL);
        BOOL ConvertFiberToThread();
        HANDLE CreateActCtxA(PCACTCTXA);
        HANDLE CreateActCtxW(PCACTCTXW);
        HANDLE CreateMemoryResourceNotification(MEMORY_RESOURCE_NOTIFICATION_TYPE);
        BOOL DeactivateActCtx(DWORD, ULONG_PTR);
        BOOL DebugActiveProcessStop(DWORD);
        BOOL DebugBreakProcess(HANDLE);
        BOOL DebugSetProcessKillOnExit(BOOL);
        BOOL FindActCtxSectionGuid(DWORD, const(GUID)*, ULONG, const(GUID)*,
          PACTCTX_SECTION_KEYED_DATA);
        BOOL FindActCtxSectionStringA(DWORD, const(GUID)*, ULONG, LPCSTR,
          PACTCTX_SECTION_KEYED_DATA);
        BOOL FindActCtxSectionStringW(DWORD, const(GUID)*, ULONG, LPCWSTR,
          PACTCTX_SECTION_KEYED_DATA);
        BOOL GetCurrentActCtx(HANDLE*);
        VOID GetNativeSystemInfo(LPSYSTEM_INFO);
        BOOL GetProcessHandleCount(HANDLE, PDWORD);
        BOOL GetSystemRegistryQuota(PDWORD, PDWORD);
        BOOL GetSystemTimes(LPFILETIME, LPFILETIME, LPFILETIME);
        UINT GetSystemWow64DirectoryA(LPSTR, UINT);
        UINT GetSystemWow64DirectoryW(LPWSTR, UINT);
        BOOL GetThreadIOPendingFlag(HANDLE, PBOOL);
        BOOL GetVolumePathNamesForVolumeNameA(LPCSTR, LPSTR, DWORD, PDWORD);
        BOOL GetVolumePathNamesForVolumeNameW(LPCWSTR, LPWSTR, DWORD, PDWORD);
        UINT GetWriteWatch(DWORD, PVOID, SIZE_T, PVOID*, PULONG_PTR, PULONG);
        BOOL HeapQueryInformation(HANDLE, HEAP_INFORMATION_CLASS, PVOID, SIZE_T, PSIZE_T);
        BOOL HeapSetInformation(HANDLE, HEAP_INFORMATION_CLASS, PVOID, SIZE_T);
        BOOL IsProcessInJob(HANDLE, HANDLE, PBOOL);
        BOOL IsWow64Process(HANDLE, PBOOL);
        BOOL QueryActCtxW(DWORD, HANDLE, PVOID, ULONG, PVOID, SIZE_T, SIZE_T*);
        BOOL QueryMemoryResourceNotification(HANDLE, PBOOL);
        void ReleaseActCtx(HANDLE);
        UINT ResetWriteWatch(LPVOID, SIZE_T);
        BOOL SetFileShortNameA(HANDLE, LPCSTR);
        BOOL SetFileShortNameW(HANDLE, LPCWSTR);
        BOOL SetFileValidData(HANDLE, LONGLONG);
        BOOL ZombifyActCtx(HANDLE);
    }

    static if (_WIN32_WINNT >= 0x502) {
        DWORD GetFirmwareEnvironmentVariableA(LPCSTR, LPCSTR, PVOID, DWORD);
        DWORD GetFirmwareEnvironmentVariableW(LPCWSTR, LPCWSTR, PVOID, DWORD);
        DWORD GetDllDirectoryA(DWORD, LPSTR);
        DWORD GetDllDirectoryW(DWORD, LPWSTR);
        DWORD GetThreadId(HANDLE);
        DWORD GetProcessId(HANDLE);
        HANDLE ReOpenFile(HANDLE, DWORD, DWORD, DWORD);
        BOOL SetDllDirectoryA(LPCSTR);
        BOOL SetDllDirectoryW(LPCWSTR);
        BOOL SetFirmwareEnvironmentVariableA(LPCSTR, LPCSTR, PVOID, DWORD);
        BOOL SetFirmwareEnvironmentVariableW(LPCWSTR, LPCWSTR, PVOID, DWORD);
    }

    // ???
    static if (_WIN32_WINNT >= 0x510) {
        VOID RestoreLastError(DWORD);
    }
}

// For compatibility with old core.sys.windows.windows:
version (LittleEndian) nothrow @nogc
{
    BOOL QueryPerformanceCounter(long* lpPerformanceCount) { return QueryPerformanceCounter(cast(PLARGE_INTEGER)lpPerformanceCount); }
    BOOL QueryPerformanceFrequency(long* lpFrequency) { return QueryPerformanceFrequency(cast(PLARGE_INTEGER)lpFrequency); }
}

mixin DECLARE_AW!("STARTUPINFO");
version (Unicode) {
    //alias STARTUPINFOW STARTUPINFO;
    alias WIN32_FIND_DATAW WIN32_FIND_DATA;
    alias ENUMRESLANGPROCW ENUMRESLANGPROC;
    alias ENUMRESNAMEPROCW ENUMRESNAMEPROC;
    alias ENUMRESTYPEPROCW ENUMRESTYPEPROC;
    alias AddAtomW AddAtom;
    alias BeginUpdateResourceW BeginUpdateResource;
    alias BuildCommDCBW BuildCommDCB;
    alias BuildCommDCBAndTimeoutsW BuildCommDCBAndTimeouts;
    alias CallNamedPipeW CallNamedPipe;
    alias CommConfigDialogW CommConfigDialog;
    alias CopyFileW CopyFile;
    alias CopyFileExW CopyFileEx;
    alias CreateDirectoryW CreateDirectory;
    alias CreateDirectoryExW CreateDirectoryEx;
    alias CreateEventW CreateEvent;
    alias CreateFileW CreateFile;
    alias CreateMailslotW CreateMailslot;
    alias CreateMutexW CreateMutex;
    alias CreateProcessW CreateProcess;
    alias CreateSemaphoreW CreateSemaphore;
    alias DeleteFileW DeleteFile;
    alias EndUpdateResourceW EndUpdateResource;
    alias EnumResourceLanguagesW EnumResourceLanguages;
    alias EnumResourceNamesW EnumResourceNames;
    alias EnumResourceTypesW EnumResourceTypes;
    alias ExpandEnvironmentStringsW ExpandEnvironmentStrings;
    alias FatalAppExitW FatalAppExit;
    alias FindAtomW FindAtom;
    alias FindFirstChangeNotificationW FindFirstChangeNotification;
    alias FindFirstFileW FindFirstFile;
    alias FindNextFileW FindNextFile;
    alias FindResourceW FindResource;
    alias FindResourceExW FindResourceEx;
    alias FormatMessageW FormatMessage;
    alias FreeEnvironmentStringsW FreeEnvironmentStrings;
    alias GetAtomNameW GetAtomName;
    alias GetCommandLineW GetCommandLine;
    alias GetComputerNameW GetComputerName;
    alias GetCurrentDirectoryW GetCurrentDirectory;
    alias GetDefaultCommConfigW GetDefaultCommConfig;
    alias GetDiskFreeSpaceW GetDiskFreeSpace;
    alias GetDiskFreeSpaceExW GetDiskFreeSpaceEx;
    alias GetDriveTypeW GetDriveType;
    alias GetEnvironmentStringsW GetEnvironmentStrings;
    alias GetEnvironmentVariableW GetEnvironmentVariable;
    alias GetFileAttributesW GetFileAttributes;
    alias GetFullPathNameW GetFullPathName;
    alias GetLogicalDriveStringsW GetLogicalDriveStrings;
    alias GetModuleFileNameW GetModuleFileName;
    alias GetModuleHandleW GetModuleHandle;
    alias GetNamedPipeHandleStateW GetNamedPipeHandleState;
    alias GetPrivateProfileIntW GetPrivateProfileInt;
    alias GetPrivateProfileSectionW GetPrivateProfileSection;
    alias GetPrivateProfileSectionNamesW GetPrivateProfileSectionNames;
    alias GetPrivateProfileStringW GetPrivateProfileString;
    alias GetPrivateProfileStructW GetPrivateProfileStruct;
    alias GetProfileIntW GetProfileInt;
    alias GetProfileSectionW GetProfileSection;
    alias GetProfileStringW GetProfileString;
    alias GetShortPathNameW GetShortPathName;
    alias GetStartupInfoW GetStartupInfo;
    alias GetSystemDirectoryW GetSystemDirectory;
    alias GetTempFileNameW GetTempFileName;
    alias GetTempPathW GetTempPath;
    alias GetUserNameW GetUserName;
    alias GetVersionExW GetVersionEx;
    alias GetVolumeInformationW GetVolumeInformation;
    alias GetWindowsDirectoryW GetWindowsDirectory;
    alias GlobalAddAtomW GlobalAddAtom;
    alias GlobalFindAtomW GlobalFindAtom;
    alias GlobalGetAtomNameW GlobalGetAtomName;
    alias IsBadStringPtrW IsBadStringPtr;
    alias LoadLibraryW LoadLibrary;
    alias LoadLibraryExW LoadLibraryEx;
    alias lstrcatW lstrcat;
    alias lstrcmpW lstrcmp;
    alias lstrcmpiW lstrcmpi;
    alias lstrcpyW lstrcpy;
    alias lstrcpynW lstrcpyn;
    alias lstrlenW lstrlen;
    alias MoveFileW MoveFile;
    alias OpenEventW OpenEvent;
    alias OpenMutexW OpenMutex;
    alias OpenSemaphoreW OpenSemaphore;
    alias OutputDebugStringW OutputDebugString;
    alias RemoveDirectoryW RemoveDirectory;
    alias SearchPathW SearchPath;
    alias SetComputerNameW SetComputerName;
    alias SetCurrentDirectoryW SetCurrentDirectory;
    alias SetDefaultCommConfigW SetDefaultCommConfig;
    alias SetEnvironmentVariableW SetEnvironmentVariable;
    alias SetFileAttributesW SetFileAttributes;
    alias SetVolumeLabelW SetVolumeLabel;
    alias WaitNamedPipeW WaitNamedPipe;
    alias WritePrivateProfileSectionW WritePrivateProfileSection;
    alias WritePrivateProfileStringW WritePrivateProfileString;
    alias WritePrivateProfileStructW WritePrivateProfileStruct;
    alias WriteProfileSectionW WriteProfileSection;
    alias WriteProfileStringW WriteProfileString;
    alias CreateWaitableTimerW CreateWaitableTimer;
    alias GetFileAttributesExW GetFileAttributesEx;
    alias GetLongPathNameW GetLongPathName;
    alias QueryDosDeviceW QueryDosDevice;

    alias HW_PROFILE_INFOW HW_PROFILE_INFO;
    alias AccessCheckAndAuditAlarmW AccessCheckAndAuditAlarm;
    alias BackupEventLogW BackupEventLog;
    alias ClearEventLogW ClearEventLog;
    alias CreateNamedPipeW CreateNamedPipe;
    alias CreateProcessAsUserW CreateProcessAsUser;
    alias DefineDosDeviceW DefineDosDevice;
    alias FindFirstFileExW FindFirstFileEx;
    alias GetBinaryTypeW GetBinaryType;
    alias GetCompressedFileSizeW GetCompressedFileSize;
    alias GetFileSecurityW GetFileSecurity;
    alias LogonUserW LogonUser;
    alias LookupAccountNameW LookupAccountName;
    alias LookupAccountSidW LookupAccountSid;
    alias LookupPrivilegeDisplayNameW LookupPrivilegeDisplayName;
    alias LookupPrivilegeNameW LookupPrivilegeName;
    alias LookupPrivilegeValueW LookupPrivilegeValue;
    alias MoveFileExW MoveFileEx;
    alias ObjectCloseAuditAlarmW ObjectCloseAuditAlarm;
    alias ObjectDeleteAuditAlarmW ObjectDeleteAuditAlarm;
    alias ObjectOpenAuditAlarmW ObjectOpenAuditAlarm;
    alias ObjectPrivilegeAuditAlarmW ObjectPrivilegeAuditAlarm;
    alias OpenBackupEventLogW OpenBackupEventLog;
    alias OpenEventLogW OpenEventLog;
    alias PrivilegedServiceAuditAlarmW PrivilegedServiceAuditAlarm;
    alias ReadEventLogW ReadEventLog;
    alias RegisterEventSourceW RegisterEventSource;
    alias ReportEventW ReportEvent;
    alias SetFileSecurityW SetFileSecurity;
    alias UpdateResourceW UpdateResource;

    static if (_WIN32_WINNT >= 0x500) {
        alias CreateFileMappingW CreateFileMapping;
        alias CreateHardLinkW CreateHardLink;
        alias CreateJobObjectW CreateJobObject;
        alias DeleteVolumeMountPointW DeleteVolumeMountPoint;
        alias DnsHostnameToComputerNameW DnsHostnameToComputerName;
        alias EncryptFileW EncryptFile;
        alias FileEncryptionStatusW FileEncryptionStatus;
        alias FindFirstVolumeW FindFirstVolume;
        alias FindFirstVolumeMountPointW FindFirstVolumeMountPoint;
        alias FindNextVolumeW FindNextVolume;
        alias FindNextVolumeMountPointW FindNextVolumeMountPoint;
        alias GetModuleHandleExW GetModuleHandleEx;
        alias GetSystemWindowsDirectoryW GetSystemWindowsDirectory;
        alias GetVolumeNameForVolumeMountPointW GetVolumeNameForVolumeMountPoint;
        alias GetVolumePathNameW GetVolumePathName;
        alias OpenFileMappingW OpenFileMapping;
        alias ReplaceFileW ReplaceFile;
        alias SetVolumeMountPointW SetVolumeMountPoint;
        alias VerifyVersionInfoW VerifyVersionInfo;
    }

    static if (_WIN32_WINNT >= 0x501) {
        alias ACTCTXW ACTCTX;
        alias CheckNameLegalDOS8Dot3W CheckNameLegalDOS8Dot3;
        alias CreateActCtxW CreateActCtx;
        alias FindActCtxSectionStringW FindActCtxSectionString;
        alias GetSystemWow64DirectoryW GetSystemWow64Directory;
        alias GetVolumePathNamesForVolumeNameW GetVolumePathNamesForVolumeName;
        alias SetFileShortNameW SetFileShortName;
    }

    static if (_WIN32_WINNT >= 0x502) {
        alias SetFirmwareEnvironmentVariableW SetFirmwareEnvironmentVariable;
        alias SetDllDirectoryW SetDllDirectory;
        alias GetDllDirectoryW GetDllDirectory;
    }

} else {
    //alias STARTUPINFOA STARTUPINFO;
    alias WIN32_FIND_DATAA WIN32_FIND_DATA;
    alias ENUMRESLANGPROCW ENUMRESLANGPROC;
    alias ENUMRESNAMEPROCW ENUMRESNAMEPROC;
    alias ENUMRESTYPEPROCW ENUMRESTYPEPROC;
    alias AddAtomA AddAtom;
    alias BeginUpdateResourceA BeginUpdateResource;
    alias BuildCommDCBA BuildCommDCB;
    alias BuildCommDCBAndTimeoutsA BuildCommDCBAndTimeouts;
    alias CallNamedPipeA CallNamedPipe;
    alias CommConfigDialogA CommConfigDialog;
    alias CopyFileA CopyFile;
    alias CopyFileExA CopyFileEx;
    alias CreateDirectoryA CreateDirectory;
    alias CreateDirectoryExA CreateDirectoryEx;
    alias CreateEventA CreateEvent;
    alias CreateFileA CreateFile;
    alias CreateMailslotA CreateMailslot;
    alias CreateMutexA CreateMutex;
    alias CreateProcessA CreateProcess;
    alias CreateSemaphoreA CreateSemaphore;
    alias DeleteFileA DeleteFile;
    alias EndUpdateResourceA EndUpdateResource;
    alias EnumResourceLanguagesA EnumResourceLanguages;
    alias EnumResourceNamesA EnumResourceNames;
    alias EnumResourceTypesA EnumResourceTypes;
    alias ExpandEnvironmentStringsA ExpandEnvironmentStrings;
    alias FatalAppExitA FatalAppExit;
    alias FindAtomA FindAtom;
    alias FindFirstChangeNotificationA FindFirstChangeNotification;
    alias FindFirstFileA FindFirstFile;
    alias FindNextFileA FindNextFile;
    alias FindResourceA FindResource;
    alias FindResourceExA FindResourceEx;
    alias FormatMessageA FormatMessage;
    alias FreeEnvironmentStringsA FreeEnvironmentStrings;
    alias GetAtomNameA GetAtomName;
    alias GetCommandLineA GetCommandLine;
    alias GetComputerNameA GetComputerName;
    alias GetCurrentDirectoryA GetCurrentDirectory;
    alias GetDefaultCommConfigA GetDefaultCommConfig;
    alias GetDiskFreeSpaceA GetDiskFreeSpace;
    alias GetDiskFreeSpaceExA GetDiskFreeSpaceEx;
    alias GetDriveTypeA GetDriveType;
    alias GetEnvironmentStringsA GetEnvironmentStrings;
    alias GetEnvironmentVariableA GetEnvironmentVariable;
    alias GetFileAttributesA GetFileAttributes;
    alias GetFullPathNameA GetFullPathName;
    alias GetLogicalDriveStringsA GetLogicalDriveStrings;
    alias GetNamedPipeHandleStateA GetNamedPipeHandleState;
    alias GetModuleHandleA GetModuleHandle;
    alias GetModuleFileNameA GetModuleFileName;
    alias GetPrivateProfileIntA GetPrivateProfileInt;
    alias GetPrivateProfileSectionA GetPrivateProfileSection;
    alias GetPrivateProfileSectionNamesA GetPrivateProfileSectionNames;
    alias GetPrivateProfileStringA GetPrivateProfileString;
    alias GetPrivateProfileStructA GetPrivateProfileStruct;
    alias GetProfileIntA GetProfileInt;
    alias GetProfileSectionA GetProfileSection;
    alias GetProfileStringA GetProfileString;
    alias GetShortPathNameA GetShortPathName;
    alias GetStartupInfoA GetStartupInfo;
    alias GetSystemDirectoryA GetSystemDirectory;
    alias GetTempFileNameA GetTempFileName;
    alias GetTempPathA GetTempPath;
    alias GetUserNameA GetUserName;
    alias GetVersionExA GetVersionEx;
    alias GetVolumeInformationA GetVolumeInformation;
    alias GetWindowsDirectoryA GetWindowsDirectory;
    alias GlobalAddAtomA GlobalAddAtom;
    alias GlobalFindAtomA GlobalFindAtom;
    alias GlobalGetAtomNameA GlobalGetAtomName;
    alias IsBadStringPtrA IsBadStringPtr;
    alias LoadLibraryA LoadLibrary;
    alias LoadLibraryExA LoadLibraryEx;
    alias lstrcatA lstrcat;
    alias lstrcmpA lstrcmp;
    alias lstrcmpiA lstrcmpi;
    alias lstrcpyA lstrcpy;
    alias lstrcpynA lstrcpyn;
    alias lstrlenA lstrlen;
    alias MoveFileA MoveFile;
    alias OpenEventA OpenEvent;
    alias OpenMutexA OpenMutex;
    alias OpenSemaphoreA OpenSemaphore;
    alias OutputDebugStringA OutputDebugString;
    alias RemoveDirectoryA RemoveDirectory;
    alias SearchPathA SearchPath;
    alias SetComputerNameA SetComputerName;
    alias SetCurrentDirectoryA SetCurrentDirectory;
    alias SetDefaultCommConfigA SetDefaultCommConfig;
    alias SetEnvironmentVariableA SetEnvironmentVariable;
    alias SetFileAttributesA SetFileAttributes;
    alias SetVolumeLabelA SetVolumeLabel;
    alias WaitNamedPipeA WaitNamedPipe;
    alias WritePrivateProfileSectionA WritePrivateProfileSection;
    alias WritePrivateProfileStringA WritePrivateProfileString;
    alias WritePrivateProfileStructA WritePrivateProfileStruct;
    alias WriteProfileSectionA WriteProfileSection;
    alias WriteProfileStringA WriteProfileString;
    alias CreateWaitableTimerA CreateWaitableTimer;
    alias GetFileAttributesExA GetFileAttributesEx;
    alias GetLongPathNameA GetLongPathName;
    alias QueryDosDeviceA QueryDosDevice;

    alias HW_PROFILE_INFOA HW_PROFILE_INFO;
    alias AccessCheckAndAuditAlarmA AccessCheckAndAuditAlarm;
    alias BackupEventLogA BackupEventLog;
    alias ClearEventLogA ClearEventLog;
    alias CreateNamedPipeA CreateNamedPipe;
    alias CreateProcessAsUserA CreateProcessAsUser;
    alias DefineDosDeviceA DefineDosDevice;
    alias FindFirstFileExA FindFirstFileEx;
    alias GetBinaryTypeA GetBinaryType;
    alias GetCompressedFileSizeA GetCompressedFileSize;
    alias GetFileSecurityA GetFileSecurity;
    alias LogonUserA LogonUser;
    alias LookupAccountNameA LookupAccountName;
    alias LookupAccountSidA LookupAccountSid;
    alias LookupPrivilegeDisplayNameA LookupPrivilegeDisplayName;
    alias LookupPrivilegeNameA LookupPrivilegeName;
    alias LookupPrivilegeValueA LookupPrivilegeValue;
    alias MoveFileExA MoveFileEx;
    alias ObjectCloseAuditAlarmA ObjectCloseAuditAlarm;
    alias ObjectDeleteAuditAlarmA ObjectDeleteAuditAlarm;
    alias ObjectOpenAuditAlarmA ObjectOpenAuditAlarm;
    alias ObjectPrivilegeAuditAlarmA ObjectPrivilegeAuditAlarm;
    alias OpenBackupEventLogA OpenBackupEventLog;
    alias OpenEventLogA OpenEventLog;
    alias PrivilegedServiceAuditAlarmA PrivilegedServiceAuditAlarm;
    alias ReadEventLogA ReadEventLog;
    alias RegisterEventSourceA RegisterEventSource;
    alias ReportEventA ReportEvent;
    alias SetFileSecurityA SetFileSecurity;
    alias UpdateResourceA UpdateResource;

    static if (_WIN32_WINNT >= 0x500) {
        alias CreateFileMappingA CreateFileMapping;
        alias CreateHardLinkA CreateHardLink;
        alias CreateJobObjectA CreateJobObject;
        alias DeleteVolumeMountPointA DeleteVolumeMountPoint;
        alias DnsHostnameToComputerNameA DnsHostnameToComputerName;
        alias EncryptFileA EncryptFile;
        alias FileEncryptionStatusA FileEncryptionStatus;
        alias FindFirstVolumeA FindFirstVolume;
        alias FindFirstVolumeMountPointA FindFirstVolumeMountPoint;
        alias FindNextVolumeA FindNextVolume;
        alias FindNextVolumeMountPointA FindNextVolumeMountPoint;
        alias GetModuleHandleExA GetModuleHandleEx;
        alias GetSystemWindowsDirectoryA GetSystemWindowsDirectory;
        alias GetVolumeNameForVolumeMountPointA GetVolumeNameForVolumeMountPoint;
        alias GetVolumePathNameA GetVolumePathName;
        alias OpenFileMappingA OpenFileMapping;
        alias ReplaceFileA ReplaceFile;
        alias SetVolumeMountPointA SetVolumeMountPoint;
        alias VerifyVersionInfoA VerifyVersionInfo;
    }

    static if (_WIN32_WINNT >= 0x501) {
        alias ACTCTXA ACTCTX;
        alias CheckNameLegalDOS8Dot3A CheckNameLegalDOS8Dot3;
        alias CreateActCtxA CreateActCtx;
        alias FindActCtxSectionStringA FindActCtxSectionString;
        alias GetSystemWow64DirectoryA GetSystemWow64Directory;
        alias GetVolumePathNamesForVolumeNameA GetVolumePathNamesForVolumeName;
        alias SetFileShortNameA SetFileShortName;
    }

    static if (_WIN32_WINNT >= 0x502) {
        alias GetDllDirectoryA GetDllDirectory;
        alias SetDllDirectoryA SetDllDirectory;
        alias SetFirmwareEnvironmentVariableA SetFirmwareEnvironmentVariable;
    }
}

alias STARTUPINFO* LPSTARTUPINFO;
alias WIN32_FIND_DATA* LPWIN32_FIND_DATA;

alias HW_PROFILE_INFO* LPHW_PROFILE_INFO;

static if (_WIN32_WINNT >= 0x501) {
    alias ACTCTX* PACTCTX, PCACTCTX;
}
