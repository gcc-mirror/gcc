module etc.c.sqlite3;
/*
** 2001 September 15
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** This header file defines the interface that the SQLite library
** presents to client programs.  If a C-function, structure, datatype,
** or constant definition does not appear in this file, then it is
** not a published API of SQLite, is subject to change without
** notice, and should not be referenced by programs that use SQLite.
**
** Some of the definitions that are in this file are marked as
** "experimental".  Experimental interfaces are normally new
** features recently added to SQLite.  We do not anticipate changes
** to experimental interfaces but reserve the right to make minor changes
** if experience from use "in the wild" suggest such changes are prudent.
**
** The official C-language API documentation for SQLite is derived
** from comments in this file.  This file is the authoritative source
** on how SQLite interfaces are suppose to operate.
**
** The name of this file under configuration management is "sqlite.h.in".
** The makefile makes some minor changes to this file (such as inserting
** the version number) and changes its name to "sqlite3.h" as
** part of the build process.
*/

import core.stdc.stdarg : va_list;

extern (C) __gshared nothrow:

/**
** CAPI3REF: Compile-Time Library Version Numbers
*/
enum SQLITE_VERSION = "3.10.2";
/// Ditto
enum SQLITE_VERSION_NUMBER = 3_010_002;
/// Ditto
enum SQLITE_SOURCE_ID = "2016-01-20 15:27:19 17efb4209f97fb4971656086b138599a91a75ff9";

/**
** CAPI3REF: Run-Time Library Version Numbers
*/
extern immutable(char)* sqlite3_version;
/// Ditto
immutable(char)* sqlite3_libversion();
/// Ditto
immutable(char)* sqlite3_sourceid();
/// Ditto
int sqlite3_libversion_number();

/**
** CAPI3REF: Run-Time Library Compilation Options Diagnostics
*/
int sqlite3_compileoption_used(const char *zOptName);
/// Ditto
immutable(char)* sqlite3_compileoption_get(int N);

/**
** CAPI3REF: Test To See If The Library Is Threadsafe
*/
int sqlite3_threadsafe();

/**
** CAPI3REF: Database Connection Handle
*/
struct sqlite3;

///
alias sqlite3_int64 = long;
///
alias sqlite3_uint64 = ulong;

/**
** CAPI3REF: Closing A Database Connection
**
*/
int sqlite3_close(sqlite3 *);
int sqlite3_close_v2(sqlite3*);

/**
** The type for a callback function.
** This is legacy and deprecated.  It is included for historical
** compatibility and is not documented.
*/
alias sqlite3_callback = int function (void*,int,char**, char**);

/**
** CAPI3REF: One-Step Query Execution Interface
*/
int sqlite3_exec(
    sqlite3*,                                         /** An open database */
    const(char)*sql,                                  /** SQL to be evaluated */
    int function (void*,int,char**,char**) callback,  /** Callback function */
    void *,                                           /** 1st argument to callback */
    char **errmsg                                     /** Error msg written here */
);

/**
** CAPI3REF: Result Codes
*/
enum
{
    SQLITE_OK          = 0,    /** Successful result */
/* beginning-of-error-codes */
/// Ditto
    SQLITE_ERROR       = 1,    /** SQL error or missing database */
    SQLITE_INTERNAL    = 2,    /** Internal logic error in SQLite */
    SQLITE_PERM        = 3,    /** Access permission denied */
    SQLITE_ABORT       = 4,    /** Callback routine requested an abort */
    SQLITE_BUSY        = 5,    /** The database file is locked */
    SQLITE_LOCKED      = 6,    /** A table in the database is locked */
    SQLITE_NOMEM       = 7,    /** A malloc() failed */
    SQLITE_READONLY    = 8,    /** Attempt to write a readonly database */
    SQLITE_INTERRUPT   = 9,    /** Operation terminated by sqlite3_interrupt()*/
    SQLITE_IOERR       = 10,   /** Some kind of disk I/O error occurred */
    SQLITE_CORRUPT     = 11,   /** The database disk image is malformed */
    SQLITE_NOTFOUND    = 12,   /** Unknown opcode in sqlite3_file_control() */
    SQLITE_FULL        = 13,   /** Insertion failed because database is full */
    SQLITE_CANTOPEN    = 14,   /** Unable to open the database file */
    SQLITE_PROTOCOL    = 15,   /** Database lock protocol error */
    SQLITE_EMPTY       = 16,   /** Database is empty */
    SQLITE_SCHEMA      = 17,   /** The database schema changed */
    SQLITE_TOOBIG      = 18,   /** String or BLOB exceeds size limit */
    SQLITE_CONSTRAINT  = 19,   /** Abort due to constraint violation */
    SQLITE_MISMATCH    = 20,   /** Data type mismatch */
    SQLITE_MISUSE      = 21,   /** Library used incorrectly */
    SQLITE_NOLFS       = 22,   /** Uses OS features not supported on host */
    SQLITE_AUTH        = 23,   /** Authorization denied */
    SQLITE_FORMAT      = 24,   /** Auxiliary database format error */
    SQLITE_RANGE       = 25,   /** 2nd parameter to sqlite3_bind out of range */
    SQLITE_NOTADB      = 26,   /** File opened that is not a database file */
    SQLITE_NOTICE      = 27,
    SQLITE_WARNING     = 28,
    SQLITE_ROW         = 100,  /** sqlite3_step() has another row ready */
    SQLITE_DONE        = 101  /** sqlite3_step() has finished executing */
}
/* end-of-error-codes */

/**
** CAPI3REF: Extended Result Codes
*/
enum
{
    SQLITE_IOERR_READ              = (SQLITE_IOERR | (1 << 8)),
    SQLITE_IOERR_SHORT_READ        = (SQLITE_IOERR | (2 << 8)),
    SQLITE_IOERR_WRITE             = (SQLITE_IOERR | (3 << 8)),
    SQLITE_IOERR_FSYNC             = (SQLITE_IOERR | (4 << 8)),
    SQLITE_IOERR_DIR_FSYNC         = (SQLITE_IOERR | (5 << 8)),
    SQLITE_IOERR_TRUNCATE          = (SQLITE_IOERR | (6 << 8)),
    SQLITE_IOERR_FSTAT             = (SQLITE_IOERR | (7 << 8)),
    SQLITE_IOERR_UNLOCK            = (SQLITE_IOERR | (8 << 8)),
    SQLITE_IOERR_RDLOCK            = (SQLITE_IOERR | (9 << 8)),
    SQLITE_IOERR_DELETE            = (SQLITE_IOERR | (10 << 8)),
    SQLITE_IOERR_BLOCKED           = (SQLITE_IOERR | (11 << 8)),
    SQLITE_IOERR_NOMEM             = (SQLITE_IOERR | (12 << 8)),
    SQLITE_IOERR_ACCESS            = (SQLITE_IOERR | (13 << 8)),
    SQLITE_IOERR_CHECKRESERVEDLOCK = (SQLITE_IOERR | (14 << 8)),
    SQLITE_IOERR_LOCK              = (SQLITE_IOERR | (15 << 8)),
    SQLITE_IOERR_CLOSE             = (SQLITE_IOERR | (16 << 8)),
    SQLITE_IOERR_DIR_CLOSE         = (SQLITE_IOERR | (17 << 8)),
    SQLITE_IOERR_SHMOPEN           = (SQLITE_IOERR | (18 << 8)),
    SQLITE_IOERR_SHMSIZE           = (SQLITE_IOERR | (19 << 8)),
    SQLITE_IOERR_SHMLOCK           = (SQLITE_IOERR | (20 << 8)),
    SQLITE_IOERR_SHMMAP            = (SQLITE_IOERR | (21 << 8)),
    SQLITE_IOERR_SEEK              = (SQLITE_IOERR | (22 << 8)),
    SQLITE_IOERR_DELETE_NOENT      = (SQLITE_IOERR | (23 << 8)),
    SQLITE_IOERR_MMAP              = (SQLITE_IOERR | (24 << 8)),
    SQLITE_LOCKED_SHAREDCACHE      = (SQLITE_LOCKED |  (1 << 8)),
    SQLITE_BUSY_RECOVERY           = (SQLITE_BUSY   |  (1 << 8)),
    SQLITE_CANTOPEN_NOTEMPDIR      = (SQLITE_CANTOPEN | (1 << 8)),
    SQLITE_IOERR_GETTEMPPATH       = (SQLITE_IOERR | (25 << 8)),
    SQLITE_IOERR_CONVPATH          = (SQLITE_IOERR | (26 << 8)),
    SQLITE_BUSY_SNAPSHOT           = (SQLITE_BUSY   |  (2 << 8)),
    SQLITE_CANTOPEN_ISDIR          = (SQLITE_CANTOPEN | (2 << 8)),
    SQLITE_CANTOPEN_FULLPATH       = (SQLITE_CANTOPEN | (3 << 8)),
    SQLITE_CANTOPEN_CONVPATH       = (SQLITE_CANTOPEN | (4 << 8)),
    SQLITE_CORRUPT_VTAB            = (SQLITE_CORRUPT | (1 << 8)),
    SQLITE_READONLY_RECOVERY       = (SQLITE_READONLY | (1 << 8)),
    SQLITE_READONLY_CANTLOCK       = (SQLITE_READONLY | (2 << 8)),
    SQLITE_READONLY_ROLLBACK       = (SQLITE_READONLY | (3 << 8)),
    SQLITE_READONLY_DBMOVED        = (SQLITE_READONLY | (4 << 8)),
    SQLITE_ABORT_ROLLBACK          = (SQLITE_ABORT | (2 << 8)),
    SQLITE_CONSTRAINT_CHECK        = (SQLITE_CONSTRAINT | (1 << 8)),
    SQLITE_CONSTRAINT_COMMITHOOK   = (SQLITE_CONSTRAINT | (2 << 8)),
    SQLITE_CONSTRAINT_FOREIGNKEY   = (SQLITE_CONSTRAINT | (3 << 8)),
    SQLITE_CONSTRAINT_FUNCTION     = (SQLITE_CONSTRAINT | (4 << 8)),
    SQLITE_CONSTRAINT_NOTNULL      = (SQLITE_CONSTRAINT | (5 << 8)),
    SQLITE_CONSTRAINT_PRIMARYKEY   = (SQLITE_CONSTRAINT | (6 << 8)),
    SQLITE_CONSTRAINT_TRIGGER      = (SQLITE_CONSTRAINT | (7 << 8)),
    SQLITE_CONSTRAINT_UNIQUE       = (SQLITE_CONSTRAINT | (8 << 8)),
    SQLITE_CONSTRAINT_VTAB         = (SQLITE_CONSTRAINT | (9 << 8)),
    SQLITE_CONSTRAINT_ROWID        = (SQLITE_CONSTRAINT |(10 << 8)),
    SQLITE_NOTICE_RECOVER_WAL      = (SQLITE_NOTICE | (1 << 8)),
    SQLITE_NOTICE_RECOVER_ROLLBACK = (SQLITE_NOTICE | (2 << 8)),
    SQLITE_WARNING_AUTOINDEX       = (SQLITE_WARNING | (1 << 8)),
    SQLITE_AUTH_USER               = (SQLITE_AUTH | (1 << 8))
}

/**
** CAPI3REF: Flags For File Open Operations
*/
enum
{
    SQLITE_OPEN_READONLY         = 0x00000001,  /** Ok for sqlite3_open_v2() */
    SQLITE_OPEN_READWRITE        = 0x00000002,  /** Ok for sqlite3_open_v2() */
    SQLITE_OPEN_CREATE           = 0x00000004,  /** Ok for sqlite3_open_v2() */
    SQLITE_OPEN_DELETEONCLOSE    = 0x00000008,  /** VFS only */
    SQLITE_OPEN_EXCLUSIVE        = 0x00000010,  /** VFS only */
    SQLITE_OPEN_AUTOPROXY        = 0x00000020,  /** VFS only */
    SQLITE_OPEN_URI              = 0x00000040,  /** Ok for sqlite3_open_v2() */
    SQLITE_OPEN_MEMORY           = 0x00000080,  /** Ok for sqlite3_open_v2() */
    SQLITE_OPEN_MAIN_DB          = 0x00000100,  /** VFS only */
    SQLITE_OPEN_TEMP_DB          = 0x00000200,  /** VFS only */
    SQLITE_OPEN_TRANSIENT_DB     = 0x00000400,  /** VFS only */
    SQLITE_OPEN_MAIN_JOURNAL     = 0x00000800,  /** VFS only */
    SQLITE_OPEN_TEMP_JOURNAL     = 0x00001000,  /** VFS only */
    SQLITE_OPEN_SUBJOURNAL       = 0x00002000,  /** VFS only */
    SQLITE_OPEN_MASTER_JOURNAL   = 0x00004000,  /** VFS only */
    SQLITE_OPEN_NOMUTEX          = 0x00008000,  /** Ok for sqlite3_open_v2() */
    SQLITE_OPEN_FULLMUTEX        = 0x00010000,  /** Ok for sqlite3_open_v2() */
    SQLITE_OPEN_SHAREDCACHE      = 0x00020000,  /** Ok for sqlite3_open_v2() */
    SQLITE_OPEN_PRIVATECACHE     = 0x00040000,  /** Ok for sqlite3_open_v2() */
    SQLITE_OPEN_WAL              = 0x00080000  /** VFS only */
}

/**
** CAPI3REF: Device Characteristics
*/
enum
{
    SQLITE_IOCAP_ATOMIC                 = 0x00000001,
    SQLITE_IOCAP_ATOMIC512              = 0x00000002,
    SQLITE_IOCAP_ATOMIC1K               = 0x00000004,
    SQLITE_IOCAP_ATOMIC2K               = 0x00000008,
    SQLITE_IOCAP_ATOMIC4K               = 0x00000010,
    SQLITE_IOCAP_ATOMIC8K               = 0x00000020,
    SQLITE_IOCAP_ATOMIC16K              = 0x00000040,
    SQLITE_IOCAP_ATOMIC32K              = 0x00000080,
    SQLITE_IOCAP_ATOMIC64K              = 0x00000100,
    SQLITE_IOCAP_SAFE_APPEND            = 0x00000200,
    SQLITE_IOCAP_SEQUENTIAL             = 0x00000400,
    SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN  = 0x00000800,
    SQLITE_IOCAP_POWERSAFE_OVERWRITE    = 0x00001000,
    SQLITE_IOCAP_IMMUTABLE              = 0x00002000
}

/**
** CAPI3REF: File Locking Levels
*/
enum
{
    SQLITE_LOCK_NONE          = 0,
    SQLITE_LOCK_SHARED        = 1,
    SQLITE_LOCK_RESERVED      = 2,
    SQLITE_LOCK_PENDING       = 3,
    SQLITE_LOCK_EXCLUSIVE     = 4
}

/**
** CAPI3REF: Synchronization Type Flags
*/
enum
{
    SQLITE_SYNC_NORMAL        = 0x00002,
    SQLITE_SYNC_FULL          = 0x00003,
    SQLITE_SYNC_DATAONLY      = 0x00010
}

/**
** CAPI3REF: OS Interface Open File Handle
*/
struct sqlite3_file
{
    const(sqlite3_io_methods)*pMethods;  /* Methods for an open file */
}

/**
** CAPI3REF: OS Interface File Virtual Methods Object
*/

struct sqlite3_io_methods
{
    int iVersion;
    int  function (sqlite3_file*) xClose;
    int  function (sqlite3_file*, void*, int iAmt, sqlite3_int64 iOfst) xRead;
    int  function (sqlite3_file*, const void*, int iAmt, sqlite3_int64 iOfst) xWrite;
    int  function (sqlite3_file*, sqlite3_int64 size) xTruncate;
    int  function (sqlite3_file*, int flags) xSync;
    int  function (sqlite3_file*, sqlite3_int64 *pSize) xFileSize;
    int  function (sqlite3_file*, int) xLock;
    int  function (sqlite3_file*, int) xUnlock;
    int  function (sqlite3_file*, int *pResOut) xCheckReservedLock;
    int  function (sqlite3_file*, int op, void *pArg) xFileControl;
    int  function (sqlite3_file*) xSectorSize;
    int  function (sqlite3_file*) xDeviceCharacteristics;
    /* Methods above are valid for version 1 */
    int  function (sqlite3_file*, int iPg, int pgsz, int, void **) xShmMap;
    int  function (sqlite3_file*, int offset, int n, int flags) xShmLock;
    void  function (sqlite3_file*) xShmBarrier;
    int  function (sqlite3_file*, int deleteFlag) xShmUnmap;
    /* Methods above are valid for version 2 */
    /* Additional methods may be added in future releases */
    int function (sqlite3_file*, sqlite3_int64 iOfst, int iAmt, void **pp) xFetch;
    int function (sqlite3_file*, sqlite3_int64 iOfst, void *p) xUnfetch;
}

/**
** CAPI3REF: Standard File Control Opcodes
*/
enum
{
    SQLITE_FCNTL_LOCKSTATE           = 1,
    SQLITE_GET_LOCKPROXYFILE         = 2,
    SQLITE_SET_LOCKPROXYFILE         = 3,
    SQLITE_LAST_ERRNO                = 4,
    SQLITE_FCNTL_SIZE_HINT           = 5,
    SQLITE_FCNTL_CHUNK_SIZE          = 6,
    SQLITE_FCNTL_FILE_POINTER        = 7,
    SQLITE_FCNTL_SYNC_OMITTED        = 8,
    SQLITE_FCNTL_WIN32_AV_RETRY      = 9,
    SQLITE_FCNTL_PERSIST_WAL         = 10,
    SQLITE_FCNTL_OVERWRITE           = 11,
    SQLITE_FCNTL_VFSNAME             = 12,
    SQLITE_FCNTL_POWERSAFE_OVERWRITE = 13,
    SQLITE_FCNTL_PRAGMA              = 14,
    SQLITE_FCNTL_BUSYHANDLER         = 15,
    SQLITE_FCNTL_TEMPFILENAME        = 16,
    SQLITE_FCNTL_MMAP_SIZE           = 18,
    SQLITE_FCNTL_TRACE               = 19,
    SQLITE_FCNTL_HAS_MOVED           = 20,
    SQLITE_FCNTL_SYNC                = 21,
    SQLITE_FCNTL_COMMIT_PHASETWO     = 22,
    SQLITE_FCNTL_WIN32_SET_HANDLE    = 23,
    SQLITE_FCNTL_WAL_BLOCK           = 24,
    SQLITE_FCNTL_ZIPVFS              = 25,
    SQLITE_FCNTL_RBU                 = 26,
    SQLITE_FCNTL_VFS_POINTER         = 27,
}

/**
** CAPI3REF: Mutex Handle
*/
struct sqlite3_mutex;

/**
** CAPI3REF: OS Interface Object
*/

alias xDlSymReturn = void * function();
/// Ditto
alias sqlite3_syscall_ptr = void function();

struct sqlite3_vfs
{
    int iVersion;            /** Structure version number (currently 2) */
    int szOsFile;            /** Size of subclassed sqlite3_file */
    int mxPathname;          /** Maximum file pathname length */
    sqlite3_vfs *pNext;      /** Next registered VFS */
    const(char)*zName;       /** Name of this virtual file system */
    void *pAppData;          /** Pointer to application-specific data */
    int function (sqlite3_vfs*, const char *zName, sqlite3_file*,
               int flags, int *pOutFlags) xOpen;
    int  function (sqlite3_vfs*, const char *zName, int syncDir) xDelete;
    int  function (sqlite3_vfs*, const char *zName, int flags, int *pResOut) xAccess;
    int  function (sqlite3_vfs*, const char *zName, int nOut, char *zOut) xFullPathname;
    void* function (sqlite3_vfs*, const char *zFilename) xDlOpen;
    void  function (sqlite3_vfs*, int nByte, char *zErrMsg) xDlError;
    xDlSymReturn function (sqlite3_vfs*,void*, const char *zSymbol) *xDlSym;
    void  function (sqlite3_vfs*, void*) xDlClose;
    int  function (sqlite3_vfs*, int nByte, char *zOut) xRandomness;
    int  function (sqlite3_vfs*, int microseconds) xSleep;
    int  function (sqlite3_vfs*, double*) xCurrentTime;
    int  function (sqlite3_vfs*, int, char *) xGetLastError;
    /*
    ** The methods above are in version 1 of the sqlite_vfs object
    ** definition.  Those that follow are added in version 2 or later
    */
    int  function (sqlite3_vfs*, sqlite3_int64*) xCurrentTimeInt64;
    /*
    ** The methods above are in versions 1 and 2 of the sqlite_vfs object.
    ** Those below are for version 3 and greater.
    */
    int function(sqlite3_vfs*, const char * zName, sqlite3_syscall_ptr) xSetSystemCall;
    sqlite3_syscall_ptr function(sqlite3_vfs*, const char * zName) xGetSystemCall;
    const(char)* function(sqlite3_vfs*, const char * zName) xNextSystemCall;
    /*
    ** The methods above are in versions 1 through 3 of the sqlite_vfs object.
    ** New fields may be appended in figure versions.  The iVersion
    ** value will increment whenever this happens.
    */
}

/**
** CAPI3REF: Flags for the xAccess VFS method
*/
enum
{
    SQLITE_ACCESS_EXISTS    = 0,

    SQLITE_ACCESS_READWRITE = 1,   /** Used by PRAGMA temp_store_directory */
    SQLITE_ACCESS_READ      = 2   /** Unused */
}

/**
** CAPI3REF: Flags for the xShmLock VFS method
*/
enum
{
    SQLITE_SHM_UNLOCK       = 1,
    SQLITE_SHM_LOCK         = 2,
    SQLITE_SHM_SHARED       = 4,
    SQLITE_SHM_EXCLUSIVE    = 8
}

/**
** CAPI3REF: Maximum xShmLock index
*/
enum SQLITE_SHM_NLOCK        = 8;


/**
** CAPI3REF: Initialize The SQLite Library
*/
int sqlite3_initialize();
/// Ditto
int sqlite3_shutdown();
/// Ditto
int sqlite3_os_init();
/// Ditto
int sqlite3_os_end();

/**
** CAPI3REF: Configuring The SQLite Library
*/
int sqlite3_config(int, ...);

/**
** CAPI3REF: Configure database connections
*/
int sqlite3_db_config(sqlite3*, int op, ...);

/**
** CAPI3REF: Memory Allocation Routines
*/
struct sqlite3_mem_methods
{
    void* function (int) xMalloc;         /** Memory allocation function */
    void function (void*) xFree;          /** Free a prior allocation */
    void* function (void*,int) xRealloc;  /** Resize an allocation */
    int function (void*) xSize;           /** Return the size of an allocation */
    int function (int) xRoundup;          /** Round up request size to allocation size */
    int function (void*) xInit;           /** Initialize the memory allocator */
    void function (void*) xShutdown;      /** Deinitialize the memory allocator */
    void *pAppData;                       /** Argument to xInit() and xShutdown() */
}

/**
** CAPI3REF: Configuration Options
*/
enum
{
    SQLITE_CONFIG_SINGLETHREAD         = 1,  /** nil */
    SQLITE_CONFIG_MULTITHREAD          = 2,  /** nil */
    SQLITE_CONFIG_SERIALIZED           = 3,  /** nil */
    SQLITE_CONFIG_MALLOC               = 4,  /** sqlite3_mem_methods* */
    SQLITE_CONFIG_GETMALLOC            = 5,  /** sqlite3_mem_methods* */
    SQLITE_CONFIG_SCRATCH              = 6,  /** void*, int sz, int N */
    SQLITE_CONFIG_PAGECACHE            = 7,  /** void*, int sz, int N */
    SQLITE_CONFIG_HEAP                 = 8,  /** void*, int nByte, int min */
    SQLITE_CONFIG_MEMSTATUS            = 9,  /** boolean */
    SQLITE_CONFIG_MUTEX                = 10,  /** sqlite3_mutex_methods* */
    SQLITE_CONFIG_GETMUTEX             = 11,  /** sqlite3_mutex_methods* */
/* previously SQLITE_CONFIG_CHUNKALLOC 12 which is now unused. */
    SQLITE_CONFIG_LOOKASIDE            = 13,  /** int int */
    SQLITE_CONFIG_PCACHE               = 14,  /** sqlite3_pcache_methods* */
    SQLITE_CONFIG_GETPCACHE            = 15,  /** sqlite3_pcache_methods* */
    SQLITE_CONFIG_LOG                  = 16,  /** xFunc, void* */
    SQLITE_CONFIG_URI                  = 17,
    SQLITE_CONFIG_PCACHE2              = 18,
    SQLITE_CONFIG_GETPCACHE2           = 19,
    SQLITE_CONFIG_COVERING_INDEX_SCAN  = 20,
    SQLITE_CONFIG_SQLLOG               = 21,
    SQLITE_CONFIG_MMAP_SIZE            = 22,
    SQLITE_CONFIG_WIN32_HEAPSIZE       = 23,
    SQLITE_CONFIG_PCACHE_HDRSZ         = 24,
    SQLITE_CONFIG_PMASZ                = 25,
}

/**
** CAPI3REF: Database Connection Configuration Options
*/
enum
{
    SQLITE_DBCONFIG_LOOKASIDE      = 1001,  /** void* int int */
    SQLITE_DBCONFIG_ENABLE_FKEY    = 1002,  /** int int* */
    SQLITE_DBCONFIG_ENABLE_TRIGGER = 1003  /** int int* */
}


/**
** CAPI3REF: Enable Or Disable Extended Result Codes
*/
int sqlite3_extended_result_codes(sqlite3*, int onoff);

/**
** CAPI3REF: Last Insert Rowid
*/
sqlite3_int64 sqlite3_last_insert_rowid(sqlite3*);

/**
** CAPI3REF: Count The Number Of Rows Modified
*/
int sqlite3_changes(sqlite3*);

/**
** CAPI3REF: Total Number Of Rows Modified
*/
int sqlite3_total_changes(sqlite3*);

/**
** CAPI3REF: Interrupt A Long-Running Query
*/
void sqlite3_interrupt(sqlite3*);

/**
** CAPI3REF: Determine If An SQL Statement Is Complete
*/
int sqlite3_complete(const char *sql);
/// Ditto
int sqlite3_complete16(const void *sql);

/**
** CAPI3REF: Register A Callback To Handle SQLITE_BUSY Errors
*/
int sqlite3_busy_handler(sqlite3*, int function (void*,int), void*);

/**
** CAPI3REF: Set A Busy Timeout
*/
int sqlite3_busy_timeout(sqlite3*, int ms);

/**
** CAPI3REF: Convenience Routines For Running Queries
*/
int sqlite3_get_table(
    sqlite3 *db,          /** An open database */
    const(char)*zSql,     /** SQL to be evaluated */
    char ***pazResult,    /** Results of the query */
    int *pnRow,           /** Number of result rows written here */
    int *pnColumn,        /** Number of result columns written here */
    char **pzErrmsg       /** Error msg written here */
);
///
void sqlite3_free_table(char **result);

/**
** CAPI3REF: Formatted String Printing Functions
*/
char *sqlite3_mprintf(const char*,...);
char *sqlite3_vmprintf(const char*, va_list);
char *sqlite3_snprintf(int,char*,const char*, ...);
char *sqlite3_vsnprintf(int,char*,const char*, va_list);

/**
** CAPI3REF: Memory Allocation Subsystem
*/
void *sqlite3_malloc(int);
/// Ditto
void *sqlite3_malloc64(sqlite3_uint64);
/// Ditto
void *sqlite3_realloc(void*, int);
/// Ditto
void *sqlite3_realloc64(void*, sqlite3_uint64);
/// Ditto
void sqlite3_free(void*);
/// Ditto
sqlite3_uint64 sqlite3_msize(void*);

/**
** CAPI3REF: Memory Allocator Statistics
*/
sqlite3_int64 sqlite3_memory_used();
sqlite3_int64 sqlite3_memory_highwater(int resetFlag);

/**
** CAPI3REF: Pseudo-Random Number Generator
*/
void sqlite3_randomness(int N, void *P);

/**
** CAPI3REF: Compile-Time Authorization Callbacks
*/
int sqlite3_set_authorizer(
    sqlite3*,
    int function (void*,int,const char*,const char*,const char*,const char*) xAuth,
    void *pUserData
);

/**
** CAPI3REF: Authorizer Return Codes
*/
enum
{
    SQLITE_DENY   = 1,   /** Abort the SQL statement with an error */
    SQLITE_IGNORE = 2   /** Don't allow access, but don't generate an error */
}

/**
** CAPI3REF: Authorizer Action Codes
*/
/******************************************* 3rd ************ 4th ***********/
enum
{
    SQLITE_CREATE_INDEX         =  1,   /** Index Name      Table Name      */
    SQLITE_CREATE_TABLE         =  2,   /** Table Name      NULL            */
    SQLITE_CREATE_TEMP_INDEX    =  3,   /** Index Name      Table Name      */
    SQLITE_CREATE_TEMP_TABLE    =  4,   /** Table Name      NULL            */
    SQLITE_CREATE_TEMP_TRIGGER  =  5,   /** Trigger Name    Table Name      */
    SQLITE_CREATE_TEMP_VIEW     =  6,   /** View Name       NULL            */
    SQLITE_CREATE_TRIGGER       =  7,   /** Trigger Name    Table Name      */
    SQLITE_CREATE_VIEW          =  8,   /** View Name       NULL            */
    SQLITE_DELETE               =  9,   /** Table Name      NULL            */
    SQLITE_DROP_INDEX           = 10,   /** Index Name      Table Name      */
    SQLITE_DROP_TABLE           = 11,   /** Table Name      NULL            */
    SQLITE_DROP_TEMP_INDEX      = 12,   /** Index Name      Table Name      */
    SQLITE_DROP_TEMP_TABLE      = 13,   /** Table Name      NULL            */
    SQLITE_DROP_TEMP_TRIGGER    = 14,   /** Trigger Name    Table Name      */
    SQLITE_DROP_TEMP_VIEW       = 15,   /** View Name       NULL            */
    SQLITE_DROP_TRIGGER         = 16,   /** Trigger Name    Table Name      */
    SQLITE_DROP_VIEW            = 17,   /** View Name       NULL            */
    SQLITE_INSERT               = 18,   /** Table Name      NULL            */
    SQLITE_PRAGMA               = 19,   /** Pragma Name     1st arg or NULL */
    SQLITE_READ                 = 20,   /** Table Name      Column Name     */
    SQLITE_SELECT               = 21,   /** NULL            NULL            */
    SQLITE_TRANSACTION          = 22,   /** Operation       NULL            */
    SQLITE_UPDATE               = 23,   /** Table Name      Column Name     */
    SQLITE_ATTACH               = 24,   /** Filename        NULL            */
    SQLITE_DETACH               = 25,   /** Database Name   NULL            */
    SQLITE_ALTER_TABLE          = 26,   /** Database Name   Table Name      */
    SQLITE_REINDEX              = 27,   /** Index Name      NULL            */
    SQLITE_ANALYZE              = 28,   /** Table Name      NULL            */
    SQLITE_CREATE_VTABLE        = 29,   /** Table Name      Module Name     */
    SQLITE_DROP_VTABLE          = 30,   /** Table Name      Module Name     */
    SQLITE_FUNCTION             = 31,   /** NULL            Function Name   */
    SQLITE_SAVEPOINT            = 32,   /** Operation       Savepoint Name  */
    SQLITE_COPY                 =  0,   /** No longer used */
    SQLITE_RECURSIVE            = 33
}

/**
** CAPI3REF: Tracing And Profiling Functions
*/
void *sqlite3_trace(sqlite3*, void function (void*,const char*) xTrace, void*);
/// Ditto
void *sqlite3_profile(sqlite3*, void function (void*,const char*,sqlite3_uint64) xProfile, void*);

/**
** CAPI3REF: Query Progress Callbacks
*/
void sqlite3_progress_handler(sqlite3*, int, int function (void*), void*);

/**
** CAPI3REF: Opening A New Database Connection
*/
int sqlite3_open(
    const(char)*filename,   /** Database filename (UTF-8) */
    sqlite3 **ppDb          /** OUT: SQLite db handle */
);
/// Ditto
int sqlite3_open16(
    const(void)*filename,   /** Database filename (UTF-16) */
    sqlite3 **ppDb          /** OUT: SQLite db handle */
);
/// Ditto
int sqlite3_open_v2(
    const(char)*filename,   /** Database filename (UTF-8) */
    sqlite3 **ppDb,         /** OUT: SQLite db handle */
    int flags,              /** Flags */
    const(char)*zVfs        /** Name of VFS module to use */
);

/*
** CAPI3REF: Obtain Values For URI Parameters
*/
const(char)* sqlite3_uri_parameter(const(char)* zFilename, const(char)* zParam);
/// Ditto
int sqlite3_uri_boolean(const(char)* zFile, const(char)* zParam, int bDefault);
/// Ditto
sqlite3_int64 sqlite3_uri_int64(const char*, const char*, sqlite3_int64);

/**
** CAPI3REF: Error Codes And Messages
*/
int sqlite3_errcode(sqlite3 *db);
/// Ditto
int sqlite3_extended_errcode(sqlite3 *db);
/// Ditto
const(char)* sqlite3_errmsg(sqlite3*);
/// Ditto
const(void)* sqlite3_errmsg16(sqlite3*);
/// Ditto
const(char)* sqlite3_errstr(int);

/**
** CAPI3REF: SQL Statement Object
*/
struct sqlite3_stmt;

/**
** CAPI3REF: Run-time Limits
*/
int sqlite3_limit(sqlite3*, int id, int newVal);

/**
** CAPI3REF: Run-Time Limit Categories
*/
enum
{
    SQLITE_LIMIT_LENGTH                    = 0,
    SQLITE_LIMIT_SQL_LENGTH                = 1,
    SQLITE_LIMIT_COLUMN                    = 2,
    SQLITE_LIMIT_EXPR_DEPTH                = 3,
    SQLITE_LIMIT_COMPOUND_SELECT           = 4,
    SQLITE_LIMIT_VDBE_OP                   = 5,
    SQLITE_LIMIT_FUNCTION_ARG              = 6,
    SQLITE_LIMIT_ATTACHED                  = 7,
    SQLITE_LIMIT_LIKE_PATTERN_LENGTH       = 8,
    SQLITE_LIMIT_VARIABLE_NUMBER           = 9,
    SQLITE_LIMIT_TRIGGER_DEPTH             = 10,
    SQLITE_LIMIT_WORKER_THREADS            = 11,
}

/**
** CAPI3REF: Compiling An SQL Statement
*/
int sqlite3_prepare(
    sqlite3 *db,            /** Database handle */
    const(char)*zSql,       /** SQL statement, UTF-8 encoded */
    int nByte,              /** Maximum length of zSql in bytes. */
    sqlite3_stmt **ppStmt,  /** OUT: Statement handle */
    const(char*)*pzTail     /** OUT: Pointer to unused portion of zSql */
);
/// Ditto
int sqlite3_prepare_v2(
    sqlite3 *db,            /** Database handle */
    const(char)*zSql,       /** SQL statement, UTF-8 encoded */
    int nByte,              /** Maximum length of zSql in bytes. */
    sqlite3_stmt **ppStmt,  /** OUT: Statement handle */
    const(char*)*pzTail     /** OUT: Pointer to unused portion of zSql */
);
/// Ditto
int sqlite3_prepare16(
    sqlite3 *db,            /** Database handle */
    const(void)*zSql,       /** SQL statement, UTF-16 encoded */
    int nByte,              /** Maximum length of zSql in bytes. */
    sqlite3_stmt **ppStmt,  /** OUT: Statement handle */
    const(void*)*pzTail     /** OUT: Pointer to unused portion of zSql */
);
/// Ditto
int sqlite3_prepare16_v2(
    sqlite3 *db,            /** Database handle */
    const(void)*zSql,       /** SQL statement, UTF-16 encoded */
    int nByte,              /** Maximum length of zSql in bytes. */
    sqlite3_stmt **ppStmt,  /** OUT: Statement handle */
    const(void*)*pzTail     /** OUT: Pointer to unused portion of zSql */
);

/**
** CAPI3REF: Retrieving Statement SQL
*/
const(char)* sqlite3_sql(sqlite3_stmt *pStmt);

/*
** CAPI3REF: Determine If An SQL Statement Writes The Database
*/
int sqlite3_stmt_readonly(sqlite3_stmt *pStmt);

/**
** CAPI3REF: Determine If A Prepared Statement Has Been Reset
*/
int sqlite3_stmt_busy(sqlite3_stmt*);


/**
** CAPI3REF: Dynamically Typed Value Object
*/
struct sqlite3_value;

/**
** CAPI3REF: SQL Function Context Object
*/
struct sqlite3_context;

/**
** CAPI3REF: Binding Values To Prepared Statements
*/
int sqlite3_bind_blob(sqlite3_stmt*, int, const void*, int n, void function (void*));
/// Ditto
int sqlite3_bind_blob64(sqlite3_stmt*, int, const void*, sqlite3_uint64,void function (void*));
/// Ditto
int sqlite3_bind_double(sqlite3_stmt*, int, double);
/// Ditto
int sqlite3_bind_int(sqlite3_stmt*, int, int);
/// Ditto
int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite3_int64);
/// Ditto
int sqlite3_bind_null(sqlite3_stmt*, int);
/// Ditto
int sqlite3_bind_text(sqlite3_stmt*, int, const char*, int n, void function (void*));
/// Ditto
int sqlite3_bind_text16(sqlite3_stmt*, int, const void*, int, void function (void*));
/// Ditto
int sqlite3_bind_text64(sqlite3_stmt*, int, const char*, sqlite3_uint64,void function (void*), ubyte encoding);
/// Ditto
int sqlite3_bind_value(sqlite3_stmt*, int, const sqlite3_value*);
/// Ditto
int sqlite3_bind_zeroblob(sqlite3_stmt*, int, int n);
/// Ditto
int sqlite3_bind_zeroblob64(sqlite3_stmt*, int, sqlite3_uint64 n);

/**
** CAPI3REF: Number Of SQL Parameters
*/
int sqlite3_bind_parameter_count(sqlite3_stmt*);

/**
** CAPI3REF: Name Of A Host Parameter
*/
const(char)* sqlite3_bind_parameter_name(sqlite3_stmt*, int);

/**
** CAPI3REF: Index Of A Parameter With A Given Name
*/
int sqlite3_bind_parameter_index(sqlite3_stmt*, const char *zName);

/**
** CAPI3REF: Reset All Bindings On A Prepared Statement
*/
int sqlite3_clear_bindings(sqlite3_stmt*);

/**
** CAPI3REF: Number Of Columns In A Result Set
*/
int sqlite3_column_count(sqlite3_stmt *pStmt);

/**
** CAPI3REF: Column Names In A Result Set
*/
const(char)* sqlite3_column_name(sqlite3_stmt*, int N);
/// Ditto
const(void)* sqlite3_column_name16(sqlite3_stmt*, int N);

/**
** CAPI3REF: Source Of Data In A Query Result
*/
const(char)* sqlite3_column_database_name(sqlite3_stmt*,int);
/// Ditto
const(void)* sqlite3_column_database_name16(sqlite3_stmt*,int);
/// Ditto
const(char)* sqlite3_column_table_name(sqlite3_stmt*,int);
/// Ditto
const (void)* sqlite3_column_table_name16(sqlite3_stmt*,int);
/// Ditto
const (char)* sqlite3_column_origin_name(sqlite3_stmt*,int);
/// Ditto
const (void)* sqlite3_column_origin_name16(sqlite3_stmt*,int);

/**
** CAPI3REF: Declared Datatype Of A Query Result
*/
const (char)* sqlite3_column_decltype(sqlite3_stmt*,int);
/// Ditto
const (void)* sqlite3_column_decltype16(sqlite3_stmt*,int);

/**
** CAPI3REF: Evaluate An SQL Statement
*/
int sqlite3_step(sqlite3_stmt*);

/**
** CAPI3REF: Number of columns in a result set
*/
int sqlite3_data_count(sqlite3_stmt *pStmt);

/**
** CAPI3REF: Fundamental Datatypes
*/
enum
{
    SQLITE_INTEGER  = 1,
    SQLITE_FLOAT    = 2,
    SQLITE_BLOB     = 4,
    SQLITE_NULL     = 5,
    SQLITE3_TEXT    = 3
}

/**
** CAPI3REF: Result Values From A Query
*/
const (void)* sqlite3_column_blob(sqlite3_stmt*, int iCol);
/// Ditto
int sqlite3_column_bytes(sqlite3_stmt*, int iCol);
/// Ditto
int sqlite3_column_bytes16(sqlite3_stmt*, int iCol);
/// Ditto
double sqlite3_column_double(sqlite3_stmt*, int iCol);
/// Ditto
int sqlite3_column_int(sqlite3_stmt*, int iCol);
/// Ditto
sqlite3_int64 sqlite3_column_int64(sqlite3_stmt*, int iCol);
/// Ditto
const (char)* sqlite3_column_text(sqlite3_stmt*, int iCol);
/// Ditto
const (void)* sqlite3_column_text16(sqlite3_stmt*, int iCol);
/// Ditto
int sqlite3_column_type(sqlite3_stmt*, int iCol);
/// Ditto
sqlite3_value *sqlite3_column_value(sqlite3_stmt*, int iCol);

/**
** CAPI3REF: Destroy A Prepared Statement Object
*/
int sqlite3_finalize(sqlite3_stmt *pStmt);

/**
** CAPI3REF: Reset A Prepared Statement Object
*/
int sqlite3_reset(sqlite3_stmt *pStmt);

/**
** CAPI3REF: Create Or Redefine SQL Functions
*/
int sqlite3_create_function(
    sqlite3 *db,
    const(char)*zFunctionName,
    int nArg,
    int eTextRep,
    void *pApp,
    void function (sqlite3_context*,int,sqlite3_value**) xFunc,
    void function (sqlite3_context*,int,sqlite3_value**) xStep,
    void function (sqlite3_context*) xFinal
);
/// Ditto
int sqlite3_create_function16(
    sqlite3 *db,
    const(void)*zFunctionName,
    int nArg,
    int eTextRep,
    void *pApp,
    void function (sqlite3_context*,int,sqlite3_value**) xFunc,
    void function (sqlite3_context*,int,sqlite3_value**) xStep,
    void function (sqlite3_context*) xFinal
);
/// Ditto
int sqlite3_create_function_v2(
    sqlite3 *db,
    const(char)*zFunctionName,
    int nArg,
    int eTextRep,
    void *pApp,
    void function (sqlite3_context*,int,sqlite3_value**) xFunc,
    void function (sqlite3_context*,int,sqlite3_value**) xStep,
    void function (sqlite3_context*) xFinal,
    void function (void*) xDestroy
);

/**
** CAPI3REF: Text Encodings
**
** These constant define integer codes that represent the various
** text encodings supported by SQLite.
*/
enum
{
    SQLITE_UTF8           = 1,
    SQLITE_UTF16LE        = 2,
    SQLITE_UTF16BE        = 3
}
/// Ditto
enum
{
    SQLITE_UTF16          = 4,    /** Use native byte order */
    SQLITE_ANY            = 5,    /** sqlite3_create_function only */
    SQLITE_UTF16_ALIGNED  = 8    /** sqlite3_create_collation only */
}

/**
** CAPI3REF: Function Flags
*/
enum SQLITE_DETERMINISTIC = 0x800;

/**
** CAPI3REF: Deprecated Functions
*/
deprecated int sqlite3_aggregate_count(sqlite3_context*);
deprecated int sqlite3_expired(sqlite3_stmt*);
deprecated int sqlite3_transfer_bindings(sqlite3_stmt*, sqlite3_stmt*);
deprecated int sqlite3_global_recover();
deprecated void sqlite3_thread_cleanup();
deprecated int sqlite3_memory_alarm(void function(void*,sqlite3_int64,int),void*,sqlite3_int64);

/**
** CAPI3REF: Obtaining SQL Function Parameter Values
*/
const (void)* sqlite3_value_blob(sqlite3_value*);
/// Ditto
int sqlite3_value_bytes(sqlite3_value*);
/// Ditto
int sqlite3_value_bytes16(sqlite3_value*);
/// Ditto
double sqlite3_value_double(sqlite3_value*);
/// Ditto
int sqlite3_value_int(sqlite3_value*);
/// Ditto
sqlite3_int64 sqlite3_value_int64(sqlite3_value*);
/// Ditto
const (char)* sqlite3_value_text(sqlite3_value*);
/// Ditto
const (void)* sqlite3_value_text16(sqlite3_value*);
/// Ditto
const (void)* sqlite3_value_text16le(sqlite3_value*);
/// Ditto
const (void)* sqlite3_value_text16be(sqlite3_value*);
/// Ditto
int sqlite3_value_type(sqlite3_value*);
/// Ditto
int sqlite3_value_numeric_type(sqlite3_value*);

/*
** CAPI3REF: Finding The Subtype Of SQL Values
*/
uint sqlite3_value_subtype(sqlite3_value*);

/*
** CAPI3REF: Copy And Free SQL Values
*/
sqlite3_value* sqlite3_value_dup(const sqlite3_value*);
void sqlite3_value_free(sqlite3_value*);

/**
** CAPI3REF: Obtain Aggregate Function Context
*/
void *sqlite3_aggregate_context(sqlite3_context*, int nBytes);

/**
** CAPI3REF: User Data For Functions
*/
void *sqlite3_user_data(sqlite3_context*);

/**
** CAPI3REF: Database Connection For Functions
*/
sqlite3 *sqlite3_context_db_handle(sqlite3_context*);

/**
** CAPI3REF: Function Auxiliary Data
*/
void *sqlite3_get_auxdata(sqlite3_context*, int N);
/// Ditto
void sqlite3_set_auxdata(sqlite3_context*, int N, void*, void function (void*));


/**
** CAPI3REF: Constants Defining Special Destructor Behavior
*/
alias sqlite3_destructor_type = void function (void*);
/// Ditto
enum
{
    SQLITE_STATIC      = (cast(sqlite3_destructor_type) 0),
    SQLITE_TRANSIENT   = (cast (sqlite3_destructor_type) -1)
}

/**
** CAPI3REF: Setting The Result Of An SQL Function
*/
void sqlite3_result_blob(sqlite3_context*, const void*, int, void function(void*));
/// Ditto
void sqlite3_result_blob64(sqlite3_context*,const void*,sqlite3_uint64,void function(void*));
/// Ditto
void sqlite3_result_double(sqlite3_context*, double);
/// Ditto
void sqlite3_result_error(sqlite3_context*, const char*, int);
/// Ditto
void sqlite3_result_error16(sqlite3_context*, const void*, int);
/// Ditto
void sqlite3_result_error_toobig(sqlite3_context*);
/// Ditto
void sqlite3_result_error_nomem(sqlite3_context*);
/// Ditto
void sqlite3_result_error_code(sqlite3_context*, int);
/// Ditto
void sqlite3_result_int(sqlite3_context*, int);
/// Ditto
void sqlite3_result_int64(sqlite3_context*, sqlite3_int64);
/// Ditto
void sqlite3_result_null(sqlite3_context*);
/// Ditto
void sqlite3_result_text(sqlite3_context*, const char*, int, void function(void*));
/// Ditto
void sqlite3_result_text64(sqlite3_context*, const char*,sqlite3_uint64,void function(void*), ubyte encoding);
/// Ditto
void sqlite3_result_text16(sqlite3_context*, const void*, int, void function(void*));
/// Ditto
void sqlite3_result_text16le(sqlite3_context*, const void*, int, void function(void*));
/// Ditto
void sqlite3_result_text16be(sqlite3_context*, const void*, int, void function(void*));
/// Ditto
void sqlite3_result_value(sqlite3_context*, sqlite3_value*);
/// Ditto
void sqlite3_result_zeroblob(sqlite3_context*, int n);
/// Ditto
int sqlite3_result_zeroblob64(sqlite3_context*, sqlite3_uint64 n);

/*
** CAPI3REF: Setting The Subtype Of An SQL Function
*/
void sqlite3_result_subtype(sqlite3_context*,uint);

/**
** CAPI3REF: Define New Collating Sequences
*/
int sqlite3_create_collation(
    sqlite3*,
    const(char)*zName,
    int eTextRep,
    void *pArg,
    int function (void*,int,const void*,int,const void*) xCompare
);
/// Ditto
int sqlite3_create_collation_v2(
    sqlite3*,
    const(char)*zName,
    int eTextRep,
    void *pArg,
    int function (void*,int,const void*,int,const void*) xCompare,
    void function (void*) xDestroy
);
/// Ditto
int sqlite3_create_collation16(
    sqlite3*,
    const(void)*zName,
    int eTextRep,
    void *pArg,
    int function (void*,int,const void*,int,const void*) xCompare
);

/**
** CAPI3REF: Collation Needed Callbacks
*/
int sqlite3_collation_needed(
    sqlite3*,
    void*,
    void function (void*,sqlite3*,int eTextRep,const char*)
);
/// Ditto
int sqlite3_collation_needed16(
    sqlite3*,
    void*,
    void function (void*,sqlite3*,int eTextRep,const void*)
);

///
int sqlite3_key(
    sqlite3 *db,                   /** Database to be rekeyed */
    const(void)*pKey, int nKey     /** The key */
);
/// Ditto
int sqlite3_key_v2(
    sqlite3 *db,                   /* Database to be rekeyed */
    const(char)* zDbName,           /* Name of the database */
    const(void)* pKey, int nKey     /* The key */
);

/**
** Change the key on an open database.  If the current database is not
** encrypted, this routine will encrypt it.  If pNew == 0 or nNew == 0, the
** database is decrypted.
**
** The code to implement this API is not available in the public release
** of SQLite.
*/
int sqlite3_rekey(
    sqlite3 *db,                   /** Database to be rekeyed */
    const(void)*pKey, int nKey     /** The new key */
);
int sqlite3_rekey_v2(
    sqlite3 *db,                   /* Database to be rekeyed */
    const(char)* zDbName,           /* Name of the database */
    const(void)* pKey, int nKey     /* The new key */
);

/**
** Specify the activation key for a SEE database.  Unless
** activated, none of the SEE routines will work.
*/
void sqlite3_activate_see(
    const(char)*zPassPhrase        /** Activation phrase */
);

/**
** Specify the activation key for a CEROD database.  Unless
** activated, none of the CEROD routines will work.
*/
void sqlite3_activate_cerod(
    const(char)*zPassPhrase        /** Activation phrase */
);

/**
** CAPI3REF: Suspend Execution For A Short Time
*/
int sqlite3_sleep(int);

/**
** CAPI3REF: Name Of The Folder Holding Temporary Files
*/
extern char *sqlite3_temp_directory;

/**
** CAPI3REF: Name Of The Folder Holding Database Files
*/
extern char *sqlite3_data_directory;

/**
** CAPI3REF: Test For Auto-Commit Mode
*/
int sqlite3_get_autocommit(sqlite3*);

/**
** CAPI3REF: Find The Database Handle Of A Prepared Statement
*/
sqlite3 *sqlite3_db_handle(sqlite3_stmt*);

/**
** CAPI3REF: Return The Filename For A Database Connection
*/
const(char)* sqlite3_db_filename(sqlite3 *db, const char* zDbName);

/**
** CAPI3REF: Determine if a database is read-only
*/
int sqlite3_db_readonly(sqlite3 *db, const char * zDbName);

/*
** CAPI3REF: Find the next prepared statement
*/
sqlite3_stmt *sqlite3_next_stmt(sqlite3 *pDb, sqlite3_stmt *pStmt);

/**
** CAPI3REF: Commit And Rollback Notification Callbacks
*/
void *sqlite3_commit_hook(sqlite3*, int function (void*), void*);
/// Ditto
void *sqlite3_rollback_hook(sqlite3*, void function (void *), void*);

/**
** CAPI3REF: Data Change Notification Callbacks
*/
void *sqlite3_update_hook(
    sqlite3*,
    void function (void *,int ,char *, char *, sqlite3_int64),
    void*
);

/**
** CAPI3REF: Enable Or Disable Shared Pager Cache
*/
int sqlite3_enable_shared_cache(int);

/**
** CAPI3REF: Attempt To Free Heap Memory
*/
int sqlite3_release_memory(int);

/**
** CAPI3REF: Free Memory Used By A Database Connection
*/
int sqlite3_db_release_memory(sqlite3*);

/*
** CAPI3REF: Impose A Limit On Heap Size
*/
sqlite3_int64 sqlite3_soft_heap_limit64(sqlite3_int64 N);

/**
** CAPI3REF: Deprecated Soft Heap Limit Interface
*/
deprecated void sqlite3_soft_heap_limit(int N);

/**
** CAPI3REF: Extract Metadata About A Column Of A Table
*/
int sqlite3_table_column_metadata(
    sqlite3 *db,                /** Connection handle */
    const(char)*zDbName,        /** Database name or NULL */
    const(char)*zTableName,     /** Table name */
    const(char)*zColumnName,    /** Column name */
    char **pzDataType,    /** OUTPUT: Declared data type */
    char **pzCollSeq,     /** OUTPUT: Collation sequence name */
    int *pNotNull,              /** OUTPUT: True if NOT NULL constraint exists */
    int *pPrimaryKey,           /** OUTPUT: True if column part of PK */
    int *pAutoinc               /** OUTPUT: True if column is auto-increment */
);

/**
** CAPI3REF: Load An Extension
*/
int sqlite3_load_extension(
    sqlite3 *db,          /** Load the extension into this database connection */
    const(char)*zFile,    /** Name of the shared library containing extension */
    const(char)*zProc,    /** Entry point.  Derived from zFile if 0 */
    char **pzErrMsg       /** Put error message here if not 0 */
);

/**
** CAPI3REF: Enable Or Disable Extension Loading
*/
int sqlite3_enable_load_extension(sqlite3 *db, int onoff);

/**
** CAPI3REF: Automatically Load Statically Linked Extensions
*/
int sqlite3_auto_extension(void function () xEntryPoint);

/**
** CAPI3REF: Cancel Automatic Extension Loading
*/
int sqlite3_cancel_auto_extension(void function() xEntryPoint);

/**
** CAPI3REF: Reset Automatic Extension Loading
*/
void sqlite3_reset_auto_extension();

/**
** The interface to the virtual-table mechanism is currently considered
** to be experimental.  The interface might change in incompatible ways.
** If this is a problem for you, do not use the interface at this time.
**
** When the virtual-table mechanism stabilizes, we will declare the
** interface fixed, support it indefinitely, and remove this comment.
*/

/**
** CAPI3REF: Virtual Table Object
*/

alias mapFunction = void function (sqlite3_context*,int,sqlite3_value**);

/// Ditto
struct sqlite3_module
{
    int iVersion;
    int function (sqlite3*, void *pAux,
               int argc, const char **argv,
               sqlite3_vtab **ppVTab, char**) xCreate;
    int function (sqlite3*, void *pAux,
               int argc, const char **argv,
               sqlite3_vtab **ppVTab, char**) xConnect;
    int function (sqlite3_vtab *pVTab, sqlite3_index_info*) xBestIndex;
    int function (sqlite3_vtab *pVTab) xDisconnect;
    int function (sqlite3_vtab *pVTab) xDestroy;
    int function (sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor) xOpen;
    int function (sqlite3_vtab_cursor*) xClose;
    int function (sqlite3_vtab_cursor*, int idxNum, const char *idxStr,
                int argc, sqlite3_value **argv) xFilter;
    int function (sqlite3_vtab_cursor*) xNext;
    int function (sqlite3_vtab_cursor*) xEof;
    int function (sqlite3_vtab_cursor*, sqlite3_context*, int) xColumn;
    int function (sqlite3_vtab_cursor*, sqlite3_int64 *pRowid) xRowid;
    int function (sqlite3_vtab *, int, sqlite3_value **, sqlite3_int64 *) xUpdate;
    int function (sqlite3_vtab *pVTab) xBegin;
    int function (sqlite3_vtab *pVTab) xSync;
    int function (sqlite3_vtab *pVTab) xCommit;
    int function (sqlite3_vtab *pVTab) xRollback;
    int function (sqlite3_vtab *pVtab, int nArg, const char *zName,
                       mapFunction*,
                       void **ppArg) xFindFunction;
    int function (sqlite3_vtab *pVtab, const char *zNew) xRename;
    int function (sqlite3_vtab *pVTab, int) xSavepoint;
    int function (sqlite3_vtab *pVTab, int) xRelease;
    int function (sqlite3_vtab *pVTab, int) xRollbackTo;
}

/**
** CAPI3REF: Virtual Table Indexing Information
*/
struct sqlite3_index_info
{
    struct sqlite3_index_constraint
    {
        int iColumn;            /** Column on left-hand side of constraint */
        char op;                /** Constraint operator */
        char usable;            /** True if this constraint is usable */
        int iTermOffset;        /** Used internally - xBestIndex should ignore */
    }
    struct sqlite3_index_orderby
    {
        int iColumn;            /** Column number */
        char desc;              /** True for DESC.  False for ASC. */
    }
    struct sqlite3_index_constraint_usage
    {
        int argvIndex;           /** if >0, constraint is part of argv to xFilter */
        char omit;               /** Do not code a test for this constraint */
    }
    /* Inputs */
    int nConstraint;           /** Number of entries in aConstraint */
    sqlite3_index_constraint* aConstraint;  /** Table of WHERE clause constraints */
    int nOrderBy;              /** Number of terms in the ORDER BY clause */
    sqlite3_index_orderby *aOrderBy; /** The ORDER BY clause */
    /* Outputs */
    sqlite3_index_constraint_usage *aConstraintUsage;
    int idxNum;                /** Number used to identify the index */
    char *idxStr;              /** String, possibly obtained from sqlite3_malloc */
    int needToFreeIdxStr;      /** Free idxStr using sqlite3_free() if true */
    int orderByConsumed;       /** True if output is already ordered */
    double estimatedCost;      /** Estimated cost of using this index */
    sqlite3_int64 estimatedRows;
    int idxFlags;
    sqlite3_uint64 colUsed;
}

/**
** CAPI3REF: Virtual Table Constraint Operator Codes
*/
enum
{
    SQLITE_INDEX_SCAN_UNIQUE       = 1,
    SQLITE_INDEX_CONSTRAINT_EQ     = 2,
    SQLITE_INDEX_CONSTRAINT_GT     = 4,
    SQLITE_INDEX_CONSTRAINT_LE     = 8,
    SQLITE_INDEX_CONSTRAINT_LT     = 16,
    SQLITE_INDEX_CONSTRAINT_GE     = 32,
    SQLITE_INDEX_CONSTRAINT_MATCH  = 64,
    SQLITE_INDEX_CONSTRAINT_LIKE   = 65,
    SQLITE_INDEX_CONSTRAINT_GLOB   = 66,
    SQLITE_INDEX_CONSTRAINT_REGEXP = 67,
}

/**
** CAPI3REF: Register A Virtual Table Implementation
*/
int sqlite3_create_module(
    sqlite3 *db,                    /* SQLite connection to register module with */
    const(char)*zName,              /* Name of the module */
    const(sqlite3_module)*p,        /* Methods for the module */
    void *pClientData               /* Client data for xCreate/xConnect */
);
/// Ditto
int sqlite3_create_module_v2(
    sqlite3 *db,                    /* SQLite connection to register module with */
    const(char)*zName,              /* Name of the module */
    const(sqlite3_module)*p,        /* Methods for the module */
    void *pClientData,              /* Client data for xCreate/xConnect */
    void function (void*) xDestroy  /* Module destructor function */
);

/**
** CAPI3REF: Virtual Table Instance Object
*/
struct sqlite3_vtab
{
    const(sqlite3_module)*pModule;  /** The module for this virtual table */
    int nRef;                       /** NO LONGER USED */
    char *zErrMsg;                  /** Error message from sqlite3_mprintf() */
    /* Virtual table implementations will typically add additional fields */
}

/**
** CAPI3REF: Virtual Table Cursor Object
*/
struct sqlite3_vtab_cursor
{
    sqlite3_vtab *pVtab;      /** Virtual table of this cursor */
    /* Virtual table implementations will typically add additional fields */
}

/**
** CAPI3REF: Declare The Schema Of A Virtual Table
*/
int sqlite3_declare_vtab(sqlite3*, const char *zSQL);

/**
** CAPI3REF: Overload A Function For A Virtual Table
*/
int sqlite3_overload_function(sqlite3*, const char *zFuncName, int nArg);

/**
** The interface to the virtual-table mechanism defined above (back up
** to a comment remarkably similar to this one) is currently considered
** to be experimental.  The interface might change in incompatible ways.
** If this is a problem for you, do not use the interface at this time.
**
** When the virtual-table mechanism stabilizes, we will declare the
** interface fixed, support it indefinitely, and remove this comment.
*/

/*
** CAPI3REF: A Handle To An Open BLOB
*/
struct sqlite3_blob;

/**
** CAPI3REF: Open A BLOB For Incremental I/O
*/
int sqlite3_blob_open(
    sqlite3*,
    const(char)* zDb,
    const(char)* zTable,
    const(char)* zColumn,
    sqlite3_int64 iRow,
    int flags,
    sqlite3_blob **ppBlob
);

/**
** CAPI3REF: Move a BLOB Handle to a New Row
*/
int sqlite3_blob_reopen(sqlite3_blob *, sqlite3_int64);

/**
** CAPI3REF: Close A BLOB Handle
*/
int sqlite3_blob_close(sqlite3_blob *);

/**
** CAPI3REF: Return The Size Of An Open BLOB
*/
int sqlite3_blob_bytes(sqlite3_blob *);

/**
** CAPI3REF: Read Data From A BLOB Incrementally
*/
int sqlite3_blob_read(sqlite3_blob *, void *Z, int N, int iOffset);

/**
** CAPI3REF: Write Data Into A BLOB Incrementally
*/
int sqlite3_blob_write(sqlite3_blob *, const void *z, int n, int iOffset);

/**
** CAPI3REF: Virtual File System Objects
*/
sqlite3_vfs *sqlite3_vfs_find(const char *zVfsName);
/// Ditto
int sqlite3_vfs_register(sqlite3_vfs*, int makeDflt);
/// Ditto
int sqlite3_vfs_unregister(sqlite3_vfs*);

/**
** CAPI3REF: Mutexes
*/
sqlite3_mutex *sqlite3_mutex_alloc(int);
/// Ditto
void sqlite3_mutex_free(sqlite3_mutex*);
/// Ditto
void sqlite3_mutex_enter(sqlite3_mutex*);
/// Ditto
int sqlite3_mutex_try(sqlite3_mutex*);
/// Ditto
void sqlite3_mutex_leave(sqlite3_mutex*);

/**
** CAPI3REF: Mutex Methods Object
*/
struct sqlite3_mutex_methods
{
    int  function () xMutexInit;
    int  function () xMutexEnd;
    sqlite3_mutex* function (int) xMutexAlloc;
    void  function (sqlite3_mutex *) xMutexFree;
    void  function (sqlite3_mutex *) xMutexEnter;
    int  function (sqlite3_mutex *) xMutexTry;
    void  function (sqlite3_mutex *) xMutexLeave;
    int  function (sqlite3_mutex *) xMutexHeld;
    int  function (sqlite3_mutex *) xMutexNotheld;
}

/**
** CAPI3REF: Mutex Verification Routines
*/

//#ifndef NDEBUG
int sqlite3_mutex_held(sqlite3_mutex*);
/// Ditto
int sqlite3_mutex_notheld(sqlite3_mutex*);
//#endif

/**
** CAPI3REF: Mutex Types
*/
enum
{
    SQLITE_MUTEX_FAST             = 0,
    SQLITE_MUTEX_RECURSIVE        = 1,
    SQLITE_MUTEX_STATIC_MASTER    = 2,
    SQLITE_MUTEX_STATIC_MEM       = 3,  /** sqlite3_malloc() */
    SQLITE_MUTEX_STATIC_MEM2      = 4,  /** NOT USED */
    SQLITE_MUTEX_STATIC_OPEN      = 4,  /** sqlite3BtreeOpen() */
    SQLITE_MUTEX_STATIC_PRNG      = 5,  /** sqlite3_random() */
    SQLITE_MUTEX_STATIC_LRU       = 6,  /** lru page list */
    SQLITE_MUTEX_STATIC_LRU2      = 7,  /** NOT USED */
    SQLITE_MUTEX_STATIC_PMEM      = 7,  /** sqlite3PageMalloc() */
    SQLITE_MUTEX_STATIC_APP1      = 8,  /** For use by application */
    SQLITE_MUTEX_STATIC_APP2      = 9,  /** For use by application */
    SQLITE_MUTEX_STATIC_APP3      = 10, /** For use by application */
    SQLITE_MUTEX_STATIC_VFS1      = 11, /** For use by built-in VFS */
    SQLITE_MUTEX_STATIC_VFS2      = 12, /** For use by extension VFS */
    SQLITE_MUTEX_STATIC_VFS3      = 13, /** For use by application VFS */
}

/**
** CAPI3REF: Retrieve the mutex for a database connection
*/
sqlite3_mutex *sqlite3_db_mutex(sqlite3*);

/**
** CAPI3REF: Low-Level Control Of Database Files
*/
int sqlite3_file_control(sqlite3*, const char *zDbName, int op, void*);

/**
** CAPI3REF: Testing Interface
*/
int sqlite3_test_control(int op, ...);

/**
** CAPI3REF: Testing Interface Operation Codes
*/
enum
{
    SQLITE_TESTCTRL_FIRST                   = 5,
    SQLITE_TESTCTRL_PRNG_SAVE               = 5,
    SQLITE_TESTCTRL_PRNG_RESTORE            = 6,
    SQLITE_TESTCTRL_PRNG_RESET              = 7,
    SQLITE_TESTCTRL_BITVEC_TEST             = 8,
    SQLITE_TESTCTRL_FAULT_INSTALL           = 9,
    SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS     = 10,
    SQLITE_TESTCTRL_PENDING_BYTE            = 11,
    SQLITE_TESTCTRL_ASSERT                  = 12,
    SQLITE_TESTCTRL_ALWAYS                  = 13,
    SQLITE_TESTCTRL_RESERVE                 = 14,
    SQLITE_TESTCTRL_OPTIMIZATIONS           = 15,
    SQLITE_TESTCTRL_ISKEYWORD               = 16,
    SQLITE_TESTCTRL_SCRATCHMALLOC           = 17,
    SQLITE_TESTCTRL_LOCALTIME_FAULT         = 18,
    SQLITE_TESTCTRL_EXPLAIN_STMT            = 19,
    SQLITE_TESTCTRL_NEVER_CORRUPT           = 20,
    SQLITE_TESTCTRL_VDBE_COVERAGE           = 21,
    SQLITE_TESTCTRL_BYTEORDER               = 22,
    SQLITE_TESTCTRL_ISINIT                  = 23,
    SQLITE_TESTCTRL_SORTER_MMAP             = 24,
    SQLITE_TESTCTRL_IMPOSTER                = 25,
    SQLITE_TESTCTRL_LAST                    = 25,
}

/**
** CAPI3REF: SQLite Runtime Status
*/
int sqlite3_status(int op, int *pCurrent, int *pHighwater, int resetFlag);
/// Ditto
int  sqlite3_status64(int op, long *pCurrent, long *pHighwater, int resetFlag);

/**
** CAPI3REF: Status Parameters
*/
enum
{
    SQLITE_STATUS_MEMORY_USED          = 0,
    SQLITE_STATUS_PAGECACHE_USED       = 1,
    SQLITE_STATUS_PAGECACHE_OVERFLOW   = 2,
    SQLITE_STATUS_SCRATCH_USED         = 3,
    SQLITE_STATUS_SCRATCH_OVERFLOW     = 4,
    SQLITE_STATUS_MALLOC_SIZE          = 5,
    SQLITE_STATUS_PARSER_STACK         = 6,
    SQLITE_STATUS_PAGECACHE_SIZE       = 7,
    SQLITE_STATUS_SCRATCH_SIZE         = 8,
    SQLITE_STATUS_MALLOC_COUNT         = 9
}

/**
** CAPI3REF: Database Connection Status
*/
int sqlite3_db_status(sqlite3*, int op, int *pCur, int *pHiwtr, int resetFlg);

/**
** CAPI3REF: Status Parameters for database connections
*/
enum
{
    SQLITE_DBSTATUS_LOOKASIDE_USED      = 0,
    SQLITE_DBSTATUS_CACHE_USED          = 1,
    SQLITE_DBSTATUS_SCHEMA_USED         = 2,
    SQLITE_DBSTATUS_STMT_USED           = 3,
    SQLITE_DBSTATUS_LOOKASIDE_HIT       = 4,
    SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE = 5,
    SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL = 6,
    SQLITE_DBSTATUS_CACHE_HIT           = 7,
    SQLITE_DBSTATUS_CACHE_MISS          = 8,
    SQLITE_DBSTATUS_CACHE_WRITE         = 9,
    SQLITE_DBSTATUS_DEFERRED_FKS        = 10,
    SQLITE_DBSTATUS_MAX                 = 10   /** Largest defined DBSTATUS */
}

/**
** CAPI3REF: Prepared Statement Status
*/
int sqlite3_stmt_status(sqlite3_stmt*, int op,int resetFlg);

/**
** CAPI3REF: Status Parameters for prepared statements
*/
enum
{
    SQLITE_STMTSTATUS_FULLSCAN_STEP     = 1,
    SQLITE_STMTSTATUS_SORT              = 2,
    SQLITE_STMTSTATUS_AUTOINDEX         = 3,
    SQLITE_STMTSTATUS_VM_STEP           = 4
}

/**
** CAPI3REF: Custom Page Cache Object
*/
struct sqlite3_pcache;

/**
** CAPI3REF: Custom Page Cache Object
*/
struct sqlite3_pcache_page
{
    void *pBuf;        /* The content of the page */
    void *pExtra;      /* Extra information associated with the page */
}

/**
** CAPI3REF: Application Defined Page Cache.
*/
struct sqlite3_pcache_methods2
{
    int iVersion;
    void *pArg;
    int function(void*) xInit;
    void function(void*) xShutdown;
    sqlite3_pcache * function(int szPage, int szExtra, int bPurgeable) xCreate;
    void function(sqlite3_pcache*, int nCachesize) xCachesize;
    int function(sqlite3_pcache*) xPagecount;
    sqlite3_pcache_page * function(sqlite3_pcache*, uint key, int createFlag) xFetch;
    void function(sqlite3_pcache*, sqlite3_pcache_page*, int discard) xUnpin;
    void function(sqlite3_pcache*, sqlite3_pcache_page*,
     uint oldKey, uint newKey) xRekey;
    void function(sqlite3_pcache*, uint iLimit) xTruncate;
    void function(sqlite3_pcache*) xDestroy;
    void function(sqlite3_pcache*) xShrink;
}

struct sqlite3_pcache_methods
{
    void *pArg;
    int  function (void*) xInit;
    void  function (void*) xShutdown;
    sqlite3_pcache* function (int szPage, int bPurgeable) xCreate;
    void  function (sqlite3_pcache*, int nCachesize) xCachesize;
    int  function (sqlite3_pcache*) xPagecount;
    void* function (sqlite3_pcache*, uint key, int createFlag) xFetch;
    void  function (sqlite3_pcache*, void*, int discard) xUnpin;
    void  function (sqlite3_pcache*, void*, uint oldKey, uint newKey) xRekey;
    void  function (sqlite3_pcache*, uint iLimit) xTruncate;
    void  function (sqlite3_pcache*) xDestroy;
}

/**
** CAPI3REF: Online Backup Object
*/
struct sqlite3_backup;

/**
** CAPI3REF: Online Backup API.
*/
sqlite3_backup *sqlite3_backup_init(
    sqlite3 *pDest,                        /** Destination database handle */
    const(char)*zDestName,                 /** Destination database name */
    sqlite3 *pSource,                      /** Source database handle */
    const(char)*zSourceName                /** Source database name */
);
/// Ditto
int sqlite3_backup_step(sqlite3_backup *p, int nPage);
/// Ditto
int sqlite3_backup_finish(sqlite3_backup *p);
/// Ditto
int sqlite3_backup_remaining(sqlite3_backup *p);
/// Ditto
int sqlite3_backup_pagecount(sqlite3_backup *p);

/**
** CAPI3REF: Unlock Notification
*/
int sqlite3_unlock_notify(
    sqlite3 *pBlocked,                               /** Waiting connection */
    void function (void **apArg, int nArg) xNotify,  /** Callback function to invoke */
    void *pNotifyArg                                 /** Argument to pass to xNotify */
);

/**
** CAPI3REF: String Comparison
*/
int sqlite3_stricmp(const char * , const char * );
int sqlite3_strnicmp(const char * , const char * , int);

/*
** CAPI3REF: String Globbing
*
*/
int sqlite3_strglob(const(char)* zGlob, const(char)* zStr);

/*
** CAPI3REF: String LIKE Matching
*/
int sqlite3_strlike(const(char)* zGlob, const(char)* zStr, uint cEsc);

/**
** CAPI3REF: Error Logging Interface
*/
void sqlite3_log(int iErrCode, const char *zFormat, ...);

/**
** CAPI3REF: Write-Ahead Log Commit Hook
*/
void *sqlite3_wal_hook(
    sqlite3*,
    int function (void *,sqlite3*,const char*,int),
    void*
);

/**
** CAPI3REF: Configure an auto-checkpoint
*/
int sqlite3_wal_autocheckpoint(sqlite3 *db, int N);

/**
** CAPI3REF: Checkpoint a database
*/
int sqlite3_wal_checkpoint(sqlite3 *db, const char *zDb);

/**
** CAPI3REF: Checkpoint a database
*/
int sqlite3_wal_checkpoint_v2(
    sqlite3 *db,                    /** Database handle */
    const(char)*zDb,                /** Name of attached database (or NULL) */
    int eMode,                      /** SQLITE_CHECKPOINT_* value */
    int *pnLog,                     /** OUT: Size of WAL log in frames */
    int *pnCkpt                     /** OUT: Total number of frames checkpointed */
);

/**
** CAPI3REF: Checkpoint operation parameters
*/
enum
{
    SQLITE_CHECKPOINT_PASSIVE  = 0,
    SQLITE_CHECKPOINT_FULL     = 1,
    SQLITE_CHECKPOINT_RESTART  = 2,
    SQLITE_CHECKPOINT_TRUNCATE = 3,
}

/*
** CAPI3REF: Virtual Table Interface Configuration
*/
int sqlite3_vtab_config(sqlite3*, int op, ...);

/**
** CAPI3REF: Virtual Table Configuration Options
*/
enum SQLITE_VTAB_CONSTRAINT_SUPPORT = 1;

/*
** 2010 August 30
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
*/

//#ifndef _SQLITE3RTREE_H_
//#define _SQLITE3RTREE_H_


/*
** CAPI3REF: Determine The Virtual Table Conflict Policy
*/
int sqlite3_vtab_on_conflict(sqlite3 *);

/*
** CAPI3REF: Conflict resolution modes
*/
enum
{
    SQLITE_ROLLBACK = 1,
    SQLITE_FAIL     = 3,
    SQLITE_REPLACE  = 5
}

/*
** CAPI3REF: Prepared Statement Scan Status Opcodes
*/
enum
{
    SQLITE_SCANSTAT_NLOOP    = 0,
    SQLITE_SCANSTAT_NVISIT   = 1,
    SQLITE_SCANSTAT_EST      = 2,
    SQLITE_SCANSTAT_NAME     = 3,
    SQLITE_SCANSTAT_EXPLAIN  = 4,
    SQLITE_SCANSTAT_SELECTID = 5,
}

/*
** CAPI3REF: Prepared Statement Scan Status
*/
int sqlite3_stmt_scanstatus(sqlite3_stmt *pStmt, int idx, int iScanStatusOp, void *pOut);

/*
** CAPI3REF: Zero Scan-Status Counters
*/
void sqlite3_stmt_scanstatus_reset(sqlite3_stmt *);

/*
** CAPI3REF: Flush caches to disk mid-transaction
*/
int sqlite3_db_cacheflush(sqlite3 *);

struct sqlite3_snapshot;

/*
** CAPI3REF: Record A Database Snapshot
*/
int sqlite3_snapshot_get(sqlite3 *db, char *zSchema, sqlite3_snapshot **ppSnapshot);

/*
** CAPI3REF: Start a read transaction on an historical snapshot
*/
int sqlite3_snapshot_open(sqlite3 *db, char *zSchema, sqlite3_snapshot *pSnapshot);

/*
** CAPI3REF: Destroy a snapshot
*/
void sqlite3_snapshot_free(sqlite3_snapshot *);

/**
** Register a geometry callback named zGeom that can be used as part of an
** R-Tree geometry query as follows:
**
**   SELECT ... FROM $(LT)rtree$(GT) WHERE $(LT)rtree col$(GT) MATCH $zGeom(... params ...)
*/
int sqlite3_rtree_geometry_callback(
    sqlite3 *db,
    const(char)*zGeom,
    int function (sqlite3_rtree_geometry *, int nCoord, double *aCoord, int *pRes) xGeom,
    void *pContext
);

/**
** A pointer to a structure of the following type is passed as the first
** argument to callbacks registered using rtree_geometry_callback().
*/
struct sqlite3_rtree_geometry
{
    void *pContext;                   /** Copy of pContext passed to s_r_g_c() */
    int nParam;                       /** Size of array aParam[] */
    double *aParam;                   /** Parameters passed to SQL geom function */
    void *pUser;                      /** Callback implementation user data */
    void function (void *) xDelUser;  /** Called by SQLite to clean up pUser */
}

int sqlite3_rtree_query_callback(
    sqlite3 *db,
    const(char)* zQueryFunc,
    int function(sqlite3_rtree_query_info*) xQueryFunc,
    void *pContext,
    void function(void*) xDestructor
);

struct sqlite3_rtree_query_info
{
    void *pContext;                   /* pContext from when function registered */
    int nParam;                       /* Number of function parameters */
    double*aParam;        /* value of function parameters */
    void *pUser;                      /* callback can use this, if desired */
    void function(void*) xDelUser;          /* function to free pUser */
    double*aCoord;        /* Coordinates of node or entry to check */
    uint *anQueue;            /* Number of pending entries in the queue */
    int nCoord;                       /* Number of coordinates */
    int iLevel;                       /* Level of current node or entry */
    int mxLevel;                      /* The largest iLevel value in the tree */
    sqlite3_int64 iRowid;             /* Rowid for current entry */
    double rParentScore;   /* Score of parent node */
    int eParentWithin;                /* Visibility of parent node */
    int eWithin;                      /* OUT: Visiblity */
    double rScore;         /* OUT: Write the score here */
    sqlite3_value **apSqlParam;       /* Original SQL values of parameters */
}

enum
{
    NOT_WITHIN    = 0,
    PARTLY_WITHIN = 1,
    FULLY_WITHIN  = 2
}

/******************************************************************************
** Interfaces to extend FTS5.
*/
struct Fts5Context;
/// Ditto
alias fts5_extension_function = void function(
    const Fts5ExtensionApi *pApi,
    Fts5Context *pFts,
    sqlite3_context *pCtx,
    int nVal,
    sqlite3_value **apVal
);
/// Ditto
struct Fts5PhraseIter
{
    const(ubyte) *a;
    const(ubyte) *b;
}
/// Ditto
struct Fts5ExtensionApi
{
    int iVersion;
    void* function(Fts5Context*) xUserData;
    int function(Fts5Context*) xColumnCount;
    int function(Fts5Context*, sqlite3_int64 *pnRow) xRowCount;
    int function(Fts5Context*, int iCol, sqlite3_int64 *pnToken) xColumnTotalSize;
    int function(Fts5Context*,
        const char *pText, int nText,
        void *pCtx,
        int function(void*, int, const char*, int, int, int) xToken
    ) xTokenize;
    int function(Fts5Context*) xPhraseCount;
    int function(Fts5Context*, int iPhrase) xPhraseSize;
    int function(Fts5Context*, int *pnInst) xInstCount;
    int function(Fts5Context*, int iIdx, int *piPhrase, int *piCol, int *piOff) xInst;
    sqlite3_int64 function(Fts5Context*) xRowid;
    int function(Fts5Context*, int iCol, const char **pz, int *pn) xColumnText;
    int function(Fts5Context*, int iCol, int *pnToken) xColumnSize;
    int function(Fts5Context*, int iPhrase, void *pUserData,
        int function(const Fts5ExtensionApi*,Fts5Context*,void*)
    ) xQueryPhrase;
    int function(Fts5Context*, void *pAux, void function(void*) xDelete) xSetAuxdata;
    void* function(Fts5Context*, int bClear) xGetAuxdata;
    void function(Fts5Context*, int iPhrase, Fts5PhraseIter*, int*, int*) xPhraseFirst;
    void function(Fts5Context*, Fts5PhraseIter*, int *piCol, int *piOff) xPhraseNext;
}
/// Ditto
struct Fts5Tokenizer;
struct fts5_tokenizer
{
    int function(void*, const char **azArg, int nArg, Fts5Tokenizer **ppOut) xCreate;
    void function(Fts5Tokenizer*) xDelete;
    int function(Fts5Tokenizer*,
        void *pCtx,
        int flags,
        const char *pText, int nText,
        int function(
            void *pCtx,
            int tflags,
            const char *pToken,
            int nToken,
            int iStart,
            int iEnd
        ) xToken
    ) xTokenize;
}
/// Ditto
enum FTS5_TOKENIZE_QUERY     = 0x0001;
/// Ditto
enum FTS5_TOKENIZE_PREFIX    = 0x0002;
/// Ditto
enum FTS5_TOKENIZE_DOCUMENT  = 0x0004;
/// Ditto
enum FTS5_TOKENIZE_AUX       = 0x0008;
/// Ditto
enum FTS5_TOKEN_COLOCATED    = 0x0001;
/// Ditto
struct fts5_api
{
    int iVersion;

    int function(
        fts5_api *pApi,
        const char *zName,
        void *pContext,
        fts5_tokenizer *pTokenizer,
        void function(void*) xDestroy
    ) xCreateTokenizer;

    int function(
        fts5_api *pApi,
        const char *zName,
        void **ppContext,
        fts5_tokenizer *pTokenizer
    ) xFindTokenizer;

    int function(
        fts5_api *pApi,
        const char *zName,
        void *pContext,
        fts5_extension_function xFunction,
        void function(void*) xDestroy
    ) xCreateFunction;
}
