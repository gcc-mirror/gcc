/**
 * D header file for Darwin
 *
 * $(LINK2 https://opensource.apple.com/source/xnu/xnu-2422.115.4/bsd/sys/attr.h.auto.html, Apple sys/attr.h)
 */
module core.sys.darwin.sys.attr;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
extern (C):
nothrow:
@nogc:

import core.internal.attributes : betterC;
import core.sys.darwin.sys.cdefs : c_ulong;
import core.sys.posix.sys.time : timeval;

// These functions aren't actually declared in attr.h but in unistd.h.
@system
{
    void getattrlist(scope const char* path, scope attrlist* attrList, scope void* attrBuf,
        size_t attrBufSize, c_ulong options);

    void setattrlist(scope const char* path, scope attrlist* attrList, scope void* attrBuf,
        size_t attrBufSize, c_ulong options);

    version (TVOS) {}
    else version (WatchOS) {}
    else
        int searchfs(scope const char* path, scope fssearchblock* searchBlock,
            scope c_ulong* numMatches, uint scriptCode, uint options, scope searchstate* state);
}

enum
{
    FSOPT_NOFOLLOW                     = 0x00000001,
    FSOPT_NOINMEMUPDATE                = 0x00000002,
    FSOPT_REPORT_FULLSIZE              = 0x00000004,
    FSOPT_PACK_INVAL_ATTRS             = 0x00000008,
    FSOPT_ATTR_CMN_EXTENDED            = 0x00000020, // macOS 10.10
}

enum SEARCHFS_MAX_SEARCHPARMS = 4096;

alias uint text_encoding_t, fsobj_type_t, fsobj_tag_t, fsfile_type_t, fsvolid_t, attrgroup_t;

struct attrlist
{
    ushort bitmapcount, reserved;
    attrgroup_t commonattr, volattr, dirattr, fileattr, forkattr;
}
enum ATTR_BIT_MAP_COUNT = 5;

struct attribute_set_t
{
    attrgroup_t commonattr, volattr, dirattr, fileattr, forkattr;
}

struct attrreference_t
{
    int     attr_dataoffset;
    uint    attr_length;
}

struct diskextent
{
    uint startblock, blockcount;
}

alias extentrecord = diskextent[8];

alias vol_capabilities_set_t = uint[4];

enum
{
    VOL_CAPABILITIES_FORMAT            = 0,
    VOL_CAPABILITIES_INTERFACES        = 1,
    VOL_CAPABILITIES_RESERVED1         = 2,
    VOL_CAPABILITIES_RESERVED2         = 3,
}

struct vol_capabilities_attr_t
{
    vol_capabilities_set_t capabilities, valid;
}

enum ATTR_MAX_BUFFER = 8192;

enum
{
    VOL_CAP_FMT_PERSISTENTOBJECTIDS    = 0x00000001,
    VOL_CAP_FMT_SYMBOLICLINKS          = 0x00000002,
    VOL_CAP_FMT_HARDLINKS              = 0x00000004,
    VOL_CAP_FMT_JOURNAL                = 0x00000008,
    VOL_CAP_FMT_JOURNAL_ACTIVE         = 0x00000010,
    VOL_CAP_FMT_NO_ROOT_TIMES          = 0x00000020,
    VOL_CAP_FMT_SPARSE_FILES           = 0x00000040,
    VOL_CAP_FMT_ZERO_RUNS              = 0x00000080,
    VOL_CAP_FMT_CASE_SENSITIVE         = 0x00000100,
    VOL_CAP_FMT_CASE_PRESERVING        = 0x00000200,
    VOL_CAP_FMT_FAST_STATFS            = 0x00000400,
    VOL_CAP_FMT_2TB_FILESIZE           = 0x00000800,
    VOL_CAP_FMT_OPENDENYMODES          = 0x00001000,
    VOL_CAP_FMT_HIDDEN_FILES           = 0x00002000,
    VOL_CAP_FMT_PATH_FROM_ID           = 0x00004000,
    VOL_CAP_FMT_NO_VOLUME_SIZES        = 0x00008000,
    VOL_CAP_FMT_DECMPFS_COMPRESSION    = 0x00010000,
    VOL_CAP_FMT_64BIT_OBJECT_IDS       = 0x00020000,
    VOL_CAP_FMT_DIR_HARDLINKS          = 0x00040000, // macOS 10.12
    VOL_CAP_FMT_DOCUMENT_ID            = 0x00080000, // macOS 10.12
    VOL_CAP_FMT_WRITE_GENERATION_COUNT = 0x00100000, // macOS 10.12
    VOL_CAP_FMT_NO_IMMUTABLE_FILES     = 0x00200000, // macOS 10.12.4
    VOL_CAP_FMT_NO_PERMISSIONS         = 0x00400000, // macOS 10.12.4
    VOL_CAP_FMT_SHARED_SPACE           = 0x00800000, // macOS 10.15
    VOL_CAP_FMT_VOL_GROUPS             = 0x01000000, // macOS 10.15
}

enum
{
    VOL_CAP_INT_SEARCHFS               = 0x00000001,
    VOL_CAP_INT_ATTRLIST               = 0x00000002,
    VOL_CAP_INT_NFSEXPORT              = 0x00000004,
    VOL_CAP_INT_READDIRATTR            = 0x00000008,
    VOL_CAP_INT_EXCHANGEDATA           = 0x00000010,
    VOL_CAP_INT_COPYFILE               = 0x00000020,
    VOL_CAP_INT_ALLOCATE               = 0x00000040,
    VOL_CAP_INT_VOL_RENAME             = 0x00000080,
    VOL_CAP_INT_ADVLOCK                = 0x00000100,
    VOL_CAP_INT_FLOCK                  = 0x00000200,
    VOL_CAP_INT_EXTENDED_SECURITY      = 0x00000400,
    VOL_CAP_INT_USERACCESS             = 0x00000800,
    VOL_CAP_INT_MANLOCK                = 0x00001000,
    VOL_CAP_INT_NAMEDSTREAMS           = 0x00002000,
    VOL_CAP_INT_EXTENDED_ATTR          = 0x00004000,
    VOL_CAP_INT_CLONE                  = 0x00010000, // macOS 10.12
    VOL_CAP_INT_SNAPSHOT               = 0x00020000, // macOS 10.12
    VOL_CAP_INT_RENAME_SWAP            = 0x00040000, // macOS 10.12
    VOL_CAP_INT_RENAME_EXCL            = 0x00080000, // macOS 10.12
    VOL_CAP_INT_RENAME_OPENFAIL        = 0x00100000, // macOS 10.15
}

struct vol_attributes_attr_t
{
    attribute_set_t validattr, nativeattr;
}

enum
{
    ATTR_CMN_NAME                      = 0x00000001,
    ATTR_CMN_DEVID                     = 0x00000002,
    ATTR_CMN_FSID                      = 0x00000004,
    ATTR_CMN_OBJTYPE                   = 0x00000008,
    ATTR_CMN_OBJTAG                    = 0x00000010,
    ATTR_CMN_OBJID                     = 0x00000020,
    ATTR_CMN_OBJPERMANENTID            = 0x00000040,
    ATTR_CMN_PAROBJID                  = 0x00000080,
    ATTR_CMN_SCRIPT                    = 0x00000100,
    ATTR_CMN_CRTIME                    = 0x00000200,
    ATTR_CMN_MODTIME                   = 0x00000400,
    ATTR_CMN_CHGTIME                   = 0x00000800,
    ATTR_CMN_ACCTIME                   = 0x00001000,
    ATTR_CMN_BKUPTIME                  = 0x00002000,
    ATTR_CMN_FNDRINFO                  = 0x00004000,
    ATTR_CMN_OWNERID                   = 0x00008000,
    ATTR_CMN_GRPID                     = 0x00010000,
    ATTR_CMN_ACCESSMASK                = 0x00020000,
    ATTR_CMN_FLAGS                     = 0x00040000,

    ATTR_CMN_GEN_COUNT                 = 0x00080000,
    ATTR_CMN_DOCUMENT_ID               = 0x00100000,

    ATTR_CMN_USERACCESS                = 0x00200000,
    ATTR_CMN_EXTENDED_SECURITY         = 0x00400000,
    ATTR_CMN_UUID                      = 0x00800000,
    ATTR_CMN_GRPUUID                   = 0x01000000,
    ATTR_CMN_FILEID                    = 0x02000000,
    ATTR_CMN_PARENTID                  = 0x04000000,
    ATTR_CMN_FULLPATH                  = 0x08000000,
    ATTR_CMN_ADDEDTIME                 = 0x10000000,
    ATTR_CMN_ERROR                     = 0x20000000, // macOS 10.10
    ATTR_CMN_DATA_PROTECT_FLAGS        = 0x40000000, // macOS 10.10
}

enum ATTR_CMN_RETURNED_ATTRS           = 0x80000000;
enum ATTR_CMN_VALIDMASK                = 0xFFFFFFFF;
enum ATTR_CMN_SETMASK                  = 0x51C7FF00;
enum ATTR_CMN_VOLSETMASK               = 0x00006700;

enum
{
    ATTR_VOL_FSTYPE                    = 0x00000001,
    ATTR_VOL_SIGNATURE                 = 0x00000002,
    ATTR_VOL_SIZE                      = 0x00000004,
    ATTR_VOL_SPACEFREE                 = 0x00000008,
    ATTR_VOL_SPACEAVAIL                = 0x00000010,
    ATTR_VOL_MINALLOCATION             = 0x00000020,
    ATTR_VOL_ALLOCATIONCLUMP           = 0x00000040,
    ATTR_VOL_IOBLOCKSIZE               = 0x00000080,
    ATTR_VOL_OBJCOUNT                  = 0x00000100,
    ATTR_VOL_FILECOUNT                 = 0x00000200,
    ATTR_VOL_DIRCOUNT                  = 0x00000400,
    ATTR_VOL_MAXOBJCOUNT               = 0x00000800,
    ATTR_VOL_MOUNTPOINT                = 0x00001000,
    ATTR_VOL_NAME                      = 0x00002000,
    ATTR_VOL_MOUNTFLAGS                = 0x00004000,
    ATTR_VOL_MOUNTEDDEVICE             = 0x00008000,
    ATTR_VOL_ENCODINGSUSED             = 0x00010000,
    ATTR_VOL_CAPABILITIES              = 0x00020000,
    ATTR_VOL_UUID                      = 0x00040000,
    ATTR_VOL_QUOTA_SIZE                = 0x10000000, // macOS 10.12.4
    ATTR_VOL_RESERVED_SIZE             = 0x20000000, // macOS 10.12.4
    ATTR_VOL_ATTRIBUTES                = 0x40000000,
    ATTR_VOL_INFO                      = 0x80000000,
}

enum ATTR_VOL_VALIDMASK                = 0xF007FFFF;
enum ATTR_VOL_SETMASK                  = 0x80002000;

enum
{
    ATTR_DIR_LINKCOUNT                 = 0x00000001,
    ATTR_DIR_ENTRYCOUNT                = 0x00000002,
    ATTR_DIR_MOUNTSTATUS               = 0x00000004,
    ATTR_DIR_ALLOCSIZE                 = 0x00000008, // macOS 10.12.4
    ATTR_DIR_IOBLOCKSIZE               = 0x00000010, // macOS 10.12.4
    ATTR_DIR_DATALENGTH                = 0x00000020, // macOS 10.12.4
}

enum
{
    DIR_MNTSTATUS_MNTPOINT             = 0x00000001,
    DIR_MNTSTATUS_TRIGGER              = 0x00000002,
}

enum ATTR_DIR_VALIDMASK                = 0x0000003f;
enum ATTR_DIR_SETMASK                  = 0x00000000;

enum
{
    ATTR_FILE_LINKCOUNT                = 0x00000001,
    ATTR_FILE_TOTALSIZE                = 0x00000002,
    ATTR_FILE_ALLOCSIZE                = 0x00000004,
    ATTR_FILE_IOBLOCKSIZE              = 0x00000008,
    ATTR_FILE_DEVTYPE                  = 0x00000020,
    ATTR_FILE_FORKCOUNT                = 0x00000080,
    ATTR_FILE_FORKLIST                 = 0x00000100,
    ATTR_FILE_DATALENGTH               = 0x00000200,
    ATTR_FILE_DATAALLOCSIZE            = 0x00000400,
    ATTR_FILE_RSRCLENGTH               = 0x00001000,
    ATTR_FILE_RSRCALLOCSIZE            = 0x00002000,
}

enum ATTR_FILE_VALIDMASK               = 0x000037FF;
enum ATTR_FILE_SETMASK                 = 0x00000020;

enum
{
    ATTR_CMNEXT_RELPATH                = 0x00000004, // macOS 10.12.4
    ATTR_CMNEXT_PRIVATESIZE            = 0x00000008, // macOS 10.12.4
    ATTR_CMNEXT_NOFIRMLINKPATH         = 0x00000020, // macOS 10.15
    ATTR_CMNEXT_REALDEVID              = 0x00000040, // macOS 10.15
    ATTR_CMNEXT_REALFSID               = 0x00000080, // macOS 10.15
}

enum ATTR_CMNEXT_VALIDMASK             = 0x000000fc;
enum ATTR_CMNEXT_SETMASK               = 0x00000000;

enum ATTR_BULK_REQUIRED = ATTR_CMN_NAME | ATTR_CMN_RETURNED_ATTRS;

enum
{
    SRCHFS_START                       = 0x00000001,
    SRCHFS_MATCHPARTIALNAMES           = 0x00000002,
    SRCHFS_MATCHDIRS                   = 0x00000004,
    SRCHFS_MATCHFILES                  = 0x00000008,
    SRCHFS_SKIPLINKS                   = 0x00000010,
    SRCHFS_SKIPINVISIBLE               = 0x00000020,
    SRCHFS_SKIPPACKAGES                = 0x00000040,
    SRCHFS_SKIPINAPPROPRIATE           = 0x00000080,

    SRCHFS_NEGATEPARAMS                = 0x80000000,
    SRCHFS_VALIDOPTIONSMASK            = 0x800000FF,
}

struct fssearchblock
{
    attrlist*        returnattrs;
    void*            returnbuffer;
    size_t           returnbuffersize;
    c_ulong          maxmatches;
    timeval          timelimit;
    void*            searchparams1;
    size_t           sizeofsearchparams1;
    void*            searchparams2;
    size_t           sizeofsearchparams2;
    attrlist         searchattrs;
}

struct searchstate
{
    uint                        ss_union_flags;
    uint                        ss_union_layer;
    ubyte[548]                  ss_fsstate;
}
static assert(searchstate.sizeof == uint.sizeof * 2 + searchstate.ss_fsstate.sizeof,
    "searchstate struct must be packed");

enum FST_EOF = -1;

@betterC @nogc nothrow pure @safe unittest
{
    // Use an enum instead of `version (Darwin)` so it works with the betterc test extractor.
    version (OSX) enum isDarwin = true;
    else version (iOS) enum isDarwin = true;
    else version (TVOS) enum isDarwin = true;
    else version (WatchOS) enum isDarwin = true;
    else enum isDarwin = false;
    static if (isDarwin)
    {
        // Verify that these types don't need __initZ and so can be used in betterC.
        attrlist al;
        attribute_set_t as;
        attrreference_t ar;
        diskextent de;
        vol_capabilities_attr_t vca;
        vol_attributes_attr_t vaa;
        fssearchblock fsb;
        searchstate ss;
    }
}
