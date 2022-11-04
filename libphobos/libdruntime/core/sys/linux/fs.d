/**
 * D header file for the linux/fs.h interface.
 *
 * This file has definitions for some important file table structures
 * and constants and structures used by various generic file system
 * ioctl's.
 *
 * Copyright: The D Language Foundation 2021.
 * License : $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors : Luís Ferreira
 */
module core.sys.linux.fs;

version (linux):

public import core.sys.posix.sys.ioctl;

import core.stdc.config : c_ulong, c_long;

extern (C):
@nogc:
nothrow:

enum INR_OPEN_CUR = 1024; /// Initial setting for nfile rlimits
enum INR_OPEN_MAX = 4096; /// Hard limit for nfile rlimits

enum BLOCK_SIZE_BITS = 10; ///
enum BLOCK_SIZE = 1 << BLOCK_SIZE_BITS; ///

enum
{
    SEEK_SET = 0, /// seek relative to beginning of file
    SEEK_CUR = 1, /// seek relative to current file position
    SEEK_END = 2, /// seek relative to end of file
    SEEK_DATA = 3, /// seek to the next data
    SEEK_HOLE = 4, /// seek to the next hole
    SEEK_MAX = SEEK_HOLE, ///
}

enum
{
    RENAME_NOREPLACE = 1 << 0, /// Don't overwrite target
    RENAME_EXCHANGE = 1 << 1, /// Exchange source and dest
    RENAME_WHITEOUT = 1 << 2, /// Whiteout source
}

struct file_clone_range
{
    long src_fd;
    ulong src_offset;
    ulong src_length;
    ulong dest_offset;
}

struct fstrim_range
{
    ulong start;
    ulong len;
    ulong minlen;
}

/**
 * extent-same (dedupe) ioctls; these MUST match the btrfs ioctl definitions
 */
enum
{
    FILE_DEDUPE_RANGE_SAME = 0,
    FILE_DEDUPE_RANGE_DIFFERS = 1,
}

/**
 * from struct btrfs_ioctl_file_extent_same_info
 */
struct file_dedupe_range_info
{
    long dest_fd; /// in - destination file
    ulong dest_offset; /// in - start of extent in destination
    ulong bytes_deduped; /// out - total # of bytes we were able to dedupe from this file.
    /** status of this dedupe operation:
     * < 0 for error
     * == FILE_DEDUPE_RANGE_SAME if dedupe succeeds
     * == FILE_DEDUPE_RANGE_DIFFERS if data differs
     */
    int status;
    uint reserved; /// must be zero
}

/**
 * from struct btrfs_ioctl_file_extent_same_args
 */
struct file_dedupe_range
{
    ulong src_offset; /// in - start of extent in source
    ulong src_length; /// in - length of extent
    ushort dest_count; /// in - total elements in info array
    ushort reserved1; /// must be zero
    uint reserved2; /// must be zero
    file_dedupe_range_info[0] info;
}

/**
 * And dynamically-tunable limits and defaults:
 */
struct files_stat_struct
{
    c_ulong nr_files; /// read only
    c_ulong nr_free_files; /// read only
    c_ulong max_files; /// tunable
}

struct inodes_stat_t
{
    c_long nr_inodes;
    c_long nr_unused;
    c_long[5] dummy; /// padding for sysctl ABI compatibility
}

enum NR_FILE = 8192;

/**
 * Structure for FS_IOC_FSGETXATTR[A] and FS_IOC_FSSETXATTR.
 */
struct fsxattr
{
    uint fsx_xflags;
    uint fsx_extsize;
    uint fsx_nextents;
    uint fsx_projid; /// project identifier
    uint fsx_cowextsize; /// CoW extsize
    ubyte[8] fsx_pad;
}

/*
 * Flags for the fsx_xflags field
 */
enum {
    S_XFLAG_REALTIME = 0x00000001, /// data in realtime volume
    S_XFLAG_PREALLOC = 0x00000002, /// preallocated file extents
    S_XFLAG_IMMUTABLE = 0x00000008, /// file cannot be modified
    S_XFLAG_APPEND = 0x00000010, /// all writes append
    S_XFLAG_SYNC = 0x00000020, /// all writes synchronous
    S_XFLAG_NOATIME = 0x00000040, /// do not update access time
    S_XFLAG_NODUMP = 0x00000080, /// do not include in backups
    S_XFLAG_RTINHERIT = 0x00000100, /// create with rt bit set
    S_XFLAG_PROJINHERIT = 0x00000200, /// create with parents projid
    S_XFLAG_NOSYMLINKS = 0x00000400, /// disallow symlink creation
    S_XFLAG_EXTSIZE = 0x00000800, /// extent size allocator hint
    S_XFLAG_EXTSZINHERIT = 0x00001000, /// inherit inode extent size
    S_XFLAG_NODEFRAG = 0x00002000, /// do not defragment
    S_XFLAG_FILESTREAM = 0x00004000, /// use filestream allocator
    S_XFLAG_DAX = 0x00008000, /// use DAX for IO
    S_XFLAG_COWEXTSIZE = 0x00010000, /// CoW extent size allocator hint
    S_XFLAG_HASATTR = 0x80000000, /// no DIFLAG for this
}

static if (__traits(compiles, _IO(1, 2)))
{
    enum BLKROSET = _IO(0x12, 93); /// set device read-only
    enum BLKROGET = _IO(0x12, 94); /// get read-only status
    enum BLKRRPART = _IO(0x12, 95); /// re-read partition table
    enum BLKGETSIZE = _IO(0x12, 96); /// return device size
    enum BLKFLSBUF = _IO(0x12, 97); /// flush buffer cache
    enum BLKRASET = _IO(0x12, 98); /// set read ahead for block device
    enum BLKRAGET = _IO(0x12, 99); /// get current read ahead setting
    enum BLKFRASET = _IO(0x12, 100); /// set filesystem
    enum BLKFRAGET = _IO(0x12, 101); /// get filesystem
    enum BLKSECTSET = _IO(0x12, 102); /// set max sectors per request
    enum BLKSECTGET = _IO(0x12, 103); /// get max sectors per request
    enum BLKSSZGET = _IO(0x12, 104); /// get block device sector size


    enum BLKBSZGET = _IOR!size_t(0x12, 112);
    enum BLKBSZSET = _IOW!size_t(0x12, 113);
    enum BLKGETSIZE64 = _IOR!size_t(0x12, 114);
    enum BLKTRACESTART = _IO(0x12, 116);
    enum BLKTRACESTOP = _IO(0x12, 117);
    enum BLKTRACETEARDOWN = _IO(0x12, 118);
    enum BLKDISCARD = _IO(0x12, 119);
    enum BLKIOMIN = _IO(0x12, 120);
    enum BLKIOOPT = _IO(0x12, 121);
    enum BLKALIGNOFF = _IO(0x12, 122);
    enum BLKPBSZGET = _IO(0x12, 123);
    enum BLKDISCARDZEROES = _IO(0x12, 124);
    enum BLKSECDISCARD = _IO(0x12, 125);
    enum BLKROTATIONAL = _IO(0x12, 126);
    enum BLKZEROOUT = _IO(0x12, 127);

    enum BMAP_IOCTL = 1; /// obsolete - kept for compatibility
    enum FIBMAP = _IO(0x00, 1); /// bmap access
    enum FIGETBSZ = _IO(0x00, 2); /// get the block size used for bmap
}

enum FSLABEL_MAX = 256; /// Max chars for the interface; each fs may differ

/**
 * Inode flags (FS_IOC_GETFLAGS / FS_IOC_SETFLAGS)
 *
 * Note: for historical reasons, these flags were originally used and
 * defined for use by ext2/ext3, and then other file systems started
 * using these flags so they wouldn't need to write their own version
 * of chattr/lsattr (which was shipped as part of e2fsprogs).  You
 * should think twice before trying to use these flags in new
 * contexts, or trying to assign these flags, since they are used both
 * as the UAPI and the on-disk encoding for ext2/3/4.  Also, we are
 * almost out of 32-bit flags.  :-)
 *
 * We have recently hoisted FS_IOC_FSGETXATTR / FS_IOC_FSSETXATTR from
 * XFS to the generic FS level interface.  This uses a structure that
 * has padding and hence has more room to grow, so it may be more
 * appropriate for many new use cases.
 */
enum {
    FS_SECRM_FL = 0x00000001, /// Secure deletion
    FS_UNRM_FL = 0x00000002, /// Undelete
    FS_COMPR_FL = 0x00000004, /// Compress file
    FS_SYNC_FL = 0x00000008, /// Synchronous updates
    FS_IMMUTABLE_FL = 0x00000010, /// Immutable file
    FS_APPEND_FL = 0x00000020, /// writes to file may only append
    FS_NODUMP_FL = 0x00000040, /// do not dump file
    FS_NOATIME_FL = 0x00000080, /// do not update atime
    FS_DIRTY_FL = 0x00000100, /// Reserved for compression usage
    FS_COMPRBLK_FL = 0x00000200, /// One or more compressed clusters
    FS_NOCOMP_FL = 0x00000400, /// Don't compress
    FS_ENCRYPT_FL = 0x00000800, /// Encrypted file
    FS_BTREE_FL = 0x00001000, /// btree format dir
    FS_INDEX_FL = 0x00001000, /// hash-indexed directory
    FS_IMAGIC_FL = 0x00002000, /// AFS directory
    FS_JOURNAL_DATA_FL = 0x00004000, /// Reserved for ext3
    FS_NOTAIL_FL = 0x00008000, /// file tail should not be merged
    FS_DIRSYNC_FL = 0x00010000, /// dirsync behaviour (directories only)
    FS_TOPDIR_FL = 0x00020000, /// Top of directory hierarchie
    FS_HUGE_FILE_FL = 0x00040000, /// Reserved for ext4
    FS_EXTENT_FL = 0x00080000, /// Extents
    FS_VERITY_FL = 0x00100000, /// Verity protected inode
    FS_EA_INODE_FL = 0x00200000, /// Inode used for large EA
    FS_EOFBLOCKS_FL = 0x00400000, /// Reserved for ext4
    FS_NOCOW_FL = 0x00800000, /// Do not cow file
    FS_DAX_FL = 0x02000000, /// Inode is DAX
    FS_INLINE_DATA_FL = 0x10000000, /// Reserved for ext4
    FS_PROJINHERIT_FL = 0x20000000, /// Create with parents projid
    FS_CASEFOLD_FL = 0x40000000, /// Folder is case insensitive
    FS_RESERVED_FL = 0x80000000, /// reserved for ext2 lib
}

enum FS_FL_USER_VISIBLE = 0x0003DFFF; /// User visible flags
enum FS_FL_USER_MODIFIABLE = 0x000380FF; /// User modifiable flags

enum SYNC_FILE_RANGE_WAIT_BEFORE = 1;
enum SYNC_FILE_RANGE_WRITE = 2;
enum SYNC_FILE_RANGE_WAIT_AFTER = 4;
enum SYNC_FILE_RANGE_WRITE_AND_WAIT = SYNC_FILE_RANGE_WRITE | SYNC_FILE_RANGE_WAIT_BEFORE | SYNC_FILE_RANGE_WAIT_AFTER;

alias __kernel_rwf_t = int;

/**
 * Flags for preadv2/pwritev2:
 */
enum : __kernel_rwf_t {
    RWF_HIPRI = 0x00000001, /// high priority request, poll if possible
    RWF_DSYNC = 0x00000002, /// per-IO O_DSYNC
    RWF_SYNC = 0x00000004, /// per-IO O_SYNC
    RWF_NOWAIT = 0x00000008, /// per-IO, return -EAGAIN if operation would block
    RWF_APPEND = 0x00000010, /// per-IO O_APPEND
}

/// mask of flags supported by the kernel
enum RWF_SUPPORTED = RWF_HIPRI | RWF_DSYNC | RWF_SYNC | RWF_NOWAIT | RWF_APPEND;
