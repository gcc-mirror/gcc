//Written in the D programming language

/++
    D header file for FreeBSD's sys/mount.h.

    Copyright: Copyright 2018 -
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP jmdavisprog.com, Jonathan M Davis)
 +/
module core.sys.freebsd.sys.mount;

version (FreeBSD):

import core.sys.freebsd.config;
import core.stdc.config : c_long;
import core.sys.posix.sys.stat : stat_t;
import core.sys.posix.sys.types : uid_t;

extern(C) @nogc nothrow:

struct fsid_t
{
    int[2] val;
}

enum MAXFIDSZ = 16;

struct fid
{
    ushort         fid_len;
    ushort         fid_data0;
    char[MAXFIDSZ] fid_data = 0;
}

enum MFSNAMELEN = 16;

static if (__FreeBSD_version >= 1200000)
{
    enum MNAMELEN   = 1024;
    enum STATFS_VERSION = 0x20140518;
}
else
{
    enum MNAMELEN   = 88;
    enum STATFS_VERSION = 0x20030518;
}

struct statfs_t
{
    uint f_version;
    uint f_type;
    ulong f_flags;
    ulong f_bsize;
    ulong f_iosize;
    ulong f_blocks;
    ulong f_bfree;
    long  f_bavail;
    ulong f_files;
    long  f_ffree;
    ulong f_syncwrites;
    ulong f_asyncwrites;
    ulong f_syncreads;
    ulong f_asyncreads;
    ulong[10] f_spare;
    uint f_namemax;
    uid_t f_owner;
    fsid_t f_fsid;
    char[80] f_charspare = 0;
    char[MFSNAMELEN] f_fstypename = 0;
    char[MNAMELEN] f_mntfromname = 0;
    char[MNAMELEN] f_mntonname = 0;
}


enum ulong MNT_RDONLY = 0x0000000000000001;
enum ulong MNT_SYNCHRONOUS = 0x0000000000000002;
enum ulong MNT_NOEXEC = 0x0000000000000004;
enum ulong MNT_NOSUID = 0x0000000000000008;
enum ulong MNT_NFS4ACLS = 0x0000000000000010;
enum ulong MNT_UNION = 0x0000000000000020;
enum ulong MNT_ASYNC = 0x0000000000000040;
enum ulong MNT_SUIDDIR = 0x0000000000100000;
enum ulong MNT_SOFTDEP = 0x0000000000200000;
enum ulong MNT_NOSYMFOLLOW = 0x0000000000400000;
enum ulong MNT_GJOURNAL = 0x0000000002000000;
enum ulong MNT_MULTILABEL = 0x0000000004000000;
enum ulong MNT_ACLS = 0x0000000008000000;
enum ulong MNT_NOATIME = 0x0000000010000000;
enum ulong MNT_NOCLUSTERR = 0x0000000040000000;
enum ulong MNT_NOCLUSTERW = 0x0000000080000000;
enum ulong MNT_SUJ = 0x0000000100000000;
enum ulong MNT_AUTOMOUNTED = 0x0000000200000000;

enum ulong MNT_EXRDONLY = 0x0000000000000080;
enum ulong MNT_EXPORTED = 0x0000000000000100;
enum ulong MNT_DEFEXPORTED = 0x0000000000000200;
enum ulong MNT_EXPORTANON = 0x0000000000000400;
enum ulong MNT_EXKERB = 0x0000000000000800;
enum ulong MNT_EXPUBLIC = 0x0000000020000000;

enum ulong MNT_LOCAL  = 0x0000000000001000;
enum ulong MNT_QUOTA  = 0x0000000000002000;
enum ulong MNT_ROOTFS = 0x0000000000004000;
enum ulong MNT_USER   = 0x0000000000008000;
enum ulong MNT_IGNORE = 0x0000000000800000;

enum MNT_VISFLAGMASK = MNT_RDONLY | MNT_SYNCHRONOUS | MNT_NOEXEC |
                       MNT_NOSUID | MNT_UNION | MNT_SUJ |
                       MNT_ASYNC | MNT_EXRDONLY  | MNT_EXPORTED |
                       MNT_DEFEXPORTED | MNT_EXPORTANON| MNT_EXKERB |
                       MNT_LOCAL | MNT_USER  | MNT_QUOTA |
                       MNT_ROOTFS | MNT_NOATIME   | MNT_NOCLUSTERR |
                       MNT_NOCLUSTERW | MNT_SUIDDIR | MNT_SOFTDEP |
                       MNT_IGNORE | MNT_EXPUBLIC  | MNT_NOSYMFOLLOW |
                       MNT_GJOURNAL | MNT_MULTILABEL | MNT_ACLS |
                       MNT_NFS4ACLS | MNT_AUTOMOUNTED;

enum MNT_UPDATEMASK = MNT_NOSUID | MNT_NOEXEC |
                      MNT_SYNCHRONOUS | MNT_UNION | MNT_ASYNC |
                      MNT_NOATIME |
                      MNT_NOSYMFOLLOW | MNT_IGNORE |
                      MNT_NOCLUSTERR | MNT_NOCLUSTERW | MNT_SUIDDIR |
                      MNT_ACLS | MNT_USER | MNT_NFS4ACLS  |
                      MNT_AUTOMOUNTED;

enum ulong MNT_UPDATE = 0x0000000000010000;
enum ulong MNT_DELEXPORT = 0x0000000000020000;
enum ulong MNT_RELOAD = 0x0000000000040000;
enum ulong MNT_FORCE = 0x0000000000080000;
enum ulong MNT_SNAPSHOT = 0x0000000001000000;
enum ulong MNT_NONBUSY = 0x0000000004000000;
enum ulong MNT_BYFSID = 0x0000000008000000;
enum ulong MNT_CMDFLAGS = MNT_UPDATE | MNT_DELEXPORT | MNT_RELOAD |
                          MNT_FORCE  | MNT_SNAPSHOT  | MNT_NONBUSY |
                          MNT_BYFSID;

enum uint MNTK_UNMOUNTF = 0x00000001;
enum uint MNTK_ASYNC = 0x00000002;
enum uint MNTK_SOFTDEP = 0x00000004;
enum uint MNTK_NOINSMNTQ = 0x00000008;
enum uint MNTK_DRAINING = 0x00000010;
enum uint MNTK_REFEXPIRE = 0x00000020;
enum uint MNTK_EXTENDED_SHARED = 0x00000040;
enum uint MNTK_SHARED_WRITES = 0x00000080;
enum uint MNTK_NO_IOPF = 0x00000100;
enum uint MNTK_VGONE_UPPER = 0x00000200;
enum uint MNTK_VGONE_WAITER = 0x00000400;
enum uint MNTK_LOOKUP_EXCL_DOTDOT = 0x00000800;
enum uint MNTK_MARKER = 0x00001000;
enum uint MNTK_UNMAPPED_BUFS = 0x00002000;
enum uint MNTK_USES_BCACHE = 0x00004000;
enum uint MNTK_NOASYNC = 0x00800000;
enum uint MNTK_UNMOUNT = 0x01000000;
enum uint MNTK_MWAIT = 0x02000000;
enum uint MNTK_SUSPEND = 0x08000000;
enum uint MNTK_SUSPEND2 = 0x04000000;
enum uint MNTK_SUSPENDED = 0x10000000;
enum uint MNTK_NULL_NOCACHE = 0x20000000;
enum uint MNTK_LOOKUP_SHARED = 0x40000000;
enum uint MNTK_NOKNOTE = 0x80000000;

enum VFS_VFSCONF = 0;
enum VFS_GENERIC = 0;

enum VFS_MAXTYPENUM = 1;
enum VFS_CONF       = 2;

enum MNT_WAIT    = 1;
enum MNT_NOWAIT  = 2;
enum MNT_LAZY    = 3;
enum MNT_SUSPEND = 4;

struct fhandle_t
{
    fsid_t fh_fsid;
    fid fh_fid;
}

// TODO - requires declarations from elsewhere that we don't currently have bindings for.
/+
struct oexport_args
{
    int ex_flags;
    uid_t ex_root;
    xucred ex_anon;
    sockaddr* ex_addr;
    ubyte ex_addrlen;
    sockaddr* ex_mask;
    ubyte ex_masklen;
    char* ex_indexfile;
}

enum MAXSECFLAVORS = 5;

struct export_args
{
    int ex_flags;
    uid_t ex_root;
    xucred ex_anon;
    sockaddr* ex_addr;
    ubyte ex_addrlen;
    sockaddr* ex_mask;
    ubyte ex_masklen;
    char* ex_indexfile;
    int ex_numsecflavors;
    int[MAXSECFLAVORS] ex_secflavors;
}

struct nfs_public
{
    int np_valid;
    fhandle_t np_handle;
    mount_t* np_mount;
    char* np_index;
}

struct vfsconf
{
    uint vfc_version;
    char[MFSNAMELEN] vfc_name = 0;
    vfsops* vfc_vfsops;
    int vfc_typenum;
    int vfc_refcount;
    int vfc_flags;
    vfsoptdecl* vfc_opts;
    TAILQ_ENTRY(vfsconf) vfc_list;
}

struct xvfsconf
{
    vfsops* vfc_vfsops;
    char[MFSNAMELEN] vfc_name = 0;
    int vfc_typenum;
    int vfc_refcount;
    int vfc_flags;
    vfsconf* vfc_next;
}
+/

struct ovfsconf
{
    void* vfc_vfsops;
    char[32] vfc_name = 0;
    int vfc_index;
    int vfc_refcount;
    int vfc_flags;
}

enum uint VFCF_STATIC = 0x00010000;
enum uint VFCF_NETWORK = 0x00020000;
enum uint VFCF_READONLY = 0x00040000;
enum uint VFCF_SYNTHETIC = 0x00080000;
enum uint VFCF_LOOPBACK = 0x00100000;
enum uint VFCF_UNICODE = 0x00200000;
enum uint VFCF_JAIL = 0x00400000;
enum uint VFCF_DELEGADMIN = 0x00800000;
enum uint VFCF_SBDRY = 0x01000000;

alias fsctlop_t = uint;

struct vfsidctl
{
    int vc_vers;
    fsid_t vc_fsid;
    char[MFSNAMELEN] vc_fstypename = 0;
    fsctlop_t vc_op;
    void* vc_ptr;
    size_t vc_len;
    uint[12] vc_spare;
}

enum uint VFS_CTL_VERS1 = 0x01;

enum uint VFS_CTL_QUERY   = 0x00010001;
enum uint VFS_CTL_TIMEO   = 0x00010002;
enum uint VFS_CTL_NOLOCKS = 0x00010003;

struct vfsquery
{
    uint vq_flags;
    uint[31] vq_spare;
}

enum uint VQ_NOTRESP  = 0x0001;
enum uint VQ_NEEDAUTH = 0x0002;
enum uint VQ_LOWDISK  = 0x0004;
enum uint VQ_MOUNT    = 0x0008;
enum uint VQ_UNMOUNT  = 0x0010;
enum uint VQ_DEAD     = 0x0020;
enum uint VQ_ASSIST   = 0x0040;
enum uint VQ_NOTRESPLOCK = 0x0080;
enum uint VQ_FLAG0100 = 0x0100;
enum uint VQ_FLAG0200 = 0x0200;
enum uint VQ_FLAG0400 = 0x0400;
enum uint VQ_FLAG0800 = 0x0800;
enum uint VQ_FLAG1000 = 0x1000;
enum uint VQ_FLAG2000 = 0x2000;
enum uint VQ_FLAG4000 = 0x4000;
enum uint VQ_FLAG8000 = 0x8000;

version (GNU)
{
    int fhopen(const fhandle_t*, int);
    int fhstat(const fhandle_t*, stat_t*);
    int fhstatfs(const fhandle_t*, statfs_t*);
    int fstatfs(int, statfs_t*);
    int getfh(const char*, fhandle_t*);
    int getfsstat(statfs_t*, c_long, int);
    int getmntinfo(statfs_t**, int);
    int lgetfh(const char*, fhandle_t*);
    int mount(const char*, const char*, int, void*);
    //int nmount(iovec*, uint, int);
    int statfs(const char*, statfs_t*);
    int unmount(const char*, int);
    //int getvfsbyname(const char*, xvfsconf*);
}
else
{
    static if (__FreeBSD_version >= 1200000)
    {
        pragma(mangle, "fhstat@FBSD_1.5")     int fhstat(const fhandle_t*, stat_t*);
        pragma(mangle, "fhstatfs@FBSD_1.5")   int fhstatfs(const fhandle_t*, statfs_t*);
        pragma(mangle, "fstatfs@FBSD_1.5")    int fstatfs(int, statfs_t*);
        pragma(mangle, "getfsstat@FBSD_1.5")  int getfsstat(statfs_t*, c_long, int);
        pragma(mangle, "getmntinfo@FBSD_1.5") int getmntinfo(statfs_t**, int);
        pragma(mangle, "statfs@FBSD_1.5")     int statfs(const char*, statfs_t*);
    }
    else
    {
        pragma(mangle, "fhstat@FBSD_1.0")     int fhstat(const fhandle_t*, stat_t*);
        pragma(mangle, "fhstatfs@FBSD_1.0")   int fhstatfs(const fhandle_t*, statfs_t*);
        pragma(mangle, "fstatfs@FBSD_1.0")    int fstatfs(int, statfs_t*);
        pragma(mangle, "getfsstat@FBSD_1.0")  int getfsstat(statfs_t*, c_long, int);
        pragma(mangle, "getmntinfo@FBSD_1.0") int getmntinfo(statfs_t**, int);
        pragma(mangle, "statfs@FBSD_1.0")     int statfs(const char*, statfs_t*);
    }
    pragma(mangle, "fhopen@@FBSD_1.0")        int fhopen(const fhandle_t*, int);
    pragma(mangle, "getfh@@FBSD_1.0")         int getfh(const char*, fhandle_t*);
    pragma(mangle, "lgetfh@@FBSD_1.0")        int lgetfh(const char*, fhandle_t*);
    pragma(mangle, "mount@@FBSD_1.0")         int mount(const char*, const char*, int, void*);
    //int nmount(iovec*, uint, int);
    pragma(mangle, "unmount@@FBSD_1.0")       int unmount(const char*, int);
    //int getvfsbyname(const char*, xvfsconf*);
}
