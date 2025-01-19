/**
 * D header file for Linux.
 */
module core.sys.linux.sys.mount;

version (linux):

public import core.sys.linux.fs :
    BLKROSET, BLKROGET, BLKRRPART, BLKGETSIZE, BLKFLSBUF, BLKRASET, BLKRAGET,
    BLKFRASET, BLKFRAGET, BLKSECTSET, BLKSECTGET, BLKSSZGET, BLKBSZGET,
    BLKBSZSET, BLKGETSIZE64;

import core.stdc.config : c_ulong;

extern (C):
nothrow:
@nogc:

enum c_ulong MS_RDONLY       = 1;
enum c_ulong MS_NOSUID       = 2;
enum c_ulong MS_NODEV        = 4;
enum c_ulong MS_NOEXEC       = 8;
enum c_ulong MS_SYNCHRONOUS  = 16;
enum c_ulong MS_REMOUNT      = 32;
enum c_ulong MS_MANDLOCK     = 64;
enum c_ulong MS_DIRSYNC      = 128;
enum c_ulong MS_NOSYMFOLLOW  = 256;
enum c_ulong MS_NOATIME      = 1024;
enum c_ulong MS_NODIRATIME   = 2048;
enum c_ulong MS_BIND         = 4096;
enum c_ulong MS_MOVE         = 8192;
enum c_ulong MS_REC          = 16384;
enum c_ulong MS_SILENT       = 32768;
enum c_ulong MS_POSIXACL     = 1 << 16;
enum c_ulong MS_UNBINDABLE   = 1 << 17;
enum c_ulong MS_PRIVATE      = 1 << 18;
enum c_ulong MS_SLAVE        = 1 << 19;
enum c_ulong MS_SHARED       = 1 << 20;
enum c_ulong MS_RELATIME     = 1 << 21;
enum c_ulong MS_KERNMOUNT    = 1 << 22;
enum c_ulong MS_I_VERSION    = 1 << 23;
enum c_ulong MS_STRICTATIME  = 1 << 24;
enum c_ulong MS_LAZYTIME     = 1 << 25;
enum c_ulong MS_NOREMOTELOCK = 1 << 27;
enum c_ulong MS_NOSEC        = 1 << 28;
enum c_ulong MS_BORN         = 1 << 29;
enum c_ulong MS_ACTIVE       = 1 << 30;
enum c_ulong MS_NOUSER       = 1 << 31;

enum MS_RMT_MASK =
    MS_RDONLY | MS_SYNCHRONOUS | MS_MANDLOCK | MS_I_VERSION | MS_LAZYTIME;

enum MS_MGC_VAL = 0xC0ED0000;
enum MS_MGC_MSK = 0xFFFF0000;

enum MNT_FORCE       = 1;
enum MNT_DETACH      = 2;
enum MNT_EXPIRE      = 4;
enum UMOUNT_NOFOLLOW = 8;

int mount(const(char)*, const(char)*, const(char)*, c_ulong, const(void)*);
int umount(const(char)*);
int umount2(const(char)*, int);
