#include <cstdint>

/*
 * This buffer definition matches the one in libgcobol/posix/cpy/statbuf.cpy. 
 * It is shared between 
 * 
 *        libgcobol/posix/udf/posix-stat.cbl
 * and 
 *        libgcobol/posix/shim/stat.cc
 * 
 * stat.cc copies information from the OS-defined stat buffer to this one. 
 */

namespace cbl {
  typedef uint64_t blkcnt_t;
  typedef uint64_t blksize_t;
  typedef uint64_t dev_t;
  typedef uint64_t gid_t;
  typedef uint64_t ino_t;
  typedef uint64_t mode_t;
  typedef uint64_t nlink_t;
  typedef uint64_t off_t;
  typedef uint64_t time_t;
  typedef uint64_t uid_t;
};

struct posix_stat_t {
  cbl::dev_t     st_dev;         /* ID of device containing file */
  cbl::ino_t     st_ino;         /* Inode number */
  cbl::mode_t    st_mode;        /* File type and mode */
  cbl::nlink_t   st_nlink;       /* Number of hard links */
  cbl::uid_t     st_uid;         /* User ID of owner */
  cbl::gid_t     st_gid;         /* Group ID of owner */
  cbl::dev_t     st_rdev;        /* Device ID (if special file) */
  cbl::off_t     st_size;        /* Total size, in bytes */
  cbl::blksize_t st_blksize;     /* Block size for filesystem I/O */
  cbl::blkcnt_t  st_blocks;      /* Number of 512B blocks allocated */
  // Cannot use st_atime etc because they are defined in the preprocessor. 
  cbl::time_t    psx_atime;      /* Time of last access */
  cbl::time_t    psx_mtime;      /* Time of last modification */
  cbl::time_t    psx_ctime;      /* Time of last status change */
};

namespace cbl {

  enum flags_t {
    PSX_O_RDONLY    =        0,
    PSX_O_WRONLY    =        1,
    PSX_O_RDWR      =        2,
    PSX_O_CREAT     =       64,
    PSX_O_EXCL      =      128,
    PSX_O_NOCTTY    =      256,
    PSX_O_TRUNC     =      512,
    PSX_O_APPEND    =     1024,
    PSX_O_NONBLOCK  =     2048,
    PSX_O_DSYNC     =     4096,
    PSX_O_DIRECT    =    16384 ,
    PSX_O_LARGEFILE =    32768,
    PSX_O_DIRECTORY =    65536,
    PSX_O_NOFOLLOW  =   131072,
    PSX_O_NOATIME   =   262144,
    PSX_O_CLOEXEC   =   524288,
    PSX_O_SYNC      =  1052672,
    PSX_O_PATH      =  2097152,
    PSX_O_TMPFILE   =  4194304 + PSX_O_DIRECTORY,
  };

  enum open_mode_t {
    PSX_S_IXOTH =    1,
    PSX_S_IWOTH =    2,
    PSX_S_IROTH =    4,
    PSX_S_IRWXO =    7,
    PSX_S_IXGRP =    8,
    PSX_S_IWGRP =   16,
    PSX_S_IRGRP =   32,
    PSX_S_IRWXG =   56,
    PSX_S_IXUSR =   64,
    PSX_S_IWUSR =  128,
    PSX_S_IRUSR =  256,
    PSX_S_IRWXU =  448,
    PSX_S_ISVTX =  512,
    PSX_S_ISGID = 1024,
    PSX_S_ISUID = 2048,
  };

};
