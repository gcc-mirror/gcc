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
