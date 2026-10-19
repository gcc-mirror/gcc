#include <cstdint>

namespace cbl {
  typedef int64_t time_t;
};

struct posix_timespec_t {
  cbl::time_t   tv_sec;         /* ID of device containing file */
  uint64_t     tv_nsec;         /* Inode number */
};