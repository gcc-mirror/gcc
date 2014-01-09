#ifndef LINUX_TYPES_WRAPPER_H
#define LINUX_TYPES_WRAPPER_H

/* Before
   https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/commit/include/linux/types.h?id=6c7c6afbb8c0e60d32a563cae7c6889211e9d9d8
   linux/types.h conflicted with sys/ustat.h.  Work around it.  */

#define ustat __asan_bad_ustat
#include_next <linux/types.h>
#undef ustat

#endif
