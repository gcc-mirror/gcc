#include <errno.h>

extern "C"
int
posix_errno(int *newval) {
  int ret = errno;
  if (newval)
    errno = *newval;
  return ret;
}
