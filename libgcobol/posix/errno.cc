#include <errno.h>

extern "C"
int
posix_errno() {
  return errno;
}
