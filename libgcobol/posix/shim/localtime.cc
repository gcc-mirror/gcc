#include <assert.h>
#include <time.h>

extern "C" {

#include "tm.h"

static struct posix_tm posix_tm;


struct posix_tm *
posix_localtime(const time_t *timep, size_t size) {

  assert(timep);
  assert(sizeof(posix_tm) == size);

  struct tm * tm = localtime(timep);

  if( tm == NULL ) return NULL;

  posix_tm.tm_sec = tm->tm_sec;
  posix_tm.tm_min = tm->tm_min;
  posix_tm.tm_hour = tm->tm_hour;
  posix_tm.tm_mday = tm->tm_mday;
  posix_tm.tm_mon = tm->tm_mon;
  posix_tm.tm_year = tm->tm_year;
  posix_tm.tm_wday = tm->tm_wday;
  posix_tm.tm_yday = tm->tm_yday;
  posix_tm.tm_isdst = tm->tm_isdst;

  return &posix_tm;
}

} // extern "C"
