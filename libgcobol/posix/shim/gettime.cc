#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

#include <sys/types.h>


#define offsetof(TYPE, MEMBER) __builtin_offsetof (TYPE, MEMBER)

extern "C" {

#include "timespec.h"

#define offset_assert(name, offset) do {                                \
  if( offsetof(posix_timespec_t, name) != offset ) {                        \
    fprintf(stderr, "C posix_timespec_t offset for %s %zu != COBOL offset %d\n", \
            #name, offsetof(posix_timespec_t, name), offset);               \
    assert(offsetof(posix_timespec_t, name) == offset);                     \
  }                                                                     \
 } while(false);

int
posix_clock_gettime(int64_t clk_id, posix_timespec_t *tp, size_t size) {
  struct timespec sb;
  int erc = clock_gettime(clk_id, &sb);

  if( sizeof(timespec) != size ) {
    fprintf(stderr, "%s:%d: %lu != received size %lu\n", __func__, __LINE__,
            (unsigned long)sizeof(struct timespec),
            (unsigned long)size);
    fflush(stdout);
    fflush(stderr);
  }
  if( tp == nullptr ) {
    fprintf(stderr, "%s:%d: received NULL statbuf\n", __func__, __LINE__);
    fflush(stdout);
    fflush(stderr);
  }

  if( true ) { // Verify last known reported COBOL offsets agree with C offsets.
    offset_assert( tv_sec,         0 );
    offset_assert( tv_nsec       ,  8 );
  }

  assert(tp);

  tp->tv_sec = sb.tv_sec;
  tp->tv_nsec = sb.tv_nsec;

  return erc;
}

} // extern "C"
