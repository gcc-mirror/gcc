#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#define offsetof(TYPE, MEMBER) __builtin_offsetof (TYPE, MEMBER)

extern "C" {

#include "stat.h"

#define offset_assert(name, offset) do {                                \
  if( offsetof(posix_stat_t, name) != offset ) {                        \
    fprintf(stderr, "C posix_stat_t offset for %s %zu != COBOL offset %d\n", \
            #name, offsetof(posix_stat_t, name), offset);               \
    assert(offsetof(posix_stat_t, name) == offset);                     \
  }                                                                     \
 } while(false);

int
posix_stat(const char *pathname, posix_stat_t *statbuf, size_t size) {
  struct stat sb;
  int erc = stat(pathname, &sb);

  if( sizeof(posix_stat_t) != size ) {
    fprintf(stderr, "%s:%d: %lu != received size %lu\n", __func__, __LINE__, 
            (unsigned long)sizeof(struct posix_stat_t), 
            (unsigned long)size);
    fflush(stdout);
    fflush(stderr);
  }
  if( statbuf == nullptr ) {
    fprintf(stderr, "%s:%d: received NULL statbuf\n", __func__, __LINE__);
    fflush(stdout);
    fflush(stderr);
  }

  if( true ) { // Verify last known reported COBOL offsets agree with C offsets.
    offset_assert( st_dev,         0 ); 
    offset_assert( st_ino       ,  8 ); 
    offset_assert( st_mode      , 16 ); 
    offset_assert( st_nlink     , 24 ); 
    offset_assert( st_uid       , 32 ); 
    offset_assert( st_gid       , 40 ); 
    offset_assert( st_rdev      , 48 ); 
    offset_assert( st_size      , 56 ); 
    offset_assert( st_blksize   , 64 ); 
    offset_assert( st_blocks    , 72 ); 
    offset_assert( psx_atime    , 80 ); 
    offset_assert( psx_mtime    , 88 ); 
    offset_assert( psx_ctime    , 96 ); 
  }

  assert(statbuf);

  if( erc == 0 ) {
    statbuf->st_dev = sb.st_dev;
    statbuf->st_ino = sb.st_ino;
    statbuf->st_mode = sb.st_mode;
    statbuf->st_nlink = sb.st_nlink;
    statbuf->st_uid = sb.st_uid;
    statbuf->st_gid = sb.st_gid;
    statbuf->st_rdev = sb.st_rdev;
    statbuf->st_size = sb.st_size;
    statbuf->st_blksize = sb.st_blksize;
    statbuf->st_blocks = sb.st_blocks;
    statbuf->psx_atime = sb.st_atime;
    statbuf->psx_mtime = sb.st_mtime;
    statbuf->psx_ctime = sb.st_ctime;
  }

  return erc;

  
}

} // extern "C"
