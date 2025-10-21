#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

extern "C" {

#include "stat.h"

int
posix_stat(const char *pathname, struct posix_stat_t *statbuf, size_t size) {
  struct stat sb;
  int erc = stat(pathname, &sb);

  if( sizeof(struct posix_stat_t) != size ) {
    fprintf(stderr, "posix_stat %lu != received size %lu\n",
            (unsigned long)sizeof(struct posix_stat_t), 
            (unsigned long)size);
  }

  assert(sizeof(struct posix_stat_t) == size);
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
    statbuf->st_atim = sb.st_atim.tv_sec;
    statbuf->st_mtim = sb.st_mtim.tv_sec;
    statbuf->st_ctim = sb.st_ctim.tv_sec;
  }

  if( 0 ) {
    printf("%4lu: st_dev: %lu = %lu\n",
           (unsigned long)offsetof(struct posix_stat_t, st_dev),
           (unsigned long)statbuf->st_dev, (unsigned long)sb.st_dev);
    printf("%4lu: st_ino: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_ino),
           (unsigned long)statbuf->st_ino, (unsigned long)sb.st_ino);
    printf("%4lu: st_mode: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_mode),
           (unsigned long)statbuf->st_mode, (unsigned long)sb.st_mode);
    printf("%4lu: st_nlink: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_nlink),
           (unsigned long)statbuf->st_nlink, (unsigned long)sb.st_nlink);
    printf("%4lu: st_uid: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_uid),
           (unsigned long)statbuf->st_uid, (unsigned long)sb.st_uid);
    printf("%4lu: st_gid: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_gid),
           (unsigned long)statbuf->st_gid, (unsigned long)sb.st_gid);
    printf("%4lu: st_rdev: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_rdev),
           (unsigned long)statbuf->st_rdev, (unsigned long)sb.st_rdev);
    printf("%4lu: st_size: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_size),
           (unsigned long)statbuf->st_size, (unsigned long)sb.st_size);
    printf("%4lu: st_blksize: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_blksize),
           (unsigned long)statbuf->st_blksize, (unsigned long)sb.st_blksize);
    printf("%4lu: st_blocks: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_blocks),
           (unsigned long)statbuf->st_blocks, (unsigned long)sb.st_blocks);
    printf("%4lu: st_atim: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_atim),
           (unsigned long)statbuf->st_atim, (unsigned long)sb.st_atim.tv_sec);
    printf("%4lu: st_mtim: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_mtim),
           (unsigned long)statbuf->st_mtim, (unsigned long)sb.st_mtim.tv_sec);
    printf("%4lu: st_ctim: %lu = %lu\n", 
           (unsigned long)offsetof(struct posix_stat_t, st_ctim),
           (unsigned long)statbuf->st_ctim, (unsigned long)sb.st_ctim.tv_sec);
  }

  return erc;

  
}

} // extern "C"
