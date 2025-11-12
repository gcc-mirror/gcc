#include <fcntl.h>           /* Definition of AT_* constants */
#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/stat.h>
#include <sys/types.h>

int
main(int argc, char *argv[])
{
  printf( "size of dev_t is %zu\n", sizeof(dev_t));
  printf( "size of ino_t is %zu\n", sizeof(ino_t));
  printf( "size of mode_t is %zu\n", sizeof(mode_t));
  printf( "size of nlink_t is %zu\n", sizeof(nlink_t));
  printf( "size of uid_t is %zu\n", sizeof(uid_t));
  printf( "size of gid_t is %zu\n", sizeof(gid_t));
  printf( "size of dev_t is %zu\n", sizeof(dev_t));
  printf( "size of off_t is %zu\n", sizeof(off_t));
  printf( "size of blksize_t is %zu\n", sizeof(blksize_t));
  printf( "size of blkcnt_t is %zu\n", sizeof(blkcnt_t));
  printf( "size of time_t is %zu\n", sizeof(time_t));
  printf( "size of struct timespec is %zu\n", sizeof(struct timespec));

  return 0;
}
