#include <assert.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <map>

#define offsetof(TYPE, MEMBER) __builtin_offsetof (TYPE, MEMBER)

extern "C" {

#include "stat.h"

int
posix_opent(const char *pathname, int cbl_flags, int cbl_mode) {

  static const std::map<int, int> flag_bits {
    { cbl::PSX_O_RDONLY, O_RDONLY },
    { cbl::PSX_O_WRONLY, O_WRONLY },
    { cbl::PSX_O_RDWR, O_RDWR }, 
    { cbl::PSX_O_CREAT, O_CREAT },
    { cbl::PSX_O_EXCL, O_EXCL }, 
    { cbl::PSX_O_NOCTTY, O_NOCTTY }, 
    { cbl::PSX_O_TRUNC, O_TRUNC },
    { cbl::PSX_O_APPEND, O_APPEND }, 
    { cbl::PSX_O_NONBLOCK, O_NONBLOCK },
    { cbl::PSX_O_DSYNC, O_DSYNC },
    { cbl::PSX_O_DIRECT, O_DIRECT }, 
    { cbl::PSX_O_LARGEFILE, O_LARGEFILE }, 
    { cbl::PSX_O_DIRECTORY, O_DIRECTORY }, 
    { cbl::PSX_O_NOFOLLOW, O_NOFOLLOW }, 
    { cbl::PSX_O_NOATIME, O_NOATIME }, 
    { cbl::PSX_O_CLOEXEC, O_CLOEXEC }, 
    { cbl::PSX_O_SYNC, O_SYNC }, 
    { cbl::PSX_O_PATH, O_PATH }, 
    { cbl::PSX_O_TMPFILE, O_TMPFILE }, 
  };    

  static const std::map<int, int> mode_bits {
    { cbl::PSX_S_IXOTH, S_IXOTH }, 
    { cbl::PSX_S_IWOTH, S_IWOTH }, 
    { cbl::PSX_S_IROTH, S_IROTH }, 
    { cbl::PSX_S_IRWXO, S_IRWXO }, 
    { cbl::PSX_S_IXGRP, S_IXGRP }, 
    { cbl::PSX_S_IWGRP, S_IWGRP }, 
    { cbl::PSX_S_IRGRP, S_IRGRP }, 
    { cbl::PSX_S_IRWXG, S_IRWXG }, 
    { cbl::PSX_S_IXUSR, S_IXUSR }, 
    { cbl::PSX_S_IWUSR, S_IWUSR }, 
    { cbl::PSX_S_IRUSR, S_IRUSR }, 
    { cbl::PSX_S_IRWXU, S_IRWXU }, 
    { cbl::PSX_S_ISVTX, S_ISVTX }, 
    { cbl::PSX_S_ISGID, S_ISGID }, 
    { cbl::PSX_S_ISUID, S_ISUID }, 
  };
  
  int flags = 0;
  mode_t mode = 0;
  
  for( auto elem : flag_bits ) {
    int cbl_bit = elem.first;
    int std_bit = elem.second;
    
    if( cbl_bit == (cbl_bit & cbl_flags) ) {
      flags |= std_bit;
    }
  }

  for( auto elem : mode_bits ) {
    int cbl_bit = elem.first;
    int std_bit = elem.second;
    
    if( cbl_bit == (cbl_bit & cbl_mode) ) {
      mode |= std_bit;
    }
  }

  int erc = open(pathname, flags, mode);

  return erc;
}

} // extern "C"
