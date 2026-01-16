#include <sys/types.h>
#include <unistd.h>

#include <cassert>
#include <map>

#define offsetof(TYPE, MEMBER) __builtin_offsetof (TYPE, MEMBER)

extern "C" {

off_t 
posix_lseek(int fd, off_t offset, int whence) {

  static const std::map<int, int> whences {
    { 2, SEEK_SET },
    { 4, SEEK_CUR },
    { 8, SEEK_END },
  };    

  /* 
   * Map valid input whence value onto C standard library value. 
   * Invalid values are passed through and rejected by lseek(2) per its documentation. 
   * (The caller always needs to check for errors anyway.)
   */
  auto p = whences.find(whence);
  if( p != whences.end() ) whence = p.second;

  return lseek(fd, offset, whence);
}

} // extern "C"
