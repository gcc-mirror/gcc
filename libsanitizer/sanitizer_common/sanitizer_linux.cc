//===-- sanitizer_linux.cc ------------------------------------------------===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is shared between AddressSanitizer and ThreadSanitizer
// run-time libraries and implements linux-specific functions from
// sanitizer_libc.h.
//===----------------------------------------------------------------------===//

#include "sanitizer_platform.h"

#if SANITIZER_FREEBSD || SANITIZER_LINUX || SANITIZER_NETBSD

#include "sanitizer_common.h"
#include "sanitizer_flags.h"
#include "sanitizer_internal_defs.h"
#include "sanitizer_libc.h"
#include "sanitizer_linux.h"
#include "sanitizer_mutex.h"
#include "sanitizer_placement_new.h"
#include "sanitizer_procmaps.h"
#include "sanitizer_stacktrace.h"
#include "sanitizer_symbolizer.h"

#if SANITIZER_LINUX
#include <asm/param.h>
#endif

#if SANITIZER_NETBSD
#include <lwp.h>
#endif

// For mips64, syscall(__NR_stat) fills the buffer in the 'struct kernel_stat'
// format. Struct kernel_stat is defined as 'struct stat' in asm/stat.h. To
// access stat from asm/stat.h, without conflicting with definition in
// sys/stat.h, we use this trick.
#if defined(__mips64)
#include <asm/unistd.h>
#include <sys/types.h>
#define stat kernel_stat
#include <asm/stat.h>
#undef stat
#endif

#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <link.h>
#include <pthread.h>
#include <sched.h>
#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/types.h>
#include <ucontext.h>
#include <unistd.h>

#if SANITIZER_LINUX
#include <sys/utsname.h>
#endif

#if SANITIZER_LINUX && !SANITIZER_ANDROID
#include <sys/personality.h>
#endif

#if SANITIZER_FREEBSD
#include <sys/exec.h>
#include <sys/sysctl.h>
#include <machine/atomic.h>
extern "C" {
// <sys/umtx.h> must be included after <errno.h> and <sys/types.h> on
// FreeBSD 9.2 and 10.0.
#include <sys/umtx.h>
}
extern char **environ;  // provided by crt1
#endif  // SANITIZER_FREEBSD

#if SANITIZER_NETBSD
#include <limits.h>  // For NAME_MAX
#include <sys/sysctl.h>
extern char **environ;  // provided by crt1
#endif                  // SANITIZER_NETBSD

#if !SANITIZER_ANDROID
#include <sys/signal.h>
#endif

#ifndef __GLIBC_PREREQ
#define __GLIBC_PREREQ(x, y) 0
#endif

#if SANITIZER_LINUX && __GLIBC_PREREQ(2, 16)
# define SANITIZER_USE_GETAUXVAL 1
#else
# define SANITIZER_USE_GETAUXVAL 0
#endif

#if SANITIZER_USE_GETAUXVAL
#include <sys/auxv.h>
#endif

#if SANITIZER_LINUX
// <linux/time.h>
struct kernel_timeval {
  long tv_sec;
  long tv_usec;
};

// <linux/futex.h> is broken on some linux distributions.
const int FUTEX_WAIT = 0;
const int FUTEX_WAKE = 1;
#endif  // SANITIZER_LINUX

// Are we using 32-bit or 64-bit Linux syscalls?
// x32 (which defines __x86_64__) has SANITIZER_WORDSIZE == 32
// but it still needs to use 64-bit syscalls.
#if SANITIZER_LINUX && (defined(__x86_64__) || defined(__powerpc64__) || \
    SANITIZER_WORDSIZE == 64)
# define SANITIZER_LINUX_USES_64BIT_SYSCALLS 1
#else
# define SANITIZER_LINUX_USES_64BIT_SYSCALLS 0
#endif

#if defined(__x86_64__) || SANITIZER_MIPS64
extern "C" {
extern void internal_sigreturn();
}
#endif

#if SANITIZER_LINUX && defined(__NR_getrandom)
# if !defined(GRND_NONBLOCK)
#  define GRND_NONBLOCK 1
# endif
# define SANITIZER_USE_GETRANDOM 1
#else
# define SANITIZER_USE_GETRANDOM 0
#endif  // SANITIZER_LINUX && defined(__NR_getrandom)

namespace __sanitizer {

#if SANITIZER_LINUX && defined(__x86_64__)
#include "sanitizer_syscall_linux_x86_64.inc"
#elif SANITIZER_LINUX && defined(__aarch64__)
#include "sanitizer_syscall_linux_aarch64.inc"
#else
#include "sanitizer_syscall_generic.inc"
#endif

// --------------- sanitizer_libc.h
#if !SANITIZER_S390
uptr internal_mmap(void *addr, uptr length, int prot, int flags, int fd,
                   OFF_T offset) {
#if SANITIZER_NETBSD
  return internal_syscall_ptr(SYSCALL(mmap), addr, length, prot, flags, fd,
                              (long)0, offset);
#elif SANITIZER_FREEBSD || SANITIZER_LINUX_USES_64BIT_SYSCALLS
  return internal_syscall(SYSCALL(mmap), (uptr)addr, length, prot, flags, fd,
                          offset);
#else
  // mmap2 specifies file offset in 4096-byte units.
  CHECK(IsAligned(offset, 4096));
  return internal_syscall(SYSCALL(mmap2), addr, length, prot, flags, fd,
                          offset / 4096);
#endif
}
#endif // !SANITIZER_S390

uptr internal_munmap(void *addr, uptr length) {
  return internal_syscall(SYSCALL(munmap), (uptr)addr, length);
}

int internal_mprotect(void *addr, uptr length, int prot) {
  return internal_syscall(SYSCALL(mprotect), (uptr)addr, length, prot);
}

uptr internal_close(fd_t fd) {
  return internal_syscall(SYSCALL(close), fd);
}

uptr internal_open(const char *filename, int flags) {
#if SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(openat), AT_FDCWD, (uptr)filename, flags);
#else
  return internal_syscall(SYSCALL(open), (uptr)filename, flags);
#endif
}

uptr internal_open(const char *filename, int flags, u32 mode) {
#if SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(openat), AT_FDCWD, (uptr)filename, flags,
                          mode);
#else
  return internal_syscall(SYSCALL(open), (uptr)filename, flags, mode);
#endif
}

uptr internal_read(fd_t fd, void *buf, uptr count) {
  sptr res;
#if SANITIZER_NETBSD
  HANDLE_EINTR(res, internal_syscall_ptr(SYSCALL(read), fd, buf, count));
#else
  HANDLE_EINTR(res, (sptr)internal_syscall(SYSCALL(read), fd, (uptr)buf,
               count));
#endif
  return res;
}

uptr internal_write(fd_t fd, const void *buf, uptr count) {
  sptr res;
#if SANITIZER_NETBSD
  HANDLE_EINTR(res, internal_syscall_ptr(SYSCALL(write), fd, buf, count));
#else
  HANDLE_EINTR(res, (sptr)internal_syscall(SYSCALL(write), fd, (uptr)buf,
               count));
#endif
  return res;
}

uptr internal_ftruncate(fd_t fd, uptr size) {
  sptr res;
#if SANITIZER_NETBSD
  HANDLE_EINTR(res, internal_syscall(SYSCALL(ftruncate), fd, 0, (s64)size));
#else
  HANDLE_EINTR(res, (sptr)internal_syscall(SYSCALL(ftruncate), fd,
               (OFF_T)size));
#endif
  return res;
}

#if !SANITIZER_LINUX_USES_64BIT_SYSCALLS && SANITIZER_LINUX
static void stat64_to_stat(struct stat64 *in, struct stat *out) {
  internal_memset(out, 0, sizeof(*out));
  out->st_dev = in->st_dev;
  out->st_ino = in->st_ino;
  out->st_mode = in->st_mode;
  out->st_nlink = in->st_nlink;
  out->st_uid = in->st_uid;
  out->st_gid = in->st_gid;
  out->st_rdev = in->st_rdev;
  out->st_size = in->st_size;
  out->st_blksize = in->st_blksize;
  out->st_blocks = in->st_blocks;
  out->st_atime = in->st_atime;
  out->st_mtime = in->st_mtime;
  out->st_ctime = in->st_ctime;
}
#endif

#if defined(__mips64)
// Undefine compatibility macros from <sys/stat.h>
// so that they would not clash with the kernel_stat
// st_[a|m|c]time fields
#undef st_atime
#undef st_mtime
#undef st_ctime
#if defined(SANITIZER_ANDROID)
// Bionic sys/stat.h defines additional macros
// for compatibility with the old NDKs and
// they clash with the kernel_stat structure
// st_[a|m|c]time_nsec fields.
#undef st_atime_nsec
#undef st_mtime_nsec
#undef st_ctime_nsec
#endif
static void kernel_stat_to_stat(struct kernel_stat *in, struct stat *out) {
  internal_memset(out, 0, sizeof(*out));
  out->st_dev = in->st_dev;
  out->st_ino = in->st_ino;
  out->st_mode = in->st_mode;
  out->st_nlink = in->st_nlink;
  out->st_uid = in->st_uid;
  out->st_gid = in->st_gid;
  out->st_rdev = in->st_rdev;
  out->st_size = in->st_size;
  out->st_blksize = in->st_blksize;
  out->st_blocks = in->st_blocks;
#if defined(__USE_MISC)     || \
    defined(__USE_XOPEN2K8) || \
    defined(SANITIZER_ANDROID)
  out->st_atim.tv_sec = in->st_atime;
  out->st_atim.tv_nsec = in->st_atime_nsec;
  out->st_mtim.tv_sec = in->st_mtime;
  out->st_mtim.tv_nsec = in->st_mtime_nsec;
  out->st_ctim.tv_sec = in->st_ctime;
  out->st_ctim.tv_nsec = in->st_ctime_nsec;
#else
  out->st_atime = in->st_atime;
  out->st_atimensec = in->st_atime_nsec;
  out->st_mtime = in->st_mtime;
  out->st_mtimensec = in->st_mtime_nsec;
  out->st_ctime = in->st_ctime;
  out->st_atimensec = in->st_ctime_nsec;
#endif
}
#endif

uptr internal_stat(const char *path, void *buf) {
#if SANITIZER_FREEBSD || SANITIZER_NETBSD
  return internal_syscall(SYSCALL(fstatat), AT_FDCWD, (uptr)path,
                          (uptr)buf, 0);
#elif SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(newfstatat), AT_FDCWD, (uptr)path,
                          (uptr)buf, 0);
#elif SANITIZER_LINUX_USES_64BIT_SYSCALLS
# if defined(__mips64)
  // For mips64, stat syscall fills buffer in the format of kernel_stat
  struct kernel_stat kbuf;
  int res = internal_syscall(SYSCALL(stat), path, &kbuf);
  kernel_stat_to_stat(&kbuf, (struct stat *)buf);
  return res;
# else
  return internal_syscall(SYSCALL(stat), (uptr)path, (uptr)buf);
# endif
#else
  struct stat64 buf64;
  int res = internal_syscall(SYSCALL(stat64), path, &buf64);
  stat64_to_stat(&buf64, (struct stat *)buf);
  return res;
#endif
}

uptr internal_lstat(const char *path, void *buf) {
#if SANITIZER_NETBSD
  return internal_syscall(SYSCALL(lstat), path, buf);
#elif SANITIZER_FREEBSD
  return internal_syscall(SYSCALL(fstatat), AT_FDCWD, (uptr)path,
                          (uptr)buf, AT_SYMLINK_NOFOLLOW);
#elif SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(newfstatat), AT_FDCWD, (uptr)path,
                         (uptr)buf, AT_SYMLINK_NOFOLLOW);
#elif SANITIZER_LINUX_USES_64BIT_SYSCALLS
# if SANITIZER_MIPS64
  // For mips64, lstat syscall fills buffer in the format of kernel_stat
  struct kernel_stat kbuf;
  int res = internal_syscall(SYSCALL(lstat), path, &kbuf);
  kernel_stat_to_stat(&kbuf, (struct stat *)buf);
  return res;
# else
  return internal_syscall(SYSCALL(lstat), (uptr)path, (uptr)buf);
# endif
#else
  struct stat64 buf64;
  int res = internal_syscall(SYSCALL(lstat64), path, &buf64);
  stat64_to_stat(&buf64, (struct stat *)buf);
  return res;
#endif
}

uptr internal_fstat(fd_t fd, void *buf) {
#if SANITIZER_FREEBSD || SANITIZER_LINUX_USES_64BIT_SYSCALLS || SANITIZER_NETBSD
# if SANITIZER_MIPS64
  // For mips64, fstat syscall fills buffer in the format of kernel_stat
  struct kernel_stat kbuf;
  int res = internal_syscall(SYSCALL(fstat), fd, &kbuf);
  kernel_stat_to_stat(&kbuf, (struct stat *)buf);
  return res;
# else
  return internal_syscall(SYSCALL(fstat), fd, (uptr)buf);
# endif
#else
  struct stat64 buf64;
  int res = internal_syscall(SYSCALL(fstat64), fd, &buf64);
  stat64_to_stat(&buf64, (struct stat *)buf);
  return res;
#endif
}

uptr internal_filesize(fd_t fd) {
  struct stat st;
  if (internal_fstat(fd, &st))
    return -1;
  return (uptr)st.st_size;
}

uptr internal_dup2(int oldfd, int newfd) {
#if SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(dup3), oldfd, newfd, 0);
#else
  return internal_syscall(SYSCALL(dup2), oldfd, newfd);
#endif
}

uptr internal_readlink(const char *path, char *buf, uptr bufsize) {
#if SANITIZER_NETBSD
  return internal_syscall_ptr(SYSCALL(readlink), path, buf, bufsize);
#elif SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(readlinkat), AT_FDCWD,
                          (uptr)path, (uptr)buf, bufsize);
#else
  return internal_syscall(SYSCALL(readlink), (uptr)path, (uptr)buf, bufsize);
#endif
}

uptr internal_unlink(const char *path) {
#if SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(unlinkat), AT_FDCWD, (uptr)path, 0);
#else
  return internal_syscall(SYSCALL(unlink), (uptr)path);
#endif
}

uptr internal_rename(const char *oldpath, const char *newpath) {
#if SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(renameat), AT_FDCWD, (uptr)oldpath, AT_FDCWD,
                          (uptr)newpath);
#else
  return internal_syscall(SYSCALL(rename), (uptr)oldpath, (uptr)newpath);
#endif
}

uptr internal_sched_yield() {
  return internal_syscall(SYSCALL(sched_yield));
}

void internal__exit(int exitcode) {
#if SANITIZER_FREEBSD || SANITIZER_NETBSD
  internal_syscall(SYSCALL(exit), exitcode);
#else
  internal_syscall(SYSCALL(exit_group), exitcode);
#endif
  Die();  // Unreachable.
}

unsigned int internal_sleep(unsigned int seconds) {
  struct timespec ts;
  ts.tv_sec = 1;
  ts.tv_nsec = 0;
  int res = internal_syscall(SYSCALL(nanosleep), &ts, &ts);
  if (res) return ts.tv_sec;
  return 0;
}

uptr internal_execve(const char *filename, char *const argv[],
                     char *const envp[]) {
  return internal_syscall(SYSCALL(execve), (uptr)filename, (uptr)argv,
                          (uptr)envp);
}

// ----------------- sanitizer_common.h
bool FileExists(const char *filename) {
  struct stat st;
#if SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  if (internal_syscall(SYSCALL(newfstatat), AT_FDCWD, filename, &st, 0))
#else
  if (internal_stat(filename, &st))
#endif
    return false;
  // Sanity check: filename is a regular file.
  return S_ISREG(st.st_mode);
}

tid_t GetTid() {
#if SANITIZER_FREEBSD
  return (uptr)pthread_self();
#elif SANITIZER_NETBSD
  return _lwp_self();
#else
  return internal_syscall(SYSCALL(gettid));
#endif
}

u64 NanoTime() {
#if SANITIZER_FREEBSD || SANITIZER_NETBSD
  timeval tv;
#else
  kernel_timeval tv;
#endif
  internal_memset(&tv, 0, sizeof(tv));
#if SANITIZER_NETBSD
  internal_syscall_ptr(SYSCALL(gettimeofday), &tv, NULL);
#else
  internal_syscall(SYSCALL(gettimeofday), (uptr)&tv, 0);
#endif
  return (u64)tv.tv_sec * 1000*1000*1000 + tv.tv_usec * 1000;
}

// Like getenv, but reads env directly from /proc (on Linux) or parses the
// 'environ' array (on FreeBSD) and does not use libc. This function should be
// called first inside __asan_init.
const char *GetEnv(const char *name) {
#if SANITIZER_FREEBSD || SANITIZER_NETBSD
  if (::environ != 0) {
    uptr NameLen = internal_strlen(name);
    for (char **Env = ::environ; *Env != 0; Env++) {
      if (internal_strncmp(*Env, name, NameLen) == 0 && (*Env)[NameLen] == '=')
        return (*Env) + NameLen + 1;
    }
  }
  return 0;  // Not found.
#elif SANITIZER_LINUX
  static char *environ;
  static uptr len;
  static bool inited;
  if (!inited) {
    inited = true;
    uptr environ_size;
    if (!ReadFileToBuffer("/proc/self/environ", &environ, &environ_size, &len))
      environ = nullptr;
  }
  if (!environ || len == 0) return nullptr;
  uptr namelen = internal_strlen(name);
  const char *p = environ;
  while (*p != '\0') {  // will happen at the \0\0 that terminates the buffer
    // proc file has the format NAME=value\0NAME=value\0NAME=value\0...
    const char* endp =
        (char*)internal_memchr(p, '\0', len - (p - environ));
    if (!endp)  // this entry isn't NUL terminated
      return nullptr;
    else if (!internal_memcmp(p, name, namelen) && p[namelen] == '=')  // Match.
      return p + namelen + 1;  // point after =
    p = endp + 1;
  }
  return nullptr;  // Not found.
#else
#error "Unsupported platform"
#endif
}

#if !SANITIZER_FREEBSD
extern "C" {
  SANITIZER_WEAK_ATTRIBUTE extern void *__libc_stack_end;
}
#endif

#if !SANITIZER_GO && !SANITIZER_FREEBSD
static void ReadNullSepFileToArray(const char *path, char ***arr,
                                   int arr_size) {
  char *buff;
  uptr buff_size;
  uptr buff_len;
  *arr = (char **)MmapOrDie(arr_size * sizeof(char *), "NullSepFileArray");
  if (!ReadFileToBuffer(path, &buff, &buff_size, &buff_len, 1024 * 1024)) {
    (*arr)[0] = nullptr;
    return;
  }
  (*arr)[0] = buff;
  int count, i;
  for (count = 1, i = 1; ; i++) {
    if (buff[i] == 0) {
      if (buff[i+1] == 0) break;
      (*arr)[count] = &buff[i+1];
      CHECK_LE(count, arr_size - 1);  // FIXME: make this more flexible.
      count++;
    }
  }
  (*arr)[count] = nullptr;
}
#endif

static void GetArgsAndEnv(char ***argv, char ***envp) {
#if !SANITIZER_FREEBSD
#if !SANITIZER_GO
  if (&__libc_stack_end) {
#endif
    uptr* stack_end = (uptr*)__libc_stack_end;
    int argc = *stack_end;
    *argv = (char**)(stack_end + 1);
    *envp = (char**)(stack_end + argc + 2);
#if !SANITIZER_GO
  } else {
    static const int kMaxArgv = 2000, kMaxEnvp = 2000;
    ReadNullSepFileToArray("/proc/self/cmdline", argv, kMaxArgv);
    ReadNullSepFileToArray("/proc/self/environ", envp, kMaxEnvp);
  }
#endif
#else
  // On FreeBSD, retrieving the argument and environment arrays is done via the
  // kern.ps_strings sysctl, which returns a pointer to a structure containing
  // this information. See also <sys/exec.h>.
  ps_strings *pss;
  size_t sz = sizeof(pss);
  if (sysctlbyname("kern.ps_strings", &pss, &sz, NULL, 0) == -1) {
    Printf("sysctl kern.ps_strings failed\n");
    Die();
  }
  *argv = pss->ps_argvstr;
  *envp = pss->ps_envstr;
#endif
}

char **GetArgv() {
  char **argv, **envp;
  GetArgsAndEnv(&argv, &envp);
  return argv;
}

void ReExec() {
  char **argv, **envp;
  GetArgsAndEnv(&argv, &envp);
  uptr rv = internal_execve("/proc/self/exe", argv, envp);
  int rverrno;
  CHECK_EQ(internal_iserror(rv, &rverrno), true);
  Printf("execve failed, errno %d\n", rverrno);
  Die();
}

enum MutexState {
  MtxUnlocked = 0,
  MtxLocked = 1,
  MtxSleeping = 2
};

BlockingMutex::BlockingMutex() {
  internal_memset(this, 0, sizeof(*this));
}

void BlockingMutex::Lock() {
  CHECK_EQ(owner_, 0);
  atomic_uint32_t *m = reinterpret_cast<atomic_uint32_t *>(&opaque_storage_);
  if (atomic_exchange(m, MtxLocked, memory_order_acquire) == MtxUnlocked)
    return;
  while (atomic_exchange(m, MtxSleeping, memory_order_acquire) != MtxUnlocked) {
#if SANITIZER_FREEBSD
    _umtx_op(m, UMTX_OP_WAIT_UINT, MtxSleeping, 0, 0);
#elif SANITIZER_NETBSD
    sched_yield(); /* No userspace futex-like synchromization */
#else
    internal_syscall(SYSCALL(futex), (uptr)m, FUTEX_WAIT, MtxSleeping, 0, 0, 0);
#endif
  }
}

void BlockingMutex::Unlock() {
  atomic_uint32_t *m = reinterpret_cast<atomic_uint32_t *>(&opaque_storage_);
  u32 v = atomic_exchange(m, MtxUnlocked, memory_order_release);
  CHECK_NE(v, MtxUnlocked);
  if (v == MtxSleeping) {
#if SANITIZER_FREEBSD
    _umtx_op(m, UMTX_OP_WAKE, 1, 0, 0);
#elif SANITIZER_NETBSD
                   /* No userspace futex-like synchromization */
#else
    internal_syscall(SYSCALL(futex), (uptr)m, FUTEX_WAKE, 1, 0, 0, 0);
#endif
  }
}

void BlockingMutex::CheckLocked() {
  atomic_uint32_t *m = reinterpret_cast<atomic_uint32_t *>(&opaque_storage_);
  CHECK_NE(MtxUnlocked, atomic_load(m, memory_order_relaxed));
}

// ----------------- sanitizer_linux.h
// The actual size of this structure is specified by d_reclen.
// Note that getdents64 uses a different structure format. We only provide the
// 32-bit syscall here.
#if SANITIZER_NETBSD
// struct dirent is different for Linux and us. At this moment, we use only
// d_fileno (Linux call this d_ino), d_reclen, and d_name.
struct linux_dirent {
  u64 d_ino;  // d_fileno
  u16 d_reclen;
  u16 d_namlen;  // not used
  u8 d_type;     // not used
  char d_name[NAME_MAX + 1];
};
#else
struct linux_dirent {
#if SANITIZER_X32 || defined(__aarch64__)
  u64 d_ino;
  u64 d_off;
#else
  unsigned long      d_ino;
  unsigned long      d_off;
#endif
  unsigned short     d_reclen;
#ifdef __aarch64__
  unsigned char      d_type;
#endif
  char               d_name[256];
};
#endif

// Syscall wrappers.
uptr internal_ptrace(int request, int pid, void *addr, void *data) {
#if SANITIZER_NETBSD
  // XXX We need additional work for ptrace:
  //   - for request, we use PT_FOO whereas Linux uses PTRACE_FOO
  //   - data is int for us, but void * for Linux
  //   - Linux sometimes uses data in the case where we use addr instead
  // At this moment, this function is used only within
  // "#if SANITIZER_LINUX && defined(__x86_64__)" block in
  // sanitizer_stoptheworld_linux_libcdep.cc.
  return internal_syscall_ptr(SYSCALL(ptrace), request, pid, (uptr)addr,
                              (uptr)data);
#else
  return internal_syscall(SYSCALL(ptrace), request, pid, (uptr)addr,
                          (uptr)data);
#endif
}

uptr internal_waitpid(int pid, int *status, int options) {
#if SANITIZER_NETBSD
  return internal_syscall(SYSCALL(wait4), pid, status, options,
                          NULL /* rusage */);
#else
  return internal_syscall(SYSCALL(wait4), pid, (uptr)status, options,
                          0 /* rusage */);
#endif
}

uptr internal_getpid() {
  return internal_syscall(SYSCALL(getpid));
}

uptr internal_getppid() {
  return internal_syscall(SYSCALL(getppid));
}

uptr internal_getdents(fd_t fd, struct linux_dirent *dirp, unsigned int count) {
#if SANITIZER_NETBSD
  return internal_syscall(SYSCALL(getdents), fd, dirp, (uptr)count);
#elif SANITIZER_FREEBSD
  return internal_syscall(SYSCALL(getdirentries), fd, (uptr)dirp, count, NULL);
#elif SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(getdents64), fd, (uptr)dirp, count);
#else
  return internal_syscall(SYSCALL(getdents), fd, (uptr)dirp, count);
#endif
}

uptr internal_lseek(fd_t fd, OFF_T offset, int whence) {
#if SANITIZER_NETBSD
  return internal_syscall64(SYSCALL(lseek), fd, 0, offset, whence);
#else
  return internal_syscall(SYSCALL(lseek), fd, offset, whence);
#endif
}

#if SANITIZER_LINUX
uptr internal_prctl(int option, uptr arg2, uptr arg3, uptr arg4, uptr arg5) {
  return internal_syscall(SYSCALL(prctl), option, arg2, arg3, arg4, arg5);
}
#endif

uptr internal_sigaltstack(const void *ss, void *oss) {
  return internal_syscall(SYSCALL(sigaltstack), (uptr)ss, (uptr)oss);
}

int internal_fork() {
#if SANITIZER_USES_CANONICAL_LINUX_SYSCALLS
  return internal_syscall(SYSCALL(clone), SIGCHLD, 0);
#else
  return internal_syscall(SYSCALL(fork));
#endif
}

#if SANITIZER_LINUX
#define SA_RESTORER 0x04000000
// Doesn't set sa_restorer if the caller did not set it, so use with caution
//(see below).
int internal_sigaction_norestorer(int signum, const void *act, void *oldact) {
  __sanitizer_kernel_sigaction_t k_act, k_oldact;
  internal_memset(&k_act, 0, sizeof(__sanitizer_kernel_sigaction_t));
  internal_memset(&k_oldact, 0, sizeof(__sanitizer_kernel_sigaction_t));
  const __sanitizer_sigaction *u_act = (const __sanitizer_sigaction *)act;
  __sanitizer_sigaction *u_oldact = (__sanitizer_sigaction *)oldact;
  if (u_act) {
    k_act.handler = u_act->handler;
    k_act.sigaction = u_act->sigaction;
    internal_memcpy(&k_act.sa_mask, &u_act->sa_mask,
                    sizeof(__sanitizer_kernel_sigset_t));
    // Without SA_RESTORER kernel ignores the calls (probably returns EINVAL).
    k_act.sa_flags = u_act->sa_flags | SA_RESTORER;
    // FIXME: most often sa_restorer is unset, however the kernel requires it
    // to point to a valid signal restorer that calls the rt_sigreturn syscall.
    // If sa_restorer passed to the kernel is NULL, the program may crash upon
    // signal delivery or fail to unwind the stack in the signal handler.
    // libc implementation of sigaction() passes its own restorer to
    // rt_sigaction, so we need to do the same (we'll need to reimplement the
    // restorers; for x86_64 the restorer address can be obtained from
    // oldact->sa_restorer upon a call to sigaction(xxx, NULL, oldact).
#if !SANITIZER_ANDROID || !SANITIZER_MIPS32
    k_act.sa_restorer = u_act->sa_restorer;
#endif
  }

  uptr result = internal_syscall(SYSCALL(rt_sigaction), (uptr)signum,
      (uptr)(u_act ? &k_act : nullptr),
      (uptr)(u_oldact ? &k_oldact : nullptr),
      (uptr)sizeof(__sanitizer_kernel_sigset_t));

  if ((result == 0) && u_oldact) {
    u_oldact->handler = k_oldact.handler;
    u_oldact->sigaction = k_oldact.sigaction;
    internal_memcpy(&u_oldact->sa_mask, &k_oldact.sa_mask,
                    sizeof(__sanitizer_kernel_sigset_t));
    u_oldact->sa_flags = k_oldact.sa_flags;
#if !SANITIZER_ANDROID || !SANITIZER_MIPS32
    u_oldact->sa_restorer = k_oldact.sa_restorer;
#endif
  }
  return result;
}

// Invokes sigaction via a raw syscall with a restorer, but does not support
// all platforms yet.
// We disable for Go simply because we have not yet added to buildgo.sh.
#if (defined(__x86_64__) || SANITIZER_MIPS64) && !SANITIZER_GO
int internal_sigaction_syscall(int signum, const void *act, void *oldact) {
  if (act == nullptr)
    return internal_sigaction_norestorer(signum, act, oldact);
  __sanitizer_sigaction u_adjust;
  internal_memcpy(&u_adjust, act, sizeof(u_adjust));
#if !SANITIZER_ANDROID || !SANITIZER_MIPS32
    if (u_adjust.sa_restorer == nullptr) {
      u_adjust.sa_restorer = internal_sigreturn;
    }
#endif
    return internal_sigaction_norestorer(signum, (const void *)&u_adjust,
                                         oldact);
}
#endif // defined(__x86_64__) && !SANITIZER_GO
#endif  // SANITIZER_LINUX

uptr internal_sigprocmask(int how, __sanitizer_sigset_t *set,
    __sanitizer_sigset_t *oldset) {
#if SANITIZER_FREEBSD || SANITIZER_NETBSD
  return internal_syscall(SYSCALL(sigprocmask), how, set, oldset);
#else
  __sanitizer_kernel_sigset_t *k_set = (__sanitizer_kernel_sigset_t *)set;
  __sanitizer_kernel_sigset_t *k_oldset = (__sanitizer_kernel_sigset_t *)oldset;
  return internal_syscall(SYSCALL(rt_sigprocmask), (uptr)how,
                          (uptr)&k_set->sig[0], (uptr)&k_oldset->sig[0],
                          sizeof(__sanitizer_kernel_sigset_t));
#endif
}

void internal_sigfillset(__sanitizer_sigset_t *set) {
  internal_memset(set, 0xff, sizeof(*set));
}

void internal_sigemptyset(__sanitizer_sigset_t *set) {
  internal_memset(set, 0, sizeof(*set));
}

#if SANITIZER_LINUX
void internal_sigdelset(__sanitizer_sigset_t *set, int signum) {
  signum -= 1;
  CHECK_GE(signum, 0);
  CHECK_LT(signum, sizeof(*set) * 8);
  __sanitizer_kernel_sigset_t *k_set = (__sanitizer_kernel_sigset_t *)set;
  const uptr idx = signum / (sizeof(k_set->sig[0]) * 8);
  const uptr bit = signum % (sizeof(k_set->sig[0]) * 8);
  k_set->sig[idx] &= ~(1 << bit);
}

bool internal_sigismember(__sanitizer_sigset_t *set, int signum) {
  signum -= 1;
  CHECK_GE(signum, 0);
  CHECK_LT(signum, sizeof(*set) * 8);
  __sanitizer_kernel_sigset_t *k_set = (__sanitizer_kernel_sigset_t *)set;
  const uptr idx = signum / (sizeof(k_set->sig[0]) * 8);
  const uptr bit = signum % (sizeof(k_set->sig[0]) * 8);
  return k_set->sig[idx] & (1 << bit);
}
#endif  // SANITIZER_LINUX

// ThreadLister implementation.
ThreadLister::ThreadLister(int pid)
  : pid_(pid),
    descriptor_(-1),
    buffer_(4096),
    error_(true),
    entry_((struct linux_dirent *)buffer_.data()),
    bytes_read_(0) {
  char task_directory_path[80];
  internal_snprintf(task_directory_path, sizeof(task_directory_path),
                    "/proc/%d/task/", pid);
  uptr openrv = internal_open(task_directory_path, O_RDONLY | O_DIRECTORY);
  if (internal_iserror(openrv)) {
    error_ = true;
    Report("Can't open /proc/%d/task for reading.\n", pid);
  } else {
    error_ = false;
    descriptor_ = openrv;
  }
}

int ThreadLister::GetNextTID() {
  int tid = -1;
  do {
    if (error_)
      return -1;
    if ((char *)entry_ >= &buffer_[bytes_read_] && !GetDirectoryEntries())
      return -1;
    if (entry_->d_ino != 0 && entry_->d_name[0] >= '0' &&
        entry_->d_name[0] <= '9') {
      // Found a valid tid.
      tid = (int)internal_atoll(entry_->d_name);
    }
    entry_ = (struct linux_dirent *)(((char *)entry_) + entry_->d_reclen);
  } while (tid < 0);
  return tid;
}

void ThreadLister::Reset() {
  if (error_ || descriptor_ < 0)
    return;
  internal_lseek(descriptor_, 0, SEEK_SET);
}

ThreadLister::~ThreadLister() {
  if (descriptor_ >= 0)
    internal_close(descriptor_);
}

bool ThreadLister::error() { return error_; }

bool ThreadLister::GetDirectoryEntries() {
  CHECK_GE(descriptor_, 0);
  CHECK_NE(error_, true);
  bytes_read_ = internal_getdents(descriptor_,
                                  (struct linux_dirent *)buffer_.data(),
                                  buffer_.size());
  if (internal_iserror(bytes_read_)) {
    Report("Can't read directory entries from /proc/%d/task.\n", pid_);
    error_ = true;
    return false;
  } else if (bytes_read_ == 0) {
    return false;
  }
  entry_ = (struct linux_dirent *)buffer_.data();
  return true;
}

#if SANITIZER_WORDSIZE == 32
// Take care of unusable kernel area in top gigabyte.
static uptr GetKernelAreaSize() {
#if SANITIZER_LINUX && !SANITIZER_X32
  const uptr gbyte = 1UL << 30;

  // Firstly check if there are writable segments
  // mapped to top gigabyte (e.g. stack).
  MemoryMappingLayout proc_maps(/*cache_enabled*/true);
  MemoryMappedSegment segment;
  while (proc_maps.Next(&segment)) {
    if ((segment.end >= 3 * gbyte) && segment.IsWritable()) return 0;
  }

#if !SANITIZER_ANDROID
  // Even if nothing is mapped, top Gb may still be accessible
  // if we are running on 64-bit kernel.
  // Uname may report misleading results if personality type
  // is modified (e.g. under schroot) so check this as well.
  struct utsname uname_info;
  int pers = personality(0xffffffffUL);
  if (!(pers & PER_MASK)
      && uname(&uname_info) == 0
      && internal_strstr(uname_info.machine, "64"))
    return 0;
#endif  // SANITIZER_ANDROID

  // Top gigabyte is reserved for kernel.
  return gbyte;
#else
  return 0;
#endif  // SANITIZER_LINUX && !SANITIZER_X32
}
#endif  // SANITIZER_WORDSIZE == 32

uptr GetMaxVirtualAddress() {
#if SANITIZER_NETBSD && defined(__x86_64__)
  return 0x7f7ffffff000ULL;  // (0x00007f8000000000 - PAGE_SIZE)
#elif SANITIZER_WORDSIZE == 64
# if defined(__powerpc64__) || defined(__aarch64__)
  // On PowerPC64 we have two different address space layouts: 44- and 46-bit.
  // We somehow need to figure out which one we are using now and choose
  // one of 0x00000fffffffffffUL and 0x00003fffffffffffUL.
  // Note that with 'ulimit -s unlimited' the stack is moved away from the top
  // of the address space, so simply checking the stack address is not enough.
  // This should (does) work for both PowerPC64 Endian modes.
  // Similarly, aarch64 has multiple address space layouts: 39, 42 and 47-bit.
  return (1ULL << (MostSignificantSetBitIndex(GET_CURRENT_FRAME()) + 1)) - 1;
# elif defined(__mips64)
  return (1ULL << 40) - 1;  // 0x000000ffffffffffUL;
# elif defined(__s390x__)
  return (1ULL << 53) - 1;  // 0x001fffffffffffffUL;
# else
  return (1ULL << 47) - 1;  // 0x00007fffffffffffUL;
# endif
#else  // SANITIZER_WORDSIZE == 32
# if defined(__s390__)
  return (1ULL << 31) - 1;  // 0x7fffffff;
# else
  uptr res = (1ULL << 32) - 1;  // 0xffffffff;
  if (!common_flags()->full_address_space)
    res -= GetKernelAreaSize();
  CHECK_LT(reinterpret_cast<uptr>(&res), res);
  return res;
# endif
#endif  // SANITIZER_WORDSIZE
}

uptr GetPageSize() {
// Android post-M sysconf(_SC_PAGESIZE) crashes if called from .preinit_array.
#if SANITIZER_ANDROID
  return 4096;
#elif SANITIZER_LINUX && (defined(__x86_64__) || defined(__i386__))
  return EXEC_PAGESIZE;
#elif SANITIZER_USE_GETAUXVAL
  return getauxval(AT_PAGESZ);
#else
  return sysconf(_SC_PAGESIZE);  // EXEC_PAGESIZE may not be trustworthy.
#endif
}

uptr ReadBinaryName(/*out*/char *buf, uptr buf_len) {
#if SANITIZER_FREEBSD || SANITIZER_NETBSD
#if SANITIZER_FREEBSD
  const int Mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1};
#else
  const int Mib[4] = {CTL_KERN, KERN_PROC_ARGS, -1, KERN_PROC_PATHNAME};
#endif
  const char *default_module_name = "kern.proc.pathname";
  size_t Size = buf_len;
  bool IsErr = (sysctl(Mib, ARRAY_SIZE(Mib), buf, &Size, NULL, 0) != 0);
  int readlink_error = IsErr ? errno : 0;
  uptr module_name_len = Size;
#else
  const char *default_module_name = "/proc/self/exe";
  uptr module_name_len = internal_readlink(
      default_module_name, buf, buf_len);
  int readlink_error;
  bool IsErr = internal_iserror(module_name_len, &readlink_error);
#endif
  if (IsErr) {
    // We can't read binary name for some reason, assume it's unknown.
    Report("WARNING: reading executable name failed with errno %d, "
           "some stack frames may not be symbolized\n", readlink_error);
    module_name_len = internal_snprintf(buf, buf_len, "%s",
                                        default_module_name);
    CHECK_LT(module_name_len, buf_len);
  }
  return module_name_len;
}

uptr ReadLongProcessName(/*out*/ char *buf, uptr buf_len) {
#if SANITIZER_LINUX
  char *tmpbuf;
  uptr tmpsize;
  uptr tmplen;
  if (ReadFileToBuffer("/proc/self/cmdline", &tmpbuf, &tmpsize, &tmplen,
                       1024 * 1024)) {
    internal_strncpy(buf, tmpbuf, buf_len);
    UnmapOrDie(tmpbuf, tmpsize);
    return internal_strlen(buf);
  }
#endif
  return ReadBinaryName(buf, buf_len);
}

// Match full names of the form /path/to/base_name{-,.}*
bool LibraryNameIs(const char *full_name, const char *base_name) {
  const char *name = full_name;
  // Strip path.
  while (*name != '\0') name++;
  while (name > full_name && *name != '/') name--;
  if (*name == '/') name++;
  uptr base_name_length = internal_strlen(base_name);
  if (internal_strncmp(name, base_name, base_name_length)) return false;
  return (name[base_name_length] == '-' || name[base_name_length] == '.');
}

#if !SANITIZER_ANDROID
// Call cb for each region mapped by map.
void ForEachMappedRegion(link_map *map, void (*cb)(const void *, uptr)) {
  CHECK_NE(map, nullptr);
#if !SANITIZER_FREEBSD
  typedef ElfW(Phdr) Elf_Phdr;
  typedef ElfW(Ehdr) Elf_Ehdr;
#endif  // !SANITIZER_FREEBSD
  char *base = (char *)map->l_addr;
  Elf_Ehdr *ehdr = (Elf_Ehdr *)base;
  char *phdrs = base + ehdr->e_phoff;
  char *phdrs_end = phdrs + ehdr->e_phnum * ehdr->e_phentsize;

  // Find the segment with the minimum base so we can "relocate" the p_vaddr
  // fields.  Typically ET_DYN objects (DSOs) have base of zero and ET_EXEC
  // objects have a non-zero base.
  uptr preferred_base = (uptr)-1;
  for (char *iter = phdrs; iter != phdrs_end; iter += ehdr->e_phentsize) {
    Elf_Phdr *phdr = (Elf_Phdr *)iter;
    if (phdr->p_type == PT_LOAD && preferred_base > (uptr)phdr->p_vaddr)
      preferred_base = (uptr)phdr->p_vaddr;
  }

  // Compute the delta from the real base to get a relocation delta.
  sptr delta = (uptr)base - preferred_base;
  // Now we can figure out what the loader really mapped.
  for (char *iter = phdrs; iter != phdrs_end; iter += ehdr->e_phentsize) {
    Elf_Phdr *phdr = (Elf_Phdr *)iter;
    if (phdr->p_type == PT_LOAD) {
      uptr seg_start = phdr->p_vaddr + delta;
      uptr seg_end = seg_start + phdr->p_memsz;
      // None of these values are aligned.  We consider the ragged edges of the
      // load command as defined, since they are mapped from the file.
      seg_start = RoundDownTo(seg_start, GetPageSizeCached());
      seg_end = RoundUpTo(seg_end, GetPageSizeCached());
      cb((void *)seg_start, seg_end - seg_start);
    }
  }
}
#endif

#if defined(__x86_64__) && SANITIZER_LINUX
// We cannot use glibc's clone wrapper, because it messes with the child
// task's TLS. It writes the PID and TID of the child task to its thread
// descriptor, but in our case the child task shares the thread descriptor with
// the parent (because we don't know how to allocate a new thread
// descriptor to keep glibc happy). So the stock version of clone(), when
// used with CLONE_VM, would end up corrupting the parent's thread descriptor.
uptr internal_clone(int (*fn)(void *), void *child_stack, int flags, void *arg,
                    int *parent_tidptr, void *newtls, int *child_tidptr) {
  long long res;
  if (!fn || !child_stack)
    return -EINVAL;
  CHECK_EQ(0, (uptr)child_stack % 16);
  child_stack = (char *)child_stack - 2 * sizeof(unsigned long long);
  ((unsigned long long *)child_stack)[0] = (uptr)fn;
  ((unsigned long long *)child_stack)[1] = (uptr)arg;
  register void *r8 __asm__("r8") = newtls;
  register int *r10 __asm__("r10") = child_tidptr;
  __asm__ __volatile__(
                       /* %rax = syscall(%rax = SYSCALL(clone),
                        *                %rdi = flags,
                        *                %rsi = child_stack,
                        *                %rdx = parent_tidptr,
                        *                %r8  = new_tls,
                        *                %r10 = child_tidptr)
                        */
                       "syscall\n"

                       /* if (%rax != 0)
                        *   return;
                        */
                       "testq  %%rax,%%rax\n"
                       "jnz    1f\n"

                       /* In the child. Terminate unwind chain. */
                       // XXX: We should also terminate the CFI unwind chain
                       // here. Unfortunately clang 3.2 doesn't support the
                       // necessary CFI directives, so we skip that part.
                       "xorq   %%rbp,%%rbp\n"

                       /* Call "fn(arg)". */
                       "popq   %%rax\n"
                       "popq   %%rdi\n"
                       "call   *%%rax\n"

                       /* Call _exit(%rax). */
                       "movq   %%rax,%%rdi\n"
                       "movq   %2,%%rax\n"
                       "syscall\n"

                       /* Return to parent. */
                     "1:\n"
                       : "=a" (res)
                       : "a"(SYSCALL(clone)), "i"(SYSCALL(exit)),
                         "S"(child_stack),
                         "D"(flags),
                         "d"(parent_tidptr),
                         "r"(r8),
                         "r"(r10)
                       : "rsp", "memory", "r11", "rcx");
  return res;
}
#elif defined(__mips__)
uptr internal_clone(int (*fn)(void *), void *child_stack, int flags, void *arg,
                    int *parent_tidptr, void *newtls, int *child_tidptr) {
  long long res;
  if (!fn || !child_stack)
    return -EINVAL;
  CHECK_EQ(0, (uptr)child_stack % 16);
  child_stack = (char *)child_stack - 2 * sizeof(unsigned long long);
  ((unsigned long long *)child_stack)[0] = (uptr)fn;
  ((unsigned long long *)child_stack)[1] = (uptr)arg;
  register void *a3 __asm__("$7") = newtls;
  register int *a4 __asm__("$8") = child_tidptr;
  // We don't have proper CFI directives here because it requires alot of code
  // for very marginal benefits.
  __asm__ __volatile__(
                       /* $v0 = syscall($v0 = __NR_clone,
                        * $a0 = flags,
                        * $a1 = child_stack,
                        * $a2 = parent_tidptr,
                        * $a3 = new_tls,
                        * $a4 = child_tidptr)
                        */
                       ".cprestore 16;\n"
                       "move $4,%1;\n"
                       "move $5,%2;\n"
                       "move $6,%3;\n"
                       "move $7,%4;\n"
                       /* Store the fifth argument on stack
                        * if we are using 32-bit abi.
                        */
#if SANITIZER_WORDSIZE == 32
                       "lw %5,16($29);\n"
#else
                       "move $8,%5;\n"
#endif
                       "li $2,%6;\n"
                       "syscall;\n"

                       /* if ($v0 != 0)
                        * return;
                        */
                       "bnez $2,1f;\n"

                       /* Call "fn(arg)". */
#if SANITIZER_WORDSIZE == 32
#ifdef __BIG_ENDIAN__
                       "lw $25,4($29);\n"
                       "lw $4,12($29);\n"
#else
                       "lw $25,0($29);\n"
                       "lw $4,8($29);\n"
#endif
#else
                       "ld $25,0($29);\n"
                       "ld $4,8($29);\n"
#endif
                       "jal $25;\n"

                       /* Call _exit($v0). */
                       "move $4,$2;\n"
                       "li $2,%7;\n"
                       "syscall;\n"

                       /* Return to parent. */
                     "1:\n"
                       : "=r" (res)
                       : "r"(flags),
                         "r"(child_stack),
                         "r"(parent_tidptr),
                         "r"(a3),
                         "r"(a4),
                         "i"(__NR_clone),
                         "i"(__NR_exit)
                       : "memory", "$29" );
  return res;
}
#elif defined(__aarch64__)
uptr internal_clone(int (*fn)(void *), void *child_stack, int flags, void *arg,
                    int *parent_tidptr, void *newtls, int *child_tidptr) {
  long long res;
  if (!fn || !child_stack)
    return -EINVAL;
  CHECK_EQ(0, (uptr)child_stack % 16);
  child_stack = (char *)child_stack - 2 * sizeof(unsigned long long);
  ((unsigned long long *)child_stack)[0] = (uptr)fn;
  ((unsigned long long *)child_stack)[1] = (uptr)arg;

  register int (*__fn)(void *)  __asm__("x0") = fn;
  register void *__stack __asm__("x1") = child_stack;
  register int   __flags __asm__("x2") = flags;
  register void *__arg   __asm__("x3") = arg;
  register int  *__ptid  __asm__("x4") = parent_tidptr;
  register void *__tls   __asm__("x5") = newtls;
  register int  *__ctid  __asm__("x6") = child_tidptr;

  __asm__ __volatile__(
                       "mov x0,x2\n" /* flags  */
                       "mov x2,x4\n" /* ptid  */
                       "mov x3,x5\n" /* tls  */
                       "mov x4,x6\n" /* ctid  */
                       "mov x8,%9\n" /* clone  */

                       "svc 0x0\n"

                       /* if (%r0 != 0)
                        *   return %r0;
                        */
                       "cmp x0, #0\n"
                       "bne 1f\n"

                       /* In the child, now. Call "fn(arg)". */
                       "ldp x1, x0, [sp], #16\n"
                       "blr x1\n"

                       /* Call _exit(%r0).  */
                       "mov x8, %10\n"
                       "svc 0x0\n"
                     "1:\n"

                       : "=r" (res)
                       : "i"(-EINVAL),
                         "r"(__fn), "r"(__stack), "r"(__flags), "r"(__arg),
                         "r"(__ptid), "r"(__tls), "r"(__ctid),
                         "i"(__NR_clone), "i"(__NR_exit)
                       : "x30", "memory");
  return res;
}
#elif defined(__powerpc64__)
uptr internal_clone(int (*fn)(void *), void *child_stack, int flags, void *arg,
                   int *parent_tidptr, void *newtls, int *child_tidptr) {
  long long res;
// Stack frame structure.
#if SANITIZER_PPC64V1
//   Back chain == 0        (SP + 112)
// Frame (112 bytes):
//   Parameter save area    (SP + 48), 8 doublewords
//   TOC save area          (SP + 40)
//   Link editor doubleword (SP + 32)
//   Compiler doubleword    (SP + 24)
//   LR save area           (SP + 16)
//   CR save area           (SP + 8)
//   Back chain             (SP + 0)
# define FRAME_SIZE 112
# define FRAME_TOC_SAVE_OFFSET 40
#elif SANITIZER_PPC64V2
//   Back chain == 0        (SP + 32)
// Frame (32 bytes):
//   TOC save area          (SP + 24)
//   LR save area           (SP + 16)
//   CR save area           (SP + 8)
//   Back chain             (SP + 0)
# define FRAME_SIZE 32
# define FRAME_TOC_SAVE_OFFSET 24
#else
# error "Unsupported PPC64 ABI"
#endif
  if (!fn || !child_stack)
    return -EINVAL;
  CHECK_EQ(0, (uptr)child_stack % 16);

  register int (*__fn)(void *) __asm__("r3") = fn;
  register void *__cstack      __asm__("r4") = child_stack;
  register int __flags         __asm__("r5") = flags;
  register void *__arg         __asm__("r6") = arg;
  register int *__ptidptr      __asm__("r7") = parent_tidptr;
  register void *__newtls      __asm__("r8") = newtls;
  register int *__ctidptr      __asm__("r9") = child_tidptr;

 __asm__ __volatile__(
           /* fn and arg are saved across the syscall */
           "mr 28, %5\n\t"
           "mr 27, %8\n\t"

           /* syscall
             r0 == __NR_clone
             r3 == flags
             r4 == child_stack
             r5 == parent_tidptr
             r6 == newtls
             r7 == child_tidptr */
           "mr 3, %7\n\t"
           "mr 5, %9\n\t"
           "mr 6, %10\n\t"
           "mr 7, %11\n\t"
           "li 0, %3\n\t"
           "sc\n\t"

           /* Test if syscall was successful */
           "cmpdi  cr1, 3, 0\n\t"
           "crandc cr1*4+eq, cr1*4+eq, cr0*4+so\n\t"
           "bne-   cr1, 1f\n\t"

           /* Set up stack frame */
           "li    29, 0\n\t"
           "stdu  29, -8(1)\n\t"
           "stdu  1, -%12(1)\n\t"
           /* Do the function call */
           "std   2, %13(1)\n\t"
#if SANITIZER_PPC64V1
           "ld    0, 0(28)\n\t"
           "ld    2, 8(28)\n\t"
           "mtctr 0\n\t"
#elif SANITIZER_PPC64V2
           "mr    12, 28\n\t"
           "mtctr 12\n\t"
#else
# error "Unsupported PPC64 ABI"
#endif
           "mr    3, 27\n\t"
           "bctrl\n\t"
           "ld    2, %13(1)\n\t"

           /* Call _exit(r3) */
           "li 0, %4\n\t"
           "sc\n\t"

           /* Return to parent */
           "1:\n\t"
           "mr %0, 3\n\t"
             : "=r" (res)
             : "0" (-1),
               "i" (EINVAL),
               "i" (__NR_clone),
               "i" (__NR_exit),
               "r" (__fn),
               "r" (__cstack),
               "r" (__flags),
               "r" (__arg),
               "r" (__ptidptr),
               "r" (__newtls),
               "r" (__ctidptr),
               "i" (FRAME_SIZE),
               "i" (FRAME_TOC_SAVE_OFFSET)
             : "cr0", "cr1", "memory", "ctr", "r0", "r27", "r28", "r29");
  return res;
}
#elif defined(__i386__) && SANITIZER_LINUX
uptr internal_clone(int (*fn)(void *), void *child_stack, int flags, void *arg,
                    int *parent_tidptr, void *newtls, int *child_tidptr) {
  int res;
  if (!fn || !child_stack)
    return -EINVAL;
  CHECK_EQ(0, (uptr)child_stack % 16);
  child_stack = (char *)child_stack - 7 * sizeof(unsigned int);
  ((unsigned int *)child_stack)[0] = (uptr)flags;
  ((unsigned int *)child_stack)[1] = (uptr)0;
  ((unsigned int *)child_stack)[2] = (uptr)fn;
  ((unsigned int *)child_stack)[3] = (uptr)arg;
  __asm__ __volatile__(
                       /* %eax = syscall(%eax = SYSCALL(clone),
                        *                %ebx = flags,
                        *                %ecx = child_stack,
                        *                %edx = parent_tidptr,
                        *                %esi  = new_tls,
                        *                %edi = child_tidptr)
                        */

                        /* Obtain flags */
                        "movl    (%%ecx), %%ebx\n"
                        /* Do the system call */
                        "pushl   %%ebx\n"
                        "pushl   %%esi\n"
                        "pushl   %%edi\n"
                        /* Remember the flag value.  */
                        "movl    %%ebx, (%%ecx)\n"
                        "int     $0x80\n"
                        "popl    %%edi\n"
                        "popl    %%esi\n"
                        "popl    %%ebx\n"

                        /* if (%eax != 0)
                         *   return;
                         */

                        "test    %%eax,%%eax\n"
                        "jnz    1f\n"

                        /* terminate the stack frame */
                        "xorl   %%ebp,%%ebp\n"
                        /* Call FN. */
                        "call    *%%ebx\n"
#ifdef PIC
                        "call    here\n"
                        "here:\n"
                        "popl    %%ebx\n"
                        "addl    $_GLOBAL_OFFSET_TABLE_+[.-here], %%ebx\n"
#endif
                        /* Call exit */
                        "movl    %%eax, %%ebx\n"
                        "movl    %2, %%eax\n"
                        "int     $0x80\n"
                        "1:\n"
                       : "=a" (res)
                       : "a"(SYSCALL(clone)), "i"(SYSCALL(exit)),
                         "c"(child_stack),
                         "d"(parent_tidptr),
                         "S"(newtls),
                         "D"(child_tidptr)
                       : "memory");
  return res;
}
#elif defined(__arm__) && SANITIZER_LINUX
uptr internal_clone(int (*fn)(void *), void *child_stack, int flags, void *arg,
                    int *parent_tidptr, void *newtls, int *child_tidptr) {
  unsigned int res;
  if (!fn || !child_stack)
    return -EINVAL;
  child_stack = (char *)child_stack - 2 * sizeof(unsigned int);
  ((unsigned int *)child_stack)[0] = (uptr)fn;
  ((unsigned int *)child_stack)[1] = (uptr)arg;
  register int r0 __asm__("r0") = flags;
  register void *r1 __asm__("r1") = child_stack;
  register int *r2 __asm__("r2") = parent_tidptr;
  register void *r3 __asm__("r3") = newtls;
  register int *r4 __asm__("r4") = child_tidptr;
  register int r7 __asm__("r7") = __NR_clone;

#if __ARM_ARCH > 4 || defined (__ARM_ARCH_4T__)
# define ARCH_HAS_BX
#endif
#if __ARM_ARCH > 4
# define ARCH_HAS_BLX
#endif

#ifdef ARCH_HAS_BX
# ifdef ARCH_HAS_BLX
#  define BLX(R) "blx "  #R "\n"
# else
#  define BLX(R) "mov lr, pc; bx " #R "\n"
# endif
#else
# define BLX(R)  "mov lr, pc; mov pc," #R "\n"
#endif

  __asm__ __volatile__(
                       /* %r0 = syscall(%r7 = SYSCALL(clone),
                        *               %r0 = flags,
                        *               %r1 = child_stack,
                        *               %r2 = parent_tidptr,
                        *               %r3  = new_tls,
                        *               %r4 = child_tidptr)
                        */

                       /* Do the system call */
                       "swi 0x0\n"

                       /* if (%r0 != 0)
                        *   return %r0;
                        */
                       "cmp r0, #0\n"
                       "bne 1f\n"

                       /* In the child, now. Call "fn(arg)". */
                       "ldr r0, [sp, #4]\n"
                       "ldr ip, [sp], #8\n"
                       BLX(ip)
                       /* Call _exit(%r0). */
                       "mov r7, %7\n"
                       "swi 0x0\n"
                       "1:\n"
                       "mov %0, r0\n"
                       : "=r"(res)
                       : "r"(r0), "r"(r1), "r"(r2), "r"(r3), "r"(r4), "r"(r7),
                         "i"(__NR_exit)
                       : "memory");
  return res;
}
#endif  // defined(__x86_64__) && SANITIZER_LINUX

#if SANITIZER_ANDROID
#if __ANDROID_API__ < 21
extern "C" __attribute__((weak)) int dl_iterate_phdr(
    int (*)(struct dl_phdr_info *, size_t, void *), void *);
#endif

static int dl_iterate_phdr_test_cb(struct dl_phdr_info *info, size_t size,
                                   void *data) {
  // Any name starting with "lib" indicates a bug in L where library base names
  // are returned instead of paths.
  if (info->dlpi_name && info->dlpi_name[0] == 'l' &&
      info->dlpi_name[1] == 'i' && info->dlpi_name[2] == 'b') {
    *(bool *)data = true;
    return 1;
  }
  return 0;
}

static atomic_uint32_t android_api_level;

static AndroidApiLevel AndroidDetectApiLevel() {
  if (!&dl_iterate_phdr)
    return ANDROID_KITKAT; // K or lower
  bool base_name_seen = false;
  dl_iterate_phdr(dl_iterate_phdr_test_cb, &base_name_seen);
  if (base_name_seen)
    return ANDROID_LOLLIPOP_MR1; // L MR1
  return ANDROID_POST_LOLLIPOP;   // post-L
  // Plain L (API level 21) is completely broken wrt ASan and not very
  // interesting to detect.
}

AndroidApiLevel AndroidGetApiLevel() {
  AndroidApiLevel level =
      (AndroidApiLevel)atomic_load(&android_api_level, memory_order_relaxed);
  if (level) return level;
  level = AndroidDetectApiLevel();
  atomic_store(&android_api_level, level, memory_order_relaxed);
  return level;
}

#endif

static HandleSignalMode GetHandleSignalModeImpl(int signum) {
  switch (signum) {
    case SIGABRT:
      return common_flags()->handle_abort;
    case SIGILL:
      return common_flags()->handle_sigill;
    case SIGFPE:
      return common_flags()->handle_sigfpe;
    case SIGSEGV:
      return common_flags()->handle_segv;
    case SIGBUS:
      return common_flags()->handle_sigbus;
  }
  return kHandleSignalNo;
}

HandleSignalMode GetHandleSignalMode(int signum) {
  HandleSignalMode result = GetHandleSignalModeImpl(signum);
  if (result == kHandleSignalYes && !common_flags()->allow_user_segv_handler)
    return kHandleSignalExclusive;
  return result;
}

#if !SANITIZER_GO
void *internal_start_thread(void(*func)(void *arg), void *arg) {
  // Start the thread with signals blocked, otherwise it can steal user signals.
  __sanitizer_sigset_t set, old;
  internal_sigfillset(&set);
#if SANITIZER_LINUX && !SANITIZER_ANDROID
  // Glibc uses SIGSETXID signal during setuid call. If this signal is blocked
  // on any thread, setuid call hangs (see test/tsan/setuid.c).
  internal_sigdelset(&set, 33);
#endif
  internal_sigprocmask(SIG_SETMASK, &set, &old);
  void *th;
  real_pthread_create(&th, nullptr, (void*(*)(void *arg))func, arg);
  internal_sigprocmask(SIG_SETMASK, &old, nullptr);
  return th;
}

void internal_join_thread(void *th) {
  real_pthread_join(th, nullptr);
}
#else
void *internal_start_thread(void (*func)(void *), void *arg) { return 0; }

void internal_join_thread(void *th) {}
#endif

#if defined(__aarch64__)
// Android headers in the older NDK releases miss this definition.
struct __sanitizer_esr_context {
  struct _aarch64_ctx head;
  uint64_t esr;
};

static bool Aarch64GetESR(ucontext_t *ucontext, u64 *esr) {
  static const u32 kEsrMagic = 0x45535201;
  u8 *aux = ucontext->uc_mcontext.__reserved;
  while (true) {
    _aarch64_ctx *ctx = (_aarch64_ctx *)aux;
    if (ctx->size == 0) break;
    if (ctx->magic == kEsrMagic) {
      *esr = ((__sanitizer_esr_context *)ctx)->esr;
      return true;
    }
    aux += ctx->size;
  }
  return false;
}
#endif

SignalContext::WriteFlag SignalContext::GetWriteFlag() const {
  ucontext_t *ucontext = (ucontext_t *)context;
#if defined(__x86_64__) || defined(__i386__)
  static const uptr PF_WRITE = 1U << 1;
#if SANITIZER_FREEBSD
  uptr err = ucontext->uc_mcontext.mc_err;
#elif SANITIZER_NETBSD
  uptr err = ucontext->uc_mcontext.__gregs[_REG_ERR];
#else
  uptr err = ucontext->uc_mcontext.gregs[REG_ERR];
#endif
  return err & PF_WRITE ? WRITE : READ;
#elif defined(__arm__)
  static const uptr FSR_WRITE = 1U << 11;
  uptr fsr = ucontext->uc_mcontext.error_code;
  return fsr & FSR_WRITE ? WRITE : READ;
#elif defined(__aarch64__)
  static const u64 ESR_ELx_WNR = 1U << 6;
  u64 esr;
  if (!Aarch64GetESR(ucontext, &esr)) return UNKNOWN;
  return esr & ESR_ELx_WNR ? WRITE : READ;
#else
  (void)ucontext;
  return UNKNOWN;  // FIXME: Implement.
#endif
}

void SignalContext::DumpAllRegisters(void *context) {
  // FIXME: Implement this.
}

static void GetPcSpBp(void *context, uptr *pc, uptr *sp, uptr *bp) {
#if defined(__arm__)
  ucontext_t *ucontext = (ucontext_t*)context;
  *pc = ucontext->uc_mcontext.arm_pc;
  *bp = ucontext->uc_mcontext.arm_fp;
  *sp = ucontext->uc_mcontext.arm_sp;
#elif defined(__aarch64__)
  ucontext_t *ucontext = (ucontext_t*)context;
  *pc = ucontext->uc_mcontext.pc;
  *bp = ucontext->uc_mcontext.regs[29];
  *sp = ucontext->uc_mcontext.sp;
#elif defined(__hppa__)
  ucontext_t *ucontext = (ucontext_t*)context;
  *pc = ucontext->uc_mcontext.sc_iaoq[0];
  /* GCC uses %r3 whenever a frame pointer is needed.  */
  *bp = ucontext->uc_mcontext.sc_gr[3];
  *sp = ucontext->uc_mcontext.sc_gr[30];
#elif defined(__x86_64__)
# if SANITIZER_FREEBSD
  ucontext_t *ucontext = (ucontext_t*)context;
  *pc = ucontext->uc_mcontext.mc_rip;
  *bp = ucontext->uc_mcontext.mc_rbp;
  *sp = ucontext->uc_mcontext.mc_rsp;
#elif SANITIZER_NETBSD
  ucontext_t *ucontext = (ucontext_t *)context;
  *pc = ucontext->uc_mcontext.__gregs[_REG_RIP];
  *bp = ucontext->uc_mcontext.__gregs[_REG_RBP];
  *sp = ucontext->uc_mcontext.__gregs[_REG_RSP];
# else
  ucontext_t *ucontext = (ucontext_t*)context;
  *pc = ucontext->uc_mcontext.gregs[REG_RIP];
  *bp = ucontext->uc_mcontext.gregs[REG_RBP];
  *sp = ucontext->uc_mcontext.gregs[REG_RSP];
# endif
#elif defined(__i386__)
# if SANITIZER_FREEBSD
  ucontext_t *ucontext = (ucontext_t*)context;
  *pc = ucontext->uc_mcontext.mc_eip;
  *bp = ucontext->uc_mcontext.mc_ebp;
  *sp = ucontext->uc_mcontext.mc_esp;
#elif SANITIZER_NETBSD
  ucontext_t *ucontext = (ucontext_t *)context;
  *pc = ucontext->uc_mcontext.__gregs[_REG_EIP];
  *bp = ucontext->uc_mcontext.__gregs[_REG_EBP];
  *sp = ucontext->uc_mcontext.__gregs[_REG_ESP];
# else
  ucontext_t *ucontext = (ucontext_t*)context;
  *pc = ucontext->uc_mcontext.gregs[REG_EIP];
  *bp = ucontext->uc_mcontext.gregs[REG_EBP];
  *sp = ucontext->uc_mcontext.gregs[REG_ESP];
# endif
#elif defined(__powerpc__) || defined(__powerpc64__)
  ucontext_t *ucontext = (ucontext_t*)context;
  *pc = ucontext->uc_mcontext.regs->nip;
  *sp = ucontext->uc_mcontext.regs->gpr[PT_R1];
  // The powerpc{,64}-linux ABIs do not specify r31 as the frame
  // pointer, but GCC always uses r31 when we need a frame pointer.
  *bp = ucontext->uc_mcontext.regs->gpr[PT_R31];
#elif defined(__sparc__)
  ucontext_t *ucontext = (ucontext_t*)context;
  uptr *stk_ptr;
# if defined (__arch64__)
  *pc = ucontext->uc_mcontext.mc_gregs[MC_PC];
  *sp = ucontext->uc_mcontext.mc_gregs[MC_O6];
  stk_ptr = (uptr *) (*sp + 2047);
  *bp = stk_ptr[15];
# else
  *pc = ucontext->uc_mcontext.gregs[REG_PC];
  *sp = ucontext->uc_mcontext.gregs[REG_O6];
  stk_ptr = (uptr *) *sp;
  *bp = stk_ptr[15];
# endif
#elif defined(__mips__)
  ucontext_t *ucontext = (ucontext_t*)context;
  *pc = ucontext->uc_mcontext.pc;
  *bp = ucontext->uc_mcontext.gregs[30];
  *sp = ucontext->uc_mcontext.gregs[29];
#elif defined(__s390__)
  ucontext_t *ucontext = (ucontext_t*)context;
# if defined(__s390x__)
  *pc = ucontext->uc_mcontext.psw.addr;
# else
  *pc = ucontext->uc_mcontext.psw.addr & 0x7fffffff;
# endif
  *bp = ucontext->uc_mcontext.gregs[11];
  *sp = ucontext->uc_mcontext.gregs[15];
#else
# error "Unsupported arch"
#endif
}

void SignalContext::InitPcSpBp() { GetPcSpBp(context, &pc, &sp, &bp); }

void MaybeReexec() {
  // No need to re-exec on Linux.
}

void PrintModuleMap() { }

void CheckNoDeepBind(const char *filename, int flag) {
#ifdef RTLD_DEEPBIND
  if (flag & RTLD_DEEPBIND) {
    Report(
        "You are trying to dlopen a %s shared library with RTLD_DEEPBIND flag"
        " which is incompatibe with sanitizer runtime "
        "(see https://github.com/google/sanitizers/issues/611 for details"
        "). If you want to run %s library under sanitizers please remove "
        "RTLD_DEEPBIND from dlopen flags.\n",
        filename, filename);
    Die();
  }
#endif
}

uptr FindAvailableMemoryRange(uptr size, uptr alignment, uptr left_padding,
                              uptr *largest_gap_found) {
  UNREACHABLE("FindAvailableMemoryRange is not available");
  return 0;
}

bool GetRandom(void *buffer, uptr length, bool blocking) {
  if (!buffer || !length || length > 256)
    return false;
#if SANITIZER_USE_GETRANDOM
  static atomic_uint8_t skip_getrandom_syscall;
  if (!atomic_load_relaxed(&skip_getrandom_syscall)) {
    // Up to 256 bytes, getrandom will not be interrupted.
    uptr res = internal_syscall(SYSCALL(getrandom), buffer, length,
                                blocking ? 0 : GRND_NONBLOCK);
    int rverrno = 0;
    if (internal_iserror(res, &rverrno) && rverrno == ENOSYS)
      atomic_store_relaxed(&skip_getrandom_syscall, 1);
    else if (res == length)
      return true;
  }
#endif  // SANITIZER_USE_GETRANDOM
  // Up to 256 bytes, a read off /dev/urandom will not be interrupted.
  // blocking is moot here, O_NONBLOCK has no effect when opening /dev/urandom.
  uptr fd = internal_open("/dev/urandom", O_RDONLY);
  if (internal_iserror(fd))
    return false;
  uptr res = internal_read(fd, buffer, length);
  if (internal_iserror(res))
    return false;
  internal_close(fd);
  return true;
}

} // namespace __sanitizer

#endif  // SANITIZER_FREEBSD || SANITIZER_LINUX || SANITIZER_NETBSD
