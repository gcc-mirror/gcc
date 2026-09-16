// -*- C++ -*-

// Semaphore type for the C++ library testsuite.
//
// Copyright (C) 2005-2026 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

#include "testsuite_semaphore.h"
#include <bits/stdexcept_throw.h>

#if defined(_GLIBCXX_HAVE_SYS_TYPES_H)	\
    && defined(_GLIBCXX_HAVE_SYS_IPC_H)	\
    && defined(_GLIBCXX_HAVE_SYS_SEM_H)
// If we have <sys/types.h>, <sys/ipc.h>, and <sys/sem.h>, then assume
// that System V semaphores are available.
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/sem.h>
# define _GLIBCXX_SYSV_SEM
#elif defined(_POSIX_SEMAPHORES) && _POSIX_SEMAPHORES > 0
# include <semaphore.h> // sem_init, sem_post etc.
# include <sys/mman.h>  // mmap, MAP_SHARED etc.
# include <errno.h>     // EINTR
# include <fcntl.h>     // open, O_RDWR
# define _GLIBCXX_POSIX_SEM
#else
# include <stdlib.h> // abort
#endif

namespace __gnu_test
{
#ifdef _GLIBCXX_SYSV_SEM
  // This union is not declared in system headers.  Instead, it must
  // be defined by user programs.
  union semun
  {
    int val = 0;
    struct semid_ds *buf;
    unsigned short *array;
  };

  static void
  destroy_semset(int set)
  {
    semctl(set, 0, IPC_RMID, semun{});
  }
#endif

#ifdef _GLIBCXX_POSIX_SEM
  // Create a new anonymous memory mapping that will be shared after fork().
  static void*
  mmap_anon(size_t len)
  {
    int flags = MAP_SHARED;
    int fd = -1;
#ifdef MAP_ANONYMOUS
    flags |= MAP_ANONYMOUS;
#elif defined MAP_ANON
    flags |= MAP_ANON;
#else
    // Use the older idiom of mapping pages from /dev/zero
    fd = open("/dev/zero", O_RDWR);
    if (fd == -1)
      std::__throw_runtime_error("could not open /dev/zero for mmap");
#endif

    void* p = mmap(nullptr, len, PROT_READ | PROT_WRITE, flags, fd, 0);
    if (fd != -1)
      close(fd);
    if (p == MAP_FAILED)
      std::__throw_runtime_error("could not create mmap mapping for semaphore");
    return p;
  }
#endif

  semaphore::semaphore()
  {
#ifdef _GLIBCXX_SYSV_SEM
    // Get a System V semaphore set with one semaphore.
    sem_set_ = semget(IPC_PRIVATE, 1, 0600);
    if (sem_set_ == -1)
      std::__throw_runtime_error("could not obtain semaphore set");

    // Initialize the semaphore.
    if (semctl(sem_set_, 0, SETVAL, semun{}) == -1)
      {
	destroy_semset(sem_set_);
	std::__throw_runtime_error("could not initialize semaphore");
      }
#elif defined _GLIBCXX_POSIX_SEM
    // Map some shared memory and initialize a POSIX semaphore there.
    sem_ = mmap_anon(sizeof(sem_t));
    if (sem_init((sem_t*)sem_, 1, 0))
      {
	munmap(sem_, sizeof(sem_t));
	std::__throw_runtime_error("could not init semaphore in mmap mapping");
      }
#else
    // There are no semaphores on this system so make the test FAIL at runtime.
    // Use { dg-require-sysv-or-posix-semaphore "" } to make it UNSUPPORTED.
    abort();
#endif

    // Remember the PID for the process that created the semaphore
    // so that only one process will destroy it.
    pid_ = getpid();
  }

  semaphore::~semaphore()
  {
    // Destroy the semaphore set only in the process that created it.
#ifdef _GLIBCXX_SYSV_SEM
    if (pid_ == getpid())
      destroy_semset(sem_set_);
#elif defined _GLIBCXX_POSIX_SEM
    if (pid_ == getpid())
      sem_destroy((sem_t*)sem_);
    munmap(sem_, sizeof(sem_t));
#endif
  }

  void
  semaphore::signal()
  {
#ifdef _GLIBCXX_SYSV_SEM
    struct sembuf op[1] = { };
    op[0].sem_op = 1;
    if (semop(sem_set_, op, 1) == -1)
      std::__throw_runtime_error("could not signal semaphore");
#elif defined _GLIBCXX_POSIX_SEM
    if (sem_post((sem_t*)sem_))
      std::__throw_runtime_error("could not signal semaphore");
#endif
  }

  void
  semaphore::wait()
  {
#ifdef _GLIBCXX_SYSV_SEM
    struct sembuf op[1] = { };
    op[0].sem_op = -1;
    op[0].sem_flg = SEM_UNDO;
    if (semop(sem_set_, op, 1) == -1)
      std::__throw_runtime_error("could not wait for semaphore");
#elif defined _GLIBCXX_POSIX_SEM
    while (true)
      {
	if (sem_wait((sem_t*)sem_) == 0)
	  return;
	if (errno != EINTR)
	  std::__throw_runtime_error("could not wait for semaphore");
      }
#endif
  }
} // namespace __gnu_test
