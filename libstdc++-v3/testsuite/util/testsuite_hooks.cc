// -*- C++ -*-

// Utility subroutines for the C++ library testsuite. 
//
// Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007
// Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <testsuite_hooks.h>

#ifdef _GLIBCXX_RES_LIMITS
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include <list>
#include <string>
#include <stdexcept>
#include <cstddef>
#include <clocale>
#include <cstdlib>
#include <locale>
#include <cxxabi.h>

// If we have <sys/types.h>, <sys/ipc.h>, and <sys/sem.h>, then assume
// that System V semaphores are available.
#if defined(_GLIBCXX_HAVE_SYS_TYPES_H)		\
    && defined(_GLIBCXX_HAVE_SYS_IPC_H)		\
    && defined(_GLIBCXX_HAVE_SYS_SEM_H)
#define _GLIBCXX_SYSV_SEM
#endif

#ifdef _GLIBCXX_SYSV_SEM
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#endif

namespace __gnu_test
{
#ifdef _GLIBCXX_RES_LIMITS
  void 
  set_memory_limits(float size)
  {
    struct rlimit r;
    // Cater to the absence of rlim_t.
    __typeof__ (r.rlim_cur) limit = (__typeof__ (r.rlim_cur))(size * 1048576);

    // Heap size, seems to be common.
#if _GLIBCXX_HAVE_LIMIT_DATA
    getrlimit(RLIMIT_DATA, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_DATA, &r);
#endif

    // Resident set size.
#if _GLIBCXX_HAVE_LIMIT_RSS
    getrlimit(RLIMIT_RSS, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_RSS, &r);
#endif

    // Mapped memory (brk + mmap).
#if _GLIBCXX_HAVE_LIMIT_VMEM
    getrlimit(RLIMIT_VMEM, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_VMEM, &r);
#endif

    // Virtual memory.  On x86_64-linux, the default is -z
    // max-page-size=0x200000 which means up to 2MB of address space
    // are accounted for PROT_NONE mappings between text and data
    // segments of each shared library.  There are 4 shared libs
    // involved in addition to the dynamic linker, maybe 5 if libgomp
    // is being used as well.  Use at least 20MB address space limit.
#if defined(__x86_64__) && defined(__linux__)
    if (limit < 20971520)
      limit = 20971520;
#endif

    // On HP-UX 11.23, a trivial C++ program that sets RLIMIT_AS to
    // anything less than 128MB cannot "malloc" even 1K of memory.
    // Therefore, we skip RLIMIT_AS on HP-UX.
#if _GLIBCXX_HAVE_LIMIT_AS && !defined(__hpux__)
    getrlimit(RLIMIT_AS, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_AS, &r);
#endif
  }

#else
  void
  set_memory_limits(float) { }
#endif 

#ifdef _GLIBCXX_RES_LIMITS
  void 
  set_file_limit(unsigned long size)
  {
#if _GLIBCXX_HAVE_LIMIT_FSIZE
    struct rlimit r;
    // Cater to the absence of rlim_t.
    __typeof__ (r.rlim_cur) limit = (__typeof__ (r.rlim_cur))(size);

    getrlimit(RLIMIT_FSIZE, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_FSIZE, &r);
#endif
  }

#else
  void
  set_file_limit(unsigned long) { }
#endif 

  void 
  verify_demangle(const char* mangled, const char* wanted)
  {
    int status = 0;
    const char* s = abi::__cxa_demangle(mangled, 0, 0, &status);
    if (!s)
      {
	switch (status)
	  {
	  case 0:
	    s = "error code = 0: success";
	    break;
	  case -1:
	    s = "error code = -1: memory allocation failure";
	    break;
	  case -2:
	    s = "error code = -2: invalid mangled name";
	    break;
	  case -3:
	    s = "error code = -3: invalid arguments";
	    break;
	  default:
	    s = "error code unknown - who knows what happened";
	  }
      }

    std::string w(wanted);
    if (w != s)
      std::__throw_runtime_error(s);
  }

  void 
  run_tests_wrapped_locale(const char* name, const func_callback& l)
  {
    using namespace std;
    
    // Set the global locale. 
    locale loc_name = locale(name);
    locale orig = locale::global(loc_name);

    const char* res = setlocale(LC_ALL, name);
    if (res != NULL)
      {
	string preLC_ALL = res;
	const func_callback::test_type* tests = l.tests();
	for (int i = 0; i < l.size(); ++i)
	  (*tests[i])();
	string postLC_ALL= setlocale(LC_ALL, NULL);
	VERIFY( preLC_ALL == postLC_ALL );
      }
    else
      {
	string s("LC_ALL for ");
	s += name;
	__throw_runtime_error(s.c_str());
      }
  }
  
  void 
  run_tests_wrapped_env(const char* name, const char* env,
			const func_callback& l)
  {
    using namespace std;
    
#ifdef _GLIBCXX_HAVE_SETENV 
    // Set the global locale. 
    locale loc_name = locale(name);
    locale orig = locale::global(loc_name);

    // Set environment variable env to value in name. 
    const char* oldENV = getenv(env);
    if (!setenv(env, name, 1))
      {
	const func_callback::test_type* tests = l.tests();
	for (int i = 0; i < l.size(); ++i)
	  (*tests[i])();
	setenv(env, oldENV ? oldENV : "", 1);
      }
    else
      {
	string s(env);
	s += string(" to ");
	s += string(name);
	__throw_runtime_error(s.c_str());
      }
#endif
  }

  counter::size_type  counter::count = 0;
  unsigned int copy_constructor::count_ = 0;
  unsigned int copy_constructor::throw_on_ = 0;
  unsigned int assignment_operator::count_ = 0;
  unsigned int assignment_operator::throw_on_ = 0;
  unsigned int destructor::_M_count = 0;
  int copy_tracker::next_id_ = 0;

#ifdef _GLIBCXX_SYSV_SEM
  // This union is not declared in system headers.  Instead, it must
  // be defined by user programs.
  union semun 
  {
    int val;
    struct semid_ds *buf;
    unsigned short *array;
  };
#endif

  semaphore::semaphore() 
  {
#ifdef _GLIBCXX_SYSV_SEM
    // Remeber the PID for the process that created the semaphore set
    // so that only one process will destroy the set.
    pid_ = getpid();

    // GLIBC does not define SEM_R and SEM_A.
#ifndef SEM_R
#define SEM_R 0400
#endif
    
#ifndef SEM_A
#define SEM_A 0200
#endif

    // Get a semaphore set with one semaphore.
    sem_set_ = semget(IPC_PRIVATE, 1, SEM_R | SEM_A);
    if (sem_set_ == -1)
      std::__throw_runtime_error("could not obtain semaphore set");

    // Initialize the semaphore.
    union semun val;
    val.val = 0;
    if (semctl(sem_set_, 0, SETVAL, val) == -1)
      std::__throw_runtime_error("could not initialize semaphore");
#else
    // There are no semaphores on this system.  We have no way to mark
    // a test as "unsupported" at runtime, so we just exit, pretending
    // that the test passed.
    exit(0);
#endif
  }

  semaphore::~semaphore() 
  {
#ifdef _GLIBCXX_SYSV_SEM
    union semun val;
    // Destroy the semaphore set only in the process that created it. 
    if (pid_ == getpid())
      semctl(sem_set_, 0, IPC_RMID, val);
#endif
  }

  void
  semaphore::signal() 
  {
#ifdef _GLIBCXX_SYSV_SEM
    struct sembuf op[1] = 
      {
	{ 0, 1, 0 }
      };
    if (semop(sem_set_, op, 1) == -1)
      std::__throw_runtime_error("could not signal semaphore");
#endif
  }

  void
  semaphore::wait() 
  {
#ifdef _GLIBCXX_SYSV_SEM
    struct sembuf op[1] = 
      {
	{ 0, -1, SEM_UNDO }
      };
    if (semop(sem_set_, op, 1) == -1)
      std::__throw_runtime_error("could not wait for semaphore");
#endif    
  }

  // For use in 22_locale/time_get and time_put.
  std::tm
  test_tm(int sec, int min, int hour, int mday, int mon,
	  int year, int wday, int yday, int isdst)
  {
    static std::tm tmp;
    tmp.tm_sec = sec;
    tmp.tm_min = min;
    tmp.tm_hour = hour;
    tmp.tm_mday = mday;
    tmp.tm_mon = mon;
    tmp.tm_year = year;
    tmp.tm_wday = wday;
    tmp.tm_yday = yday;
    tmp.tm_isdst = isdst;
    return tmp;
  }
} // namespace __gnu_test
