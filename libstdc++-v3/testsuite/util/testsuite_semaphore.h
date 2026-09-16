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

#ifndef _GLIBCXX_TESTSUITE_SEMAPHORE_H
#define _GLIBCXX_TESTSUITE_SEMAPHORE_H

#include <bits/c++config.h>

#ifdef _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
#endif

namespace __gnu_test
{
  // A binary semaphore for use across multiple processes.
  class semaphore
  {
  public:
    // Creates a binary semaphore.  The semaphore is initially in the
    // unsignaled state.
    semaphore();

    // Destroy the semaphore.
    ~semaphore();

    // Signal the semaphore.  If there are processes blocked in
    // "wait", exactly one will be permitted to proceed.
    void signal();

    // Wait until the semaphore is signaled.
    void wait();

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++11-extensions"
    semaphore(const semaphore&) = delete;
    semaphore& operator=(const semaphore&) = delete;
#pragma GCC diagnostic pop

  private:
    union {
      int sem_set_;
      void* sem_;
    };
    pid_t pid_;
  };

} // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_SEMAPHORE_H

