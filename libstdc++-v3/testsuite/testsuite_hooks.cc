// Utility subroutines for the C++ library testsuite.
//
// Copyright (C) 2002 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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

#ifdef _GLIBCPP_MEM_LIMITS
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

void
__set_testsuite_memlimit(float __size)
{
    struct rlimit r;
    // Cater to the absence of rlim_t.
    __typeof__ (r.rlim_cur) limit
      = (__typeof__ (r.rlim_cur))(__size * 1048576);

    // Heap size, seems to be common.
#if _GLIBCPP_HAVE_MEMLIMIT_DATA
    getrlimit(RLIMIT_DATA, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_DATA, &r);
#endif

    // Resident set size.
#if _GLIBCPP_HAVE_MEMLIMIT_RSS
    getrlimit(RLIMIT_RSS, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_RSS, &r);
#endif

    // Mapped memory (brk + mmap).
#if _GLIBCPP_HAVE_MEMLIMIT_VMEM
    getrlimit(RLIMIT_VMEM, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_VMEM, &r);
#endif

    // Virtual memory.
#if _GLIBCPP_HAVE_MEMLIMIT_AS
    getrlimit(RLIMIT_AS, &r);
    r.rlim_cur = limit;
    setrlimit(RLIMIT_AS, &r);
#endif
}
#else
void
__set_testsuite_memlimit(float) { }
#endif /* _GLIBCPP_MEM_LIMITS */


gnu_counting_struct::size_type  gnu_counting_struct::count = 0;

unsigned int gnu_copy_constructor::count_ = 0;
unsigned int gnu_copy_constructor::throw_on_ = 0;
unsigned int gnu_assignment_operator::count_ = 0;
unsigned int gnu_assignment_operator::throw_on_ = 0;
unsigned int gnu_destructor::count_ = 0;
int gnu_copy_tracker::next_id_ = 0;

