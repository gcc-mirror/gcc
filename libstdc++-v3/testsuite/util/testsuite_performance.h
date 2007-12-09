// -*- C++ -*-
// Testing performance utilities for the C++ library testsuite.
//
// Copyright (C) 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_PERFORMANCE_H
#define _GLIBCXX_PERFORMANCE_H

#include <sys/times.h>
#include <sys/resource.h>
#include <cstdlib>
#include <cstring>
#include <string>
#include <fstream>
#include <iomanip>

#ifdef __linux__
#include <malloc.h>
#elif defined (__FreeBSD__)
extern "C"
{
  struct mallinfo
  {
    int uordblks;
    int hblkhd;
  };

  struct mallinfo
  mallinfo(void)
  {
    struct mallinfo m = { (((size_t) sbrk (0) + 1023) / 1024), 0 };
    return m;
  }
}
#elif !defined (__hpux__)
extern "C"
{
  struct mallinfo
  {
    int uordblks;
    int hblkhd;
  };

  struct mallinfo empty = { 0, 0 };

  struct mallinfo
  mallinfo(void)
  { return empty; }
}
#endif

namespace __gnu_test
{
  class time_counter
  {
  private:
    clock_t	elapsed_begin;
    clock_t	elapsed_end;
    tms		tms_begin;
    tms		tms_end;

  public:
    explicit
    time_counter() : elapsed_begin(), elapsed_end(), tms_begin(), tms_end()
    { }

    void 
    clear() throw()
    {
      elapsed_begin = clock_t();
      elapsed_end = clock_t();
      tms_begin = tms();
      tms_end = tms();
    }

    void
    start()
    { 
      this->clear();
      elapsed_begin = times(&tms_begin); 
      const clock_t err = clock_t(-1);
      if (elapsed_begin == err)
	std::__throw_runtime_error("time_counter::start");
    }
    
    void
    stop()
    { 
      elapsed_end = times(&tms_end); 
      const clock_t err = clock_t(-1);
      if (elapsed_end == err)
	std::__throw_runtime_error("time_counter::stop");
    }

    size_t
    real_time() const
    { return elapsed_end - elapsed_begin; }

    size_t
    user_time() const
    { return tms_end.tms_utime - tms_begin.tms_utime; }

    size_t
    system_time() const
    { return tms_end.tms_stime - tms_begin.tms_stime; }
  };

  class resource_counter
  {
    int                 who;
    rusage	        rusage_begin;
    rusage	        rusage_end;
    struct mallinfo  	allocation_begin;
    struct mallinfo  	allocation_end;

  public:
    resource_counter(int i = RUSAGE_SELF) : who(i)
    { this->clear(); }
    
    void 
    clear() throw()
    { 
      memset(&rusage_begin, 0, sizeof(rusage_begin)); 
      memset(&rusage_end, 0, sizeof(rusage_end)); 
      memset(&allocation_begin, 0, sizeof(allocation_begin)); 
      memset(&allocation_end, 0, sizeof(allocation_end)); 
    }

    void
    start()
    { 
      if (getrusage(who, &rusage_begin) != 0 )
	memset(&rusage_begin, 0, sizeof(rusage_begin));
      malloc(0); // Needed for some implementations.
      allocation_begin = mallinfo();
    }
    
    void
    stop()
    { 
      if (getrusage(who, &rusage_end) != 0 )
	memset(&rusage_end, 0, sizeof(rusage_end));
      allocation_end = mallinfo();
    }

    int
    allocated_memory() const
    { return ((allocation_end.uordblks - allocation_begin.uordblks)
	      + (allocation_end.hblkhd - allocation_begin.hblkhd)); }
    
    long 
    hard_page_fault() const
    { return rusage_end.ru_majflt - rusage_begin.ru_majflt; }

    long 
    swapped() const
    { return rusage_end.ru_nswap - rusage_begin.ru_nswap; }
  };

  inline void
  start_counters(time_counter& t, resource_counter& r)
  {
    t.start();
    r.start();
  }

  inline void
  stop_counters(time_counter& t, resource_counter& r)
  {
    t.stop();
    r.stop();
  }

  inline void
  clear_counters(time_counter& t, resource_counter& r)
  {
    t.clear();
    r.clear();
  }

  void
  report_performance(const std::string file, const std::string comment, 
		     const time_counter& t, const resource_counter& r)
  {
    const char space = ' ';
    const char tab = '\t';
    const char* name = "libstdc++-performance.sum";
    std::string::const_iterator i = file.begin() + file.find_last_of('/') + 1;
    std::string testname(i, file.end());

    std::ofstream out(name, std::ios_base::app);

#ifdef __GTHREADS
    if (__gthread_active_p())
      testname.append("-thread");
#endif

    out.setf(std::ios_base::left);
    out << std::setw(25) << testname << tab;
    out << std::setw(25) << comment << tab;

    out.setf(std::ios_base::right);
    out << std::setw(4) << t.real_time() << "r" << space;
    out << std::setw(4) << t.user_time() << "u" << space;
    out << std::setw(4) << t.system_time() << "s" << space;
    out << std::setw(8) << r.allocated_memory() << "mem" << space;
    out << std::setw(4) << r.hard_page_fault() << "pf" << space;
    
    out << std::endl;
    out.close();
  }

  void
  report_header(const std::string file, const std::string header)
  {
    const char space = ' ';
    const char tab = '\t';
    const char* name = "libstdc++-performance.sum";
    std::string::const_iterator i = file.begin() + file.find_last_of('/') + 1;
    std::string testname(i, file.end());

    std::ofstream out(name, std::ios_base::app);

#ifdef __GTHREADS
    if (__gthread_active_p ())
      testname.append("-thread");
#endif

    out.setf(std::ios_base::left);
    out << std::setw(25) << testname << tab;
    out << std::setw(40) << header << tab;

    out << std::endl;
    out.close();
  }
} // namespace __gnu_test

#endif // _GLIBCXX_PERFORMANCE_H

