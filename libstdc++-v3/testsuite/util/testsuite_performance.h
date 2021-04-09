// -*- C++ -*-
// Testing performance utilities for the C++ library testsuite.
//
// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_PERFORMANCE_H
#define _GLIBCXX_PERFORMANCE_H

#include <sys/times.h>
#include <sys/resource.h>
#include <cstdlib>
#include <cstring>
#include <string>
#include <fstream>
#include <iomanip>
#include <typeinfo>
#include <stdexcept>
#include <sstream>
#include <cxxabi.h>
#include <testsuite_common_types.h>

#if defined (__linux__) || defined (__GLIBC__)
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
    struct mallinfo m = { (((std::size_t) sbrk (0) + 1023) / 1024), 0 };
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
    std::size_t splits[3];

  public:
    explicit
    time_counter()
    : elapsed_begin(), elapsed_end(), tms_begin(), tms_end(), splits()
    { }

    void
    clear() throw()
    {
      elapsed_begin = clock_t();
      elapsed_end = clock_t();
      tms_begin = tms();
      tms_end = tms();
      splits[0] = splits[1] = splits[2] = 0;
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

    void
    restart()
    {
      splits[0] += (elapsed_end - elapsed_begin);
      splits[1] += (tms_end.tms_utime - tms_begin.tms_utime);
      splits[2] += (tms_end.tms_stime - tms_begin.tms_stime);
      elapsed_begin = times(&tms_begin);
      const clock_t err = clock_t(-1);
      if (elapsed_begin == err)
	std::__throw_runtime_error("time_counter::restart");
    }

    std::size_t
    real_time() const
    { return (elapsed_end - elapsed_begin) + splits[0]; }

    std::size_t
    user_time() const
    { return (tms_end.tms_utime - tms_begin.tms_utime) + splits[1]; }

    std::size_t
    system_time() const
    { return (tms_end.tms_stime - tms_begin.tms_stime) + splits[1]; }
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
      void* p __attribute__((unused)) = malloc(0); // Needed for some implementations.
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


// Ah, we wish it wasn't so...
bool first_container = false;
extern const char* filename;

typedef std::string::size_type (*callback_type) (std::string&);

template<typename Container, int Iter, bool Thread>
  void
  write_viz_container(callback_type find_container, const char* filename)
  {
    typedef std::string string;

    // Create title.
    {
      const char ws(' ');
      std::ostringstream title;

      std::string titlename(filename);
      std::string::size_type n = titlename.find('.');
      if (n != string::npos)
	titlename = std::string(titlename.begin(), titlename.begin() + n);

      title << titlename;
      title << ws;
      title << Iter;
      title << ws;
#if 0
      title << "thread<";
      std::boolalpha(title);
      title << Thread;
      title << '>';
#endif

      titlename += ".title";
      std::ofstream titlefile(titlename.c_str());
      if (!titlefile.good())
	throw std::runtime_error("write_viz_data cannot open titlename");
      titlefile << title.str() << std::endl;
    }

    // Create compressed type name.
    Container obj;
    int status;
    std::string type(abi::__cxa_demangle(typeid(obj).name(), 0, 0, &status));

    // Extract fully-qualified typename.
    // Assumes "set" or "map" are uniquely determinate.
    string::iterator beg = type.begin();
    string::iterator end;
    string::size_type n = (*find_container)(type);

    // Find start of fully-qualified name.
    // Assume map, find end.
    string::size_type nend = type.find('<', n);
    if (nend != string::npos)
      end = type.begin() + nend;

    string compressed_type;
    compressed_type += '"';
    compressed_type += string(beg, end);
    compressed_type += '<';
#if 0
    typename Container::key_type v;
    compressed_type += typeid(v).name();
#else
    compressed_type += "int";
#endif
    compressed_type += ", A>";

    // XXX
    if (Thread == true)
      compressed_type += " thread";
    compressed_type += '"';

    std::ofstream file(filename, std::ios_base::app);
    if (!file.good())
      throw std::runtime_error("write_viz_data cannot open filename");

    file << compressed_type;
    first_container = false;
  }


void
write_viz_data(__gnu_test::time_counter& time, const char* filename)
{
  std::ofstream file(filename, std::ios_base::app);
  if (!file.good())
    throw std::runtime_error("write_viz_data cannot open filename");

  // Print out score in appropriate column.
  const char tab('\t');
  int score = time.real_time();
  file << tab << score;
}

void
write_viz_endl(const char* filename)
{
  std::ofstream file(filename, std::ios_base::app);
  if (!file.good())
    throw std::runtime_error("write_viz_endl cannot open filename");
  file << std::endl;
}


// Function template, function objects for the tests.
template<typename TestType>
  struct value_type : public std::pair<const TestType, TestType>
  {
    inline value_type& operator++()
    {
      ++this->second;
      return *this;
    }

    inline operator TestType() const { return this->second; }
  };

template<typename Container, int Iter>
  void
  do_loop();

template<typename Container, int Iter>
  void*
  do_thread(void* p = 0)
  {
    do_loop<Container, Iter>();
    return p;
  }

template<typename Container, int Iter, bool Thread>
  void
  test_container(const char* filename)
  {
    using namespace __gnu_test;
    time_counter time;
    resource_counter resource;
    {
      start_counters(time, resource);
      if (!Thread)
	{
	  // No threads, so run 4x.
	  do_loop<Container, Iter * 4>();
	}
      else
	{
#if defined (_GLIBCXX_GCC_GTHR_POSIX_H) && !defined (NOTHREAD)
	  pthread_t  t1, t2, t3, t4;
	  pthread_create(&t1, 0, &do_thread<Container, Iter>, 0);
	  pthread_create(&t2, 0, &do_thread<Container, Iter>, 0);
	  pthread_create(&t3, 0, &do_thread<Container, Iter>, 0);
	  pthread_create(&t4, 0, &do_thread<Container, Iter>, 0);

	  pthread_join(t1, 0);
	  pthread_join(t2, 0);
	  pthread_join(t3, 0);
	  pthread_join(t4, 0);
#endif
	}
      stop_counters(time, resource);

      // Detailed text data.
      Container obj;
      int status;
      std::ostringstream comment;
      comment << "type: " << abi::__cxa_demangle(typeid(obj).name(),
                                                 0, 0, &status);
      report_header(filename, comment.str());
      report_performance("", "", time, resource);

      // Detailed data for visualization.
      std::string vizfilename(filename);
      vizfilename += ".dat";
      write_viz_data(time, vizfilename.c_str());
    }
  }

template<bool Thread>
  struct test_sequence
  {
    test_sequence(const char* filename) : _M_filename(filename) { }

    template<class Container>
      void
      operator()(Container)
      {
	const int i = 20000;
	test_container<Container, i, Thread>(_M_filename);
      }

  private:
    const char* _M_filename;
  };


inline std::string::size_type
sequence_find_container(std::string& type)
{
  const std::string::size_type npos = std::string::npos;
  std::string::size_type n1 = type.find("vector");
  std::string::size_type n2 = type.find("list");
  std::string::size_type n3 = type.find("deque");
  std::string::size_type n4 = type.find("string");

  if (n1 != npos || n2 != npos || n3 != npos || n4 != npos)
    return std::min(std::min(n1, n2), std::min(n3, n4));
  else
    throw std::runtime_error("sequence_find_container not found");
}

inline std::string::size_type
associative_find_container(std::string& type)
{
  using std::string;
  string::size_type n1 = type.find("map");
  string::size_type n2 = type.find("set");
  if (n1 != string::npos || n2 != string::npos)
    return std::min(n1, n2);
  else
    throw std::runtime_error("associative_find_container not found");
}

#endif // _GLIBCXX_PERFORMANCE_H

