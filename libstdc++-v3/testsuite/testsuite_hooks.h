// -*- C++ -*-
// Utility subroutines for the C++ library testsuite. 
//
// Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005
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

// This file provides the following:
//
// 1)  VERIFY(), via _GLIBCXX_ASSERT, from Brent Verner <brent@rcfile.org>.
//   This file is included in the various testsuite programs to provide
//   #define(able) assert() behavior for debugging/testing. It may be
//   a suitable location for other furry woodland creatures as well.
//
// 2)  set_memory_limits()
//   set_memory_limits() uses setrlimit() to restrict dynamic memory
//   allocation.  We provide a default memory limit if none is passed by the
//   calling application.  The argument to set_memory_limits() is the
//   limit in megabytes (a floating-point number).  If _GLIBCXX_RES_LIMITS is
//   not #defined before including this header, then no limiting is attempted.
//
// 3)  counter
//   This is a POD with a static data member, gnu_counting_struct::count,
//   which starts at zero, increments on instance construction, and decrements
//   on instance destruction.  "assert_count(n)" can be called to VERIFY()
//   that the count equals N.
//
// 4)  copy_tracker, from Stephen M. Webb <stephen@bregmasoft.com>.
//   A class with nontrivial ctor/dtor that provides the ability to track the
//   number of copy ctors and dtors, and will throw on demand during copy.

#ifndef _GLIBCXX_TESTSUITE_HOOKS_H
#define _GLIBCXX_TESTSUITE_HOOKS_H

#include <bits/c++config.h>
#include <bits/functexcept.h>
#include <cstddef>
#include <locale>
#ifdef _GLIBCXX_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef _GLIBCXX_ASSERT
# include <cassert>
# define VERIFY(fn) assert(fn)
#else
# define VERIFY(fn) test &= (fn)
#endif

#ifdef _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
#else
# define unlink(x)
#endif

namespace __gnu_test
{
  // All macros are defined in GLIBCXX_CONFIGURE_TESTSUITE and imported
  // from c++config.h

  // Set memory limits if possible, if not set to 0.
#ifndef _GLIBCXX_RES_LIMITS
#  define MEMLIMIT_MB 0
#else
# ifndef MEMLIMIT_MB
#  define MEMLIMIT_MB 16.0
# endif
#endif
  extern void
  set_memory_limits(float __size = MEMLIMIT_MB);

  extern void
  set_file_limit(unsigned long __size);

  // Check mangled name demangles (using __cxa_demangle) as expected.
  void
  verify_demangle(const char* mangled, const char* wanted);

  // 17.3.2.1.2 - Bitmask types [lib.bitmask.types]
  // bitmask_operators
  template<typename bitmask_type>
    void
    bitmask_operators()
    {
      bitmask_type a;
      bitmask_type b;
      a | b;
      a & b;
      a ^ b;
      ~b;
      a |= b; // set
      a &= ~b; // clear
      a ^= b;
    }

  // Simple callback structure for variable numbers of tests (all with
  // same signature).  Assume all unit tests are of the signature
  // void test01(); 
  class func_callback
  {
  public:
    typedef void (*test_type) (void);

  private:
    int		_M_size;
    test_type	_M_tests[15];

    func_callback&
    operator=(const func_callback&);

    func_callback(const func_callback&);

  public:
    func_callback(): _M_size(0) { };

    int
    size() const { return _M_size; }

    const test_type*
    tests() const { return _M_tests; }

    void
    push_back(test_type test)
    {
      _M_tests[_M_size] = test;
      ++_M_size;
    }
  };


  // Run select unit tests after setting global locale.
  void 
  run_tests_wrapped_locale(const char*, const func_callback&);

  // Run select unit tests after setting environment variables.
  void 
  run_tests_wrapped_env(const char*, const char*, const func_callback&);

  // Counting.
  struct counter
  {
    // Specifically and glaringly-obviously marked 'signed' so that when
    // COUNT mistakenly goes negative, we can track the patterns of
    // deletions more easily.
    typedef  signed int     size_type;
    static size_type   count;
    counter() { ++count; }
    counter (const counter&) { ++count; }
    ~counter() { --count; }
  };
  
#define assert_count(n)   VERIFY(__gnu_test::counter::count == n)
  
  // A (static) class for counting copy constructors and possibly throwing an
  // exception on a desired count.
  class copy_constructor
  {
  public:
    static unsigned int
    count() { return count_; }
    
    static void
    mark_call()
    {
      count_++;
      if (count_ == throw_on_)
	std::__throw_runtime_error("copy_constructor::mark_call");
    }
      
    static void
    reset()
    {
      count_ = 0;
      throw_on_ = 0;
    }
      
    static void
    throw_on(unsigned int count) { throw_on_ = count; }

  private:
    static unsigned int count_;
    static unsigned int throw_on_;
  };
  
  // A (static) class for counting assignment operator calls and
  // possibly throwing an exception on a desired count.
  class assignment_operator
  {
  public:
    static unsigned int
    count() { return count_; }
    
    static void
    mark_call()
    {
      count_++;
      if (count_ == throw_on_)
	std::__throw_runtime_error("assignment_operator::mark_call");
    }

    static void
    reset()
    {
      count_ = 0;
      throw_on_ = 0;
    }

    static void
    throw_on(unsigned int count) { throw_on_ = count; }

  private:
    static unsigned int count_;
    static unsigned int throw_on_;
  };
  
  // A (static) class for tracking calls to an object's destructor.
  class destructor
  {
  public:
    static unsigned int
    count() { return _M_count; }
    
    static void
    mark_call() { _M_count++; }

    static void
    reset() { _M_count = 0; }

  private:
    static unsigned int _M_count;
  };
  
  // An class of objects that can be used for validating various
  // behaviours and guarantees of containers and algorithms defined in
  // the standard library.
  class copy_tracker
  {
  public:
    // Creates a copy-tracking object with the given ID number.  If
    // "throw_on_copy" is set, an exception will be thrown if an
    // attempt is made to copy this object.
    copy_tracker(int id = next_id_--, bool throw_on_copy = false)
    : id_(id) , throw_on_copy_(throw_on_copy) { }

    // Copy-constructs the object, marking a call to the copy
    // constructor and forcing an exception if indicated.
    copy_tracker(const copy_tracker& rhs)
    : id_(rhs.id()), throw_on_copy_(rhs.throw_on_copy_)
    {
      if (throw_on_copy_)
	copy_constructor::throw_on(copy_constructor::count() + 1);
      copy_constructor::mark_call();
    }

    // Assigns the value of another object to this one, tracking the
    // number of times this member function has been called and if the
    // other object is supposed to throw an exception when it is
    // copied, well, make it so.
    copy_tracker&
    operator=(const copy_tracker& rhs)
    { 
      id_ = rhs.id();
      if (rhs.throw_on_copy_)
        assignment_operator::throw_on(assignment_operator::count() + 1);
      assignment_operator::mark_call();
      return *this;
    }

    ~copy_tracker()
    { destructor::mark_call(); }

    int
    id() const { return id_; }

  private:
    int   id_;
    const bool  throw_on_copy_;

  public:
    static void
    reset()
    {
      copy_constructor::reset();
      assignment_operator::reset();
      destructor::reset();
    }

    // for backwards-compatibility
    static int
    copyCount() 
    { return copy_constructor::count(); }

    // for backwards-compatibility
    static int
    dtorCount() 
    { return destructor::count(); }

  private:
    static int next_id_;
  };

  inline bool
  operator==(const copy_tracker& lhs, const copy_tracker& rhs)
  { return lhs.id() == rhs.id(); }

  // Class for checking required type conversions, implicit and
  // explicit for given library data structures. 
  template<typename _Container>
    struct conversion
    {
      typedef typename _Container::const_iterator const_iterator;
      
      // Implicit conversion iterator to const_iterator.
      static const_iterator
      iterator_to_const_iterator()
      {
	_Container v;
	const_iterator it = v.begin();
	const_iterator end = v.end();
	return it == end ? v.end() : it;
      }
    };

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

  private:
    int sem_set_;

    pid_t pid_;
  };
} // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_HOOKS_H

