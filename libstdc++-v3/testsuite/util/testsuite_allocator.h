// -*- C++ -*-
// Testing allocator for the C++ library testsuite.
//
// Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
// Free Software Foundation, Inc.
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

// This file provides an test instrumentation allocator that can be
// used to verify allocation functionality of standard library
// containers.  2002.11.25 smw

#ifndef _GLIBCXX_TESTSUITE_ALLOCATOR_H
#define _GLIBCXX_TESTSUITE_ALLOCATOR_H

#include <tr1/unordered_map>
#include <cassert>

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#include <bits/move.h>
#endif

namespace __gnu_test
{
  class tracker_allocator_counter
  {
  public:
    typedef std::size_t    size_type; 
    
    static void*
    allocate(size_type blocksize)
    {
      allocationCount_ += blocksize;
      return ::operator new(blocksize);
    }
    
    static void
    construct() { constructCount_++; }

    static void
    destroy() { destructCount_++; }

    static void
    deallocate(void* p, size_type blocksize)
    {
      ::operator delete(p);
      deallocationCount_ += blocksize;
    }
    
    static size_type
    get_allocation_count() { return allocationCount_; }
    
    static size_type
    get_deallocation_count() { return deallocationCount_; }
    
    static int
    get_construct_count() { return constructCount_; }

    static int
    get_destruct_count() { return destructCount_; }
    
    static void
    reset()
    {
      allocationCount_ = 0;
      deallocationCount_ = 0;
      constructCount_ = 0;
      destructCount_ = 0;
    }

 private:
    static size_type  allocationCount_;
    static size_type  deallocationCount_;
    static int        constructCount_;
    static int        destructCount_;
  };

  // A simple basic allocator that just forwards to the
  // tracker_allocator_counter to fulfill memory requests.  This class
  // is templated on the target object type, but tracker isn't.
  template<class T>
  class tracker_allocator
  {
  private:
    typedef tracker_allocator_counter counter_type;

  public:
    typedef T              value_type;
    typedef T*             pointer;
    typedef const T*       const_pointer;
    typedef T&             reference;
    typedef const T&       const_reference;
    typedef std::size_t    size_type; 
    typedef std::ptrdiff_t difference_type; 
    
    template<class U> struct rebind { typedef tracker_allocator<U> other; };
    
    pointer
    address(reference value) const
    { return &value; }
    
    const_pointer
    address(const_reference value) const
    { return &value; }
    
    tracker_allocator() throw()
    { }

    tracker_allocator(const tracker_allocator&) throw()
    { }

    template<class U>
      tracker_allocator(const tracker_allocator<U>&) throw()
      { }

    ~tracker_allocator() throw()
    { }

    size_type
    max_size() const throw()
    { return size_type(-1) / sizeof(T); }

    pointer
    allocate(size_type n, const void* = 0)
    { return static_cast<pointer>(counter_type::allocate(n * sizeof(T))); }

    void
    construct(pointer p, const T& value)
    {
      ::new ((void *)p) T(value);
      counter_type::construct();
    }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
      template<typename... Args>
        void
        construct(pointer p, Args&&... args) 
	{
	  ::new((void *)p) T(std::forward<Args>(args)...);
	  counter_type::construct();
	}
#endif

    void
    destroy(pointer p)
    {
      p->~T();
      counter_type::destroy();
    }

    void
    deallocate(pointer p, size_type num)
    { counter_type::deallocate(p, num * sizeof(T)); }
  };

  template<class T1, class T2>
    bool
    operator==(const tracker_allocator<T1>&, 
	       const tracker_allocator<T2>&) throw()
    { return true; }

  template<class T1, class T2>
    bool
    operator!=(const tracker_allocator<T1>&, 
	       const tracker_allocator<T2>&) throw()
    { return false; }

  bool
  check_construct_destroy(const char* tag, int expected_c, int expected_d);

  template<typename Alloc>
    bool
    check_deallocate_null()
    {
      // Let's not core here...
      Alloc  a;
      a.deallocate(0, 1);
      a.deallocate(0, 10);
      return true;
    }

  template<typename Alloc>
    bool 
    check_allocate_max_size()
    {
      Alloc a;
      try
	{
	  a.allocate(a.max_size() + 1);
	}
      catch(std::bad_alloc&)
	{
	  return true;
	}
      catch(...)
	{
	  throw;
	}
      throw;
    }


  // A simple allocator which can be constructed endowed of a given
  // "personality" (an integer), queried in operator== to simulate the
  // behavior of realworld "unequal" allocators (i.e., not exploiting
  // the provision in 20.1.5/4, first bullet).  A global unordered_map,
  // filled at allocation time with (pointer, personality) pairs, is
  // then consulted to enforce the requirements in Table 32 about
  // deallocation vs allocator equality.  Note that this allocator is
  // swappable, not assignable, consistently with Option 3 of DR 431
  // (see N1599).
  struct uneq_allocator_base
  {
    typedef std::tr1::unordered_map<void*, int>   map_type;

    // Avoid static initialization troubles and/or bad interactions
    // with tests linking testsuite_allocator.o and playing globally
    // with operator new/delete.
    static map_type&
    get_map()
    {
      static map_type alloc_map;
      return alloc_map;
    }
  };

  template<typename Tp>
    class uneq_allocator
    : private uneq_allocator_base
    {
    public:
      typedef std::size_t                         size_type;
      typedef std::ptrdiff_t                      difference_type;
      typedef Tp*                                 pointer;
      typedef const Tp*                           const_pointer;
      typedef Tp&                                 reference;
      typedef const Tp&                           const_reference;
      typedef Tp                                  value_type;
      
      template<typename Tp1>
        struct rebind
	{ typedef uneq_allocator<Tp1> other; };

      uneq_allocator() throw()
      : personality(0) { }

      uneq_allocator(int person) throw()
      : personality(person) { }
      
      template<typename Tp1>
        uneq_allocator(const uneq_allocator<Tp1>& b) throw()
	: personality(b.get_personality()) { }

      int get_personality() const { return personality; }
      
      pointer
      address(reference x) const { return &x; }
    
      const_pointer
      address(const_reference x) const { return &x; }
    
      pointer
      allocate(size_type n, const void* = 0)
      { 
	if (__builtin_expect(n > this->max_size(), false))
	  std::__throw_bad_alloc();
	
	pointer p = static_cast<Tp*>(::operator new(n * sizeof(Tp)));
	try
	  {
	    get_map().insert(map_type::value_type(reinterpret_cast<void*>(p),
						  personality));
	  }
	catch(...)
	  {
	    ::operator delete(p);
	    __throw_exception_again;
	  }
	return p;
      }
      
      void
      deallocate(pointer p, size_type)
      {
	assert( p );
	
	map_type::iterator it = get_map().find(reinterpret_cast<void*>(p));
	assert( it != get_map().end() );

	// Enforce requirements in Table 32 about deallocation vs
	// allocator equality.
	assert( it->second == personality );
	
	get_map().erase(it);
	::operator delete(p);
      }
      
      size_type
      max_size() const throw() 
      { return size_type(-1) / sizeof(Tp); }
      
      void 
      construct(pointer p, const Tp& val) 
      { ::new((void *)p) Tp(val); }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
      template<typename... Args>
        void
        construct(pointer p, Args&&... args) 
	{ ::new((void *)p) Tp(std::forward<Args>(args)...); }
#endif

      void 
      destroy(pointer p) { p->~Tp(); }

    private:
      // Not assignable...
      uneq_allocator&
      operator=(const uneq_allocator&);

      // ... yet swappable!
      friend inline void
      swap(uneq_allocator& a, uneq_allocator& b)
      { std::swap(a.personality, b.personality); } 
      
      template<typename Tp1>
        friend inline bool
        operator==(const uneq_allocator& a, const uneq_allocator<Tp1>& b)
        { return a.personality == b.personality; }

      template<typename Tp1>
        friend inline bool
        operator!=(const uneq_allocator& a, const uneq_allocator<Tp1>& b)
        { return !(a == b); }
      
      int personality;
    };
} // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_ALLOCATOR_H
