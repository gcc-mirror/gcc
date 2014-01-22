// -*- C++ -*-
// Testing allocator for the C++ library testsuite.
//
// Copyright (C) 2002-2014 Free Software Foundation, Inc.
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
#include <bits/move.h>
#include <ext/pointer.h>
#include <testsuite_hooks.h>

namespace __gnu_test
{
  class tracker_allocator_counter
  {
  public:
    typedef std::size_t    size_type; 

    static void*
    allocate(size_type blocksize)
    {
      void* p = ::operator new(blocksize);
      allocationCount_ += blocksize;
      return p;
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
    address(reference value) const _GLIBCXX_NOEXCEPT
    { return std::__addressof(value); }

    const_pointer
    address(const_reference value) const _GLIBCXX_NOEXCEPT
    { return std::__addressof(value); }

    tracker_allocator() _GLIBCXX_USE_NOEXCEPT
    { }

    tracker_allocator(const tracker_allocator&) _GLIBCXX_USE_NOEXCEPT
    { }

    template<class U>
      tracker_allocator(const tracker_allocator<U>&) _GLIBCXX_USE_NOEXCEPT
      { }

    ~tracker_allocator() _GLIBCXX_USE_NOEXCEPT
    { }

    size_type
    max_size() const _GLIBCXX_USE_NOEXCEPT
    { return size_type(-1) / sizeof(T); }

    pointer
    allocate(size_type n, const void* = 0)
    { return static_cast<pointer>(counter_type::allocate(n * sizeof(T))); }

#if __cplusplus >= 201103L
    template<typename U, typename... Args>
      void
      construct(U* p, Args&&... args) 
      {
	::new((void *)p) U(std::forward<Args>(args)...);
	counter_type::construct();
      }

    template<typename U>
      void
      destroy(U* p)
      {
	p->~U();
	counter_type::destroy();
      }
#else
    void
    construct(pointer p, const T& value)
    {
      ::new ((void *)p) T(value);
      counter_type::construct();
    }

    void
    destroy(pointer p)
    {
      p->~T();
      counter_type::destroy();
    }
#endif

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

#if __cplusplus >= 201103L
      typedef std::true_type                      propagate_on_container_swap;
#endif

      template<typename Tp1>
        struct rebind
	{ typedef uneq_allocator<Tp1> other; };

      uneq_allocator() _GLIBCXX_USE_NOEXCEPT
      : personality(0) { }

      uneq_allocator(int person) _GLIBCXX_USE_NOEXCEPT
      : personality(person) { }
      
      template<typename Tp1>
        uneq_allocator(const uneq_allocator<Tp1>& b) _GLIBCXX_USE_NOEXCEPT
	: personality(b.get_personality()) { }

      ~uneq_allocator() _GLIBCXX_USE_NOEXCEPT
      { }

      int get_personality() const { return personality; }
      
      pointer
      address(reference x) const _GLIBCXX_NOEXCEPT
      { return std::__addressof(x); }
    
      const_pointer
      address(const_reference x) const _GLIBCXX_NOEXCEPT
      { return std::__addressof(x); }

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
	bool test __attribute__((unused)) = true;

	VERIFY( p );

	map_type::iterator it = get_map().find(reinterpret_cast<void*>(p));
	VERIFY( it != get_map().end() );

	// Enforce requirements in Table 32 about deallocation vs
	// allocator equality.
	VERIFY( it->second == personality );

	get_map().erase(it);
	::operator delete(p);
      }

      size_type
      max_size() const _GLIBCXX_USE_NOEXCEPT 
      { return size_type(-1) / sizeof(Tp); }

#if __cplusplus >= 201103L
      template<typename U, typename... Args>
        void
        construct(U* p, Args&&... args) 
	{ ::new((void *)p) U(std::forward<Args>(args)...); }

      template<typename U>
	void 
	destroy(U* p) { p->~U(); }

      // Not copy assignable...
      uneq_allocator&
      operator=(const uneq_allocator&) = delete;
#else
      void 
      construct(pointer p, const Tp& val) 
      { ::new((void *)p) Tp(val); }

      void 
      destroy(pointer p) { p->~Tp(); }

    private:
      // Not assignable...
      uneq_allocator&
      operator=(const uneq_allocator&);
#endif

    private:

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

#if __cplusplus >= 201103L
  // An uneq_allocator which can be used to test allocator propagation.
  template<typename Tp, bool Propagate>
    class propagating_allocator : public uneq_allocator<Tp>
    {
      typedef uneq_allocator<Tp> base_alloc;
      base_alloc& base() { return *this; }
      const base_alloc& base() const  { return *this; }
      void swap_base(base_alloc& b) { swap(b, this->base()); }

      typedef std::integral_constant<bool, Propagate> trait_type;

    public:
      // default allocator_traits::rebind_alloc would select
      // uneq_allocator::rebind so we must define rebind here
      template<typename Up>
	struct rebind { typedef propagating_allocator<Up, Propagate> other; };

      propagating_allocator(int i) noexcept
      : base_alloc(i)
      { }

      template<typename Up>
	propagating_allocator(const propagating_allocator<Up, Propagate>& a)
       	noexcept
	: base_alloc(a)
	{ }

      propagating_allocator() noexcept = default;

      propagating_allocator(const propagating_allocator&) noexcept = default;

      propagating_allocator&
      operator=(const propagating_allocator& a) noexcept
      {
	static_assert(Propagate, "assigning propagating_allocator<T, true>");
	propagating_allocator(a).swap_base(*this);
	return *this;
      }

      template<bool P2>
  	propagating_allocator&
  	operator=(const propagating_allocator<Tp, P2>& a) noexcept
  	{
	  static_assert(P2, "assigning propagating_allocator<T, true>");
	  propagating_allocator(a).swap_base(*this);
	  return *this;
  	}

      // postcondition: a.get_personality() == 0
      propagating_allocator(propagating_allocator&& a) noexcept
      : base_alloc()
      { swap_base(a); }

      // postcondition: a.get_personality() == 0
      propagating_allocator&
      operator=(propagating_allocator&& a) noexcept
      {
	propagating_allocator(std::move(a)).swap_base(*this);
	return *this;
      }

      typedef trait_type propagate_on_container_copy_assignment;
      typedef trait_type propagate_on_container_move_assignment;
      typedef trait_type propagate_on_container_swap;

      propagating_allocator select_on_container_copy_construction() const
      { return Propagate ? *this : propagating_allocator(); }
    };

  // Class template supporting the minimal interface that satisfies the
  // Allocator requirements, from example in [allocator.requirements]
  template <class Tp>
    struct SimpleAllocator
    {
      typedef Tp value_type;

      SimpleAllocator() noexcept { }

      template <class T>
        SimpleAllocator(const SimpleAllocator<T>& other) { }

      Tp *allocate(std::size_t n)
      { return std::allocator<Tp>().allocate(n); }

      void deallocate(Tp *p, std::size_t n)
      { std::allocator<Tp>().deallocate(p, n); }
    };

  template <class T, class U>
    bool operator==(const SimpleAllocator<T>&, const SimpleAllocator<U>&)
    { return true; }
  template <class T, class U>
    bool operator!=(const SimpleAllocator<T>&, const SimpleAllocator<U>&)
    { return false; }

#endif

  template<typename Tp>
    struct ExplicitConsAlloc : std::allocator<Tp>
    {
      ExplicitConsAlloc() { }

      template<typename Up>
        explicit
        ExplicitConsAlloc(const ExplicitConsAlloc<Up>&) { }

      template<typename Up>
        struct rebind
        { typedef ExplicitConsAlloc<Up> other; };
    };

#if __cplusplus >= 201103L
  template<typename Tp>
    class CustomPointerAlloc : public std::allocator<Tp>
    {
      template<typename Up, typename Sp = __gnu_cxx::_Std_pointer_impl<Up>>
	using Ptr =  __gnu_cxx::_Pointer_adapter<Sp>;

    public:
      CustomPointerAlloc() = default;

      template<typename Up>
        CustomPointerAlloc(const CustomPointerAlloc<Up>&) { }

      template<typename Up>
        struct rebind
        { typedef CustomPointerAlloc<Up> other; };

      typedef Ptr<Tp> 		pointer;
      typedef Ptr<const Tp>	const_pointer;
      typedef Ptr<void>		void_pointer;
      typedef Ptr<const void>	const_void_pointer;

      pointer allocate(std::size_t n, pointer = {})
      { return pointer(std::allocator<Tp>::allocate(n)); }

      void deallocate(pointer p, std::size_t n)
      { std::allocator<Tp>::deallocate(std::addressof(*p), n); }
    };
#endif

} // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_ALLOCATOR_H
