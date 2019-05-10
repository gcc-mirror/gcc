// -*- C++ -*-
// Testing allocator for the C++ library testsuite.
//
// Copyright (C) 2002-2019 Free Software Foundation, Inc.
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
#include <ext/alloc_traits.h>
#include <testsuite_hooks.h>
#if __cplusplus >= 201703L
# include <memory_resource>
# include <new>
#endif

namespace __gnu_test
{
  class tracker_allocator_counter
  {
  public:
    typedef std::size_t    size_type;

    static void
    allocate(size_type blocksize)
    { allocationCount_ += blocksize; }

    static void
    construct() { ++constructCount_; }

    static void
    destroy() { ++destructCount_; }

    static void
    deallocate(size_type blocksize)
    { deallocationCount_ += blocksize; }

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

  // Helper to detect inconsistency between type used to instantiate an
  // allocator and the underlying allocator value_type.
  template<typename T, typename Alloc,
	   typename = typename Alloc::value_type>
    struct check_consistent_alloc_value_type;

  template<typename T, typename Alloc>
    struct check_consistent_alloc_value_type<T, Alloc, T>
    { typedef T value_type; };

  // An allocator facade that intercepts allocate/deallocate/construct/destroy
  // calls and track them through the tracker_allocator_counter class. This
  // class is templated on the target object type, but tracker isn't.
  template<typename T, typename Alloc = std::allocator<T> >
    class tracker_allocator : public Alloc
    {
    private:
      typedef tracker_allocator_counter counter_type;

      typedef __gnu_cxx::__alloc_traits<Alloc> AllocTraits;

    public:
      typedef typename
      check_consistent_alloc_value_type<T, Alloc>::value_type value_type;
      typedef typename AllocTraits::pointer pointer;
      typedef typename AllocTraits::size_type size_type;

      template<class U>
	struct rebind
	{
	  typedef tracker_allocator<U,
		typename AllocTraits::template rebind<U>::other> other;
	};

#if __cplusplus >= 201103L
      tracker_allocator() = default;
      tracker_allocator(const tracker_allocator&) = default;
      tracker_allocator(tracker_allocator&&) = default;
      tracker_allocator& operator=(const tracker_allocator&) = default;
      tracker_allocator& operator=(tracker_allocator&&) = default;

      // Perfect forwarding constructor.
      template<typename... _Args>
	tracker_allocator(_Args&&... __args)
	  : Alloc(std::forward<_Args>(__args)...)
	{ }
#else
      tracker_allocator()
      { }

      tracker_allocator(const tracker_allocator&)
      { }

      ~tracker_allocator()
      { }
#endif

      template<class U>
	tracker_allocator(const tracker_allocator<U,
	  typename AllocTraits::template rebind<U>::other>& alloc)
	    _GLIBCXX_USE_NOEXCEPT
	  : Alloc(alloc)
	{ }

      pointer
      allocate(size_type n, const void* = 0)
      {
	pointer p = AllocTraits::allocate(*this, n);
	counter_type::allocate(n * sizeof(T));
	return p;
      }

#if __cplusplus >= 201103L
      template<typename U, typename... Args>
	void
	construct(U* p, Args&&... args)
	{
	  AllocTraits::construct(*this, p, std::forward<Args>(args)...);
	  counter_type::construct();
	}

      template<typename U>
	void
	destroy(U* p)
	{
	  AllocTraits::destroy(*this, p);
	  counter_type::destroy();
	}
#else
      void
      construct(pointer p, const T& value)
      {
	AllocTraits::construct(*this, p, value);
	counter_type::construct();
      }

      void
      destroy(pointer p)
      {
	AllocTraits::destroy(*this, p);
	counter_type::destroy();
      }
#endif

      void
      deallocate(pointer p, size_type num)
      {
	counter_type::deallocate(num * sizeof(T));
	AllocTraits::deallocate(*this, p, num);
      }

      // Implement swap for underlying allocators that might need it.
      friend inline void
      swap(tracker_allocator& a, tracker_allocator& b)
      {
	using std::swap;

	Alloc& aa = a;
	Alloc& ab = b;
	swap(aa, ab);
      }
    };

  template<class T1, class Alloc1, class T2, class Alloc2>
    bool
    operator==(const tracker_allocator<T1, Alloc1>& lhs,
	       const tracker_allocator<T2, Alloc2>& rhs) throw()
    {
      const Alloc1& alloc1 = lhs;
      const Alloc2& alloc2 = rhs;
      return alloc1 == alloc2;
    }

  template<class T1, class Alloc1, class T2, class Alloc2>
    bool
    operator!=(const tracker_allocator<T1, Alloc1>& lhs,
	       const tracker_allocator<T2, Alloc2>& rhs) throw()
    { return !(lhs == rhs); }

  bool
  check_construct_destroy(const char* tag, int expected_c, int expected_d);

  template<typename Alloc>
    bool
    check_deallocate_null()
    {
      // Let's not core here...
      Alloc a;
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
	  (void) a.allocate(a.max_size() + 1);
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
  // swappable, not copy assignable, consistently with Option 3 of DR 431
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

  template<typename Tp, typename Alloc = std::allocator<Tp> >
    class uneq_allocator
    : private uneq_allocator_base,
      public Alloc
    {
      typedef __gnu_cxx::__alloc_traits<Alloc> AllocTraits;

      Alloc& base() { return *this; }
      const Alloc& base() const  { return *this; }
      void swap_base(Alloc& b) { using std::swap; swap(b, this->base()); }

    public:
      typedef typename check_consistent_alloc_value_type<Tp, Alloc>::value_type
	value_type;
      typedef typename AllocTraits::size_type	size_type;
      typedef typename AllocTraits::pointer	pointer;

#if __cplusplus >= 201103L
      typedef std::true_type			propagate_on_container_swap;
      typedef std::false_type			is_always_equal;
#endif

      template<typename Tp1>
	struct rebind
	{
	  typedef uneq_allocator<Tp1,
		typename AllocTraits::template rebind<Tp1>::other> other;
	};

      uneq_allocator() _GLIBCXX_USE_NOEXCEPT
      : personality(0) { }

      uneq_allocator(int person) _GLIBCXX_USE_NOEXCEPT
      : personality(person) { }

#if __cplusplus >= 201103L
      uneq_allocator(const uneq_allocator&) = default;
      uneq_allocator(uneq_allocator&&) = default;
#endif

      template<typename Tp1>
	uneq_allocator(const uneq_allocator<Tp1,
		       typename AllocTraits::template rebind<Tp1>::other>& b)
	_GLIBCXX_USE_NOEXCEPT
	: personality(b.get_personality()) { }

      ~uneq_allocator() _GLIBCXX_USE_NOEXCEPT
      { }

      int get_personality() const { return personality; }

      pointer
      allocate(size_type n, const void* = 0)
      {
	pointer p = AllocTraits::allocate(*this, n);

	try
	  {
	    get_map().insert(map_type::value_type(reinterpret_cast<void*>(p),
						  personality));
	  }
	catch(...)
	  {
	    AllocTraits::deallocate(*this, p, n);
	    __throw_exception_again;
	  }

	return p;
      }

      void
      deallocate(pointer p, size_type n)
      {
	VERIFY( p );

	map_type::iterator it = get_map().find(reinterpret_cast<void*>(p));
	VERIFY( it != get_map().end() );

	// Enforce requirements in Table 32 about deallocation vs
	// allocator equality.
	VERIFY( it->second == personality );

	get_map().erase(it);
	AllocTraits::deallocate(*this, p, n);
      }

#if __cplusplus >= 201103L
      // Not copy assignable...
      uneq_allocator&
      operator=(const uneq_allocator&) = delete;

      // ... but still moveable if base allocator is.
      uneq_allocator&
      operator=(uneq_allocator&&) = default;
#else
    private:
      // Not assignable...
      uneq_allocator&
      operator=(const uneq_allocator&);
#endif

    private:
      // ... yet swappable!
      friend inline void
      swap(uneq_allocator& a, uneq_allocator& b)
      {
	std::swap(a.personality, b.personality);
	a.swap_base(b);
      }

      template<typename Tp1>
	friend inline bool
	operator==(const uneq_allocator& a,
		   const uneq_allocator<Tp1,
		   typename AllocTraits::template rebind<Tp1>::other>& b)
	{ return a.personality == b.personality; }

      template<typename Tp1>
	friend inline bool
	operator!=(const uneq_allocator& a,
		   const uneq_allocator<Tp1,
		   typename AllocTraits::template rebind<Tp1>::other>& b)
	{ return !(a == b); }

      int personality;
    };

#if __cplusplus >= 201103L
  // An uneq_allocator which can be used to test allocator propagation.
  template<typename Tp, bool Propagate, typename Alloc = std::allocator<Tp>>
    class propagating_allocator : public uneq_allocator<Tp, Alloc>
    {
      typedef __gnu_cxx::__alloc_traits<Alloc> AllocTraits;

      typedef uneq_allocator<Tp, Alloc> base_alloc;
      base_alloc& base() { return *this; }
      const base_alloc& base() const  { return *this; }
      void swap_base(base_alloc& b) { swap(b, this->base()); }

      typedef std::integral_constant<bool, Propagate> trait_type;

    public:
      // default allocator_traits::rebind_alloc would select
      // uneq_allocator::rebind so we must define rebind here
      template<typename Up>
	struct rebind
	{
	  typedef propagating_allocator<Up, Propagate,
		typename AllocTraits::template rebind<Up>::other> other;
	};

      propagating_allocator(int i) noexcept
      : base_alloc(i)
      { }

      template<typename Up>
	propagating_allocator(const propagating_allocator<Up, Propagate,
			      typename AllocTraits::template rebind<Up>::other>& a)
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
	operator=(const propagating_allocator<Tp, P2, Alloc>& a) noexcept
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
        SimpleAllocator(const SimpleAllocator<T>&) { }

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

  template<typename T>
    struct default_init_allocator
    {
      using value_type = T;

      default_init_allocator() = default;

      template<typename U>
        default_init_allocator(const default_init_allocator<U>& a)
	  : state(a.state)
        { }

      T*
      allocate(std::size_t n)
      { return std::allocator<T>().allocate(n); }

      void
      deallocate(T* p, std::size_t n)
      { std::allocator<T>().deallocate(p, n); }

      int state;
    };

  template<typename T, typename U>
    bool operator==(const default_init_allocator<T>& t,
		    const default_init_allocator<U>& u)
    { return t.state == u.state; }

  template<typename T, typename U>
    bool operator!=(const default_init_allocator<T>& t,
		    const default_init_allocator<U>& u)
    { return !(t == u); }
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

      pointer allocate(std::size_t n, const_void_pointer = {})
      { return pointer(std::allocator<Tp>::allocate(n)); }

      void deallocate(pointer p, std::size_t n)
      { std::allocator<Tp>::deallocate(std::addressof(*p), n); }
    };

  // Utility for use as CRTP base class of custom pointer types
  template<typename Derived, typename T>
    struct PointerBase
    {
      typedef T element_type;

      // typedefs for iterator_traits
      typedef T value_type;
      typedef std::ptrdiff_t difference_type;
      typedef std::random_access_iterator_tag iterator_category;
      typedef Derived pointer;
      typedef T& reference;

      T* value;

      explicit PointerBase(T* p = nullptr) : value(p) { }

      PointerBase(std::nullptr_t) : value(nullptr) { }

      template<typename D, typename U,
	       typename = decltype(static_cast<T*>(std::declval<U*>()))>
	PointerBase(const PointerBase<D, U>& p) : value(p.value) { }

      T& operator*() const { return *value; }
      T* operator->() const { return value; }
      T& operator[](difference_type n) const { return value[n]; }

      Derived& operator++() { ++value; return derived(); }
      Derived operator++(int) { Derived tmp(derived()); ++value; return tmp; }
      Derived& operator--() { --value; return derived(); }
      Derived operator--(int) { Derived tmp(derived()); --value; return tmp; }

      Derived& operator+=(difference_type n) { value += n; return derived(); }
      Derived& operator-=(difference_type n) { value -= n; return derived(); }

      explicit operator bool() const { return value != nullptr; }

      Derived
      operator+(difference_type n) const
      {
	Derived p(derived());
	return p += n;
      }

      Derived
      operator-(difference_type n) const
      {
	Derived p(derived());
	return p -= n;
      }

    private:
      Derived&
      derived() { return static_cast<Derived&>(*this); }

      const Derived&
      derived() const { return static_cast<const Derived&>(*this); }
    };

    template<typename D, typename T>
    std::ptrdiff_t operator-(PointerBase<D, T> l, PointerBase<D, T> r)
    { return l.value - r.value; }

    template<typename D, typename T>
    bool operator==(PointerBase<D, T> l, PointerBase<D, T> r)
    { return l.value == r.value; }

    template<typename D, typename T>
    bool operator!=(PointerBase<D, T> l, PointerBase<D, T> r)
    { return l.value != r.value; }

    // implementation for void specializations
    template<typename T>
    struct PointerBase_void
    {
      typedef T element_type;

      // typedefs for iterator_traits
      typedef T value_type;
      typedef std::ptrdiff_t difference_type;
      typedef std::random_access_iterator_tag iterator_category;

      T* value;

      explicit PointerBase_void(T* p = nullptr) : value(p) { }

      template<typename D, typename U,
	       typename = decltype(static_cast<T*>(std::declval<U*>()))>
	PointerBase_void(const PointerBase<D, U>& p) : value(p.value) { }

      explicit operator bool() const { return value != nullptr; }
    };

    template<typename Derived>
    struct PointerBase<Derived, void> : PointerBase_void<void>
    {
      using PointerBase_void::PointerBase_void;
      typedef Derived pointer;
    };

    template<typename Derived>
    struct PointerBase<Derived, const void> : PointerBase_void<const void>
    {
      using PointerBase_void::PointerBase_void;
      typedef Derived pointer;
    };
#endif // C++11

#if __cplusplus >= 201703L
#if __cpp_aligned_new && __cpp_rtti
    // A concrete memory_resource, with error checking.
    class memory_resource : public std::pmr::memory_resource
    {
    public:
      memory_resource()
      : lists(new allocation_lists)
      { }

      memory_resource(const memory_resource& r) noexcept
      : lists(r.lists)
      { lists->refcount++; }

      memory_resource& operator=(const memory_resource&) = delete;

      ~memory_resource()
      {
	if (lists->refcount-- == 1)
	  delete lists;  // last one out turns out the lights
      }

      struct bad_size { };
      struct bad_alignment { };
      struct bad_address { };

      // Deallocate everything (moving the tracking info to the freed list)
      void
      deallocate_everything()
      {
	while (lists->active)
	  {
	    auto a = lists->active;
	    // Intentionally virtual dispatch, to inform derived classes:
	    this->do_deallocate(a->p, a->bytes, a->alignment);
	  }
      }

      // Clear the freed list
      void
      forget_freed_allocations()
      { lists->forget_allocations(lists->freed); }

      // Count how many allocations have been done and not freed.
      std::size_t
      number_of_active_allocations() const noexcept
      {
	std::size_t n = 0;
	for (auto a = lists->active; a != nullptr; a = a->next)
	  ++n;
	return n;
      }

    protected:
      void*
      do_allocate(std::size_t bytes, std::size_t alignment) override
      {
	// TODO perform a single allocation and put the allocation struct
	// in the buffer using placement new? It means deallocation won't
	// actually return memory to the OS, as it will stay in lists->freed.
	//
	// TODO adjust the returned pointer to be minimally aligned?
	// e.g. if alignment==1 don't return something aligned to 2 bytes.
	// Maybe not worth it, at least monotonic_buffer_resource will
	// never ask upstream for anything with small alignment.
	void* p = ::operator new(bytes, std::align_val_t(alignment));
	lists->active = new allocation{p, bytes, alignment, lists->active};
	return p;
      }

      void
      do_deallocate(void* p, std::size_t bytes, std::size_t alignment) override
      {
	allocation** aptr = &lists->active;
	while (*aptr)
	  {
	    allocation* a = *aptr;
	    if (p == a->p)
	      {
		if (bytes != a->bytes)
		  throw bad_size();
		if (alignment != a->alignment)
		  throw bad_alignment();
#if __cpp_sized_deallocation
		::operator delete(p, bytes, std::align_val_t(alignment));
#else
		::operator delete(p, std::align_val_t(alignment));
#endif
		*aptr = a->next;
		a->next = lists->freed;
		lists->freed = a;
		return;
	      }
	    aptr = &a->next;
	  }
	throw bad_address();
      }

      bool
      do_is_equal(const std::pmr::memory_resource& r) const noexcept override
      {
	// Equality is determined by sharing the same allocation_lists object.
	if (auto p = dynamic_cast<const memory_resource*>(&r))
	  return p->lists == lists;
	return false;
      }

    private:
      struct allocation
      {
	void* p;
	std::size_t bytes;
	std::size_t alignment;
	allocation* next;
      };

      // Maintain list of allocated blocks and list of freed blocks.
      // Copies of this memory_resource share the same ref-counted lists.
      struct allocation_lists
      {
	unsigned refcount = 1;
	allocation* active = nullptr;
	allocation* freed = nullptr;

	void forget_allocations(allocation*& list)
	{
	  while (list)
	    {
	      auto p = list;
	      list = list->next;
	      delete p;
	    }
	}

	~allocation_lists()
	{
	  forget_allocations(active); // Anything in this list is a leak!
	  forget_allocations(freed);
	}
      };

      allocation_lists* lists;
    };
#endif // aligned-new && rtti

    // Set the default resource, and restore the previous one on destruction.
    struct default_resource_mgr
    {
      explicit default_resource_mgr(std::pmr::memory_resource* r)
      : prev(std::pmr::set_default_resource(r))
      { }

      ~default_resource_mgr()
      { std::pmr::set_default_resource(prev); }

      std::pmr::memory_resource* prev;
    };

#endif // C++17

} // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_ALLOCATOR_H
