// <memory_resource> -*- C++ -*-

// Copyright (C) 2018-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/bits/memory_resource.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory_resource}
 */

#ifndef _GLIBCXX_MEMORY_RESOURCE_H
#define _GLIBCXX_MEMORY_RESOURCE_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#if __cplusplus >= 201703L

#include <new>				// operator new(size_t, void*)
#include <cstddef>			// size_t, max_align_t, byte
#include <bits/functexcept.h>		// __throw_bad_array_new_length
#include <bits/uses_allocator.h>	// allocator_arg_t, __use_alloc
#include <bits/uses_allocator_args.h>	// uninitialized_construct_using_alloc
#include <ext/numeric_traits.h>		// __int_traits
#include <debug/assertions.h>

#if ! __glibcxx_make_obj_using_allocator
# include <bits/utility.h>		// index_sequence
# include <tuple>			// tuple, forward_as_tuple
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace pmr
{
  /// Class `memory_resource`
  /**
   * @ingroup pmr
   * @headerfile memory_resource
   * @since C++17
   */
  class memory_resource
  {
    static constexpr size_t _S_max_align = alignof(max_align_t);

  public:
    memory_resource() = default;
    memory_resource(const memory_resource&) = default;
    virtual ~memory_resource(); // key function

    memory_resource& operator=(const memory_resource&) = default;

    [[nodiscard]]
    void*
    allocate(size_t __bytes, size_t __alignment = _S_max_align)
    __attribute__((__returns_nonnull__,__alloc_size__(2),__alloc_align__(3)))
    { return ::operator new(__bytes, do_allocate(__bytes, __alignment)); }

    void
    deallocate(void* __p, size_t __bytes, size_t __alignment = _S_max_align)
    __attribute__((__nonnull__))
    { return do_deallocate(__p, __bytes, __alignment); }

    [[nodiscard]]
    bool
    is_equal(const memory_resource& __other) const noexcept
    { return do_is_equal(__other); }

  private:
    virtual void*
    do_allocate(size_t __bytes, size_t __alignment) = 0;

    virtual void
    do_deallocate(void* __p, size_t __bytes, size_t __alignment) = 0;

    virtual bool
    do_is_equal(const memory_resource& __other) const noexcept = 0;
  };

  [[nodiscard]]
  inline bool
  operator==(const memory_resource& __a, const memory_resource& __b) noexcept
  { return &__a == &__b || __a.is_equal(__b); }

#if __cpp_impl_three_way_comparison < 201907L
  [[nodiscard]]
  inline bool
  operator!=(const memory_resource& __a, const memory_resource& __b) noexcept
  { return !(__a == __b); }
#endif

  // C++17 23.12.3 Class template polymorphic_allocator

  /// Class template polymorphic_allocator
  /**
   * @ingroup pmr
   * @headerfile memory_resource
   * @since C++17
   */
  template<typename _Tp>
    class polymorphic_allocator
    {
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 2975. Missing case for pair construction in polymorphic allocators
      template<typename _Up>
	struct __not_pair { using type = void; };

      template<typename _Up1, typename _Up2>
	struct __not_pair<pair<_Up1, _Up2>> { };

    public:
      using value_type = _Tp;

      polymorphic_allocator() noexcept
      {
	extern memory_resource* get_default_resource() noexcept
	  __attribute__((__returns_nonnull__));
	_M_resource = get_default_resource();
      }

      polymorphic_allocator(memory_resource* __r) noexcept
      __attribute__((__nonnull__))
      : _M_resource(__r)
      { _GLIBCXX_DEBUG_ASSERT(__r); }

      polymorphic_allocator(const polymorphic_allocator& __other) = default;

      template<typename _Up>
	polymorphic_allocator(const polymorphic_allocator<_Up>& __x) noexcept
	: _M_resource(__x.resource())
	{ }

      polymorphic_allocator&
      operator=(const polymorphic_allocator&) = delete;

      [[nodiscard]]
      _Tp*
      allocate(size_t __n)
      __attribute__((__returns_nonnull__))
      {
	if ((__gnu_cxx::__int_traits<size_t>::__max / sizeof(_Tp)) < __n)
	  std::__throw_bad_array_new_length();
	return static_cast<_Tp*>(_M_resource->allocate(__n * sizeof(_Tp),
						       alignof(_Tp)));
      }

      void
      deallocate(_Tp* __p, size_t __n) noexcept
      __attribute__((__nonnull__))
      { _M_resource->deallocate(__p, __n * sizeof(_Tp), alignof(_Tp)); }

#ifdef __glibcxx_polymorphic_allocator // >= C++20
      [[nodiscard]] void*
      allocate_bytes(size_t __nbytes,
		     size_t __alignment = alignof(max_align_t))
      { return _M_resource->allocate(__nbytes, __alignment); }

      void
      deallocate_bytes(void* __p, size_t __nbytes,
		       size_t __alignment = alignof(max_align_t))
      { _M_resource->deallocate(__p, __nbytes, __alignment); }

      template<typename _Up>
	[[nodiscard]] _Up*
	allocate_object(size_t __n = 1)
	{
	  if ((__gnu_cxx::__int_traits<size_t>::__max / sizeof(_Up)) < __n)
	    std::__throw_bad_array_new_length();
	  return static_cast<_Up*>(allocate_bytes(__n * sizeof(_Up),
						  alignof(_Up)));
	}

      template<typename _Up>
	void
	deallocate_object(_Up* __p, size_t __n = 1)
	{ deallocate_bytes(__p, __n * sizeof(_Up), alignof(_Up)); }

      template<typename _Up, typename... _CtorArgs>
	[[nodiscard]] _Up*
	new_object(_CtorArgs&&... __ctor_args)
	{
	  _Up* __p = allocate_object<_Up>();
	  __try
	    {
	      construct(__p, std::forward<_CtorArgs>(__ctor_args)...);
	    }
	  __catch (...)
	    {
	      deallocate_object(__p);
	      __throw_exception_again;
	    }
	  return __p;
	}

      template<typename _Up>
	void
	delete_object(_Up* __p)
	{
	  __p->~_Up();
	  deallocate_object(__p);
	}
#endif // C++20

#if ! __glibcxx_make_obj_using_allocator // >= C++20
      template<typename _Tp1, typename... _Args>
	__attribute__((__nonnull__))
	typename __not_pair<_Tp1>::type
	construct(_Tp1* __p, _Args&&... __args)
	{
	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 2969. polymorphic_allocator::construct() shouldn't pass resource()
	  using __use_tag
	    = std::__uses_alloc_t<_Tp1, polymorphic_allocator, _Args...>;
	  if constexpr (is_base_of_v<__uses_alloc0, __use_tag>)
	    ::new(__p) _Tp1(std::forward<_Args>(__args)...);
	  else if constexpr (is_base_of_v<__uses_alloc1_, __use_tag>)
	    ::new(__p) _Tp1(allocator_arg, *this,
			    std::forward<_Args>(__args)...);
	  else
	    ::new(__p) _Tp1(std::forward<_Args>(__args)..., *this);
	}

      template<typename _Tp1, typename _Tp2,
	       typename... _Args1, typename... _Args2>
	__attribute__((__nonnull__))
	void
	construct(pair<_Tp1, _Tp2>* __p, piecewise_construct_t,
		  tuple<_Args1...> __x, tuple<_Args2...> __y)
	{
	  auto __x_tag =
	    __use_alloc<_Tp1, polymorphic_allocator, _Args1...>(*this);
	  auto __y_tag =
	    __use_alloc<_Tp2, polymorphic_allocator, _Args2...>(*this);
	  index_sequence_for<_Args1...> __x_i;
	  index_sequence_for<_Args2...> __y_i;

	  ::new(__p) pair<_Tp1, _Tp2>(piecewise_construct,
				      _S_construct_p(__x_tag, __x_i, __x),
				      _S_construct_p(__y_tag, __y_i, __y));
	}

      template<typename _Tp1, typename _Tp2>
	__attribute__((__nonnull__))
	void
	construct(pair<_Tp1, _Tp2>* __p)
	{ this->construct(__p, piecewise_construct, tuple<>(), tuple<>()); }

      template<typename _Tp1, typename _Tp2, typename _Up, typename _Vp>
	__attribute__((__nonnull__))
	void
	construct(pair<_Tp1, _Tp2>* __p, _Up&& __x, _Vp&& __y)
	{
	  this->construct(__p, piecewise_construct,
	      std::forward_as_tuple(std::forward<_Up>(__x)),
	      std::forward_as_tuple(std::forward<_Vp>(__y)));
	}

      template <typename _Tp1, typename _Tp2, typename _Up, typename _Vp>
	__attribute__((__nonnull__))
	void
	construct(pair<_Tp1, _Tp2>* __p, const std::pair<_Up, _Vp>& __pr)
	{
	  this->construct(__p, piecewise_construct,
	      std::forward_as_tuple(__pr.first),
	      std::forward_as_tuple(__pr.second));
	}

      template<typename _Tp1, typename _Tp2, typename _Up, typename _Vp>
	__attribute__((__nonnull__))
	void
	construct(pair<_Tp1, _Tp2>* __p, pair<_Up, _Vp>&& __pr)
	{
	  this->construct(__p, piecewise_construct,
	      std::forward_as_tuple(std::forward<_Up>(__pr.first)),
	      std::forward_as_tuple(std::forward<_Vp>(__pr.second)));
	}
#else // make_obj_using_allocator
      template<typename _Tp1, typename... _Args>
	__attribute__((__nonnull__))
	void
	construct(_Tp1* __p, _Args&&... __args)
	{
	  std::uninitialized_construct_using_allocator(__p, *this,
	      std::forward<_Args>(__args)...);
	}
#endif

      template<typename _Up>
	__attribute__((__nonnull__))
	void
	destroy(_Up* __p)
	{ __p->~_Up(); }

      polymorphic_allocator
      select_on_container_copy_construction() const noexcept
      { return polymorphic_allocator(); }

      memory_resource*
      resource() const noexcept
      __attribute__((__returns_nonnull__))
      { return _M_resource; }

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 3683. operator== for polymorphic_allocator cannot deduce template arg
      [[nodiscard]]
      friend bool
      operator==(const polymorphic_allocator& __a,
		 const polymorphic_allocator& __b) noexcept
      { return *__a.resource() == *__b.resource(); }

#if __cpp_impl_three_way_comparison < 201907L
      [[nodiscard]]
      friend bool
      operator!=(const polymorphic_allocator& __a,
		 const polymorphic_allocator& __b) noexcept
      { return !(__a == __b); }
#endif

    private:
#if ! __glibcxx_make_obj_using_allocator // >= C++20
      using __uses_alloc1_ = __uses_alloc1<polymorphic_allocator>;
      using __uses_alloc2_ = __uses_alloc2<polymorphic_allocator>;

      template<typename _Ind, typename... _Args>
	static tuple<_Args&&...>
	_S_construct_p(__uses_alloc0, _Ind, tuple<_Args...>& __t)
	{ return std::move(__t); }

      template<size_t... _Ind, typename... _Args>
	static tuple<allocator_arg_t, polymorphic_allocator, _Args&&...>
	_S_construct_p(__uses_alloc1_ __ua, index_sequence<_Ind...>,
		       tuple<_Args...>& __t)
	{
	  return {
	      allocator_arg, *__ua._M_a, std::get<_Ind>(std::move(__t))...
	  };
	}

      template<size_t... _Ind, typename... _Args>
	static tuple<_Args&&..., polymorphic_allocator>
	_S_construct_p(__uses_alloc2_ __ua, index_sequence<_Ind...>,
		       tuple<_Args...>& __t)
	{ return { std::get<_Ind>(std::move(__t))..., *__ua._M_a }; }
#endif

      memory_resource* _M_resource;
    };

  template<typename _Tp1, typename _Tp2>
    [[nodiscard]]
    inline bool
    operator==(const polymorphic_allocator<_Tp1>& __a,
	       const polymorphic_allocator<_Tp2>& __b) noexcept
    { return *__a.resource() == *__b.resource(); }

#if __cpp_impl_three_way_comparison < 201907L
  template<typename _Tp1, typename _Tp2>
    [[nodiscard]]
    inline bool
    operator!=(const polymorphic_allocator<_Tp1>& __a,
	       const polymorphic_allocator<_Tp2>& __b) noexcept
    { return !(__a == __b); }
#endif

} // namespace pmr

  template<typename _Alloc> struct allocator_traits;

  /// Partial specialization for `std::pmr::polymorphic_allocator`
  /**
   * @ingroup pmr
   * @headerfile memory_resource
   * @since C++17
   */
  template<typename _Tp>
    struct allocator_traits<pmr::polymorphic_allocator<_Tp>>
    {
      /// The allocator type
      using allocator_type = pmr::polymorphic_allocator<_Tp>;

      /// The allocated type
      using value_type = _Tp;

      /// The allocator's pointer type.
      using pointer = _Tp*;

      /// The allocator's const pointer type.
      using const_pointer = const _Tp*;

      /// The allocator's void pointer type.
      using void_pointer = void*;

      /// The allocator's const void pointer type.
      using const_void_pointer = const void*;

      /// The allocator's difference type
      using difference_type = std::ptrdiff_t;

      /// The allocator's size type
      using size_type = std::size_t;

      /** @{
       * A `polymorphic_allocator` does not propagate when a
       * container is copied, moved, or swapped.
       */
      using propagate_on_container_copy_assignment = false_type;
      using propagate_on_container_move_assignment = false_type;
      using propagate_on_container_swap = false_type;

      static allocator_type
      select_on_container_copy_construction(const allocator_type&) noexcept
      { return allocator_type(); }
      /// @}

      /// Whether all instances of the allocator type compare equal.
      using is_always_equal = false_type;

      template<typename _Up>
	using rebind_alloc = pmr::polymorphic_allocator<_Up>;

      template<typename _Up>
	using rebind_traits = allocator_traits<pmr::polymorphic_allocator<_Up>>;

      /**
       *  @brief  Allocate memory.
       *  @param  __a  An allocator.
       *  @param  __n  The number of objects to allocate space for.
       *
       *  Calls `a.allocate(n)`.
      */
      [[nodiscard]] static pointer
      allocate(allocator_type& __a, size_type __n)
      { return __a.allocate(__n); }

      /**
       *  @brief  Allocate memory.
       *  @param  __a  An allocator.
       *  @param  __n  The number of objects to allocate space for.
       *  @return Memory of suitable size and alignment for `n` objects
       *          of type `value_type`.
       *
       *  The third parameter is ignored..
       *
       *  Returns `a.allocate(n)`.
      */
      [[nodiscard]] static pointer
      allocate(allocator_type& __a, size_type __n, const_void_pointer)
      { return __a.allocate(__n); }

      /**
       *  @brief  Deallocate memory.
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to the memory to deallocate.
       *  @param  __n  The number of objects space was allocated for.
       *
       *  Calls `a.deallocate(p, n)`.
      */
      static void
      deallocate(allocator_type& __a, pointer __p, size_type __n)
      { __a.deallocate(__p, __n); }

      /**
       *  @brief  Construct an object of type `_Up`
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to memory of suitable size and alignment for
       *	       an object of type `_Up`.
       *  @param  __args Constructor arguments.
       *
       *  Calls `__a.construct(__p, std::forward<_Args>(__args)...)`
       *  in C++11, C++14 and C++17. Changed in C++20 to call
       *  `std::construct_at(__p, std::forward<_Args>(__args)...)` instead.
      */
      template<typename _Up, typename... _Args>
	static void
	construct(allocator_type& __a, _Up* __p, _Args&&... __args)
	{ __a.construct(__p, std::forward<_Args>(__args)...); }

      /**
       *  @brief  Destroy an object of type `_Up`
       *  @param  __a  An allocator.
       *  @param  __p  Pointer to the object to destroy
       *
       *  Calls `p->_Up()`.
      */
      template<typename _Up>
	static _GLIBCXX20_CONSTEXPR void
	destroy(allocator_type&, _Up* __p)
	noexcept(is_nothrow_destructible<_Up>::value)
	{ __p->~_Up(); }

      /**
       *  @brief  The maximum supported allocation size
       *  @return `numeric_limits<size_t>::max() / sizeof(value_type)`
      */
      static _GLIBCXX20_CONSTEXPR size_type
      max_size(const allocator_type&) noexcept
      { return size_t(-1) / sizeof(value_type); }
    };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // C++17
#endif // _GLIBCXX_MEMORY_RESOURCE_H
