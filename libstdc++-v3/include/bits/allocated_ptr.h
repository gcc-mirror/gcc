// Guarded Allocation -*- C++ -*-

// Copyright (C) 2014-2026 Free Software Foundation, Inc.
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

/** @file bits/allocated_ptr.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _ALLOCATED_PTR_H
#define _ALLOCATED_PTR_H 1

#if __cplusplus < 201103L
# include <bits/c++0xwarning.h>
#else
# include <type_traits>
# include <bits/ptr_traits.h>
# include <bits/alloc_traits.h>
# include <bits/utility.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
/// @cond undocumented

  /// Non-standard RAII type for managing pointers obtained from allocators.
  template<typename _Alloc>
    struct __allocated_ptr
    {
      using pointer = typename allocator_traits<_Alloc>::pointer;
      using value_type = typename allocator_traits<_Alloc>::value_type;

      /// Take ownership of __ptr
      __allocated_ptr(_Alloc& __a, pointer __ptr) noexcept
      : _M_alloc(std::__addressof(__a)), _M_ptr(__ptr)
      { }

      /// Convert __ptr to allocator's pointer type and take ownership of it
      template<typename _Ptr,
	       typename _Req = _Require<is_same<_Ptr, value_type*>>>
      __allocated_ptr(_Alloc& __a, _Ptr __ptr)
      : _M_alloc(std::__addressof(__a)),
	_M_ptr(pointer_traits<pointer>::pointer_to(*__ptr))
      { }

      /// Transfer ownership of the owned pointer
      __allocated_ptr(__allocated_ptr&& __gd) noexcept
      : _M_alloc(__gd._M_alloc), _M_ptr(__gd._M_ptr)
      { __gd._M_ptr = nullptr; }

      /// Deallocate the owned pointer
      ~__allocated_ptr()
      {
	if (_M_ptr != nullptr)
	  std::allocator_traits<_Alloc>::deallocate(*_M_alloc, _M_ptr, 1);
      }

      /// Release ownership of the owned pointer
      __allocated_ptr&
      operator=(std::nullptr_t) noexcept
      {
	_M_ptr = nullptr;
	return *this;
      }

      explicit operator bool() const noexcept { return (bool)_M_ptr; }

      /// Get the address that the owned pointer refers to.
      value_type* get() const { return std::__to_address(_M_ptr); }

      pointer release() { return std::__exchange(_M_ptr, nullptr); }

    private:
      _Alloc* _M_alloc;
      pointer _M_ptr;
    };

  /// Allocate space for a single object using __a.
  template<typename _Alloc>
    inline __allocated_ptr<_Alloc>
    __allocate_guarded(_Alloc& __a)
    {
      return { __a, std::allocator_traits<_Alloc>::allocate(__a, 1) };
    }

  /// RAII type for constructing/destroying an object with an allocated pointer
  template<typename _Alloc>
    struct __allocated_obj : __allocated_ptr<_Alloc>
    {
      using value_type = typename __allocated_ptr<_Alloc>::value_type;

      __allocated_obj(__allocated_obj<_Alloc>&&) = default;

      // Default-initialize a value_type at *__ptr
      __allocated_obj(__allocated_ptr<_Alloc>&& __ptr)
      : __allocated_ptr<_Alloc>(std::move(__ptr))
      { ::new ((void*)this->get()) value_type; }

      // Call the destructor if an object is owned.
      ~__allocated_obj()
      {
	if (static_cast<bool>(*this))
	  this->get()->~value_type();
      }

      using __allocated_ptr<_Alloc>::operator=;

      value_type& operator*() const { return *this->get(); }
      value_type* operator->() const { return this->get(); }
    };

  /// Construct an object in storage allocated using __a.
  template<typename _Alloc>
    inline __allocated_obj<_Alloc>
    __allocate_guarded_obj(_Alloc& __a)
    {
      return { std::__allocate_guarded(__a) };
    }

  // An RAII type that acquires memory from an allocator.
  // N.B.  'scoped' here in in the RAII sense, not the scoped allocator model,
  // so this has nothing to do with `std::scoped_allocator_adaptor`.
  // This class can be used to simplify the common pattern:
  //
  //     auto ptr = alloc.allocate(1);
  //     try {
  //       std::construct_at(std::to_address(ptr), args);
  //       m_ptr = ptr;
  //     } catch (...) {
  //       alloc.deallocate(ptr, 1);
  //       throw;
  //     }
  //
  // Instead you can do:
  //
  //     _Scoped_allocation sa(alloc);
  //     m_ptr = std::construct_at(sa.get(), args);
  //     (void) sa.release();
  //
  // Or even simpler:
  //
  //     _Scoped_allocation sa(alloc, std::in_place, args);
  //     m_ptr = sa.release();
  //
  template<typename _Alloc>
    struct _Scoped_allocation
    {
      using value_type = typename allocator_traits<_Alloc>::value_type;
      using pointer = typename allocator_traits<_Alloc>::pointer;

      // Use `a` to allocate memory for `n` objects.
      constexpr explicit
      _Scoped_allocation(const _Alloc& __a, size_t __n = 1)
      : _M_a(__a), _M_n(__n), _M_p(_M_a.allocate(__n))
      { }

#if __glibcxx_optional >= 201606L
      // Allocate memory for a single object and if that succeeds,
      // construct an object using args.
      //
      // Does not do uses-allocator construction; don't use if you need that.
      //
      // CAUTION: the destructor will *not* destroy this object, it will only
      // free the memory. That means the following pattern is unsafe:
      //
      //     _Scoped_allocation  sa(alloc, in_place, args);
      //     potentially_throwing_operations();
      //     return sa.release();
      //
      // If the middle operation throws, the object will not be destroyed.
      template<typename... _Args>
	constexpr explicit
	_Scoped_allocation(const _Alloc& __a, in_place_t, _Args&&... __args)
	: _Scoped_allocation(__a, 1)
	{
	  // The target constructor has completed, so if the next line throws,
	  // the destructor will deallocate the memory.
	  allocator_traits<_Alloc>::construct(_M_a, get(),
					      std::forward<_Args>(__args)...);
	}
#endif

      _GLIBCXX20_CONSTEXPR
      ~_Scoped_allocation()
      {
	if (_M_p) [[__unlikely__]]
	  _M_a.deallocate(_M_p, _M_n);
      }

      _Scoped_allocation(_Scoped_allocation&&) = delete;

      constexpr _Alloc
      get_allocator() const noexcept { return _M_a; }

      constexpr value_type*
      get() const noexcept
      { return std::__to_address(_M_p); }

      [[__nodiscard__]]
      constexpr pointer
      release() noexcept { return std::__exchange(_M_p, nullptr); }

    private:
      [[__no_unique_address__]] _Alloc _M_a;
      size_t _M_n;
      pointer _M_p;
    };

#if __glibcxx_optional >= 201606L && __cpp_deduction_guides >= 201606L
  template<typename _Alloc, typename... _Args>
    _Scoped_allocation(_Alloc, in_place_t, _Args...)
      -> _Scoped_allocation<_Alloc>;
#endif

/// @endcond
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif
#endif
