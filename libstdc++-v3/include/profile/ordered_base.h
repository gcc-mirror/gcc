// Profiling unordered containers implementation details -*- C++ -*-

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

/** @file profile/ordered_base.h
 *  This file is a GNU profile extension to the Standard C++ Library.
 */

#ifndef _GLIBCXX_PROFILE_ORDERED
#define _GLIBCXX_PROFILE_ORDERED 1

namespace std _GLIBCXX_VISIBILITY(default)
{
namespace __profile
{
  template<typename _Cont>
    class _Ordered_profile
    {
    public:
      void
      _M_profile_iterate(int __rewind = 0) const
      { __profcxx_map2umap_iterate(this->_M_map2umap_info, __rewind); }

    protected:
      _Ordered_profile() _GLIBCXX_NOEXCEPT
      { _M_profile_construct(); }

#if __cplusplus >= 201103L
      _Ordered_profile(const _Ordered_profile&) noexcept
      : _Ordered_profile() { }
      _Ordered_profile(_Ordered_profile&& __other) noexcept
      : _Ordered_profile()
      { _M_swap(__other); }

      _Ordered_profile&
      operator=(const _Ordered_profile&) noexcept
      {
	_M_profile_destruct();
	_M_profile_construct();
      }

      _Ordered_profile&
      operator=(_Ordered_profile&& __other) noexcept
      {
	_M_swap(__other);
	
	__other._M_profile_destruct();
	__other._M_profile_construct();
      }
#endif

      ~_Ordered_profile()
      { _M_profile_destruct(); }

      void
      _M_profile_construct() _GLIBCXX_NOEXCEPT
      { _M_map2umap_info = __profcxx_map2umap_construct(); }

      void
      _M_profile_destruct() _GLIBCXX_NOEXCEPT
      {
	__profcxx_map2umap_destruct(_M_map2umap_info);
	_M_map2umap_info = 0;
      }

      void
      _M_swap(_Ordered_profile& __other)
      { std::swap(_M_map2umap_info, __other._M_map2umap_info); }

      __gnu_profile::__map2umap_info* _M_map2umap_info;

    private:
      _Cont&
      _M_conjure()
      { return *static_cast<_Cont*>(this); }
    };

} // namespace __profile
} // namespace std

#endif
