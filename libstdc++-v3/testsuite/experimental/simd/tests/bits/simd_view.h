// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#ifndef VC_TESTS_SIMD_VIEW_H_
#define VC_TESTS_SIMD_VIEW_H_

#include <experimental/simd>

_GLIBCXX_SIMD_BEGIN_NAMESPACE

namespace experimental
{
  namespace imported_begin_end
  {
    using std::begin;
    using std::end;

    template <class T>
      using begin_type = decltype(begin(std::declval<T>()));

    template <class T>
      using end_type = decltype(end(std::declval<T>()));
  }  // namespace imported_begin_end

  template <class V, class It, class End>
    class viewer
    {
      It it;
      const End end;

      template <class F>
	void
	for_each_impl(F &&fun, std::index_sequence<0, 1, 2>)
	{
	  for (; it + V::size() <= end; it += V::size())
	    {
	      fun(V([&](auto i) { return std::get<0>(it[i].as_tuple()); }),
		  V([&](auto i) { return std::get<1>(it[i].as_tuple()); }),
		  V([&](auto i) { return std::get<2>(it[i].as_tuple()); }));
	    }
	  if (it != end)
	    {
	      fun(V([&](auto i)
	      {
		auto ii = it + i < end ? i + 0 : 0;
		return std::get<0>(it[ii].as_tuple());
	      }),
		  V([&](auto i) {
		    auto ii = it + i < end ? i + 0 : 0;
		    return std::get<1>(it[ii].as_tuple());
		  }),
		  V([&](auto i) {
		    auto ii = it + i < end ? i + 0 : 0;
		    return std::get<2>(it[ii].as_tuple());
		  }));
	    }
	}

      template <class F>
	void
	for_each_impl(F &&fun, std::index_sequence<0, 1>)
	{
	  for (; it + V::size() <= end; it += V::size())
	    {
	      fun(V([&](auto i) { return std::get<0>(it[i].as_tuple()); }),
		  V([&](auto i) { return std::get<1>(it[i].as_tuple()); }));
	    }
	  if (it != end)
	    {
	      fun(V([&](auto i) {
		auto ii = it + i < end ? i + 0 : 0;
		return std::get<0>(it[ii].as_tuple());
	      }),
		  V([&](auto i) {
		    auto ii = it + i < end ? i + 0 : 0;
		    return std::get<1>(it[ii].as_tuple());
		  }));
	    }
	}

    public:
      viewer(It _it, End _end)
      : it(_it), end(_end) {}

      template <class F>
	void
	for_each(F &&fun)
	{
	  constexpr size_t N
	    = std::tuple_size<std::decay_t<decltype(it->as_tuple())>>::value;
	  for_each_impl(std::forward<F>(fun), std::make_index_sequence<N>());
	}
    };

  template <class V, class Cont>
    viewer<V, imported_begin_end::begin_type<const Cont &>,
	   imported_begin_end::end_type<const Cont &>>
    simd_view(const Cont &data)
    {
      using std::begin;
      using std::end;
      return {begin(data), end(data)};
    }
}  // namespace experimental
_GLIBCXX_SIMD_END_NAMESPACE

#endif  // VC_TESTS_SIMD_VIEW_H_
