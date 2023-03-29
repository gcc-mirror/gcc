// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// expensive: * [1-9] * *
// timeout-factor: 2
#include "bits/main.h"

template <typename V, typename U>
  void
  load_store()
  {
    // types, tags, and constants
    using T = typename V::value_type;
    auto&& gen = make_vec<V>;
    using std::experimental::element_aligned;
    using std::experimental::vector_aligned;

    // stride_alignment: consider V::size() == 6. The only reliable alignment is
    // 2 * sizeof(U). I.e. if the first address is aligned to 8 * sizeof(U),
    // then the next address is 6 * sizeof(U) larger, thus only aligned to 2 *
    // sizeof(U).
    // => the LSB determines the stride alignment
    constexpr size_t stride_alignment = size_t(1) << __builtin_ctz(V::size());
    using stride_aligned_t = std::conditional_t<
      V::size() == stride_alignment, decltype(vector_aligned),
      std::experimental::overaligned_tag<stride_alignment * sizeof(U)>>;
    constexpr stride_aligned_t stride_aligned = {};
    constexpr size_t alignment
      = 2 * std::experimental::memory_alignment_v<V, U>;
    constexpr auto overaligned = std::experimental::overaligned<alignment>;
    const V indexes_from_0([](auto i) { return i; });
    for (std::size_t i = 0; i < V::size(); ++i)
      {
	COMPARE(indexes_from_0[i], T(i));
      }

    // loads
    cvt_inputs<T, U> test_values;

    constexpr auto mem_size
      = test_values.size() > 3 * V::size() ? test_values.size() : 3 * V::size();
    alignas(std::experimental::memory_alignment_v<V, U> * 2) U mem[mem_size]
      = {};
    alignas(std::experimental::memory_alignment_v<V, T> * 2)
      T reference[mem_size]
      = {};
    for (std::size_t i = 0; i < test_values.size(); ++i)
      {
	const U value = test_values[i];
	mem[i] = value;
	reference[i] = static_cast<T>(value);
      }
    for (std::size_t i = test_values.size(); i < mem_size; ++i)
      {
	mem[i] = U(i);
	reference[i] = mem[i];
      }

    V x(&mem[V::size()], stride_aligned);
    auto&& compare = [&](const std::size_t offset) {
      static int n = 0;
      const V ref(&reference[offset], element_aligned);
      for (auto i = 0ul; i < V::size(); ++i)
	{
	  if (is_conversion_undefined<T>(mem[i + offset]))
	    {
	      continue;
	    }
	  COMPARE(x[i], reference[i + offset])
	    << "\nbefore conversion: " << mem[i + offset]
	    << "\n   offset = " << offset << "\n        x = " << x
	    << "\nreference = " << ref << "\nx == ref  = " << (x == ref)
	    << "\ncall no. " << n;
	}
      ++n;
    };
    compare(V::size());
    x = V{mem, overaligned};
    compare(0);
    x = {&mem[1], element_aligned};
    compare(1);

    x.copy_from(&mem[V::size()], stride_aligned);
    compare(V::size());
    x.copy_from(&mem[1], element_aligned);
    compare(1);
    x.copy_from(mem, vector_aligned);
    compare(0);

    for (std::size_t i = 0; i < mem_size - V::size(); ++i)
      {
	x.copy_from(&mem[i], element_aligned);
	compare(i);
      }

    for (std::size_t i = 0; i < test_values.size(); ++i)
      {
	mem[i] = U(i);
      }
    x = indexes_from_0;
    using M = typename V::mask_type;
    const M alternating_mask = make_mask<M>({0, 1});
    where(alternating_mask, x).copy_from(&mem[V::size()], stride_aligned);

    const V indexes_from_size = gen({T(V::size())}, 1);
    COMPARE(x == indexes_from_size, alternating_mask)
      << "x: " << x << "\nindexes_from_size: " << indexes_from_size;
    COMPARE(x == indexes_from_0, !alternating_mask);
    where(alternating_mask, x).copy_from(&mem[1], element_aligned);

    const V indexes_from_1 = gen({1, 2, 3, 4}, 4);
    COMPARE(x == indexes_from_1, alternating_mask);
    COMPARE(x == indexes_from_0, !alternating_mask);
    where(!alternating_mask, x).copy_from(mem, overaligned);
    COMPARE(x == indexes_from_0, !alternating_mask);
    COMPARE(x == indexes_from_1, alternating_mask);

    x = where(alternating_mask, V()).copy_from(&mem[V::size()], stride_aligned);
    COMPARE(x == indexes_from_size, alternating_mask);
    COMPARE(x == 0, !alternating_mask);

    x = where(!alternating_mask, V()).copy_from(&mem[1], element_aligned);
    COMPARE(x == indexes_from_1, !alternating_mask);
    COMPARE(x == 0, alternating_mask);

    // stores
    auto&& init_mem = [&mem](U init) {
      for (auto i = mem_size; i; --i)
	{
	  mem[i - 1] = init;
	}
    };
    init_mem(-1);
    x = indexes_from_1;
    x.copy_to(&mem[V::size()], stride_aligned);
    std::size_t i = 0;
    for (; i < V::size(); ++i)
      {
	COMPARE(mem[i], U(-1)) << "i: " << i;
      }
    for (; i < 2 * V::size(); ++i)
      {
	COMPARE(mem[i], U(i - V::size() + 1)) << "i: " << i;
      }
    for (; i < 3 * V::size(); ++i)
      {
	COMPARE(mem[i], U(-1)) << "i: " << i;
      }

    init_mem(-1);
    x.copy_to(&mem[1], element_aligned);
    COMPARE(mem[0], U(-1));
    for (i = 1; i <= V::size(); ++i)
      {
	COMPARE(mem[i], U(i));
      }
    for (; i < 3 * V::size(); ++i)
      {
	COMPARE(mem[i], U(-1));
      }

    init_mem(-1);
    x.copy_to(mem, vector_aligned);
    for (i = 0; i < V::size(); ++i)
      {
	COMPARE(mem[i], U(i + 1));
      }
    for (; i < 3 * V::size(); ++i)
      {
	COMPARE(mem[i], U(-1));
      }

    init_mem(-1);
    where(alternating_mask, indexes_from_0)
      .copy_to(&mem[V::size()], stride_aligned);
    for (i = 0; i < V::size() + 1; ++i)
      {
	COMPARE(mem[i], U(-1));
      }
    for (; i < 2 * V::size(); i += 2)
      {
	COMPARE(mem[i], U(i - V::size()));
      }
    for (i = V::size() + 2; i < 2 * V::size(); i += 2)
      {
	COMPARE(mem[i], U(-1));
      }
    for (; i < 3 * V::size(); ++i)
      {
	COMPARE(mem[i], U(-1));
      }
  }

template <typename V>
  void
  test()
  {
    load_store<V, long double>();
    load_store<V, double>();
    load_store<V, float>();
    load_store<V, long long>();
    load_store<V, unsigned long long>();
    load_store<V, unsigned long>();
    load_store<V, long>();
    load_store<V, int>();
    load_store<V, unsigned int>();
    load_store<V, short>();
    load_store<V, unsigned short>();
    load_store<V, char>();
    load_store<V, signed char>();
    load_store<V, unsigned char>();
    load_store<V, char32_t>();
    load_store<V, char16_t>();
    load_store<V, wchar_t>();
  }
