// { dg-do compile }

// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <numeric>

#if __cplusplus > 201703L
#define CONSTEXPR constexpr
#else
#define CONSTEXPR
#endif

namespace std {
  template <class InputIterator, class T>
    CONSTEXPR
    T accumulate(InputIterator first, InputIterator last, T init);

  template <class InputIterator, class T, class BinaryOperation>
    CONSTEXPR
    T accumulate(InputIterator first, InputIterator last, T init,
                 BinaryOperation binary_op);

  template <class InputIterator1, class InputIterator2, class T>
    CONSTEXPR
    T inner_product(InputIterator1 first1, InputIterator1 last1,
                    InputIterator2 first2, T init);

  template <class InputIterator1, class InputIterator2, class T,
            class BinaryOperation1, class BinaryOperation2>
    CONSTEXPR
    T inner_product(InputIterator1 first1, InputIterator1 last1,
                    InputIterator2 first2, T init,
                    BinaryOperation1 binary_op1,
                    BinaryOperation2 binary_op2);

  template <class InputIterator, class OutputIterator>
    CONSTEXPR
    OutputIterator partial_sum(InputIterator first,
                               InputIterator last,
                               OutputIterator result);

  template <class InputIterator, class OutputIterator,
            class BinaryOperation>
    CONSTEXPR
    OutputIterator partial_sum(InputIterator first,
                               InputIterator last,
                               OutputIterator result,
                               BinaryOperation binary_op);

  template <class InputIterator, class OutputIterator>
    CONSTEXPR
    OutputIterator adjacent_difference(InputIterator first,
                                       InputIterator last,
                                       OutputIterator result);

  template <class InputIterator, class OutputIterator,
            class BinaryOperation>
    CONSTEXPR
    OutputIterator adjacent_difference(InputIterator first,
                                       InputIterator last,
                                       OutputIterator result,
                                       BinaryOperation binary_op);
#if __cplusplus >= 201103L
  template<class ForwardIterator, class T>
    CONSTEXPR void iota(ForwardIterator first, ForwardIterator last, T value);
#endif // C++11

#if __cplusplus >= 201703L
  template<class InputIterator>
    CONSTEXPR typename iterator_traits<InputIterator>::value_type
    reduce(InputIterator first, InputIterator last);

  template<class InputIterator, class T>
    CONSTEXPR T reduce(InputIterator first, InputIterator last, T init);

  template<class InputIterator, class T, class BinaryOperation>
    CONSTEXPR T reduce(InputIterator first, InputIterator last, T init,
		       BinaryOperation binary_op);

  template<class InputIterator, class OutputIterator, class T>
    CONSTEXPR OutputIterator
    exclusive_scan(InputIterator first, InputIterator last,
		   OutputIterator result, T init);

  template<class InputIterator, class OutputIterator, class T,
	   class BinaryOperation>
    CONSTEXPR OutputIterator
    exclusive_scan(InputIterator first, InputIterator last,
		   OutputIterator result, T init, BinaryOperation binary_op);

  template<class InputIterator, class OutputIterator>
    CONSTEXPR OutputIterator
    inclusive_scan(InputIterator first, InputIterator last,
		   OutputIterator result);

  template<class InputIterator, class OutputIterator, class BinaryOperation>
    CONSTEXPR OutputIterator
    inclusive_scan(InputIterator first, InputIterator last,
		   OutputIterator result, BinaryOperation binary_op);

  template<class InputIterator, class OutputIterator, class BinaryOperation,
	   class T>
    CONSTEXPR OutputIterator
    inclusive_scan(InputIterator first, InputIterator last,
		   OutputIterator result, BinaryOperation binary_op, T init);

  template<class InputIterator1, class InputIterator2, class T>
    CONSTEXPR T transform_reduce(InputIterator1 first1, InputIterator1 last1,
				 InputIterator2 first2, T init);

  template<class InputIterator1, class InputIterator2, class T,
	   class BinaryOperation1, class BinaryOperation2>
    CONSTEXPR T transform_reduce(InputIterator1 first1, InputIterator1 last1,
				 InputIterator2 first2, T init,
				 BinaryOperation1 binary_op1,
				 BinaryOperation2 binary_op2);

  template<class InputIterator, class T,
	   class BinaryOperation, class UnaryOperation>
    CONSTEXPR T transform_reduce(InputIterator first, InputIterator last, T init,
				 BinaryOperation binary_op,
				 UnaryOperation unary_op);

  template<class InputIterator, class OutputIterator, class T,
	   class BinaryOperation, class UnaryOperation>
    CONSTEXPR OutputIterator
    transform_exclusive_scan(InputIterator first, InputIterator last,
			     OutputIterator result, T init,
			     BinaryOperation binary_op, UnaryOperation unary_op);

  template<class InputIterator, class OutputIterator,
	   class BinaryOperation, class UnaryOperation>
    CONSTEXPR OutputIterator
    transform_inclusive_scan(InputIterator first, InputIterator last,
			     OutputIterator result,
			     BinaryOperation binary_op, UnaryOperation unary_op);

  template<class InputIterator, class OutputIterator,
	   class BinaryOperation, class UnaryOperation, class T>
    CONSTEXPR OutputIterator
    transform_inclusive_scan(InputIterator first, InputIterator last,
			     OutputIterator result,
			     BinaryOperation binary_op, UnaryOperation unary_op,
			     T init);
#endif // C++17

#if __cplusplus > 201703L
  template<class M, class N>
    constexpr common_type_t<M,N> gcd(M m, N n);

  template<class M, class N>
    constexpr common_type_t<M,N> lcm(M m, N n);

  template<class T>
    constexpr T midpoint(T a, T b) noexcept;

  template<class T>
    constexpr T* midpoint(T* a, T* b);
#endif // C++2a
}
