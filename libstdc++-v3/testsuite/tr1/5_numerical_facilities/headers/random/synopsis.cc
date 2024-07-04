// { dg-do compile }

// 2007-02-04  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <tr1/random>

namespace std {
namespace tr1 {

  // [5.1.3] Class template variate_generator
  template<class UniformRandomNumberGenerator, class Distribution>
  class variate_generator;

  // [5.1.4.1] Class template linear_congruential
  template<class IntType, IntType a, IntType c, IntType m>
  class linear_congruential;

  // [5.1.4.2] Class template mersenne_twister
  template<class UIntType, int w, int n, int m, int r,
	   UIntType a, int u, int s, UIntType b, int t, UIntType c, int l>
  class mersenne_twister;

  // [5.1.4.3] Class template subtract_with_carry
  template<class IntType, IntType m, int s, int r>
  class subtract_with_carry;

  // [5.1.4.4] Class template subtract_with_carry_01
  template<class RealType, int w, int s, int r>
  class subtract_with_carry_01;

  // [5.1.4.5] Class template discard_block
  template<class UniformRandomNumberGenerator, int p, int r>
  class discard_block;

  // [5.1.4.6] Class template xor_combine
  template<class UniformRandomNumberGenerator1, int s1,
	   class UniformRandomNumberGenerator2, int s2>
  class xor_combine;

  // [5.1.6] Class random_device
  class random_device;

  // [5.1.7.1] Class template uniform_int
  template<class IntType>
  class uniform_int;

  // [5.1.7.2] Class bernoulli_distribution
  class bernoulli_distribution;

  // [5.1.7.3] Class template geometric_distribution
  template<class IntType, class RealType>
  class geometric_distribution;

  // [5.1.7.4] Class template poisson_distribution
  template<class IntType, class RealType>
  class poisson_distribution;

  // [5.1.7.5] Class template binomial_distribution
  template<class IntType, class RealType>
  class binomial_distribution;

  // [5.1.7.6] Class template uniform_real
  template<class RealType>
  class uniform_real;

  // [5.1.7.7] Class template exponential_distribution
  template<class RealType>
  class exponential_distribution;

  // [5.1.7.8] Class template normal_distribution
  template<class RealType>
  class normal_distribution;

  // [5.1.7.9] Class template gamma_distribution
  template<class RealType>
  class gamma_distribution;

} // namespace tr1
} // namespace std
