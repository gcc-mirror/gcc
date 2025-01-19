// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#ifndef TESTS_BITS_MAIN_H_
#define TESTS_BITS_MAIN_H_

#include "verify.h"

template <class T>
  void
  iterate_abis()
  {
    using namespace std::experimental::parallelism_v2;
#ifndef EXTENDEDTESTS
    invoke_test<simd<T, simd_abi::scalar>>(int());
    invoke_test<simd<T, simd_abi::_VecBuiltin<16>>>(int());
    invoke_test<simd<T, simd_abi::_VecBltnBtmsk<64>>>(int());
    invoke_test<simd<T, simd_abi::_SveAbi<16>>>(int());
    invoke_test<simd<T, simd_abi::_SveAbi<32>>>(int());
    invoke_test<simd<T, simd_abi::_SveAbi<64>>>(int());
#elif EXTENDEDTESTS == 0
    invoke_test<simd<T, simd_abi::_VecBuiltin<8>>>(int());
    invoke_test<simd<T, simd_abi::_VecBuiltin<12>>>(int());
    invoke_test<simd<T, simd_abi::_VecBuiltin<24>>>(int());
    invoke_test<simd<T, simd_abi::_VecBuiltin<32>>>(int());
    invoke_test<simd<T, simd_abi::_VecBltnBtmsk<56>>>(int());
#elif EXTENDEDTESTS == 1
    invoke_test<simd<T, simd_abi::fixed_size<8>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<16>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<24>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<32>>>(int());
#elif EXTENDEDTESTS == 2
    invoke_test<simd<T, simd_abi::fixed_size<1>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<9>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<17>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<25>>>(int());
#elif EXTENDEDTESTS == 3
    invoke_test<simd<T, simd_abi::fixed_size<2>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<10>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<18>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<26>>>(int());
#elif EXTENDEDTESTS == 4
    invoke_test<simd<T, simd_abi::fixed_size<3>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<19>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<11>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<27>>>(int());
#elif EXTENDEDTESTS == 5
    invoke_test<simd<T, simd_abi::fixed_size<4>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<12>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<20>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<28>>>(int());
#elif EXTENDEDTESTS == 6
    invoke_test<simd<T, simd_abi::fixed_size<5>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<13>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<21>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<29>>>(int());
#elif EXTENDEDTESTS == 7
    invoke_test<simd<T, simd_abi::fixed_size<6>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<14>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<22>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<30>>>(int());
#elif EXTENDEDTESTS == 8
    invoke_test<simd<T, simd_abi::fixed_size<7>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<15>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<23>>>(int());
    invoke_test<simd<T, simd_abi::fixed_size<31>>>(int());
#endif
  }

int main()
{
  iterate_abis<_GLIBCXX_SIMD_TESTTYPE>();
  return 0;
}

#endif  // TESTS_BITS_MAIN_H_
