// -*- C++ -*-
// { dg-options "-ltbb" }
// { dg-do run { target c++17 } }
// { dg-timeout-factor 3 }
// { dg-require-effective-target tbb_backend }

//===-- set.pass.cpp ------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// Note: This file was derived from set.pass.cpp which is part of the upstream
// source.

#include "pstl/pstl_test_config.h"

#ifdef PSTL_STANDALONE_TESTS

#include <cmath>
#include <chrono>

#include "pstl/execution"
#include "pstl/algorithm"
#else
#include <execution>
#include <algorithm>
#endif // PSTL_STANDALONE_TESTS

#include "pstl/test_utils.h"

#include "set_util.h"

using namespace TestUtils;

template <typename Type>
struct test_set_intersection
{
    template <typename Policy, typename InputIterator1, typename InputIterator2, typename Compare>
    typename std::enable_if<!TestUtils::isReverse<InputIterator1>::value, void>::type
    operator()(Policy&& exec, InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2,
               Compare comp)
    {
        using T1 = typename std::iterator_traits<InputIterator1>::value_type;

        auto n1 = std::distance(first1, last1);
        auto n2 = std::distance(first2, last2);
        auto n = n1 + n2;
        Sequence<T1> expect(n);
        Sequence<T1> out(n);

        auto expect_res = std::set_intersection(first1, last1, first2, last2, expect.begin(), comp);
        auto res = std::set_intersection(exec, first1, last1, first2, last2, out.begin(), comp);

        EXPECT_TRUE(expect_res - expect.begin() == res - out.begin(), "wrong result for set_intersection");
        EXPECT_EQ_N(expect.begin(), out.begin(), std::distance(out.begin(), res), "wrong set_intersection effect");
    }

    template <typename Policy, typename InputIterator1, typename InputIterator2, typename Compare>
    typename std::enable_if<TestUtils::isReverse<InputIterator1>::value, void>::type
    operator()(Policy&&, InputIterator1, InputIterator1, InputIterator2, InputIterator2, Compare)
    {
    }
};

template <typename T>
struct test_non_const_set_intersection
{
    template <typename Policy, typename InputIterator, typename OutputInterator>
    void
    operator()(Policy&& exec, InputIterator input_iter, OutputInterator out_iter)
    {
        set_intersection(exec, input_iter, input_iter, input_iter, input_iter, out_iter, non_const(std::less<T>()));
    }
};

int
main()
{
    test_set_op<test_set_intersection<float64_t>, float64_t, float64_t>(std::less<>());
    test_set_op<test_set_intersection<Num<int64_t>>, Num<int64_t>, Num<int32_t>>([](const Num<int64_t>& x, const Num<int32_t>& y) { return x < y; });

    test_set_op<test_set_intersection<MemoryChecker>, MemoryChecker, MemoryChecker>([](const MemoryChecker& val1, const MemoryChecker& val2) -> bool {
        return val1.value() < val2.value();
    });
    EXPECT_FALSE(MemoryChecker::alive_objects() < 0, "wrong effect from set algorithms: number of ctors calls < num of dtors calls");
    EXPECT_FALSE(MemoryChecker::alive_objects() > 0, "wrong effect from set algorithms: number of ctors calls > num of dtors calls");

    std::cout << done() << std::endl;

    test_algo_basic_double<int32_t>(run_for_rnd_fw<test_non_const_set_intersection<int32_t>>());
}
