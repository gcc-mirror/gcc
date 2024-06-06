// -*- C++ -*-

//===-- set.pass.cpp ------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// Note: This file was derived from set.pass.cpp which is part of the upstream
// source.

#ifndef __PSTL_TEST_SET_UTIL_H
#define __PSTL_TEST_SET_UTIL_H

namespace TestUtils
{
    template <typename T>
    struct Num
    {
        T val;

        Num() : val{} {}
        Num(const T& v) : val(v) {}

        //for "includes" checks
        template <typename T1>
        bool
        operator<(const Num<T1>& v1) const
        {
            return val < v1.val;
        }

        //The types Type1 and Type2 must be such that an object of type InputIt can be dereferenced and then implicitly converted to both of them
        template <typename T1>
        operator Num<T1>() const
        {
            return Num<T1>((T1)val);
        }

        friend bool
        operator==(const Num& v1, const Num& v2)
        {
            return v1.val == v2.val;
        }
    };

    template <typename Operation,
              typename T1, typename T2, typename Compare>
    void
    test_set_op(Compare compare)
    {
#ifdef _GLIBCXX_DEBUG
        const std::size_t n_max = 1000;
#else
        const std::size_t n_max = 100000;
#endif

        // The rand()%(2*n+1) encourages generation of some duplicates.
        std::srand(4200);

        for (std::size_t n = 0; n < n_max; n = n <= 16 ? n + 1 : size_t(3.1415 * n))
        {
            for (std::size_t m = 0; m < n_max; m = m <= 16 ? m + 1 : size_t(2.71828 * m))
            {
                //prepare the input ranges
                Sequence<T1> in1(n, [](std::size_t k) { return rand() % (2 * k + 1); });
                Sequence<T2> in2(m, [m](std::size_t k) { return (m % 2) * rand() + rand() % (k + 1); });

                std::sort(in1.begin(), in1.end(), compare);
                std::sort(in2.begin(), in2.end(), compare);

                invoke_on_all_policies(Operation(), in1.begin(), in1.end(), in2.cbegin(), in2.cend(),
                                          compare);
            }
        }
    }
} // namespace TestUtils
#endif // __PSTL_TEST_SET_UTIL_H
