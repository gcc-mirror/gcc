// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

#include <atomic>
#include <testsuite_common_types.h>

#define TEST_ALL_ORDERS()                       \
  do {                                          \
    ORDER_TEST(std::memory_order_relaxed);      \
    ORDER_TEST(std::memory_order_consume);      \
    ORDER_TEST(std::memory_order_acquire);      \
    ORDER_TEST(std::memory_order_release);      \
    ORDER_TEST(std::memory_order_acq_rel);      \
    ORDER_TEST(std::memory_order_seq_cst);      \
  } while(0)
  
void test01()
{
#define ORDER_TEST(ORDER)                                               \
  do {                                                                  \
    __gnu_test::compare_exchange_order_lowering<ORDER> test;            \
    __gnu_cxx::typelist::apply_generator(test,                          \
                                         __gnu_test::integral_types::type()); \
  } while (0);
  TEST_ALL_ORDERS();
#undef ORDER_TEST

  enum e { a, b, c };
#define ORDER_TEST(ORDER)                               \
  do {                                                  \
    std::atomic<e> x(a);                                \
    e expected = a;                                     \
    x.compare_exchange_strong(expected, b, ORDER);      \
    x.compare_exchange_weak(expected, c, ORDER);        \
  } while (0);
  TEST_ALL_ORDERS();
#undef ORDER_TEST

#define ORDER_TEST(ORDER)                       \
  do {                                          \
    std::atomic<void*> x(nullptr);              \
    void* expected = nullptr;                   \
    x.compare_exchange_strong(expected, nullptr, ORDER);        \
    x.compare_exchange_weak(expected, nullptr, ORDER);          \
  } while (0);
  TEST_ALL_ORDERS();
#undef ORDER_TEST
}
