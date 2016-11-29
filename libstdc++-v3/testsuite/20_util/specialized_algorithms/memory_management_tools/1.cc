// Copyright (C) 2016 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }

#include <memory>
#include <testsuite_hooks.h>
#include <string>
#include <array>
#include <vector>
#include <sstream>

int del_count = 0;
int ctor_count = 0;
int throw_after = 0;

struct DelCount
{
  ~DelCount() { ++del_count; }
};

struct ThrowAfterN
{
  ThrowAfterN()
  {
    if (++ctor_count == throw_after) {
      std::ostringstream os;
      os << "ThrowAfterN(), ctor_count: " << ctor_count
	 << " throw_after: " << throw_after << std::endl;
      throw std::runtime_error(os.str());
    }
  }
  ThrowAfterN(ThrowAfterN&&)
  {
    if (++ctor_count == throw_after) {
      std::ostringstream os;
      os << "ThrowAfterN(), ctor_count: " << ctor_count
	 << " throw_after: " << throw_after << std::endl;
      throw std::runtime_error(os.str());
    }
  }
  DelCount dc;
};

void test01()
{
  char test_data[] = "123456";
  std::uninitialized_default_construct(std::begin(test_data),
				       std::end(test_data));
  VERIFY(std::string(test_data) == "123456");
}

void test02()
{
  char test_data[] = "123456";
  std::uninitialized_value_construct(std::begin(test_data),
				       std::end(test_data));
  VERIFY(std::string(test_data, 6) == std::string("\0\0\0\0\0\0", 6));
}

void test03()
{
  char test_data[] = "123456";
  auto end = std::uninitialized_default_construct_n(std::begin(test_data), 6);
  VERIFY(std::string(test_data) == "123456");
  VERIFY( end == test_data + 6 );
}

void test04()
{
  char test_data[] = "123456";
  auto end = std::uninitialized_value_construct_n(std::begin(test_data), 5);
  VERIFY(std::string(test_data, 6) == std::string("\0\0\0\0\0" "6", 6));
  VERIFY( end == test_data + 5 );
}

void test05()
{
  del_count = 0;
  DelCount* x = (DelCount*)malloc(sizeof(DelCount));
  new (x) DelCount;
  std::destroy_at(&x[0]);
  VERIFY(del_count == 1);
  del_count = 0;
  free(x);
}

void test06()
{
  del_count = 0;
  DelCount* x = (DelCount*)malloc(sizeof(DelCount)*10);
  for (int i = 0; i < 10; ++i) new (x+i) DelCount;
  std::destroy(x, x+10);
  VERIFY(del_count == 10);
  del_count = 0;
  free(x);
}

void test07()
{
  del_count = 0;
  DelCount* x = (DelCount*)malloc(sizeof(DelCount)*10);
  for (int i = 0; i < 10; ++i) new (x+i) DelCount;
  auto end = std::destroy_n(x, 10);
  VERIFY(del_count == 10);
  VERIFY( end == x + 10 );
  del_count = 0;
  free(x);
}

void test08()
{
  std::vector<std::unique_ptr<int>> source;
  for (int i = 0; i < 10; ++i) source.push_back(std::make_unique<int>(i));
  std::unique_ptr<int>* target =
    (std::unique_ptr<int>*)malloc(sizeof(std::unique_ptr<int>)*10);
  std::uninitialized_move(source.begin(), source.end(), target);
  for (const auto& x : source) VERIFY(!x);
  for (int i = 0; i < 10; ++i) VERIFY(bool(*(target+i)));
  auto end = std::destroy_n(target, 10);
  VERIFY( end == target + 10 );
  free(target);
}

void test09()
{
  std::vector<std::unique_ptr<int>> source;
  for (int i = 0; i < 10; ++i) source.push_back(std::make_unique<int>(i));
  std::unique_ptr<int>* target =
    (std::unique_ptr<int>*)malloc(sizeof(std::unique_ptr<int>)*10);
  auto end = std::uninitialized_move_n(source.begin(), 10, target);
  VERIFY( end.first == source.begin() + 10 );
  VERIFY( end.second == target + 10 );
  for (const auto& x : source) VERIFY(!x);
  for (int i = 0; i < 10; ++i) VERIFY(bool(*(target+i)));
  auto end2 = std::destroy_n(target, 10);
  VERIFY( end2 == target + 10 );
  free(target);
}

void test10()
{
  char* x = (char*)malloc(sizeof(char)*10);
  for (int i = 0; i < 10; ++i) new (x+i) char;
  std::destroy(x, x+10);
  free(x);
}

void test11()
{
  char* x = (char*)malloc(sizeof(char)*10);
  for (int i = 0; i < 10; ++i) new (x+i) char;
  auto end = std::destroy_n(x, 10);
  VERIFY( end == x + 10 );
  free(x);
}

void test12()
{
  throw_after = 5;
  del_count = 0;
  ctor_count = 0;
  ThrowAfterN* target =
    (ThrowAfterN*)malloc(sizeof(ThrowAfterN)*10);
  try {
    std::uninitialized_default_construct(target, target+10);
  } catch (...) {
  }
  VERIFY(ctor_count == 5);
  VERIFY(del_count == 5);
  throw_after = 0;
  del_count = 0;
  ctor_count = 0;
}

void test13()
{
  throw_after = 5;
  del_count = 0;
  ctor_count = 0;
  ThrowAfterN* target =
    (ThrowAfterN*)malloc(sizeof(ThrowAfterN)*10);
  try {
    std::uninitialized_value_construct(target, target+10);
  } catch (...) {
  }
  VERIFY(ctor_count == 5);
  VERIFY(del_count == 5);
  throw_after = 0;
  del_count = 0;
  ctor_count = 0;
}

void test14()
{
  throw_after = 5;
  del_count = 0;
  ctor_count = 0;
  ThrowAfterN* target =
    (ThrowAfterN*)malloc(sizeof(ThrowAfterN)*10);
  try {
    std::uninitialized_default_construct_n(target, 10);
  } catch (...) {
  }
  VERIFY(ctor_count == 5);
  VERIFY(del_count == 5);
  throw_after = 0;
  del_count = 0;
  ctor_count = 0;
}

void test15()
{
  throw_after = 5;
  del_count = 0;
  ctor_count = 0;
  ThrowAfterN* target =
    (ThrowAfterN*)malloc(sizeof(ThrowAfterN)*10);
  try {
    std::uninitialized_value_construct_n(target, 10);
  } catch (...) {
  }
  VERIFY(ctor_count == 5);
  VERIFY(del_count == 5);
  throw_after = 0;
  del_count = 0;
  ctor_count = 0;
}

void test16()
{
  std::vector<ThrowAfterN> source(10);
  del_count = 0;
  ctor_count = 0;
  throw_after = 5;
  throw_after = 5;
  ThrowAfterN* target =
    (ThrowAfterN*)malloc(sizeof(ThrowAfterN)*10);
  try {
    std::uninitialized_move(source.begin(), source.end(), target);
  } catch (...) {
  }
  VERIFY(ctor_count == 5);
  VERIFY(del_count == 5);
  throw_after = 0;
  del_count = 0;
  ctor_count = 0;
}

void test17()
{
  std::vector<ThrowAfterN> source(10);
  del_count = 0;
  ctor_count = 0;
  throw_after = 5;
  ThrowAfterN* target =
    (ThrowAfterN*)malloc(sizeof(ThrowAfterN)*10);
  try {
    std::uninitialized_move_n(source.begin(), 10, target);
  } catch (...) {
  }
  VERIFY(ctor_count == 5);
  VERIFY(del_count == 5);
  throw_after = 0;
  del_count = 0;
  ctor_count = 0;
}

void test18()
{
  char test_source[] = "123456";
  char test_target[] = "000000";
  std::uninitialized_move(std::begin(test_source),
			  std::end(test_source),
			  test_target);
  VERIFY(std::string(test_target) == "123456");
}

void test19()
{
  char test_source[] = "123456";
  char test_target[] = "000000";
  auto end = std::uninitialized_move_n(std::begin(test_source),
                                       6,
                                       test_target);
  VERIFY(std::string(test_target) == "123456");
  VERIFY( end.first == test_source + 6 );
  VERIFY( end.second == test_target + 6 );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  test09();
  test10();
  test11();
  test12();
  test13();
  test14();
  test15();
  test16();
  test17();
  test18();
  test19();
}
