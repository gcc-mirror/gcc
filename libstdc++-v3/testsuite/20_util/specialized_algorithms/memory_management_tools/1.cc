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

int del_count = 0;

struct DelCount
{
  ~DelCount() { ++del_count; }
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
  std::uninitialized_default_construct_n(std::begin(test_data), 6);
  VERIFY(std::string(test_data) == "123456");
}

void test04()
{
  char test_data[] = "123456";
  std::uninitialized_value_construct_n(std::begin(test_data), 6);
  VERIFY(std::string(test_data, 6) == std::string("\0\0\0\0\0\0", 6));
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
  std::destroy_n(x, 10);
  VERIFY(del_count == 10);
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
  std::destroy_n(target, 10);
  free(target);
}

void test09()
{
  std::vector<std::unique_ptr<int>> source;
  for (int i = 0; i < 10; ++i) source.push_back(std::make_unique<int>(i));
  std::unique_ptr<int>* target =
    (std::unique_ptr<int>*)malloc(sizeof(std::unique_ptr<int>)*10);
  std::uninitialized_move_n(source.begin(), 10, target);
  for (const auto& x : source) VERIFY(!x);
  for (int i = 0; i < 10; ++i) VERIFY(bool(*(target+i)));
  std::destroy_n(target, 10);
  free(target);
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
}
