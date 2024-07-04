// { dg-do run { target c++11 } }
// { dg-options "-g -O0" }

// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

#include <forward_list>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <memory>
#include <iostream>
#include <future>
#include <initializer_list>
#include <atomic>
#include "../util/testsuite_allocator.h" // NullablePointer

typedef std::tuple<int, int> ExTuple;

template<class T>
void
placeholder(const T &s)
{
  std::cout << s;
}

template<class T, class S>
void
placeholder(const std::pair<T,S> &s)
{
  std::cout << s.first;
}

template<class T>
void
use(const T &container)
{
  for (typename T::const_iterator i = container.begin();
       i != container.end();
       ++i)
    placeholder(*i);
}

struct datum
{
  std::string s;
  int i;
};

std::unique_ptr<datum> global;

struct custom_cat : std::error_category {
  const char* name() const noexcept { return "miaow"; }
  std::string message(int) const { return ""; }
};

int
main()
{
  std::forward_list<int> efl;
// { dg-final { regexp-test efl "empty std::(__debug::)?forward_list" } }

  std::forward_list<int> &refl = efl;
// { dg-final { regexp-test refl "empty std::(__debug::)?forward_list" } }

  std::forward_list<int> fl;
  fl.push_front(2);
  fl.push_front(1);
// { dg-final { regexp-test fl {std::(__debug::)?forward_list = {\[0\] = 1, \[1\] = 2}} } }

  std::forward_list<int> &rfl = fl;
// { dg-final { regexp-test rfl {std::(__debug::)?forward_list = {\[0\] = 1, \[1\] = 2}} } }

  std::unordered_map<int, std::string> eum;
// { dg-final { regexp-test eum "std::(__debug::)?unordered_map with 0 elements" } }
  std::unordered_map<int, std::string> &reum = eum;
// { dg-final { regexp-test reum "std::(__debug::)?unordered_map with 0 elements" } }

  std::unordered_multimap<int, std::string> eumm;
// { dg-final { regexp-test eumm "std::(__debug::)?unordered_multimap with 0 elements" } }
  std::unordered_multimap<int, std::string> &reumm = eumm;
// { dg-final { regexp-test reumm "std::(__debug::)?unordered_multimap with 0 elements" } }

  std::unordered_set<int> eus;
// { dg-final { regexp-test eus "std::(__debug::)?unordered_set with 0 elements" } }
  std::unordered_set<int> &reus = eus;
// { dg-final { regexp-test reus "std::(__debug::)?unordered_set with 0 elements" } }

  std::unordered_multiset<int> eums;
// { dg-final { regexp-test eums "std::(__debug::)?unordered_multiset with 0 elements" } }
  std::unordered_multiset<int> &reums = eums;
// { dg-final { regexp-test reums "std::(__debug::)?unordered_multiset with 0 elements" } }

  std::unordered_map<int, std::string> uom;
  uom[5] = "three";
  uom[3] = "seven";
// { dg-final { regexp-test uom {std::(__debug::)?unordered_map with 2 elements = {\[3\] = "seven", \[5\] = "three"}} } }

  std::unordered_map<int, std::string> &ruom = uom;
// { dg-final { regexp-test ruom {std::(__debug::)?unordered_map with 2 elements = {\[3\] = "seven", \[5\] = "three"}} } }

  std::unordered_multimap<int, std::string> uomm;
  uomm.insert(std::pair<int, std::string> (5, "three"));
  uomm.insert(std::pair<int, std::string> (5, "seven"));
// { dg-final { regexp-test uomm {std::(__debug::)?unordered_multimap with 2 elements = {\[5\] = "seven", \[5\] = "three"}} } }
  std::unordered_multimap<int, std::string> &ruomm = uomm;
// { dg-final { regexp-test ruomm {std::(__debug::)?unordered_multimap with 2 elements = {\[5\] = "seven", \[5\] = "three"}} } }

  std::unordered_set<int> uos;
  uos.insert(5);
// { dg-final { regexp-test uos {std::(__debug::)?unordered_set with 1 element = {\[0\] = 5}} } }
  std::unordered_set<int> &ruos = uos;
// { dg-final { regexp-test ruos {std::(__debug::)?unordered_set with 1 element = {\[0\] = 5}} } }

  std::unordered_multiset<int> uoms;
  uoms.insert(5);
// { dg-final { regexp-test uoms {std::(__debug::)?unordered_multiset with 1 element = {\[0\] = 5}} } }
  std::unordered_multiset<int> &ruoms = uoms;
// { dg-final { regexp-test ruoms {std::(__debug::)?unordered_multiset with 1 element = {\[0\] = 5}} } }

  std::unique_ptr<datum> uptr (new datum);
  uptr->s = "hi bob";
  uptr->i = 23;
// { dg-final { regexp-test uptr {std::unique_ptr.datum. = {get\(\) = 0x.*}} } }
  std::unique_ptr<datum> &ruptr = uptr;
// { dg-final { regexp-test ruptr {std::unique_ptr.datum. = {get\(\) = 0x.*}} } }

  using data = datum[];
  std::unique_ptr<data> arrptr (new datum[2]);
// { dg-final { regexp-test arrptr {std::unique_ptr.datum \[\]. = {get\(\) = 0x.*}} } }
  std::unique_ptr<data>& rarrptr = arrptr;
// { dg-final { regexp-test rarrptr {std::unique_ptr.datum \[\]. = {get\(\) = 0x.*}} } }

  struct Deleter
  {
    int deleter_member = -1;
    using pointer = __gnu_test::NullablePointer<void>;
    void operator()(pointer) const noexcept { }
  };
  static_assert( !std::is_empty<Deleter>(), "Deleter is not empty" );
  static_assert( std::is_empty<Deleter::pointer>(), "but pointer is empty" );

  std::unique_ptr<int, Deleter> empty_ptr;
// { dg-final { note-test empty_ptr {std::unique_ptr<int> = {get() = {<No data fields>}}} } }
  std::unique_ptr<int, Deleter>& rempty_ptr = empty_ptr;
// { dg-final { note-test rempty_ptr {std::unique_ptr<int> = {get() = {<No data fields>}}} } }

  struct Deleter_pr103086
  {
    int deleter_member = -1;
    void operator()(int*) const noexcept { }
  };

  std::unique_ptr<int, Deleter_pr103086> uniq_ptr;
// { dg-final { note-test uniq_ptr {std::unique_ptr<int> = {get() = 0x0}} } }
  std::unique_ptr<int, Deleter_pr103086>& runiq_ptr = uniq_ptr;
// { dg-final { note-test runiq_ptr {std::unique_ptr<int> = {get() = 0x0}} } }

  ExTuple tpl(6,7);
// { dg-final { note-test tpl {std::tuple containing = {[0] = 6, [1] = 7}} } }
  ExTuple &rtpl = tpl;
// { dg-final { note-test rtpl {std::tuple containing = {[0] = 6, [1] = 7}} } }

  std::error_code e0;
  // { dg-final { note-test e0 {std::error_code = { }} } }
  std::error_condition ec0;
  // { dg-final { note-test ec0 {std::error_condition = { }} } }
  std::error_code einval = std::make_error_code(std::errc::invalid_argument);
  // { dg-final { note-test einval {std::error_code = {"generic": EINVAL}} } }
  std::error_condition ecinval = std::make_error_condition(std::errc::invalid_argument);
  // { dg-final { note-test ecinval {std::error_condition = {"generic": EINVAL}} } }

  custom_cat cat;
  std::error_code emiaow(42, cat);
  // { dg-final { note-test emiaow {std::error_code = {custom_cat: 42}} } }
  std::error_condition ecmiaow(42, cat);
  // { dg-final { note-test ecmiaow {std::error_condition = {custom_cat: 42}} } }

  std::error_code ecio = std::make_error_code(std::io_errc::stream);
  // { dg-final { note-test ecio {std::error_code = {"io": stream}} } }
  std::error_code ecfut0 = std::make_error_code(std::future_errc{});
  // { dg-final { note-test ecfut0 {std::error_code = {"future": 0}} } }

  std::initializer_list<int> emptyIl = {};
  // { dg-final { note-test emptyIl {std::initializer_list of length 0} } }
  std::initializer_list<int> il = {3, 4};
  // { dg-final { note-test il {std::initializer_list of length 2 = {3, 4}} } }

  std::atomic<int> ai{100};
  // { dg-final { note-test ai {std::atomic<int> = { 100 }} } }
  long l{};
  std::atomic<long*> ap{&l};
  // { dg-final { regexp-test ap {std::atomic.long \*. = { 0x.* }} } }
  struct Value { int i, j; };
  std::atomic<Value> av{{8, 9}};
  // { dg-final { note-test av {std::atomic<Value> = { {i = 8, j = 9} }} } }

  std::integral_constant<int, 1> one;
  // { dg-final { note-test one {std::integral_constant<int, 1>} } }
  std::integral_constant<bool, true> truth;
  // { dg-final { note-test truth {std::true_type} } }
  std::integral_constant<bool, 0> lies;
  // { dg-final { note-test lies {std::false_type} } }

  placeholder(""); // Mark SPOT
  use(efl);
  use(fl);
  use(eum);
  use(eumm);
  use(eus);
  use(eums);
  use(uoms);
  use(uptr->s);
  use(arrptr[0].s);

  std::cout << "\n";
  return 0;
}

// { dg-final { gdb-test SPOT } }
