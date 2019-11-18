// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <thread>
#include <chrono>
#include <cassert>
#include <atomic>

using namespace::std::literals;

//------------------------------------------------------

void test_no_stop_token()
{
  // test the basic jthread API (not taking stop_token arg)

  assert(std::jthread::hardware_concurrency() == std::thread::hardware_concurrency()); 
  std::stop_token stoken;
  assert(!stoken.stop_possible());
  {
    std::jthread::id t1ID{std::this_thread::get_id()};
    std::atomic<bool> t1AllSet{false};
    std::jthread t1([&t1ID, &t1AllSet] {
                   t1ID = std::this_thread::get_id();
                   t1AllSet.store(true);
                   for (int c='9'; c>='0'; --c) {
                      std::this_thread::sleep_for(222ms);
                   }
                 });
    for (int i=0; !t1AllSet.load(); ++i) {
      std::this_thread::sleep_for(10ms);
    }
    assert(t1.joinable());
    assert(t1ID == t1.get_id());
    stoken = t1.get_stop_token();
    assert(!stoken.stop_requested());
  } 
  assert(stoken.stop_requested());
}

//------------------------------------------------------

void test_stop_token()
{
  // test the basic thread API (taking stop_token arg)

  std::stop_source ssource;
  std::stop_source origsource;
  assert(ssource.stop_possible());
  assert(!ssource.stop_requested());
  {
    std::jthread::id t1ID{std::this_thread::get_id()};
    std::atomic<bool> t1AllSet{false};
    std::atomic<bool> t1done{false};
    std::jthread t1([&t1ID, &t1AllSet, &t1done] (std::stop_token st) {
                       // check some values of the started thread:
                       t1ID = std::this_thread::get_id();
                       t1AllSet.store(true);
                       for (int i=0; !st.stop_requested(); ++i) {
                          std::this_thread::sleep_for(100ms);
                       }
                       t1done.store(true);
                     },
                     ssource.get_token());
    for (int i=0; !t1AllSet.load(); ++i) {
      std::this_thread::sleep_for(10ms);
    }
    // and check all values:
    assert(t1.joinable());
    assert(t1ID == t1.get_id());

    std::this_thread::sleep_for(470ms);
    origsource = std::move(ssource);
    ssource = t1.get_stop_source();
    assert(!ssource.stop_requested());
    auto ret = ssource.request_stop();
    assert(ret);
    ret = ssource.request_stop();
    assert(!ret);
    assert(ssource.stop_requested());
    assert(!t1done.load());
    assert(!origsource.stop_requested());

    std::this_thread::sleep_for(470ms);
    origsource.request_stop();
  } 
  assert(origsource.stop_requested());
  assert(ssource.stop_requested());
}

//------------------------------------------------------

void test_join()
{
  std::stop_source ssource;
  assert(ssource.stop_possible());
  {
    std::jthread t1([](std::stop_token stoken) {
                      for (int i=0; !stoken.stop_requested(); ++i) {
                         std::this_thread::sleep_for(100ms);
                      }
                 });
    ssource = t1.get_stop_source();
    std::jthread t2([ssource] () mutable {
                     for (int i=0; i < 10; ++i) {
                       std::this_thread::sleep_for(70ms);
                     }
                     ssource.request_stop();
                   });
    // wait for all thread to finish:
    t2.join();
    assert(!t2.joinable());
    assert(t1.joinable());
    t1.join();
    assert(!t1.joinable());
  }
}

//------------------------------------------------------

void test_detach()
{
  std::stop_source ssource;
  assert(ssource.stop_possible());
  std::atomic<bool> t1FinallyInterrupted{false};
  {
    std::jthread t0;
    std::jthread::id t1ID{std::this_thread::get_id()};
    bool t1IsInterrupted;
    std::stop_token t1InterruptToken;
    std::atomic<bool> t1AllSet{false};
    std::jthread t1([&t1ID, &t1IsInterrupted, &t1InterruptToken, &t1AllSet, &t1FinallyInterrupted]
                    (std::stop_token stoken) {
                   // check some values of the started thread:
                   t1ID = std::this_thread::get_id();
                   t1InterruptToken = stoken;
                   t1IsInterrupted = stoken.stop_requested();
                   assert(stoken.stop_possible());
                   assert(!stoken.stop_requested());
                   t1AllSet.store(true);
                   for (int i=0; !stoken.stop_requested(); ++i) {
                      std::this_thread::sleep_for(100ms);
                   }
                   t1FinallyInterrupted.store(true);
                 });
    for (int i=0; !t1AllSet.load(); ++i) {
      std::this_thread::sleep_for(10ms);
    }
    assert(!t0.joinable());
    assert(t1.joinable());
    assert(t1ID == t1.get_id());
    assert(t1IsInterrupted == false);
    assert(t1InterruptToken == t1.get_stop_source().get_token());
    ssource = t1.get_stop_source();
    assert(t1InterruptToken.stop_possible());
    assert(!t1InterruptToken.stop_requested());
    t1.detach();
    assert(!t1.joinable());
  }

  assert(!t1FinallyInterrupted.load());
  ssource.request_stop();
  assert(ssource.stop_requested());
  for (int i=0; !t1FinallyInterrupted.load() && i < 100; ++i) {
    std::this_thread::sleep_for(100ms);
  }
  assert(t1FinallyInterrupted.load());
}

int main()
{
  std::set_terminate([](){
                       assert(false);
                     });

  test_no_stop_token();
  test_stop_token();
  test_join();
  test_detach();
}

