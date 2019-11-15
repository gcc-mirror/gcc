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

// { dg-options "-std=gnu++2a -pthread" }
// { dg-do run }
// { dg-require-effective-target c++2a }
// { dg-require-effective-target pthread }

#include <condition_variable>
#include <thread>
#include <mutex>
#include <chrono>
#include <testsuite_hooks.h>

using namespace::std::literals;

void test_wait_on_stop()
{
  bool ready = false;
  std::mutex mtx;
  std::condition_variable_any cv;

  std::stop_source src;

  auto tok = src.get_token();
  std::thread t([&ready, &mtx, &cv, tok]
                {
                  std::unique_lock lck(mtx);
                  auto res = cv.wait_on(lck, tok, [&ready] { return ready; });
                  if (!res)
                    {
                      VERIFY(tok.stop_requested());
                    }
                });

  std::this_thread::sleep_for(0.5s);
  VERIFY(!src.stop_requested());
  src.request_stop();
  t.join();
  VERIFY(src.stop_requested());
}

void test_wait_on_until(bool ck = true)
{
  bool ready = false;
  std::mutex mtx;
  std::condition_variable_any cv;

  std::stop_source src;

  auto abst = std::chrono::steady_clock::now() + 1.0s;
  auto tok = src.get_token();
  std::thread t([ck, &ready, &mtx, &cv, abst, tok]
                {
                  std::unique_lock lck(mtx);
                  auto res = cv.wait_on_until(lck, tok, abst, [&ready] { return ready; });
                  if (!res && ck)
                    {
                      VERIFY(tok.stop_requested());
                    }
                });

  if (ck)
    {
      std::this_thread::sleep_for(0.5s);
      VERIFY(!src.stop_requested());
      src.request_stop();
      t.join();
      VERIFY(src.stop_requested());
    }
  else
    {
      std::this_thread::sleep_for(1.5s);
      t.join();
      VERIFY(!src.stop_requested());
    }
}

void test_wait_on_for(bool ck = true)
{
  bool ready = false;
  std::mutex mtx;
  std::condition_variable_any cv;

  std::stop_source src;

  auto tok = src.get_token();
  std::thread t([ck, &ready, &mtx, &cv, tok]
                {
                  std::unique_lock lck(mtx);
                  auto res = cv.wait_on_for(lck, tok, 1.0s, [&ready] { return ready; });
                  if (!res && ck)
                    {
                      VERIFY(tok.stop_requested());
                    }
                });

  if (ck)
    {
      std::this_thread::sleep_for(0.5s);
      VERIFY(!src.stop_requested());
      src.request_stop();
      t.join();
      VERIFY(src.stop_requested());
    }
  else
    {
      std::this_thread::sleep_for(1.5s);
      t.join();
      VERIFY(!src.stop_requested());
    };
}

int main()
{
  test_wait_on_stop();
  test_wait_on_until(false);
  test_wait_on_until();
  test_wait_on_for();
  test_wait_on_for(false);
  return 0;
}
