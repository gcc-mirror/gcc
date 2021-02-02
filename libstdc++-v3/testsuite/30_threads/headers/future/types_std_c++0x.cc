// { dg-do compile { target c++11 } }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2021 Free Software Foundation, Inc.
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

#include <future>

void test01()
{
  typedef std::future_errc errc_t;

  using std::future_category;

  typedef std::future_error error_t;

  typedef std::future<int> uniq_t;
  typedef std::future<int&> uniqr_t;
  typedef std::future<void> uniqv_t;

  typedef std::shared_future<int> shar_t;
  typedef std::shared_future<int&> sharr_t;
  typedef std::shared_future<void> sharv_t;

  typedef std::promise<int> promise_t;
  typedef std::promise<int&> promiser_t;
  typedef std::promise<void> promisev_t;

  typedef std::packaged_task<int> ptask_t;
  typedef std::packaged_task<int&> ptaskr_t;
  typedef std::packaged_task<void> ptaskv_t;

  using std::async;
}
