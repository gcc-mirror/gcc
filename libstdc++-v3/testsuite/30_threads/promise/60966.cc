// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

// libstdc++/60966
// This test hangs if std::promise::~promise() destroys the
// shared state before std::promise::set_value() finishes using it.

#include <future>
#include <thread>
#include <vector>

const int THREADS = 10;

void run_task(std::promise<void>* pr)
{
  std::this_thread::sleep_for(std::chrono::milliseconds(100));
  pr->set_value();
}

int main()
{
  std::vector<std::promise<void>*> tasks(THREADS);
  std::vector<std::thread> threads(THREADS);
  std::vector<std::future<void>> futures(THREADS);

  for (int i = 0; i < THREADS; ++i)
  {
    std::promise<void>* task = new std::promise<void>;
    tasks[i] = task;
    futures[i] = task->get_future();
    threads[i] = std::thread(run_task, task);
  }

  for (int i = 0; i < THREADS; ++i)
  {
    // the temporary future releases the state as soon as wait() returns
    std::future<void>(std::move(futures[i])).wait();
    // state is ready, should now be safe to delete promise, so it
    // releases the shared state too
    delete tasks[i];
  }

  for (auto& t : threads)
    t.join();
}
