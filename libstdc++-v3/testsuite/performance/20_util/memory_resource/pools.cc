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

// Override the -std flag in the check_performance script: STD=gnu++17

// Run the test as both single- and multi-threaded: TEST_B

#include <memory_resource>
#include <list>
#include <string>
#include <testsuite_performance.h>

const int iterations = 100;

// Insert and remove elements of various sizes in std::list containers.
// If timers!=nullptr the function will pause the timer while the lists
// are cleared and deallocated, so that only insertions/removals are timed.
// Otherwise, the time taken to deallocate the lists is also counted.
void
populate_lists(std::pmr::memory_resource* r, __gnu_test::time_counter* timers,
	       int kmax = iterations)
{
  struct size16 { char c[16]; };
  struct size32 { char c[32]; };
  struct size64 { char c[64]; };
  struct size128 { char c[128]; };

  std::pmr::list<int> l4(r);
  std::pmr::list<size16> l16(r);
  std::pmr::list<size32> l32(r);
  std::pmr::list<size64> l64(r);
  std::pmr::list<size128> l128(r);

  const int imax = 1000;
  const int jmax = 100;
  for (int k = 0; k < kmax; ++k)
  {
    for (int i = 0; i < imax; ++i)
    {
      for (int j = 0; j < jmax; ++j)
      {
        l4.emplace_back();
        l16.emplace_back();
        l32.emplace_back();
        l64.emplace_back();
        l128.emplace_back();
      }
      l4.pop_front();
      l16.pop_front();
      l32.pop_front();
      l64.pop_front();
      l128.pop_front();
    }

    if (timers)
      timers->stop();

    // Deallocate everything:
    l4.clear();
    l16.clear();
    l32.clear();
    l64.clear();
    l128.clear();

    if (timers)
      timers->restart();
  }
}

// Test allocations and deallocations of node-based containers (std::list).
// In this test pmr::unsynchronized_pool_resource should be faster than
// pmr::new_delete_resource().
void test_lists_single_thread()
{
  std::pmr::memory_resource* newdel = std::pmr::new_delete_resource();
  std::pmr::unsynchronized_pool_resource pool;
#ifndef NOTHREAD
  std::pmr::synchronized_pool_resource syncpool;
#endif

  auto run_test = [](auto* memres, std::string name, bool time_dtors) {
    name += " std::list push/pop";
    if (time_dtors)
      name += "/destroy";
    __gnu_test::time_counter time;
    __gnu_test::resource_counter resource;
    start_counters(time, resource);
    populate_lists(memres, time_dtors ? nullptr : &time);
    stop_counters(time, resource);
    report_performance(__FILE__, name, time, resource);
  };

  for (auto time_dtors : {false, true})
  {
    run_test(newdel, "new-delete-1 ", time_dtors);
    run_test(newdel, "new-delete-2 ", time_dtors);
    run_test(newdel, "new-delete-3 ", time_dtors);

    // Start with an empty set of pools:
    pool.release();
    run_test(&pool, "unsync-pool-1", time_dtors);
    // Destroy pools and start fresh:
    pool.release();
    run_test(&pool, "unsync-pool-2", time_dtors);
    // Do not destroy pools, reuse allocated memory:
    run_test(&pool, "unsync-pool-3", time_dtors);

#ifndef NOTHREAD
    syncpool.release();
    run_test(&syncpool, "sync-pool-1  ", time_dtors);
    // Destroy pools and start fresh:
    syncpool.release();
    run_test(&syncpool, "sync-pool-2  ", time_dtors);
    // Do not destroy pools, reuse allocated memory:
    run_test(&syncpool, "sync-pool-3  ", time_dtors);
#endif
  }
}

// TODO test non-pooled large allocations from (un)synchronized_pool_resource

#ifndef NOTHREAD
# include <thread>
# include <mutex>
# include <cassert>

// Multithreaded std::list test with each thread having its own resource.
// (pmr::new_delete vs pmr::unsynchronized_pool vs pmr::synchronized_pool)
//
// In this test both pmr::unsynchronized_pool_resource and
// pmr::synchronized_pool_resource should be faster than
// pmr::new_delete_resource().
void test_lists_resource_per_thread()
{
  std::mutex mx;
  std::unique_lock<std::mutex> gate(mx, std::defer_lock);

  struct state
  {
    std::thread thread;

    // Per-thread pool resources:
    std::pmr::unsynchronized_pool_resource unsync;
    std::pmr::synchronized_pool_resource sync;

    std::pmr::memory_resource* memres[3] = {
      std::pmr::new_delete_resource(), &unsync, &sync
    };
  };

  state states[4];

  const std::string resnames[] = {"new-delete ", "unsync-pool", "sync-pool  "};

  auto run_test = [&mx] (std::pmr::memory_resource* memres,
			 __gnu_test::time_counter* timers)
  {
    std::lock_guard<std::mutex>{mx};  // block until the mutex can be locked
    populate_lists(memres, timers);
  };

  auto time_threads = [&] (std::string testname, bool time_dtors, int which) {
    __gnu_test::time_counter time;
    __gnu_test::resource_counter resource;
    gate.lock();
    auto* time_ptr = time_dtors ? nullptr : &time;
    for (auto& s : states)
      s.thread = std::thread{ run_test, s.memres[which], time_ptr };
    start_counters(time, resource);
    gate.unlock(); // let the threads run
    for (auto& s : states)
      s.thread.join();
    stop_counters(time, resource);
    report_performance(__FILE__, resnames[which] + testname, time, resource);
  };

  for (auto time_dtors : {false, true})
  {
    std::string testname = " resource-per-thread std::list push/pop";
    if (time_dtors)
      testname += "/destroy";
    for (int which : {0, 1, 2})
      time_threads(testname, time_dtors, which);
  }
}

// A naive memory_resource that adds a mutex to unsynchronized_pool_resource
struct locking_pool_resource : std::pmr::unsynchronized_pool_resource
{
  void* do_allocate(std::size_t b, std::size_t a) override
  {
    std::lock_guard<std::mutex> l(m);
    return unsynchronized_pool_resource::do_allocate(b, a);
  }

  void do_deallocate(void* p, std::size_t b, std::size_t a) override
  {
    std::lock_guard<std::mutex> l(m);
    return unsynchronized_pool_resource::do_deallocate(p, b, a);
  }

  std::mutex m;
};

// Multithreaded std::list test with all threads sharing the same resource.
// (new_delete vs unsynchronized_pool+mutex vs synchronized_pool)
//
// pmr::synchronized_pool_resource is not expected to be anywhere near
// as fast as pmr::new_delete_resource() here, but should perform much
// better than the naive locking_pool_resource type.
void test_lists_shared_resource()
{
  std::mutex mx;
  std::unique_lock<std::mutex> gate(mx, std::defer_lock);

  locking_pool_resource unsync;
  std::pmr::synchronized_pool_resource sync;

  std::pmr::memory_resource* memres[3] = {
    std::pmr::new_delete_resource(), &unsync, &sync
  };

  std::thread threads[4];

  const std::string resnames[3] = { "new-delete", "mutex-pool", "sync-pool " };

  auto run_test = [&mx] (std::pmr::memory_resource* memres,
			 __gnu_test::time_counter* timers)
  {
    std::lock_guard<std::mutex>{mx};  // block until the mutex can be locked
    populate_lists(memres, timers);
  };

  auto time_threads = [&] (std::string testname, bool time_dtors, int which) {
    __gnu_test::time_counter time;
    __gnu_test::resource_counter resource;
    gate.lock();
    auto* time_ptr = time_dtors ? nullptr : &time;
    for (auto& t : threads)
      t = std::thread{ run_test, memres[which], time_ptr };
    start_counters(time, resource);
    gate.unlock(); // let the threads run
    for (auto& t : threads)
      t.join();
    stop_counters(time, resource);
    report_performance(__FILE__, resnames[which] + testname, time, resource);
  };

  for (auto time_dtors : {false, true})
  {
    std::string testname = " shared-resource std::list push/pop";
    if (time_dtors)
      testname += "/destroy";
    for (int which : {0, 1, 2})
      time_threads(testname, time_dtors, which);
  }
}

// TODO threaded test just doing loads of allocations, no deallocs
// both with per-thread resource (unsync vs sync vs newdel)
// and shared resource (locked vs sync vs newdel)

// TODO threaded test just doing loads of deallocations, no allocs
// both with per-thread resource (unsync vs sync vs newdel)
// and shared resource (locked vs sync vs newdel)

// Multithreaded test where deallocations happen on different threads.
// (new_delete vs unsynchronized_pool+mutex vs synchronized_pool)
//
// This hits the slow path for pmr::synchronized_pool_resource, where
// an exclusive lock must be taken to access other threads' pools.
// pmr::synchronized_pool_resource is not expected to be anywhere near
// as fast as pmr::new_delete_resource() here, but should perform much
// better than the naive locking_pool_resource type.
void test_cross_thread_dealloc()
{
  const int num_threads = 4;

  struct X {
    void* ptr;
    unsigned size;
  };

  // A buffer for each thread, and extra buffers for half of the threads:
  std::vector<X> allocs[num_threads * 3 / 2];
  for (auto& v : allocs)
    v.resize(1000 * iterations);

  // Use a few different pools
  const std::size_t sizes[] = { 8, 16, 8, 16, 32, 64, 8, 16, 32, 64 };

  std::mutex mx;

  auto run_test =
  [&, num_threads] (std::pmr::memory_resource* memres, int i, bool with_exit)
  {
    std::size_t counter = 0;
    std::lock_guard<std::mutex>{mx};
    // Fill this thread's buffer with allocations:
    for (X& x : allocs[i])
    {
      x.size = sizes[counter++ % 10];
      x.ptr = memres->allocate(x.size, 1);
    }

    if (with_exit && i == 0)
    {
      // One of the threads exits, so that its pools transfer to the
      // non-thread-specific list of pools.
      return;
    }
    else if (i < num_threads / 2)
    {
      // Other threads continue allocating, into the extra buffers:
      for (X& x : allocs[num_threads + i])
      {
	x.size = sizes[counter++ % 10];
	x.ptr = memres->allocate(x.size, 1);
      }
    }
    else
    {
      // Half of the threads start deallocating their own memory and the
      // memory belonging to another pool
      const int other = i - num_threads / 2;
      for (unsigned n = 0; n < allocs[i].size(); ++n)
      {
	// Deallocate memory allocated in this thread:
	X& x1 = allocs[i][n];
	memres->deallocate(x1.ptr, x1.size, 1);
	x1 = {};
	// Deallocate memory allocated in another thread:
	X& x2 = allocs[other][n];
	memres->deallocate(x2.ptr, x2.size, 1);
	x2 = {};
      }
    }
  };

  std::thread threads[num_threads];

  locking_pool_resource unsync;
  std::pmr::synchronized_pool_resource sync;

  std::pmr::memory_resource* memres[3] = {
    std::pmr::new_delete_resource(), &unsync, &sync
  };
  const std::string resnames[3] = { "new-delete", "mutex-pool", "sync-pool " };

  auto time_threads = [&] (std::string name, int which, bool with_exit)
  {
    __gnu_test::time_counter time;
    __gnu_test::resource_counter resource;
    std::unique_lock<std::mutex> gate(mx);
    for (auto& t : threads)
      t = std::thread{ run_test, memres[which], &t - threads, with_exit };
    start_counters(time, resource);
    gate.unlock();
    for (auto& t : threads)
      t.join();
    stop_counters(time, resource);
    report_performance(__FILE__, resnames[which] + name, time, resource);

    // Clean up:
    for (auto& a : allocs)
    {
      const int i = (&a - allocs);
      if (i < num_threads) // These allocations were freed
	for (auto& x : a)
	{
	  assert(x.ptr == nullptr);
	}
      else if (with_exit && i == num_threads)
	;
      else
	for (auto& x : a)
	{
	  memres[which]->deallocate(x.ptr, x.size, 1);
	  x = {};
	}
    }
  };

  for (int which : {0, 1, 2})
    time_threads(" cross-thread dealloc", which, false);
  for (int which : {0, 1, 2})
    time_threads(" cross-thread dealloc w/exit", which, true);
}
#endif

int main()
{
  test_lists_single_thread();
#ifndef NOTHREAD
  test_lists_resource_per_thread();
  test_lists_shared_resource();
  test_cross_thread_dealloc();
#endif
}
