// { dg-do run { target c++20 } }
// { dg-require-gthreads "" }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-add-options libatomic }

// A notify_all() call must not be lost when it follows the store that a
// waiter is waiting for.  The library decides whether to issue the
// FUTEX_WAKE by checking a per-address waiter count; that check has to be
// ordered after the caller's store, otherwise the notifier can observe
// "no waiters" (store-buffering reorder) while the waiter observes the old
// value and blocks forever.
//
// This exercises the direct-futex path (a 4-byte scalar on Linux).  A
// watchdog thread detects a waiter still blocked after the value already
// changed, recovers with an extra notify_all(), and records the failure.
// The reproduction is timing-dependent (it needs real hardware store
// buffering, seen on x86-64), but on a correct library it always completes
// quickly with no lost wakeups, so the test never produces a false failure.

#include <atomic>
#include <chrono>
#include <thread>
#include <vector>
#include <cstddef>
#include <testsuite_hooks.h>

int
main ()
{
  using namespace std::chrono;

  constexpr unsigned iterations = 100000;
  constexpr unsigned cold_stores = 64;        // delay visibility of the store
  constexpr unsigned jitter_mod = 4000;       // place the notify near the
					      // waiter's spin -> block window
  // Walked by the cold stores below so that each store misses at least the
  // L2 cache on common hardware; that is what keeps the store to x pending
  // long enough to expose the missing ordering.  A fixed size cannot exceed
  // every possible last-level cache, so reproduction is best-effort, but the
  // test always passes quickly on a correct library.
  std::vector<char> buffer (std::size_t (32) << 20);

  alignas(64) std::atomic<unsigned> x{0};
  alignas(64) std::atomic<unsigned> go{0};
  alignas(64) std::atomic<unsigned> done{0};
  std::atomic<bool> stop{false};
  std::atomic<bool> failed{false};

  std::thread waiter([&] {
    for (unsigned i = 0; i < iterations; ++i)
      {
	if (failed.load(std::memory_order_acquire))
	  break;
	go.store(i + 1, std::memory_order_release);
	x.wait(i, std::memory_order_acquire);
	done.store(i + 1, std::memory_order_release);
      }
  });

  // Keep x's cache line bouncing between cores, which keeps the notifier's
  // store to x in its store buffer a little longer and widens the window.
  std::thread stirrer([&] {
    while (!stop.load(std::memory_order_relaxed))
      (void) x.load(std::memory_order_relaxed);
  });

  // Watchdog: if the waiter has not made progress for a while although x
  // already holds the value it is waiting to leave, its wakeup was lost.
  std::thread watchdog([&] {
    unsigned last = 0;
    auto last_progress = steady_clock::now();
    while (done.load(std::memory_order_acquire) != iterations
	   && !failed.load(std::memory_order_acquire))
      {
	std::this_thread::sleep_for(milliseconds(5));
	const unsigned d = done.load(std::memory_order_acquire);
	if (d != last)
	  {
	    last = d;
	    last_progress = steady_clock::now();
	  }
	else if (steady_clock::now() - last_progress > milliseconds(500)
		 && x.load(std::memory_order_acquire) == d + 1)
	  {
	    failed.store(true, std::memory_order_release);
	    x.notify_all();                 // release the stuck waiter
	  }
      }
  });

  unsigned seed = 1;
  std::size_t pos = 0;
  volatile unsigned spin_sink = 0;
  for (unsigned i = 0; i < iterations; ++i)
    {
      while (go.load(std::memory_order_acquire) != i + 1)
	if (failed.load(std::memory_order_acquire))
	  break;
      if (failed.load(std::memory_order_acquire))
	break;

      seed = seed * 1103515245 + 12345;
      for (unsigned d = (seed >> 16) % jitter_mod; d != 0; --d)
	spin_sink = spin_sink + 1;
      for (unsigned k = 0; k < cold_stores; ++k)
	{
	  buffer[pos] = static_cast<char>(i);
	  pos = (pos + 4160) % buffer.size();
	}

      x.store(i + 1, std::memory_order_release);
      x.notify_all();

      while (done.load(std::memory_order_acquire) != i + 1)
	if (failed.load(std::memory_order_acquire))
	  break;
    }

  stop.store(true, std::memory_order_relaxed);
  watchdog.join();
  stirrer.join();
  // The waiter may be parked in its final wait; wake it so it can exit.
  x.store(iterations + 1, std::memory_order_release);
  x.notify_all();
  waiter.join();

  VERIFY( !failed.load() );
  return 0;
}
