// { dg-do run { target c++14 } }

#include <shared_mutex>
#include <chrono>
#include <future>
#include <initializer_list>
#include <testsuite_hooks.h>

namespace chrono = std::chrono;

// thread.timedmutex.requirements.general:
//   If abs_time has already passed, the function attempts to obtain
//   ownership without blocking (as if by calling try_lock()).

template <typename Clock>
void
test_exclusive_absolute(chrono::nanoseconds offset)
{
  std::shared_timed_mutex stm;
  chrono::time_point<Clock> tp(offset);
  VERIFY(stm.try_lock_until(tp));
  VERIFY(!stm.try_lock_until(tp));
}

template <typename Clock>
void
test_shared_absolute(chrono::nanoseconds offset)
{
  std::shared_timed_mutex stm;
  chrono::time_point<Clock> tp(offset);
  VERIFY(stm.try_lock_shared_until(tp));
  stm.unlock_shared();

  VERIFY(stm.try_lock_for(chrono::seconds{10}));

  {
    // NPTL will give us EDEADLK if pthread_rwlock_timedrdlock() is called on
    // the same thread that already holds the exclusive (write) lock, so let's
    // arrange for a different thread to try to acquire the shared lock.
    auto t = std::async(std::launch::async, [&stm, tp]() {
	VERIFY(!stm.try_lock_shared_until(tp));
      });
  }
}

// The type of clock used for the actual wait depends on whether
// _GLIBCXX_USE_PTHREAD_RWLOCK_CLOCKLOCK is defined. We might as well just test
// both steady_clock and system_clock.
template <typename Clock>
void
test_exclusive_relative(chrono::nanoseconds offset)
{
  std::shared_timed_mutex stm;
  const auto d = -Clock::now().time_since_epoch() + offset;
  VERIFY(stm.try_lock_for(d));
  VERIFY(!stm.try_lock_for(d));
}

template <typename Clock>
void
test_shared_relative(chrono::nanoseconds offset)
{
  std::shared_timed_mutex stm;
  const auto d = -Clock::now().time_since_epoch() + offset;
  VERIFY(stm.try_lock_shared_for(d));
  stm.unlock_shared();
  // Should complete immediately
  VERIFY(stm.try_lock_for(chrono::seconds{10}));
  VERIFY(!stm.try_lock_shared_for(d));
}

int main()
{
  // Try once with an offset that ought to result in tv_sec == 0, tv_nsec < 0
  // and one with an offset that ought to result in tv_sec < 0, tv_nsec == 0
  // for the absolute calls at least.  It's not really possible to arrange for
  // the relative calls to have tv_nsec == 0 due to time advancing.
  using namespace std::chrono_literals;
  for (const chrono::nanoseconds offset : {
      // tv_sec == 0, tv_nsec == 0
      chrono::nanoseconds{0},
      // tv_sec == 0, tv_nsec < 0
      chrono::nanoseconds{-10ms},
      // tv_sec < 0
      chrono::nanoseconds{-10s}
    }) {
    test_exclusive_absolute<chrono::system_clock>(offset);
    test_shared_absolute<chrono::system_clock>(offset);
    test_exclusive_relative<chrono::system_clock>(offset);
    test_shared_relative<chrono::system_clock>(offset);

    test_exclusive_absolute<chrono::steady_clock>(offset);
    test_shared_absolute<chrono::steady_clock>(offset);
    test_exclusive_relative<chrono::steady_clock>(offset);
    test_shared_relative<chrono::steady_clock>(offset);
  }
}
