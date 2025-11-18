// { dg-do run { target c++14 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }
// { dg-require-effective-target hosted }

#include <shared_mutex>
#include <chrono>
#include <future>
#include <initializer_list>
#include <testsuite_hooks.h>

namespace chrono = std::chrono;

// [thread.timedmutex.requirements.general]:
//   If abs_time has already passed, the function attempts to obtain
//   ownership without blocking (as if by calling try_lock()).

// C++14 [thread.sharedtimedmutex.class] 3.2 says it's undefined for a thread
// to attempt to recursively gain any ownership of a shared_timed_mutex.
// This isn't just theoretical, as Glibc's pthread_rwlock_timedrdlock will
// return EDEADLK if called on the same thread that already holds the
// exclusive (write) lock.
#define VERIFY_IN_NEW_THREAD(X) \
  (void) std::async(std::launch::async, [&] { VERIFY(X); })

// Unfortunately POSIX says that pthread_rwlock_clockwrlock,
// pthread_rwlock_clockrdlock, pthread_rwlock_timedwrlock and
// pthread_rwlock_timedrdlock are allowed to deadlock rather than return
// EDEADLK or just time out if the thread already has the associated lock. This
// means that we can't enable the deadlock tests by default. The glibc
// implementation does return EDEADLK and __shared_mutex_cv times out so we can
// test both of them.
#if (defined(__GLIBC__) || ! (_GLIBCXX_USE_PTHREAD_RWLOCK_T && _GTHREAD_USE_MUTEX_TIMEDLOCK)) \
  && ! defined(_GLIBCXX_ASSERTIONS)
#define DEADLOCK_VERIFY(X) \
  VERIFY(X)
#else
#define DEADLOCK_VERIFY(X) \
  do {} while(0)
#endif

template <typename Clock>
void
test_exclusive_absolute(chrono::nanoseconds offset)
{
  std::shared_timed_mutex stm;
  chrono::time_point<Clock> tp(offset);
  VERIFY(stm.try_lock_until(tp));

  // Trying to acquire the same long on the same thread
  DEADLOCK_VERIFY(!stm.try_lock_until(tp));

  // Check that false is returned on timeouts even if the implementation
  // returned EDEADLK above by trying to lock on a different thread.
  VERIFY_IN_NEW_THREAD(!stm.try_lock_until(tp));
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
  DEADLOCK_VERIFY(!stm.try_lock_shared_until(tp));
  VERIFY_IN_NEW_THREAD(!stm.try_lock_shared_until(tp));
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
  DEADLOCK_VERIFY(!stm.try_lock_for(d));
  VERIFY_IN_NEW_THREAD(!stm.try_lock_for(d));
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
  DEADLOCK_VERIFY(!stm.try_lock_shared_for(d));
  VERIFY_IN_NEW_THREAD(!stm.try_lock_shared_for(d));
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
