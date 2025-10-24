// { dg-do run { target c++11 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }
// { dg-require-effective-target hosted }

#include <condition_variable>
#include <chrono>
#include <mutex>
#include <initializer_list>
#include <testsuite_hooks.h>

namespace chrono = std::chrono;

// thread.timedmutex.requirements.general:
//   If abs_time has already passed, the function attempts to obtain
//   ownership without blocking (as if by calling try_lock()).

template <typename Clock>
void
test_absolute(chrono::nanoseconds offset)
{
  std::mutex mtx;
  std::condition_variable cv;
  chrono::time_point<Clock> tp(offset);
  std::unique_lock<std::mutex> lock(mtx);
  // Doesn't cope with spurious wakeup
  VERIFY(cv.wait_until(lock, tp) == std::cv_status::timeout);
}

// The type of clock used for the actual wait depends on whether
// _GLIBCXX_USE_PTHREAD_MUTEX_CLOCKLOCK is defined. We might as well just test
// both steady_clock and system_clock.
template <typename Clock>
void
test_relative(chrono::nanoseconds offset)
{
  std::mutex mtx;
  std::condition_variable cv;
  const auto d = -Clock::now().time_since_epoch() + offset;
  std::unique_lock<std::mutex> lock(mtx);
  // Doesn't cope with spurious wakeup
  VERIFY(cv.wait_for(lock, d) == std::cv_status::timeout);
}

int main()
{
  // It's not really possible to arrange for the relative calls to have
  // tv_nsec == 0 due to time advancing.
  for (const chrono::nanoseconds offset : {
      // tv_sec == 0, tv_nsec == 0
      chrono::nanoseconds{0},
      // tv_sec == 0, tv_nsec < 0
      chrono::duration_cast<chrono::nanoseconds>(chrono::milliseconds{-10}),
      // tv_sec < 0
      chrono::duration_cast<chrono::nanoseconds>(chrono::seconds{-10})
    }) {
    test_absolute<chrono::system_clock>(offset);
    test_relative<chrono::system_clock>(offset);

    test_absolute<chrono::steady_clock>(offset);
    test_relative<chrono::steady_clock>(offset);
  }
}
