// { dg-do compile { target { { i?86-*-linux* x86_64-*-linux* } && lp64 } } }
// { dg-require-effective-target c++11 }
// { dg-options "-fsanitize=thread" }

// PR libstdc++/121496 no member named '_M_clocklock' with -fsanitize=thread

#include <mutex>
#include <chrono>

void
test_pr121496(std::timed_mutex& m)
{
  (void) m.try_lock_until(std::chrono::steady_clock::time_point{});
}
