// { dg-do run { target c++11 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }

// PR libstdc++/113327
// std::sleep_for(std::chrono::hours::max()) returns immediately

#include <thread>
#include <chrono>
#include <cstdlib>
#include <csignal>

int main()
{
  std::thread sleepy([] {
    // Rather than overflowing to a negative value, the timeout should be
    // truncated to seconds::max() and so sleep for 292 billion years.
    std::this_thread::sleep_for(std::chrono::minutes::max());
    // This should not happen:
    throw 1;
  });
  // Give the new thread a chance to start sleeping:
  std::this_thread::yield();
  std::this_thread::sleep_for(std::chrono::seconds(2));
  // If we get here without the other thread throwing an exception
  // then it should be sleeping peacefully, so the test passed.
  // pthread_kill(sleepy.native_handle(), SIGINT);
  std::_Exit(0);
}
