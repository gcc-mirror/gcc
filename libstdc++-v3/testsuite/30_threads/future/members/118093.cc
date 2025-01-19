// { dg-do run { target c++11 } }

#include <chrono>
#include <future>

void
test_sys()
{
  std::promise<void> p;
  std::chrono::system_clock::time_point tp(std::chrono::milliseconds{-10});
  (void) p.get_future().wait_until(tp);
}

void
test_steady()
{
  std::promise<void> p;
  std::chrono::steady_clock::time_point tp(std::chrono::milliseconds{-10});
  (void) p.get_future().wait_until(tp);
}

int main()
{
  test_sys();
  test_steady();
}
