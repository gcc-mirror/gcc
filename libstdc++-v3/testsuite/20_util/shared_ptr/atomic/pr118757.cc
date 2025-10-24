// { dg-do run { target c++20 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }
// { dg-require-effective-target hosted }

#include <memory>
#include <chrono>
#include <thread>
#include <barrier>

std::shared_ptr<int> q = std::make_shared<int>(42);
std::atomic<std::shared_ptr<int>> p = q;

std::barrier bar(2);

void signaller()
{
  std::this_thread::sleep_for(std::chrono::seconds(1));
  p.store(std::shared_ptr<int>(q, nullptr));
  p.notify_one();
  bar.arrive_and_wait();
}

int main(int, char**)
{
  std::thread thr(signaller);
  p.wait(q);
  bar.arrive_and_wait();
  thr.join();
}
