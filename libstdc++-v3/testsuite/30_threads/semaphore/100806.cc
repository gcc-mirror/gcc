// { dg-options "-std=gnu++2a -pthread" }
// { dg-do run { target c++2a } }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }

#include <iostream>
#include <sstream>

#include <thread>
#include <semaphore>
#include <mutex>
#include <chrono>
#include <vector>

std::counting_semaphore<4> semaphore{6};

std::mutex mtx;
std::vector<std::string> results;

void thread_main(size_t x)
{
  semaphore.acquire();
  std::this_thread::sleep_for(std::chrono::milliseconds(100));
  semaphore.release();
  {
    std::ostringstream stm;
    stm << "Thread " << x << " finished.";
    std::lock_guard g{ mtx };
    results.push_back(stm.str());
  }
}

int main()
{
    constexpr auto nthreads = 10;

    std::vector<std::thread> threads(nthreads);

    size_t counter{0};
    for(auto& t : threads)
      {
	t = std::thread(thread_main, counter++);
      }

    for(auto& t : threads)
      {
	t.join();
	{
	  std::lock_guard g{ mtx };
	  for (auto&& r : results)
	    std::cout << r << '\n';
	  std::cout.flush();
	  results.clear();
	}
      }
}
