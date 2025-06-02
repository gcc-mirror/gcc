// { dg-do run { target c++20 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }
// { dg-options "-DSIMULATOR_TEST" { target simulator } }

// Bug libstdc++/104928 - std::counting_semaphore on Linux can sleep forever

#include <semaphore>
#include <thread>
#include <chrono>
#include <climits>

#ifdef SIMULATOR_TEST
const int loop_count = 100;
const int thread_count = 6;
#else
const int loop_count = 1000000;
const int thread_count = 20;
#endif

template<std::ptrdiff_t N, typename Acquire>
void
test_acquire(Acquire acq_func)
{
  std::counting_semaphore<N * loop_count> s{0};
  std::thread threads[thread_count];
  for (int i = 0; i < thread_count; i += 2) {
    threads[i] = std::thread([&s, &acq_func]() {
      for (int i = 0; i < loop_count; ++i)
	acq_func(s);
    });
    threads[i+1] = std::thread([&s]() {
      for (int i = 0; i < loop_count; ++i)
	s.release();
    });
  }
  for (auto& t : threads)
    t.join();
}

template<typename Acquire>
void
test_all(Acquire f)
{
  const int max = INT_MAX / loop_count;
  test_acquire<max>(f);  // can use futex
#if PTRDIFF_MAX > INT_MAX
  test_acquire<max * 10>(f); // cannot use futex
#endif
}

int main()
{
  test_all([](auto& sem) { sem.acquire(); });

  test_all([](auto& sem) { while (!sem.try_acquire()) { } });

  using namespace std::chrono;

  test_all([](auto& sem) { while (!sem.try_acquire_for(1h)) { } });

  auto try_acquire_until = [](auto& sem, auto time) {
    while (!sem.try_acquire_until(time + 1h))
    { }
  };
  test_all([&](auto& sem) { try_acquire_until(sem, system_clock::now()); });
  test_all([&](auto& sem) { try_acquire_until(sem, steady_clock::now()); });
  test_all([&](auto& sem) { try_acquire_until(sem, utc_clock::now()); });
}
