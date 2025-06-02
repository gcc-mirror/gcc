// { dg-do run { target c++20 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }

// Bug libstdc++/104928 - std::counting_semaphore on Linux can sleep forever

#include <semaphore>
#include <thread>
#include <chrono>
#include <atomic>

std::binary_semaphore t1(1);
std::binary_semaphore sem2(0);
std::atomic<int> room1 = 0;
int room2 = 0;

std::atomic<bool> run{true};

enum class AcquireKind { Acquire, Try, TryFor };

template<std::ptrdiff_t N, AcquireKind Kind>
struct Morris
{
  using Semaphore = std::counting_semaphore<N>;

  Semaphore sem1{1};
  Semaphore sem2{0};
  unsigned counter = 0;

  void operator()()
  {
    while (run)
    {
      room1 += 1;

      acquire(sem1);
      room2 += 1;
      room1 -= 1;
      if (room1 == 0)
	sem2.release();
      else
	sem1.release();

      acquire(sem2);
      room2 -= 1;

      // critical region
      ++counter;
      // end critical region

      if (room2 == 0)
	sem1.release();
      else
	sem2.release();
    }
  }

  void acquire(Semaphore& sem)
  {
    using enum AcquireKind;
    using namespace std::chrono;
    if constexpr (Kind == Acquire)
      sem.acquire();
    else if constexpr (Kind == Try)
      while (!sem.try_acquire()) { }
    else if constexpr (Kind == TryFor)
      while (!sem.try_acquire_for(1h)) { }
  }
};

template<std::ptrdiff_t N, AcquireKind Kind>
void
test_morris_kind()
{
  Morris<N, Kind> algo;
  std::thread t1(std::ref(algo));
  std::thread t2(std::ref(algo));
  std::this_thread::sleep_for(std::chrono::seconds(2));
  run = false;
  t1.join();
  t2.join();
}

template<std::ptrdiff_t N>
void
test_morris()
{
  test_morris_kind<N, AcquireKind::Acquire>();
  test_morris_kind<N, AcquireKind::Try>();
  test_morris_kind<N, AcquireKind::TryFor>();
}

int main()
{
  test_morris<1>();    // std::binary_semaphore
  test_morris<1000>(); // std::counting_semaphore that can use futex
#if PTRDIFF_MAX > INT_MAX
  // test_morris<PTRDIFF_MAX>(); // std::counting_semaphore that cannot use futex
#endif
}
