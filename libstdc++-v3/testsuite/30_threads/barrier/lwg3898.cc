// { dg-do run { target c++20 } }
// { dg-require-effective-target gthreads }

#include <barrier>
#include <exception>
#include <cstdlib>
#if !_GLIBCXX_USE_C99_STDLIB && defined _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
#endif

void handle_terminate()
{
#if _GLIBCXX_USE_C99_STDLIB
  std::_Exit(0);
#elif defined _GLIBCXX_HAVE_UNISTD_H
  _exit(0);
#else
  std::exit(0);
#endif
}

struct F
{
  void operator()()
  {
    std::set_terminate(handle_terminate);
    throw 1;
  }
};

void
test_lwg3898()
{
  std::barrier<F> b(1, F{});
  // This should call the terminate handler and exit with zero status:
  b.arrive_and_wait();
  // Should not reach here:
  std::abort();
}

int
main()
{
  test_lwg3898();
}
