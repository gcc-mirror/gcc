// { dg-do run { target c++11 } }
// { dg-require-effective-target exceptions_enabled }

#include <exception>
#include <cstdlib>

int exit_status = 1;
[[noreturn]] void terminate_cleanly() noexcept { std::exit(exit_status); }

struct A { virtual ~A() = default; };

int main()
{
  std::set_terminate(terminate_cleanly);
  try
  {
    // At this point std::current_exception() == nullptr so the
    // std::nested_exception object is empty.
    std::throw_with_nested(A{});

    // Should not reach this point.
    std::abort();
  }
  catch (const A& a)
  {
    // This means the expected std::terminate() call will exit cleanly,
    // so this test will PASS.
    exit_status = 0;

    std::rethrow_if_nested(a);
#if __cpp_rtti
    // No nested exception, so trying to rethrow it calls std::terminate()
    // which calls std::exit(0). Should not reach this point.
    std::abort();
#else
    // Without RTTI we can't dynamic_cast<const std::nested_exception*>(&a)
    // so std::rethrow_if_nested(a) just returns normally.
    return 0;
#endif
  }
}
