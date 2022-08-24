// { dg-do run { target c++11 } }
// { dg-skip-if "" { *-*-* } { "-fno-exceptions" } }

#include <exception>
#include <cstdlib>

[[noreturn]] void terminate_cleanly() noexcept { std::exit(0); }

struct A { virtual ~A() = default; };

int main()
{
  try
  {
    // At this point std::current_exception() == nullptr so the
    // std::nested_exception object is empty.
    std::throw_with_nested(A{});
  }
  catch (const A& a)
  {
    std::set_terminate(terminate_cleanly);
    std::rethrow_if_nested(a);
#if __cpp_rtti
    // No nested exception, so trying to rethrow it calls std::terminate()
    // which calls std::exit(0). Shoud not reach this point.
    std::abort();
#else
    // Without RTTI we can't dynamic_cast<const std::nested_exception*>(&a)
    // so std::rethrow_if_nested(a) just returns normally.
    return 0;
#endif
  }
}
