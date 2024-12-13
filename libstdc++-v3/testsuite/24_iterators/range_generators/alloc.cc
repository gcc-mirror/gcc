// { dg-do run { target c++23 } }

#include <generator>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

template<typename... Args>
std::pmr::generator<int>
gen(Args...)
{
  co_yield 1;
  co_yield 2;
}

struct S
{
  template<typename... Args>
  std::pmr::generator<char>
  gen(Args...)
  {
    co_yield '1';
    co_yield '2';
  }

  template<typename Self, typename... Args>
  std::pmr::generator<long long>
  genx(this Self& self, Args...)
  {
    co_yield 1LL;
    co_yield 2LL;
  }
};

int
main()
{
  __gnu_test::memory_resource mr;
  for (auto _ : gen())
    VERIFY(mr.number_of_active_allocations() == 0);

  for (auto _ : gen(std::allocator_arg))
    VERIFY(mr.number_of_active_allocations() == 0);

  for (auto _ : gen(std::allocator_arg, std::pmr::new_delete_resource()))
    VERIFY(mr.number_of_active_allocations() == 0);

#if __cpp_exceptions
  try {
    for (auto _ : gen(std::allocator_arg, std::pmr::null_memory_resource()))
      VERIFY(false);
  } catch (const std::bad_alloc&) {
  }
#endif

  VERIFY(mr.number_of_active_allocations() == 0);

  for (auto _ : gen(std::allocator_arg , &mr))
    VERIFY(mr.number_of_active_allocations() == 1);

  VERIFY(mr.number_of_active_allocations() == 0);

  S s;
  for (auto _ : s.gen(std::allocator_arg , &mr))
    VERIFY(mr.number_of_active_allocations() == 1);

  VERIFY(mr.number_of_active_allocations() == 0);
  for (auto _ : s.genx(std::allocator_arg , &mr))
    VERIFY(mr.number_of_active_allocations() == 1);

  VERIFY(mr.number_of_active_allocations() == 0);
}
