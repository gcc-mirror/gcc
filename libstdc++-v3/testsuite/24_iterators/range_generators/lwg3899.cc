// { dg-do run { target c++23 } }

// LWG 3899.
// co_yielding elements of an lvalue generator is unnecessarily inefficient

#include <generator>
#include <memory_resource>
#include <testsuite_hooks.h>

struct memory_resource : std::pmr::memory_resource
{
  std::size_t count = 0;

  void* do_allocate(std::size_t n, std::size_t a) override
  {
    count += n;
    return std::pmr::new_delete_resource()->allocate(n, a);
  }

  void do_deallocate(void* p, std::size_t n, std::size_t a) override
  {
    return std::pmr::new_delete_resource()->deallocate(p, n, a);
  }

  bool do_is_equal(const std::pmr::memory_resource& mr) const noexcept override
  { return this == &mr; }
};

std::pmr::generator<int>
f(std::allocator_arg_t, std::pmr::polymorphic_allocator<>, int init)
{
  co_yield init + 0;
  co_yield init + 1;
}

std::pmr::generator<int>
g(std::allocator_arg_t, std::pmr::polymorphic_allocator<> alloc)
{
  auto gen = f(std::allocator_arg, alloc, 0);
  auto gen2 = f(std::allocator_arg, alloc, 2);
  co_yield std::ranges::elements_of(std::move(gen), alloc);
  co_yield std::ranges::elements_of(gen2, alloc);
}

int
main()
{
  std::size_t counts[4];
  memory_resource mr;
  for (auto d : g(std::allocator_arg , &mr))
    counts[d] = mr.count;
  VERIFY(counts[0] != 0);
  // No allocations after the first one:
  VERIFY(counts[1] == counts[0]);
  VERIFY(counts[2] == counts[0]);
  VERIFY(counts[3] == counts[0]);
}
