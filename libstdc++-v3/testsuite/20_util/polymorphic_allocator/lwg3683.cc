// { dg-do compile { target c++17 } }

#include <memory_resource>

bool
test_lwg3683(const std::pmr::polymorphic_allocator<int>& a)
{
  if (a == std::pmr::get_default_resource())
    return true;
  if (std::pmr::get_default_resource() != a)
    return false;
  throw a;
}
