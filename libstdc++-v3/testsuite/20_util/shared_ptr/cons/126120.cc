// PR libstdc++/126120 - is_constructible_v with shared_ptr from an invalid deleter
// { dg-do compile { target c++11 } }

#include <memory>
#include <type_traits>

struct NonDeleter { };

struct NonMoveableDeleter
{
  NonMoveableDeleter() = default;
  NonMoveableDeleter(const NonMoveableDeleter&) = delete;
  void operator()(int* p) const { delete p; }
};

using shared_ptr = std::shared_ptr<int>;
using Alloc = std::allocator<int>;

// 1. Pointer + Deleter
static_assert( ! std::is_constructible<shared_ptr, int*, NonDeleter>::value, "");
static_assert( ! std::is_constructible<shared_ptr, int*, NonMoveableDeleter>::value, "");

// 2. Pointer + Deleter + Allocator
static_assert( ! std::is_constructible<shared_ptr, int*, NonDeleter, Alloc>::value, "");
static_assert( ! std::is_constructible<shared_ptr, int*, NonMoveableDeleter, Alloc>::value, "");

// 3. nullptr_t + Deleter
static_assert( ! std::is_constructible<shared_ptr, std::nullptr_t, NonDeleter>::value, "");
static_assert( ! std::is_constructible<shared_ptr, std::nullptr_t, NonMoveableDeleter>::value, "");

// 4. nullptr_t + Deleter + Allocator
static_assert( ! std::is_constructible<shared_ptr, std::nullptr_t, NonDeleter, Alloc>::value, "");
static_assert( ! std::is_constructible<shared_ptr, std::nullptr_t, NonMoveableDeleter, Alloc>::value, "");

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++20-extensions" // lambda template
void
test_lwg4110()
{
  // LWG 4110 - shared_ptr(nullptr_t, Deleter) is overconstrained, breaking some sensible deleters
  auto deleter = []<typename T>(T pointer) { delete pointer; };
  shared_ptr p(new int, deleter);
  shared_ptr q(new int, deleter, Alloc{});
}
#pragma GCC diagnostic pop
