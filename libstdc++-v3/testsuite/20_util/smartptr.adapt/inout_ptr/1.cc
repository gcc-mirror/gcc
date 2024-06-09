// { dg-do run { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

// C++23 [inout.ptr.t] Class template inout_ptr_t

struct star_fish* star_fish_alloc();
int star_fish_populate(struct star_fish** ps, const char* description);

struct star_fish_deleter {
  void operator() (struct star_fish* c) const noexcept;
};

using star_fish_ptr = std::unique_ptr<star_fish, star_fish_deleter>;

// Example 1 from [inout.ptr.t]
int main(int, char**)
{
  star_fish_ptr peach(star_fish_alloc());
  // ...
  // used, need to re-make
  int err = star_fish_populate(std::inout_ptr(peach), "caring clown-fish liker");
  return 0;
}

#include <cstdint>

star_fish* star_fish_alloc()
{
  static std::uintptr_t counter = 1;
  return reinterpret_cast<star_fish*>(counter++);
}

void star_fish_deleter::operator()(star_fish* c) const noexcept
{
  static std::uintptr_t counter = 1;
  VERIFY(reinterpret_cast<std::uintptr_t>(c) == counter++);
}

int star_fish_populate(star_fish** ps, const char*)
{
  VERIFY(ps);
  star_fish_deleter()(*ps);
  *ps = star_fish_alloc();
  return 0;
}
