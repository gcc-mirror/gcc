// { dg-do run { target c++17 } }
// PR libstdc++/105258 std::get_temporary_buffer() does not respect alignment

#include <algorithm>
#include <cstdint>
#include <testsuite_hooks.h>

struct alignas(__STDCPP_DEFAULT_NEW_ALIGNMENT__ * 2) Overaligned
{
  ~Overaligned()
  {
    auto alignment = reinterpret_cast<std::uintptr_t>(this);
    VERIFY( (alignment % alignof(Overaligned)) == 0 );
  }

  bool operator<(const Overaligned&) const { return false; }
};

void
test_pr105258()
{
  Overaligned o[2];
  std::stable_sort(o, o+2);
}

int main()
{
  test_pr105258();
}
