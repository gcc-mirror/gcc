// { dg-do compile { target c++11 } }

#include <ostream>

struct CustomStream : std::ostream {};

namespace N {
    class A{};
}

std::ostream& operator<<(std::ostream& s, const N::A&)
{
    return s;
}

CustomStream&& operator<<(CustomStream&& s, const N::A& v)
{
    static_cast<std::ostream&>(s) << v;
    return std::move(s);
}

void test_pr99692()
{
  // PR libstdc++/99692
    CustomStream() << N::A{};
}

int test_shift_ios_enum()
{
  // https://gcc.gnu.org/pipermail/libstdc++/2021-May/052507.html
  int i = 1 << std::ios::erase_event;

  return i;
}
