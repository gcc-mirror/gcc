// { dg-do run { target c++26 } }

#include <memory>
#include <array>
#include <testsuite_hooks.h>

void
test01()
{
  constexpr size_t N = 4;
  constexpr size_t M = 2*N + 1;
  alignas(N) std::array<char, M> buffer{};

  auto* ptr = buffer.data();
  VERIFY(std::is_sufficiently_aligned<1>(ptr+0));
  VERIFY(std::is_sufficiently_aligned<1>(ptr+1));

  VERIFY(std::is_sufficiently_aligned<2>(ptr+0));
  VERIFY(!std::is_sufficiently_aligned<2>(ptr+1));
  VERIFY(std::is_sufficiently_aligned<2>(ptr+2));

  for (size_t i = 0; i < M; ++i)
    VERIFY(std::is_sufficiently_aligned<N>(ptr + i) == (i % N == 0));
}

int
main()
{
  test01();
  return 0;
}
