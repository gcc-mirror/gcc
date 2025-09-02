// { dg-do run { target c++26 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <mdspan>
#include <array>

void
test_unaligned_access()
{
  constexpr size_t N = 4;
  alignas(N) std::array<char, 128> buffer{};
  auto* unaligned = buffer.data() + 1;
  auto a = std::aligned_accessor<char, N>{};

  [[maybe_unused]] char x = a.access(unaligned, 0);
}

int
main()
{
  test_unaligned_access();
  return 0;
};
