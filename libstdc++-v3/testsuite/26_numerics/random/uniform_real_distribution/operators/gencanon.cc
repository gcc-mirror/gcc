// { dg-do run { target { c++11 && { ! simulator } } } }
// { dg-skip-if "requires new impl" { *-*-* } { -D_GLIBCXX_USE_OLD_GENERATE_CANONICAL -D_GLIBCXX_USE_OLD_GENERATE_CANONICAL=1 } }
// { dg-additional-options -fexcess-precision=standard }

#include <random>
#include <limits>
#include <type_traits>
#include <cmath>
#include <testsuite_hooks.h>
#include <array>

template <std::uint64_t max_val>
struct local_rng : std::mt19937
{
  static constexpr std::uint64_t min() { return 0; }
  static constexpr std::uint64_t max() { return max_val; }
  std::uint64_t operator()()
    { return static_cast<std::mt19937&>(*this)() % (max() + 1); }
  local_rng(std::mt19937 const& arg) : std::mt19937(arg) {}
};

// Verify P0952R2 implementation requiring a second round-trip
// if first yields exactly 1. In this test, the RNG delivering
// 32 bits per call is seeded such that this occurs once on the
// sixth iteration for float, and not at all for double.
// However, each double iteration requires two calls to the RNG.

template<typename T>
int ifloor(T t)
{ return static_cast<int>(std::floor(t)); }

#ifdef __SIZEOF_FLOAT128__
int ifloor(__float128 t)
{ return static_cast<int>(t); }
#endif

template <typename T, typename RNG>
void
test01(RNG& rng, RNG& rng2, int& skips)
{
  constexpr size_t mantissa = std::numeric_limits<T>::digits;
  constexpr size_t call_per_elem = (mantissa + 31) / 32;

  const auto size = 1000000, buckets = 100;
  std::array<int, buckets> histo{};
  int zero = 0;
  for (auto i = 0; i != size; ++i) {
    T sample = std::generate_canonical<T, -1u>(rng);
    VERIFY(sample >= T(0.0));
    VERIFY(sample < T(1.0));  // libstdc++/64351
    if (sample == T(0.0)) {
      ++zero;
    }
    auto bucket = ifloor(sample * buckets);
    ++histo[bucket];
    rng2.discard(call_per_elem);
    if (rng != rng2) {
      ++skips;
      rng2.discard(call_per_elem);
      VERIFY(rng == rng2);
    }
  }

  if (!std::numeric_limits<T>::is_iec559)
    return;

  int deviation = 0, max = 0, rms = 0, devsquare = 0;
  for (int i = 0; i < buckets; ++i) {
    const auto expected = size / buckets;
    auto count = histo[i];
    auto diff = count - expected;
    if (diff < 0) diff = -diff;
    deviation += diff;
    devsquare += diff * diff;
    if (diff > max) max = diff;
  }
  rms = std::sqrt(devsquare);

  switch (mantissa)
  {
  case 24: // ieee32
    VERIFY(deviation == 7032);
    VERIFY(max == 276);
    VERIFY(rms == 906);
    VERIFY(zero == 0);
    break;
  case 53: // ieee64
  case 64: // ieee80 
    VERIFY(deviation == 7650);
    VERIFY(max == 259);
    VERIFY(rms == 975);
    VERIFY(zero == 0);
    break;
  case 113: // ieee128
    VERIFY(deviation == 9086);
    VERIFY(max == 290);
    VERIFY(rms == 1142);
    VERIFY(zero == 0);
    break;
  default:
    VERIFY(false);
    break;
  }
}

// This one is for use with local_rng
template <typename T, typename RNG>
void
test02(RNG& rng, RNG& rng2,
  int& deviation, int& max, int& rms, int& zero, int& skips)
{
  const auto size = 1000000, buckets = 100;
  std::array<int, buckets> histo{};
  for (auto i = 0; i != size; ++i) {
    T sample = std::generate_canonical<T, -1u>(rng);
    VERIFY(sample >= T(0.0));
    VERIFY(sample < T(1.0));  // libstdc++/64351
    if (sample == T(0.0)) {
      ++zero;
    }
    auto bucket = static_cast<int>(std::floor(sample * buckets));
    ++histo[bucket];
    rng2.discard(2);
    if (rng != rng2) {
      ++skips;
      rng2.discard(2);
      VERIFY(rng == rng2);
    }
  }
  int devsquare = 0;
  for (int i = 0; i < buckets; ++i) {
    const auto expected = size / buckets;
    auto count = histo[i];
    auto diff = count - expected;
    if (diff < 0) diff = -diff;
    deviation += diff;
    devsquare += diff * diff;
    if (diff > max) max = diff;
  }
  rms = std::sqrt(devsquare);
}

// This one is for the edge-case local_rng. It takes a bit count
// to use that is smaller than the floating point mantissa's.
template <typename T, unsigned bits, typename RNG>
void
test03(RNG& rng, RNG& rng2,
  int& deviation, int& max, int& rms, int& zero, int& skips)
{
  const auto size = 1000000, buckets = 100;
  std::array<int, buckets> histo{};
  for (auto i = 0; i != size; ++i) {
    T sample = std::generate_canonical<T, bits>(rng);
    VERIFY(sample >= T(0.0));
    VERIFY(sample < T(1.0));  // libstdc++/64351
    if (sample == T(0.0)) {
      ++zero;
    }
    auto bucket = static_cast<int>(std::floor(sample * buckets));
    ++histo[bucket];
    rng2.discard(2);
    if (rng != rng2) {
      ++skips;
      rng2.discard(2);
      VERIFY(rng == rng2);
    }
  }
  int devsquare = 0;
  for (int i = 0; i < buckets; ++i) {
    const auto expected = size / buckets;
    auto count = histo[i];
    auto diff = count - expected;
    if (diff < 0) diff = -diff;
    deviation += diff;
    devsquare += diff * diff;
    if (diff > max) max = diff;
  }
  rms = std::sqrt(devsquare);
}

void
test00()
{
  std::mt19937 rng(8890);
  std::seed_seq sequence{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  rng.seed(sequence);
  rng.discard(12 * 629143);

  { // float
    int skips{};
    auto rng2{rng};
    auto rng3{rng};
    test01<float>(rng2, rng3, skips);
    VERIFY(skips == 1);
  }
  { // double
    int skips{};
    auto rng2{rng};
    auto rng3{rng};
    test01<double>(rng2, rng3, skips);
    VERIFY(skips == 0);
  }
  { // long double
    int skips{};
    auto rng2{rng};
    auto rng3{rng};
    test01<long double>(rng2, rng3, skips);
    VERIFY(skips == 0);
  }
#ifndef _GLIBCXX_GENERATE_CANONICAL_STRICT
#  ifdef __SIZEOF_FLOAT128__
  {
    int skips{};
    auto rng2{rng};
    auto rng3{rng};
    test01<__float128>(rng2, rng3, skips);
    VERIFY(skips == 0);
  }
#  endif
#endif

  { // local RNG, returns [0..999'999)
    int deviation{}, max{}, rms{}, zero{}, skips{};
    local_rng<999999ULL> rng2{rng};
    local_rng<999999ULL> rng3{rng};
    test02<float>(rng2, rng3, deviation, max, rms, zero, skips);

    if (std::numeric_limits<float>::is_iec559)
    {
      VERIFY(deviation == 8146);
      VERIFY(max == 250);
      VERIFY(rms == 1021);
      VERIFY(zero == 0);
    }
    VERIFY(skips == 18);
  }

  { // local RNG, returns [0..0x0'7fff'fffe)
    int deviation{}, max{}, rms{}, zero{}, skips{};
    local_rng<0x07ffffffeULL> rng2{rng};
    local_rng<0x07ffffffeULL> rng3{rng};
    test03<double, 32u>(rng2, rng3, deviation, max, rms, zero, skips);

    if (std::numeric_limits<double>::is_iec559)
    {
      VERIFY(deviation == 7820);
      VERIFY(max == 240);
      VERIFY(rms == 950);
      VERIFY(zero == 0);
    }
    VERIFY(skips == 0);
  }
}

int main()
{
  test00();
}
