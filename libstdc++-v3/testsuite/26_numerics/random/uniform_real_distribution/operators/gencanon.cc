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
  { return std::mt19937::operator()() % (max() + 1); }

  local_rng(std::mt19937 const& arg) : std::mt19937(arg) {}
};

template<typename T>
int ifloor(T t)
{ return static_cast<int>(std::floor(t)); }

#ifdef __SIZEOF_FLOAT128__
int ifloor(__float128 t)
{ return static_cast<int>(t); }
#endif

// Verify P0952R2 implementation requiring a second round-trip
// if first yields exactly 1.
template<typename T, size_t bits,
	 size_t call_per_elem, size_t max_skips_per_elem,
	 typename RNG>
int run_generator(const RNG& rng, int& deviation, int& max, int& rms, int& zeros)
{
  constexpr int iters = 1000000, buckets = 100;
  std::array<int, buckets> histo{};

  RNG rng1(rng), rng2(rng);
  int skips = 0;
  for (auto i = 0; i != iters; ++i) {
    T sample = std::generate_canonical<T, bits>(rng1);
    VERIFY(sample >= T(0.0));
    VERIFY(sample < T(1.0));  // libstdc++/64351
    if (sample == T(0.0)) {
      ++zeros;
    }
    auto bucket = ifloor(sample * buckets);
    ++histo[bucket];
    rng2.discard(call_per_elem);
    for (int j = 0; j < max_skips_per_elem; ++j) {
      if (rng1 == rng2)
	break;
      rng2.discard(call_per_elem);
      ++skips;
    }
    VERIFY(rng1 == rng2);
  }

  int devsquare = 0;
  const int expected = iters / buckets;
  for (int i = 0; i < buckets; ++i) {
    auto count = histo[i];
    auto diff = count - expected;
    if (diff < 0) diff = -diff;
    deviation += diff;
    devsquare += diff * diff;
    if (diff > max) max = diff;
  }
  rms = std::sqrt(devsquare);

  return skips;
}

// In this test, the RNG delivering 32 bits per call is seeded
// such that this occurs once on the sixth iteration for float,
// and not at all for double and other types requiring two
// calls to the RNG.
template <typename T>
void
test_2p32(const std::mt19937& rng)
{
  if (!std::numeric_limits<T>::is_iec559)
    return;

  constexpr size_t mantissa = std::numeric_limits<T>::digits;
  constexpr size_t call_per_elem = (mantissa + 31) / 32;

  int deviation = 0, max = 0, rms = 0, zeros = 0;
  int skips = run_generator<T, -1u, call_per_elem, 1u>
		(rng, deviation, max, rms, zeros);
  if (call_per_elem == 1)
    VERIFY(skips == 1);
  else // would require constitutive elements with all bits set
    VERIFY(skips == 0);

  switch (mantissa)
  {
  case 24: // ieee32
    VERIFY(deviation == 7032);
    VERIFY(max == 276);
    VERIFY(rms == 906);
    VERIFY(zeros == 0);
    break;
  case 53: // ieee64
  case 64: // ieee80
    VERIFY(deviation == 7650);
    VERIFY(max == 259);
    VERIFY(rms == 975);
    VERIFY(zeros == 0);
    break;
  case 106: // ibm128
  case 113: // ieee128
    VERIFY(deviation == 9086);
    VERIFY(max == 290);
    VERIFY(rms == 1142);
    VERIFY(zeros == 0);
    break;
  default:
    VERIFY(false);
    break;
  }
}

// Uses a generate that emits range of size 10^6.
// The elements are multiplied and then dividided,
// skips are more common.
template <typename T>
void
test_10p6(const std::mt19937& rng)
{
  if (!std::numeric_limits<T>::is_iec559)
    return;

  constexpr size_t mantissa = std::numeric_limits<T>::digits;
  static_assert(mantissa < 120);
  constexpr size_t call_per_elem = (mantissa / 20) + 1;
  const local_rng<999999ULL> lrng{rng};

  int deviation = 0, max = 0, rms = 0, zeros = 0;
  int skips = run_generator<T, -1u, call_per_elem, 2u>
		(lrng, deviation, max, rms, zeros);

  switch (mantissa)
  {
  case 24: // ieee32
    VERIFY(skips == 18);
    VERIFY(deviation == 8146);
    VERIFY(max == 250);
    VERIFY(rms == 1021);
    VERIFY(zeros == 0);
    break;
  case 53: // ieee64
    VERIFY(skips == 211);
    VERIFY(deviation == 7492);
    VERIFY(max == 235);
    VERIFY(rms == 921);
    VERIFY(zeros == 0);
    break;
  case 64: // ieee80
    VERIFY(skips == 1);
    VERIFY(deviation == 7774);
    VERIFY(max == 250);
    VERIFY(rms == 958);
    VERIFY(zeros == 0);
    break;
  case 106: // ibm128
    VERIFY(skips == 96);
    VERIFY(deviation == 6980);
    VERIFY(max == 260);
    VERIFY(rms == 876);
    VERIFY(zeros == 0);
    break;
  case 113: // ieee128
    VERIFY(skips == 3074);
    VERIFY(deviation == 7568);
    VERIFY(max == 282);
    VERIFY(rms == 1001);
    VERIFY(zeros == 0);
    break;
  default:
    VERIFY(false);
    break;
  }
}

// Uses a generate that emits range of size 2^31-1.
// It takes a bit count to use that is smaller than the floating point mantissa's.
template <typename T>
void
test_2p31m1(const std::mt19937& rng)
{
  if (!std::numeric_limits<T>::is_iec559)
    return;

  constexpr size_t mantissa = std::numeric_limits<T>::digits;
  constexpr size_t bits = mantissa - 5;
  static_assert(bits < 124);
  constexpr size_t call_per_elem = (bits / 31) + 1;
  const local_rng<0x07ffffffeULL> lrng{rng};

  int deviation = 0, max = 0, rms = 0, zeros = 0;
  int skips = run_generator<T, bits, call_per_elem, 6u>
		(lrng, deviation, max, rms, zeros);

  switch (mantissa)
  {
  case 24: // ieee32
    VERIFY(skips == 215);
    VERIFY(deviation == 7624);
    VERIFY(max == 217);
    VERIFY(rms == 933);
    VERIFY(zeros == 1);
    break;
  case 53: // ieee64
    VERIFY(skips == 62);
    VERIFY(deviation == 7698);
    VERIFY(max == 234);
    VERIFY(rms == 937);
    VERIFY(zeros == 0);
    break;
  case 64: // ieee80
    VERIFY(skips == 143342);
    VERIFY(deviation == 7788);
    VERIFY(max == 296);
    VERIFY(rms == 977);
    VERIFY(zeros == 0);
    break;
  case 106: // ibm128
    VERIFY(skips == 0);
    VERIFY(deviation == 8828);
    VERIFY(max == 330);
    VERIFY(rms == 1084);
    VERIFY(zeros == 0);
    break;
  case 113: // ieee128
    VERIFY(skips == 8);
    VERIFY(deviation == 8824);
    VERIFY(max == 334);
    VERIFY(rms == 1086);
    VERIFY(zeros == 0);
    break;
  default:
    VERIFY(false);
    break;
  }
}

template<std::uint64_t Max, typename Under = std::mt19937_64>
struct trimmed_engine
{
  using result_type = std::uint64_t;

  static constexpr
  result_type min()
  { return result_type(0); }

  static constexpr
  result_type max()
  { return result_type(Max); }

  trimmed_engine() : dist(min(), max())
  {}

  trimmed_engine(const Under& gen)
  : under(gen), dist(min(), max())
  {}

  result_type operator()()
  { return dist(under); }

  void discard(std::size_t n)
  {
    for (size_t i = 0; i < n; ++i)
      (void)dist(under);
  }

  friend bool
  operator==(const trimmed_engine& lhs, const trimmed_engine& rhs)
  { return lhs.under == rhs.under; }

private:
  Under under;
  std::uniform_int_distribution<result_type> dist;
};

// Uses a generator that emits range of size 2^55+2.
// To produce 128bit IEE float, we need 3 calls to above generator,
// that can produce value that have 155 bits, and uses separate path.
template <typename T>
void
test_2p55p1(const std::mt19937& rng)
{
  if (!std::numeric_limits<T>::is_iec559)
    return;

  constexpr size_t mantissa = std::numeric_limits<T>::digits;
  constexpr size_t call_per_elem = (mantissa / 55) + 1;
  const trimmed_engine<(1ULL << 55) + 1ULL, std::mt19937> lrng{rng};

  int deviation = 0, max = 0, rms = 0, zeros = 0;
  int skips = run_generator<T, -1u, call_per_elem, 1u>
		(lrng, deviation, max, rms, zeros);

  switch (mantissa)
  {
  case 24: // ieee32
  case 53: // ieee64
    VERIFY(skips == 0);
    VERIFY(deviation == 7682);
    VERIFY(max == 295);
    VERIFY(rms == 971);
    VERIFY(zeros == 0);
    break;
  case 64: // ieee80
  case 106: // ibm128
    VERIFY(skips == 0);
    VERIFY(deviation == 6788);
    VERIFY(max == 248);
    VERIFY(rms == 854);
    VERIFY(zeros == 0);
    break;
  case 113: // ieee128
    VERIFY(skips == 0);
    VERIFY(deviation == 8222);
    VERIFY(max == 257);
    VERIFY(rms == 999);
    VERIFY(zeros == 0);
    break;
  default:
    VERIFY(false);
    break;
  }
}

template <typename T>
void
test_2p7p1(const std::mt19937& rng)
{
  if (!std::numeric_limits<T>::is_iec559)
    return;

  constexpr size_t mantissa = std::numeric_limits<T>::digits;
  constexpr size_t call_per_elem = (mantissa / 7) + 1;
  const trimmed_engine<128U, std::mt19937> lrng{rng};

  int deviation = 0, max = 0, rms = 0, zeros = 0;
  int skips = run_generator<T, -1u, call_per_elem, 5u>
		(lrng, deviation, max, rms, zeros);

  switch (mantissa)
  {
  case 24: // ieee32
    VERIFY(skips == 31641);
    VERIFY(deviation == 8508);
    VERIFY(max == 294);
    VERIFY(rms == 1077);
    VERIFY(zeros == 0);
    break;
  case 53: // ieee64
    VERIFY(skips == 63971);
    VERIFY(deviation == 7500);
    VERIFY(max == 245);
    VERIFY(rms == 919);
    VERIFY(zeros == 0);
    break;
  case 64: // ieee80
    VERIFY(skips == 2538);
    VERIFY(deviation == 8266);
    VERIFY(max == 310);
    VERIFY(rms == 1065);
    VERIFY(zeros == 0);
    break;
  case 106: // ibm128
    VERIFY(skips == 6651);
    VERIFY(deviation == 7740);
    VERIFY(max == 282);
    VERIFY(rms == 1000);
    VERIFY(zeros == 0);
    break;
  case 113: // ieee128
    VERIFY(skips == 679);
    VERIFY(deviation == 7450);
    VERIFY(max == 284);
    VERIFY(rms == 936);
    VERIFY(zeros == 0);
    break;
  default:
    VERIFY(false);
    break;
  }
}

int main()
{
  std::mt19937 rng(8890);
  std::seed_seq sequence{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  rng.seed(sequence);
  rng.discard(12 * 629143);

  test_2p32<float>(rng);
  test_10p6<float>(rng);
  test_2p31m1<float>(rng);
  test_2p55p1<float>(rng);
  test_2p7p1<float>(rng);

  test_2p32<double>(rng);
  test_10p6<double>(rng);
  test_2p31m1<double>(rng);
  test_2p55p1<double>(rng);
  test_2p7p1<double>(rng);

  test_2p32<long double>(rng);
  test_10p6<long double>(rng);
  test_2p31m1<long double>(rng);
  test_2p55p1<long double>(rng);
  test_2p7p1<long double>(rng);

#ifndef _GLIBCXX_GENERATE_CANONICAL_STRICT
#  ifdef __SIZEOF_FLOAT128__
  test_2p32<__float128>(rng);
  test_10p6<__float128>(rng);
  test_2p31m1<__float128>(rng);
  test_2p55p1<__float128>(rng);
  test_2p7p1<__float128>(rng);
#  endif
#endif
}

