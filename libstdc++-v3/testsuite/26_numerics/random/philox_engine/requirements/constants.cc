// { dg-do compile { target c++26 } }

// N5014 29.5.4.5 Class Template philox_engine

#include <random>

namespace test1
{
  using P = std::philox_engine<std::uint32_t, 32, 4, 5, 9, 99, 999, 9999>;
  constexpr std::same_as<std::uint32_t> auto min = P::min();
  static_assert( min == 0 );
  constexpr std::same_as<std::uint32_t> auto max = P::max();
  static_assert( max == 0xffffffff );
  constexpr std::same_as<std::size_t> auto w = P::word_size;
  static_assert( w == 32 );
  constexpr std::same_as<std::size_t> auto n = P::word_count;
  static_assert( n == 4 );
  constexpr std::same_as<std::size_t> auto r = P::round_count;
  static_assert( r == 5 );
  constexpr std::array<std::uint32_t, 2> muls = P::multipliers;
  static_assert( muls[0] == 9 && muls[1] == 999 );
  constexpr std::array<std::uint32_t, 2> consts = P::round_consts;
  static_assert( consts[0] == 99 && consts[1] == 9999 );
  constexpr std::same_as<std::uint32_t> auto def = P::default_seed;
  static_assert( def == 20111115u );
}

namespace test2
{
  using P = std::philox_engine<std::uint64_t, 64, 2, 12, 77, 777>;
  constexpr std::same_as<std::uint64_t> auto min = P::min();
  static_assert( min == 0 );
  constexpr std::same_as<std::uint64_t> auto max = P::max();
  static_assert( max == 0xffffffffffffffff );
  constexpr std::same_as<std::size_t> auto w = P::word_size;
  static_assert( w == 64 );
  constexpr std::same_as<std::size_t> auto n = P::word_count;
  static_assert( n == 2 );
  constexpr std::same_as<std::size_t> auto r = P::round_count;
  static_assert( r == 12 );
  constexpr std::array<std::uint64_t, 1> muls = P::multipliers;
  static_assert( muls[0] == 77 );
  constexpr std::array<std::uint64_t, 1> consts = P::round_consts;
  static_assert( consts[0] == 777 );
  constexpr std::same_as<std::uint64_t> auto def = P::default_seed;
  static_assert( def == 20111115u );
}
