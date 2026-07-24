// { dg-do compile { target c++23 } }
#include <mdspan>
#include <cstdint>

using std::array;
template<size_t Rank>
 using mapping = std::layout_stride::mapping<std::dextents<size_t, Rank>>;

// Only some permutations are valid, invalid permutations detected only in debug mode.

// Unique if stride 4 corresponds to extent 3
constexpr auto m28 = mapping<2>(array{3, 4}, array{4, 10}); // { dg-bogus "expansion of" }
constexpr auto m29 = mapping<2>(array{3, 4}, array{10, 4}); // { dg-error "expansion of" "" { target debug_mode } }
constexpr auto m30 = mapping<2>(array{4, 3}, array{10, 4}); // { dg-bogus "expansion of" }
constexpr auto m31 = mapping<2>(array{4, 3}, array{4, 10}); // { dg-error "expansion of" "" { target debug_mode } }

// Unique if stride 11 corresponds to extent 3, 2 * (5-1) = 8 < 11, and 11 * (3-1) + 8 = 30 < 35
constexpr auto m32 = mapping<3>(array{3, 4, 5}, array{11, 2, 35}); // { dg-bogus "expansion of" }
constexpr auto m33 = mapping<3>(array{3, 4, 5}, array{11, 35, 2}); // { dg-bogus "expansion of" }
constexpr auto m34 = mapping<3>(array{3, 4, 5}, array{2, 11, 35}); // { dg-error "expansion of" "" { target debug_mode } }
constexpr auto m35 = mapping<3>(array{3, 4, 5}, array{35, 2, 11}); // { dg-error "expansion of" "" { target debug_mode } }
constexpr auto m36 = mapping<3>(array{5, 4, 3}, array{2, 35, 11}); // { dg-bogus "expansion of" }
constexpr auto m37 = mapping<3>(array{5, 4, 3}, array{2, 11, 35}); // { dg-error "expansion of" "" { target debug_mode } }
constexpr auto m38 = mapping<3>(array{5, 3, 4}, array{35, 11, 2}); // { dg-bogus "expansion of" }
constexpr auto m39 = mapping<3>(array{3, 5, 4}, array{35, 2, 11}); // { dg-error "expansion of" "" { target debug_mode } }

// Unique for stride -> extent mapping: 3 * (5-1) = 12 < 16, 15 * (4-1) + 12 = 57 < 65, 65 * (3-1) + 57 = 187 < 200
constexpr auto m40 = mapping<4>(array{3, 4, 5, 6}, array{65, 16, 3, 200}); // { dg-bogus "expansion of" }
constexpr auto m41 = mapping<4>(array{3, 4, 5, 6}, array{3, 16, 65, 200}); // { dg-error "expansion of" "" { target debug_mode } }
constexpr auto m42 = mapping<4>(array{3, 4, 5, 6}, array{200, 65, 3, 16}); // { dg-error "expansion of" "" { target debug_mode } }
constexpr auto m43 = mapping<4>(array{5, 3, 6, 4}, array{3, 65, 200, 16}); // { dg-bogus "expansion of" }
constexpr auto m44 = mapping<4>(array{5, 3, 6, 4}, array{3, 16, 200, 65}); // { dg-error "expansion of" "" { target debug_mode } }

// Examples based on one from LWG 4606
constexpr auto m45 = mapping<2>(array{2, 3}, array{5, 2}); // { dg-bogus "expansion of" }
constexpr auto m46 = mapping<3>(array{2, 3, 5}, array{5, 2, 10}); // { dg-bogus "expansion of" }
constexpr auto m47 = mapping<3>(array{2, 3, 1}, array{5, 2, 99}); // { dg-bogus "expansion of" }
constexpr auto m48 = mapping<4>(array{2, 3, 5, 4}, array{5, 2, 10, 50}); // { dg-bogus "expansion of" }
constexpr auto m49 = mapping<4>(array{2, 1, 3, 5}, array{5, 99, 2, 10}); // { dg-bogus "expansion of" }

// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "__glibcxx_assert_fail()" }
