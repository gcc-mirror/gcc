// { dg-do compile { target c++26 } }
#include <mdspan>

#include <cstdint>

constexpr size_t dyn = std::dynamic_extent;

constexpr auto dyn_empty = std::extents<int32_t, dyn>{0};
constexpr auto sta_empty = std::extents<uint32_t, 0>{};

constexpr auto dyn_uexts = std::extents<uint8_t, dyn>{5};
constexpr auto sta_uexts = std::extents<uint16_t, 5>{5};
constexpr auto dyn_sexts = std::extents<int8_t, dyn>{5};
constexpr auto sta_sexts = std::extents<int16_t, 5>{5};

constexpr bool
test_rank_mismatch()
{
  auto exts = std::extents(1);
  std::submdspan_canonicalize_slices(exts, 0, 0); // { dg-error "no matching" }
  return true;
}

template<typename Int, typename Extents>
constexpr bool
test_under1(Int i1, Extents exts)
{
  auto [s1] = std::submdspan_canonicalize_slices(exts, i1);
  return true;
}

static_assert(test_under1(-1, dyn_sexts));   // { dg-error "expansion of" }
static_assert(test_under1(-1, dyn_uexts));   // { dg-error "expansion of" }
static_assert(test_under1(-1, sta_sexts));   // { dg-error "expansion of" }
static_assert(test_under1(-1, sta_uexts));   // { dg-error "expansion of" }

static_assert(test_under1(std::cw<-1>, dyn_sexts));   // { dg-error "required from" }
static_assert(test_under1(std::cw<-1>, dyn_uexts));   // { dg-error "required from" }
static_assert(test_under1(std::cw<-1>, sta_sexts));   // { dg-error "required from" }
static_assert(test_under1(std::cw<-1>, sta_uexts));   // { dg-error "required from" }

template<typename Int, typename Extents>
constexpr bool
test_over1(Int i1, Extents exts)
{
  auto [s1] = std::submdspan_canonicalize_slices(exts, i1);
  return true;
}

static_assert(test_over1(0, dyn_empty));   // { dg-error "expansion of" }
static_assert(test_over1(0, sta_empty));   // { dg-error "expansion of" }
static_assert(test_over1(5, dyn_sexts));   // { dg-error "expansion of" }
static_assert(test_over1(5, dyn_uexts));   // { dg-error "expansion of" }
static_assert(test_over1(5, sta_sexts));   // { dg-error "expansion of" }
static_assert(test_over1(5, sta_uexts));   // { dg-error "expansion of" }

static_assert(test_over1(std::cw<0>, dyn_empty));   // { dg-error "expansion of" }
static_assert(test_over1(std::cw<0>, sta_empty));   // { dg-error "expansion of" }
static_assert(test_over1(std::cw<5>, dyn_sexts));   // { dg-error "expansion of" }
static_assert(test_over1(std::cw<5>, dyn_uexts));   // { dg-error "expansion of" }
static_assert(test_over1(std::cw<5>, sta_sexts));   // { dg-error "expansion of" }
static_assert(test_over1(std::cw<5>, sta_uexts));   // { dg-error "expansion of" }

template<typename Offset, typename Extent, typename Stride, typename Extents>
  constexpr bool
  test_under2(Offset o, Extent e, Stride s, Extents exts)
  {
    std::submdspan_canonicalize_slices(exts, std::strided_slice{o, e, s});
    return true;
  }

constexpr auto i8_1 = int8_t{1};

static_assert(test_under2(-i8_1, 0, 1, dyn_uexts));   // { dg-error "expansion of" }
static_assert(test_under2(0, -i8_1, 1, dyn_uexts));   // { dg-error "expansion of" }
static_assert(test_under2(0, 1, -i8_1, dyn_uexts));   // { dg-error "expansion of" }
static_assert(test_under2(-i8_1, 0, 1, dyn_sexts));   // { dg-error "expansion of" }
static_assert(test_under2(0, -i8_1, 1, dyn_sexts));   // { dg-error "expansion of" }
static_assert(test_under2(0, 1, -i8_1, dyn_sexts));   // { dg-error "expansion of" }
static_assert(test_under2(-i8_1, 0, 1, sta_uexts));   // { dg-error "expansion of" }
static_assert(test_under2(0, -i8_1, 1, sta_uexts));   // { dg-error "expansion of" }
static_assert(test_under2(0, 1, -i8_1, sta_uexts));   // { dg-error "expansion of" }
static_assert(test_under2(-i8_1, 0, 1, sta_sexts));   // { dg-error "expansion of" }
static_assert(test_under2(0, -i8_1, 1, sta_sexts));   // { dg-error "expansion of" }
static_assert(test_under2(0, 1, -i8_1, sta_sexts));   // { dg-error "expansion of" }

constexpr auto c_i8_m1 = std::cw<int8_t{-1}>;
constexpr auto c_i16_m1 = std::cw<int16_t{-1}>;
constexpr auto c_i64_m1 = std::cw<int64_t{-1}>;

static_assert(test_under2(c_i8_m1, 0, 1, dyn_uexts));   // { dg-error "required from" }
static_assert(test_under2(0, c_i16_m1, 1, dyn_uexts));  // { dg-error "required from" }
static_assert(test_under2(0, 1, c_i64_m1, dyn_uexts));  // { dg-error "required from" }
static_assert(test_under2(c_i8_m1, 0, 1, dyn_sexts));   // { dg-error "required from" }
static_assert(test_under2(0, c_i16_m1, 1, dyn_sexts));  // { dg-error "required from" }
static_assert(test_under2(0, 1, c_i64_m1, dyn_sexts));  // { dg-error "required from" }
static_assert(test_under2(c_i8_m1, 0, 1, sta_uexts));   // { dg-error "required from" }
static_assert(test_under2(0, c_i16_m1, 1, sta_uexts));  // { dg-error "required from" }
static_assert(test_under2(0, 1, c_i64_m1, sta_uexts));  // { dg-error "required from" }
static_assert(test_under2(c_i8_m1, 0, 1, sta_sexts));   // { dg-error "required from" }
static_assert(test_under2(0, c_i16_m1, 1, sta_sexts));  // { dg-error "required from" }
static_assert(test_under2(0, 1, c_i64_m1, sta_sexts));  // { dg-error "required from" }

template<typename Offset, typename Extent, typename Stride, typename Extents>
  constexpr bool
  test_over2(Offset o, Extent e, Stride s, Extents exts)
  {
    std::submdspan_canonicalize_slices(exts, std::strided_slice{o, e, s});
    return true;
  }

constexpr auto i8_6 = int8_t{6};
constexpr auto c_i8_6 = std::cw<int8_t{6}>;
constexpr auto c2 = std::cw<2>;
constexpr auto c4 = std::cw<4>;

static_assert(test_over2(i8_6, 0, 1, dyn_uexts));    // { dg-error "expansion of" }
static_assert(test_over2(0, i8_6, 1, dyn_uexts));    // { dg-error "expansion of" }
static_assert(test_over2(2, 4, 0, dyn_uexts));       // { dg-error "expansion of" }
static_assert(test_over2(c_i8_6, 0, 1, dyn_uexts));  // { dg-error "expansion of" }
static_assert(test_over2(0, c_i8_6, 1, dyn_uexts));  // { dg-error "expansion of" }
static_assert(test_over2(c2, 4, 1, dyn_uexts));      // { dg-error "expansion of" }
static_assert(test_over2(2, c4, 1, dyn_uexts));      // { dg-error "expansion of" }
static_assert(test_over2(c2, c4, 1, dyn_uexts));     // { dg-error "expansion of" }

static_assert(test_over2(i8_6, 0, 1, dyn_sexts));    // { dg-error "expansion of" }
static_assert(test_over2(0, i8_6, 1, dyn_sexts));    // { dg-error "expansion of" }
static_assert(test_over2(2, 4, 0, dyn_sexts));       // { dg-error "expansion of" }
static_assert(test_over2(c_i8_6, 0, 1, dyn_sexts));  // { dg-error "expansion of" }
static_assert(test_over2(0, c_i8_6, 1, dyn_sexts));  // { dg-error "expansion of" }
static_assert(test_over2(c2, 4, 1, dyn_sexts));      // { dg-error "expansion of" }
static_assert(test_over2(2, c4, 1, dyn_sexts));      // { dg-error "expansion of" }
static_assert(test_over2(c2, c4, 1, dyn_sexts));     // { dg-error "expansion of" }

static_assert(test_over2(i8_6, 0, 1, sta_uexts));    // { dg-error "expansion of" }
static_assert(test_over2(0, i8_6, 1, sta_uexts));    // { dg-error "expansion of" }
static_assert(test_over2(2, 4, 0, sta_uexts));       // { dg-error "expansion of" }
static_assert(test_over2(c_i8_6, 0, 1, sta_uexts));  // { dg-error "expansion of" }
static_assert(test_over2(0, c_i8_6, 1, sta_uexts));  // { dg-error "expansion of" }
static_assert(test_over2(c2, 4, 1, sta_uexts));      // { dg-error "expansion of" }
static_assert(test_over2(2, c4, 1, sta_uexts));      // { dg-error "expansion of" }
static_assert(test_over2(c2, c4, 1, sta_uexts));     // { dg-error "expansion of" }

static_assert(test_over2(i8_6, 0, 1, sta_sexts));    // { dg-error "expansion of" }
static_assert(test_over2(0, i8_6, 1, sta_sexts));    // { dg-error "expansion of" }
static_assert(test_over2(2, 4, 0, sta_sexts));       // { dg-error "expansion of" }
static_assert(test_over2(c_i8_6, 0, 1, sta_sexts));  // { dg-error "expansion of" }
static_assert(test_over2(0, c_i8_6, 1, sta_sexts));  // { dg-error "expansion of" }
static_assert(test_over2(c2, 4, 1, sta_sexts));      // { dg-error "expansion of" }
static_assert(test_over2(2, c4, 1, sta_sexts));      // { dg-error "expansion of" }
static_assert(test_over2(c2, c4, 1, sta_sexts));     // { dg-error "expansion of" }

// Checks the precondition: offset + extent <= exts.extent(0) for unsigned
// index_type when offset + extent overflows.
constexpr bool
test_overflow1(auto o, auto e)
{
  auto exts = std::extents<uint8_t, dyn>{255};
  auto slice = std::strided_slice{o, e, 1};
  std::submdspan_canonicalize_slices(exts, slice);
  return true;
}

static_assert(test_overflow1(128, 128));                    // { dg-error "expansion of" }
static_assert(test_overflow1(std::cw<128>, 128));           // { dg-error "expansion of" }
static_assert(test_overflow1(128, std::cw<128>));           // { dg-error "expansion of" }
static_assert(test_overflow1(std::cw<128>, std::cw<128>));  // { dg-error "expansion of" }

constexpr bool
test_overflow2(auto b, auto e)
{
  auto exts = std::extents<uint8_t, dyn>{255};
  auto slice = std::pair{b, e};
  std::submdspan_canonicalize_slices(exts, slice);
  return true;
}

static_assert(test_overflow2(5, 4));                    // { dg-error "expansion of" }
static_assert(test_overflow2(std::cw<5>, 4));           // { dg-error "expansion of" }
static_assert(test_overflow2(5, std::cw<4>));           // { dg-error "expansion of" }
static_assert(test_overflow2(std::cw<5>, std::cw<4>));  // { dg-error "expansion of" }

constexpr auto u8_4 = uint8_t{4};
constexpr auto u8_5 = uint8_t{5};
static_assert(test_overflow2(u8_5, u8_4));                    // { dg-error "expansion of" }
static_assert(test_overflow2(std::cw<u8_5>, u8_4));           // { dg-error "expansion of" }
static_assert(test_overflow2(u8_5, std::cw<u8_4>));           // { dg-error "expansion of" }
static_assert(test_overflow2(std::cw<u8_5>, std::cw<u8_4>));  // { dg-error "expansion of" }

constexpr bool
test_invalid(auto e, auto s)
{
  auto exts = std::extents(5);
  auto slice = std::strided_slice(0, e, s);
  std::submdspan_canonicalize_slices(exts, slice);
  return true;
}

static_assert(test_invalid(3, 0));                   // { dg-error "expansion of" }
static_assert(test_invalid(3, std::cw<0>));          // { dg-error "expansion of" }
static_assert(test_invalid(3, std::cw<0>));          // { dg-error "expansion of" }
static_assert(test_invalid(std::cw<3>, std::cw<0>)); // { dg-error "expansion of" }


// { dg-prune-output "static assertion failed" }
// { dg-prune-output "__glibcxx_assert_fail" }
// { dg-prune-output "__glibcxx_assert" }
// { dg-prune-output "non-constant condition" }
