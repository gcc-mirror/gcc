// { dg-do compile { target c++23 } }
#include<mdspan>

#include <cstdint>

constexpr size_t dyn = std::dynamic_extent;
static constexpr size_t n = std::numeric_limits<uint8_t>::max() / 2;

template<typename Layout>
  struct A
  {
    typename Layout::mapping<std::extents<uint8_t, n, 2>> m0;
    typename Layout::mapping<std::extents<uint8_t, n, 2, dyn>> m1;
    typename Layout::mapping<std::extents<uint8_t, n, 2, 0>> m2;

    using extents_type = std::extents<uint8_t, n, 4>;
    typename Layout::mapping<extents_type> m3; // { dg-error "required from" }
  };

template<size_t Count, typename Layout, typename OLayout>
  bool
  B()
  {
    using Extents = std::extents<uint8_t, dyn, dyn, Count>;
    using OExtents = std::extents<uint16_t, n, 4, Count>;

    using Mapping = typename Layout::mapping<Extents>;
    using OMapping = typename OLayout::mapping<OExtents>;

    Mapping m{OMapping{}};
    return true;
  };

A<std::layout_left> a_left;                      // { dg-error "required from" }
A<std::layout_right> a_right;                    // { dg-error "required from" }
A<std::layout_stride> a_stride;                  // { dg-error "required from" }

auto b1 = B<1, std::layout_left, std::layout_left>();     // { dg-error "required from" }
auto b2 = B<2, std::layout_left, std::layout_stride>();   // { dg-error "required from" }

auto b3 = B<3, std::layout_right, std::layout_right>();   // { dg-error "required from" }
auto b4 = B<4, std::layout_right, std::layout_stride>();  // { dg-error "required from" }

auto b5 = B<5, std::layout_stride, std::layout_right>();  // { dg-error "required from" }
auto b6 = B<6, std::layout_stride, std::layout_left>();   // { dg-error "required from" }
auto b7 = B<7, std::layout_stride, std::layout_stride>(); // { dg-error "required from" }

// { dg-prune-output "must be representable as index_type" }
// { dg-prune-output "static assertion failed" }
