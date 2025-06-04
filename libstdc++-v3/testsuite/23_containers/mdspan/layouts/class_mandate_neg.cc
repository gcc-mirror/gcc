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
  struct B
  {
    using Extents = std::extents<uint8_t, dyn, dyn, Count>;
    using OExtents = std::extents<uint16_t, n, 4, Count>;

    using Mapping = typename Layout::mapping<Extents>;
    using OMapping = typename OLayout::mapping<OExtents>;

    Mapping m{OMapping{}};
  };

A<std::layout_left> a_left;                      // { dg-error "required from" }

B<1, std::layout_left, std::layout_left> b0;     // { dg-error "required here" }

// { dg-prune-output "must be representable as index_type" }
