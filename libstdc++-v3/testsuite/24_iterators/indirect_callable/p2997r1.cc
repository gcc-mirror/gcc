// Test the example from P2997R1 "Removing the common reference requirement
// from the indirectly invocable concepts".
// { dg-do compile { target c++20 } }

#include <algorithm>
#include <ranges>

struct C {
    auto f() -> void;
};

struct Iterator {
    using value_type = C;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::input_iterator_tag;

    auto operator*() const -> C&&;
    auto operator++() -> Iterator&;
    auto operator++(int) -> void;
    auto operator==(Iterator const&) const -> bool;
};

static_assert(std::input_iterator<Iterator>);
static_assert(std::same_as<std::iter_value_t<Iterator>, C>);
static_assert(std::same_as<std::iter_reference_t<Iterator>, C&&>);

struct R {
    auto begin() -> Iterator;
    auto end() -> Iterator;
};

static_assert(std::ranges::range<R>);
static_assert(std::same_as<std::ranges::range_reference_t<R>, C&&>);

auto f(R r) -> void {
    std::ranges::for_each(r, [](auto&& c){ c.f(); });
}
