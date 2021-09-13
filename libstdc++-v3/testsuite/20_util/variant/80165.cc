// { dg-do compile { target c++17 } }

#include <tuple>
#include <variant>

int main() {
  using variant_t = std::variant<short, int, long>;
  constexpr auto variant_v = variant_t{std::in_place_index_t<0>{}, short{}};
  constexpr auto tuple = std::make_tuple(variant_v);
  constexpr std::tuple tuple_v{variant_v};
}
