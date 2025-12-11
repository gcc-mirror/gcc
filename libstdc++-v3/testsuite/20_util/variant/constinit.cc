// { dg-do compile { target c++17 } }

#include <variant>

struct NonEmpty { int x; };
struct NonTrivial
{
  constexpr NonTrivial() : x(0) {}
  NonTrivial(int p) : x(p) {}
  ~NonTrivial() {}

  int x;
};

struct TrivialEmpty {};
struct NonTrivialEmpty
{
  NonTrivialEmpty() = default;
  NonTrivialEmpty(float) {}
  ~NonTrivialEmpty() {}
};

std::variant<NonEmpty> vNonEmpty(std::in_place_type<NonEmpty>);
// { dg-final { scan-assembler-dem-not "(std::in_place_type_t<NonEmpty>)" } }

std::variant<NonTrivial> vNonTrivial(std::in_place_type<NonTrivial>);
// { dg-final { scan-assembler-dem-not "(std::in_place_type_t<NonTrivial>)" } }

std::variant<int, NonTrivial> vNonTrivialNonConstexpr(std::in_place_index<1>, 2);
// { dg-final { scan-assembler-dem "(std::in_place_index_t<1ul?>, int&&)" } }

std::variant<TrivialEmpty> vTrivialEmpty(std::in_place_type<TrivialEmpty>);
// { dg-final { scan-assembler-dem-not "(std::in_place_type_t<TrivialEmpty>)" } }

std::variant<NonTrivialEmpty> vNonTrivialEmpty(std::in_place_type<NonTrivialEmpty>);
// { dg-final { scan-assembler-dem-not "(std::in_place_type_t<NonTrivialEmpty>)" } }

std::variant<int, NonTrivialEmpty> vNonTrivialEmptyNonConstexpr(std::in_place_index<1>, 2.0);
// { dg-final { scan-assembler-dem "(std::in_place_index_t<1ul?>, double&&)" } }

