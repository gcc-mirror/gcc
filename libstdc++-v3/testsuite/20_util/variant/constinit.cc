// { dg-options "-fdump-tree-gimple" }
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
// { dg-final { scan-tree-dump-not "std::variant<NonEmpty>::variant" "gimple" } }

std::variant<NonTrivial> vNonTrivial(std::in_place_type<NonTrivial>);
// { dg-final { scan-tree-dump-not "std::variant<NonTrivial>::variant" "gimple" } }

std::variant<int, NonTrivial> vNonTrivialNonConstexpr(std::in_place_index<1>, 2);
// { dg-final { scan-tree-dump "std::variant<int, NonTrivial>::variant" "gimple" } }

std::variant<TrivialEmpty> vTrivialEmpty(std::in_place_type<TrivialEmpty>);
// { dg-final { scan-tree-dump-not "std::variant<TrivialEmpty>::variant" "gimple" } }

std::variant<NonTrivialEmpty> vNonTrivialEmpty(std::in_place_type<NonTrivialEmpty>);
// { dg-final { scan-tree-dump-not "std::variant<NonTrivialEmpty>::variant" "gimple" } }

std::variant<int, NonTrivialEmpty> vNonTrivialEmptyNonConstexpr(std::in_place_index<1>, 2.0);
// { dg-final { scan-tree-dump "std::variant<int, NonTrivialEmpty>::variant" "gimple" } }

