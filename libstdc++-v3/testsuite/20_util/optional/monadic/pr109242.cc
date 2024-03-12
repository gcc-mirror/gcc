// { dg-do compile { target c++23 } }

#include <optional>

// PR libstdc++/109242
// transform omits required std::remove_cv_t from return optional type

struct A { };
struct B { };
struct C { };
struct D { };

struct F
{
  const A operator()(int&);
  const B operator()(const int&);
  const C operator()(int&&);
  const D operator()(const int&&);
} f;

std::optional<int> o;
const auto& co = o;

auto o1 = o.transform(f);
static_assert(std::is_same_v<decltype(o1), std::optional<A>>);

auto o2 = co.transform(f);
static_assert(std::is_same_v<decltype(o2), std::optional<B>>);

auto o3 = std::move(o).transform(f);
static_assert(std::is_same_v<decltype(o3), std::optional<C>>);

auto o4 = std::move(co).transform(f);
static_assert(std::is_same_v<decltype(o4), std::optional<D>>);
