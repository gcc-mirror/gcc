// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <optional>

void
test01()
{
  std::optional<int> o;
  o.or_else([&] { return o; }); // OK
  o.or_else([] { return std::optional<short>(); }); // { dg-error "here" }
  o.or_else([] { return 1; }); // { dg-error "here" }
  std::move(o).or_else([] { return std::optional<short>(); }); // { dg-error "here" }
  std::move(o).or_else([] { return 1; }); // { dg-error "here" }
}

// { dg-prune-output "static assertion failed" }

void
test02()
{
  struct move_only
  {
    move_only() { }
    move_only(move_only&&) { }
  };

  std::optional<move_only> mo;
  mo.or_else([]{ return std::optional<move_only>{}; }); // { dg-error "no matching function" }
}
