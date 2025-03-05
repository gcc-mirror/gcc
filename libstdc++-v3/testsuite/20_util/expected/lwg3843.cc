// { dg-do compile { target c++23 } }
// { dg-options "-fno-exceptions" }

#include <expected>

// 3843. std::expected<T,E>::value() & assumes E is copy constructible
void
test_lwg3843()
{
  struct E1 {
      E1(int) { }
      E1(E1&) { }
      E1(const E1&) = delete;
  };
  std::expected<int, E1> v1;
  v1.value(); // { dg-error "here" }
  const auto& v1c = v1;
  v1c.value(); // { dg-error "here" }

  struct E2 {
    E2(int) { }
    E2(const E2&) { }
    E2(E2&&) = delete;
  };
  std::expected<int, E2> v2;
  v2.value();
  std::move(v2).value(); // { dg-error "here" }
  const auto& v2c = v2;
  v2c.value();
  std::move(v2c).value();

  struct E3 {
    E3(int) { }
    E3(const E3&) { }
    E3(E3&&) { }
    E3(const E3&&) = delete;
  };
  std::expected<int, E3> v3;
  v3.value();
  std::move(v3).value();
  const auto& v3c = v3;
  v3c.value();
  std::move(v3c).value(); // { dg-error "here" }
}

// 3940. std::expected<void, E>::value() also needs E to be copy constructible
void
test_lwg3940()
{
  struct E1 {
      E1(int) { }
      E1(E1&) { }
      E1(const E1&) = delete;
  };
  std::expected<void, E1> v1;
  v1.value(); // { dg-error "here" }

  struct E2 {
    E2(int) { }
    E2(const E2&) { }
    E2(E2&&) = delete;
  };
  std::expected<void, E2> v2;
  std::move(v2).value(); // { dg-error "here" }
}

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
// { dg-prune-output "use of deleted function" }
// { dg-prune-output "control reaches end of non-void function" }
