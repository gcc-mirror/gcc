// { dg-do compile { target c++23 } }

#include <expected>

void
test_unexpected()
{
  int i[2]{};

  // std::unexpected<E> is ill-formed if E is a non-object type,

  std::unexpected<int&> ref(i[0]); // { dg-error "here" }
  std::unexpected<void()> func(test_unexpected); // { dg-error "here" }
  // { dg-error "no matching function for call to" "" { target *-*-* } 0 }
  // { dg-error "invalidly declared function type" "" { target *-*-* } 0 }

  // an array type,
  std::unexpected<int[2]> array(i); // { dg-error "here" }

  // a specialization of std::unexpected,
  std::unexpected<int> u(1);
  std::unexpected<std::unexpected<int>> nested(u); // { dg-error "here" }
						   //
  // or a cv-qualified type.
  std::unexpected<const int> c_int(1); // { dg-error "here" }
  std::unexpected<volatile int> v_int(1); // { dg-error "here" }
}

void
test_expected_value()
{
  // std::expected<T, E> is ill-formed if T is a reference type,
  std::expected<int&, int> ref(std::unexpect); // { dg-error "here" }
  //  { dg-error "reference type" "" { target *-*-* } 0 }

  // a function type,
  std::expected<void(), int> func(std::unexpect); // { dg-error "here" }
  //  { dg-error "returning a function" "" { target *-*-* } 0 }
						 //
  // possibly cv-qualified types in_place_t,
  std::expected<std::in_place_t, int> tag(std::unexpect); // { dg-error "here" }
  std::expected<const std::in_place_t, int> ctag(std::unexpect); // { dg-error "here" }
  // unexpect_t,
  std::expected<std::unexpect_t, int> utag(std::unexpect); // { dg-error "here" }
  std::expected<const std::unexpect_t, int> cutag(std::unexpect); // { dg-error "here" }
  // or a specialization of unexpected.
  std::expected<std::unexpected<int>, int> unex(std::in_place, 1); // { dg-error "here" }
  std::expected<const std::unexpected<int>, int> cunex(std::in_place, 1); // { dg-error "here" }
}

void
test_expected_error()
{

  // std::expected<T, E> is ill-formed if std::unexpected<E> would be
  // ill-formed. Test the same types as in test_unexpected().

  std::expected<int, int&> ref; // { dg-error "here" }
  std::expected<int, void()> func; // { dg-error "here" }
  std::expected<int, int[2]> array; // { dg-error "here" }
  std::expected<int, std::unexpected<int>> nested; // { dg-error "here" }
  std::expected<int, const int> c_int; // { dg-error "here" }
  std::expected<int, volatile int> v_int; // { dg-error "here" }
}

// { dg-prune-output "static assertion failed" }
// { dg-prune-output "function returning an array" }
