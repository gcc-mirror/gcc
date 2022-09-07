// { dg-do compile { target c++11 } }

// 3629. make_error_code and make_error_condition are customization points
// Verify that make_error_code is looked up using ADL only.

namespace user
{
  struct E1;
}

// N.B. not in associated namespace of E1, and declared before <system_error>.
user::E1 make_error_code(user::E1);

#include <future> // declares std::make_error_code(future_errc)
#include <system_error>

namespace user
{
  struct E1
  {
    operator std::error_code() const;
  };

  struct E2
  {
    operator std::future_errc() const;
  };

  struct E3
  {
    operator std::errc() const;
  };
}

template<> struct std::is_error_code_enum<user::E1> : std::true_type { };
template<> struct std::is_error_code_enum<user::E2> : std::true_type { };
template<> struct std::is_error_code_enum<user::E3> : std::true_type { };

// ::make_error_code(E1) should not be found by name lookup.
std::error_code e1( user::E1{} ); // { dg-error "here" }

// std::make_error_code(errc) should not be found by name lookup.
std::error_code e2( user::E2{} ); // { dg-error "here" }

// std::make_error_code(future_errc) should not be found by name lookup.
std::error_code e3( user::E3{} ); // { dg-error "here" }

// { dg-error "use of deleted function" "" { target *-*-* } 0 }
