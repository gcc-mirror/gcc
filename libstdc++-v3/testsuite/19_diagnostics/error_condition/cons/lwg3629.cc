// { dg-do compile { target c++11 } }

// 3629. make_error_code and make_error_condition are customization points
// Verify that make_error_condition is looked up using ADL only.

namespace User
{
  struct E1;
}

// N.B. not in associated namespace of E1, and declared before <system_error>.
User::E1 make_error_condition(User::E1);

#include <future> // declares std::make_error_condition(future_errc)
#include <system_error>

namespace User
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

template<> struct std::is_error_condition_enum<User::E1> : std::true_type { };
template<> struct std::is_error_condition_enum<User::E2> : std::true_type { };
template<> struct std::is_error_condition_enum<User::E3> : std::true_type { };

// ::make_error_condition(E1) should not be found by name lookup.
std::error_condition e1( User::E1{} ); // { dg-error "here" }

// std::make_error_condition(future_errc) should not be found by name lookup.
std::error_condition e2( User::E2{} ); // { dg-error "here" }

// std::make_error_condition(errc) should not be found by name lookup.
std::error_condition e3( User::E3{} ); // { dg-error "here" }

// { dg-error "use of deleted function" "" { target *-*-* } 0 }
