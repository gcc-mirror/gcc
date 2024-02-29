// { dg-do compile { target c++20 } }

// P2905R2 Runtime format strings

#include <format>

std::string rval() { return "path/etic/experience"; }

void test_rval()
{
  (void)std::make_format_args(rval()); // { dg-error "cannot bind non-const lvalue reference" }
}

void test_missing_specialization()
{
  struct X { };
  X x;
  (void)std::make_format_args(x); // { dg-error "here" }
// { dg-error "std::formatter must be specialized" "" { target *-*-* } 0 }
}

struct Y { };
template<> class std::formatter<Y> {
public:
  constexpr typename format_parse_context::iterator
  parse(format_parse_context& c)
  { return c.begin(); }

  template<class C>
  typename C::iterator format(Y&, C&) const;
};

void test(std::formatter<Y>& f, std::format_parse_context& pc) {
  f.parse(pc);
}

void test_const_arg()
{
  const Y y;
  (void)std::make_format_args(y); // { dg-error "here" }
// { dg-error "format arg must be non-const" "" { target *-*-* } 0 }
}

// { dg-prune-output "no matching function for call to .*::basic_format_arg<" }
