// { dg-do compile { target c++20 } }

// BasicFormatter requirements do not require a const parameter.

#include <format>

struct X { };

template<> struct std::formatter<X, char>
{
  constexpr auto parse(format_parse_context& ctx)
  { return ctx.begin(); }

  // Takes non-const X&
  format_context::iterator format(X&, format_context& ctx) const
  {
    auto out = ctx.out();
    *out++ = 'x';
    return out;
  }
};

X x;
auto s = std::format("{}", x);
