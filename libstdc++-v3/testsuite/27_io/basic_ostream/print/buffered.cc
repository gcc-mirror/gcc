// { dg-additional-options "-lstdc++exp" { target { *-*-mingw* } } }
// { dg-do run { target c++23 } }

#include <sstream>
#include <algorithm>
#include <testsuite_hooks.h>

struct ThrowingFormat
{
  std::string val;
  bool throw_from_format = false;
};

template<>
struct std::formatter<ThrowingFormat, char>
{
   constexpr
   std::format_parse_context::iterator
   parse(std::format_parse_context& ctx) const
   { return ctx.begin(); }

   template<typename Out>
   Out
   format(const ThrowingFormat& t, std::basic_format_context<Out, char>& fc) const
   {
     if (t.throw_from_format)
       throw std::logic_error("Formatting stopped");
     return std::ranges::copy(t.val, fc.out()).out;
   }
};

void
test_throwing()
{
  std::string s;
  s.reserve(100);
  std::ostringstream out(std::move(s));
  ThrowingFormat tf1{std::string(500, 'a'), false}, tf2{std::string(500, 'b'), true};

  try
  {
    std::print(out, "{} {}", tf1, tf2);
    VERIFY(false);
  } catch (...) {
    VERIFY(true);
  }
  VERIFY( out.view().empty() );
}

struct MixedFormat
{
  std::ostream* out;
  std::string val;
};

template<>
struct std::formatter<MixedFormat, char>
{
   constexpr
   std::format_parse_context::iterator
   parse(std::format_parse_context& ctx) const
   { return ctx.begin(); }

   template<typename Out>
   Out
   format(const MixedFormat& t, std::basic_format_context<Out, char>& fc) const
   {
     if (t.out)
       *t.out << "<<[" << t.val << "]";
     return std::ranges::copy(t.val, fc.out()).out;
   }
};

void
test_interleaved()
{
  std::string s;
  s.reserve(100);
  std::ostringstream out(std::move(s));
  MixedFormat mf1{&out, "abc"}, mf2{&out, "DEF"};

  std::print(out, "f[{}]f[{}]", mf1, mf2);
  VERIFY( out.view() == "<<[abc]<<[DEF]f[abc]f[DEF]" );
}

int main()
{
  test_throwing();
  test_interleaved();
}
