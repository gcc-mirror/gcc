// { dg-do run { target c++26 } }
// { dg-require-cpp-feature-test "__cpp_lib_text_encoding" }

#include <text_encoding>
#include <string_view>
#include <testsuite_hooks.h>

using namespace std::string_view_literals;

constexpr void
test_default_construct()
{
  std::text_encoding e0;
  VERIFY( e0.mib() == std::text_encoding::unknown );
  VERIFY( e0.name()[0] == '\0' ); // P2862R1 name() should never return null
  VERIFY( e0.aliases().empty() );
}

constexpr void
test_construct_by_name()
{
  std::string_view s;
  std::text_encoding e0(s);
  VERIFY( e0.mib() == std::text_encoding::other );
  VERIFY( e0.name() == s );
  VERIFY( e0.aliases().empty() );

  s = "not a real encoding";
  std::text_encoding e1(s);
  VERIFY( e1.mib() == std::text_encoding::other );
  VERIFY( e1.name() == s );
  VERIFY( e1.aliases().empty() );

  VERIFY( e1 != e0 );
  VERIFY( e1 == e0.mib() );

  s = "utf8";
  std::text_encoding e2(s);
  VERIFY( e2.mib() == std::text_encoding::UTF8 );
  VERIFY( e2.name() == s );
  VERIFY( ! e2.aliases().empty() );
  VERIFY( e2.aliases().front() == "UTF-8"sv );

  s = "Latin-1"; // matches "latin1"
  std::text_encoding e3(s);
  VERIFY( e3.mib() == std::text_encoding::ISOLatin1 );
  VERIFY( e3.name() == s );
  VERIFY( ! e3.aliases().empty() );
  VERIFY( e3.aliases().front() == "ISO_8859-1:1987"sv ); // primary name

  s = "U.S."; // matches "us"
  std::text_encoding e4(s);
  VERIFY( e4.mib() == std::text_encoding::ASCII );
  VERIFY( e4.name() == s );
  VERIFY( ! e4.aliases().empty() );
  VERIFY( e4.aliases().front() == "US-ASCII"sv ); // primary name

  s = "ascii";
  std::text_encoding e5(s);
  VERIFY( e5.mib() == std::text_encoding::ASCII );
  VERIFY( e5.name() == s );
}

constexpr void
test_construct_by_id()
{
  std::text_encoding e0(std::text_encoding::other);
  VERIFY( e0.mib() == std::text_encoding::other );
  VERIFY( e0.name() == ""sv );
  VERIFY( e0.aliases().empty() );

  std::text_encoding e1(std::text_encoding::unknown);
  VERIFY( e1.mib() == std::text_encoding::unknown );
  VERIFY( e1.name() == ""sv );
  VERIFY( e1.aliases().empty() );

  std::text_encoding e2(std::text_encoding::UTF8);
  VERIFY( e2.mib() == std::text_encoding::UTF8 );
  VERIFY( e2.name() == "UTF-8"sv );
  VERIFY( ! e2.aliases().empty() );
  VERIFY( e2.aliases().front() == std::string_view(e2.name()) );
  bool found = false;
  for (auto alias : e2.aliases())
    if (alias == "csUTF8"sv)
    {
      found = true;
      break;
    }
  VERIFY( found );
}

constexpr void
test_copy_construct()
{
  std::text_encoding e0;
  std::text_encoding e1 = e0;
  VERIFY( e1 == e0 );

  std::text_encoding e2(std::text_encoding::UTF8);
  auto e3 = e2;
  VERIFY( e3 == e2 );

  e1 = e3;
  VERIFY( e1 == e2 );
}

int main()
{
  auto run_tests = [] {
    test_default_construct();
    test_construct_by_name();
    test_construct_by_id();
    test_copy_construct();
    return true;
  };

  run_tests();
  static_assert( run_tests() );
}
