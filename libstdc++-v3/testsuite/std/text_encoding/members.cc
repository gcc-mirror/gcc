// { dg-options "-lstdc++exp" }
// { dg-do run { target c++26 } }
// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-namedlocale "fr_FR.ISO8859-15" }

#include <text_encoding>
#include <string_view>
#include <locale.h>
#include <testsuite_hooks.h>

using namespace std::string_view_literals;

void
test_literal()
{
  const std::text_encoding lit = std::text_encoding::literal();
  VERIFY( lit.name() == std::string_view(__GNUC_EXECUTION_CHARSET_NAME) );
}

void
test_env()
{
  const std::text_encoding env = std::text_encoding::environment();

  if (env.mib() == std::text_encoding::UTF8)
    VERIFY( std::text_encoding::environment_is<std::text_encoding::UTF8>() );

  ::setlocale(LC_ALL, ISO_8859(1, en_US));
  const std::text_encoding env1 = std::text_encoding::environment();
  VERIFY( env1 == env );

  ::setlocale(LC_ALL, ISO_8859(15, fr_FR));
  const std::text_encoding env2 = std::text_encoding::environment();
  VERIFY( env2 == env );
}

void
test_every_id()
{
  for (int i = 0; i <= 2260; ++i)
  {
    std::text_encoding::id mib{i};
    switch (i)
    {
    case 0:
    case 33 ... 34:
    case 107 ... 108:
    case 120 ... 999:
    case 1022 ... 1999:
    case 2110 ... 2249:
      // These do not correspond to an enumerator of std::text_encoding::id
      // so are not valid arguments to the constructor. Without assertions
      // enabled, we map bad IDs to id::unknown as a libstdc++ extension.
#ifndef _GLIBCXX_ASSERTIONS
      {
        std::text_encoding bad{mib};
        VERIFY( bad.mib() == std::text_encoding::unknown );
        VERIFY( bad.name() == ""sv );
      }
#endif
      continue;
    }
    std::text_encoding enc{mib};
    auto aliases = enc.aliases();
    if (i == 1 || i == 2)
      VERIFY( enc.name() == ""sv );
    else
      VERIFY( enc.name() == std::string_view(aliases.front()) );
    auto begin = aliases.begin();
    auto end = aliases.end();
    VERIFY( (begin + std::ranges::distance(aliases)) == end );
#ifndef _GLIBCXX_ASSERTIONS
    // These ops violate preconditions, but as libstdc++ extensions they are
    // guaranteed to either assert or have well-defined behaviour.

    // This erroneously returns ""sv:
    VERIFY( begin[std::ranges::distance(aliases)] == ""sv );
    // Likewise:
    VERIFY( begin[999999] == ""sv );

    auto iter = begin;
    std::ranges::advance(iter, end);
    // Erroneously sets iter to a value-initialized state.
    ++iter;
    VERIFY( iter == decltype(iter){} );
    VERIFY( *iter == ""sv );

    iter = begin;
    // Erroneously sets iter to a value-initialized state.
    --iter;
    VERIFY( iter == decltype(iter){} );
#endif
  }
}

int main()
{
  test_literal();
  test_env();
  test_every_id();
}
