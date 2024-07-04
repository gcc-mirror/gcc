// { dg-options "-lstdc++exp" }
// { dg-do run { target c++26 } }
// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-namedlocale "fr_FR.ISO8859-15" }

#include <locale>
#include <testsuite_hooks.h>

void
test_encoding()
{
  const std::locale c = std::locale::classic();
  std::text_encoding c_enc = c.encoding();
  VERIFY( c_enc == std::text_encoding::ASCII );

  const std::locale fr = std::locale(ISO_8859(15, fr_FR));
  std::text_encoding fr_enc = fr.encoding();
  VERIFY( fr_enc == std::text_encoding::ISO885915 );

  const std::locale en = std::locale(ISO_8859(1, en_US));
  std::text_encoding en_enc = en.encoding();
  VERIFY( en_enc == std::text_encoding::ISOLatin1 );

#if __cpp_exceptions
  try {
    const std::locale c_utf8 = std::locale("C.UTF-8");
    VERIFY( c_utf8.encoding() == std::text_encoding::UTF8 );
  } catch (...) {
  }
#endif
}

int main()
{
  test_encoding();
}
