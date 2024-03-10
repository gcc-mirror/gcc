// { dg-options "-lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// 8.4.1 path constructors [path.construct]

#include <experimental/filesystem>
#include <testsuite_hooks.h>

using std::experimental::filesystem::path;

#define CHECK(E, S) (path(E##S) == path(u8##S))

void
test_wide()
{
  VERIFY( CHECK(L, "\u00E4") ); // PR libstdc++/95048
  VERIFY( CHECK(L, "\U0001F4C1") ); // folder
  VERIFY( CHECK(L, "\U0001F4C2") ); // open folder
  VERIFY( CHECK(L, "\U0001F4C4") ); // filing cabient

  VERIFY( path(u8"\U0001D11E").wstring() == L"\U0001D11E" ); // G Clef
}

void
test_u16()
{
  VERIFY( CHECK(u, "\u00E4") ); // PR libstdc++/95048
  VERIFY( CHECK(u, "\U0001F4C1") ); // folder
  VERIFY( CHECK(u, "\U0001F4C2") ); // open folder
  VERIFY( CHECK(u, "\U0001F4C4") ); // filing cabient

  VERIFY( path(u8"\U0001D11E").u16string() == u"\U0001D11E" ); // G Clef
}

void
test_u32()
{
  VERIFY( CHECK(U, "\u00E4") ); // PR libstdc++/95048
  VERIFY( CHECK(U, "\U0001F4C1") ); // folder
  VERIFY( CHECK(U, "\U0001F4C2") ); // open folder
  VERIFY( CHECK(U, "\U0001F4C4") ); // filing cabient

  VERIFY( path(u8"\U0001D11E").u32string() == U"\U0001D11E" ); // G Clef
}

int
main()
{
  test_wide();
  test_u16();
  test_u32();
}
