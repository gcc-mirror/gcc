// { dg-do run }

// PR libstdc++/121054 std::bitset<0>("zero") should throw std::invalid_argument
#include <bitset>
#include <stdexcept>
#include <testsuite_hooks.h>

void
test01()
{
  try {
    std::bitset<0>(std::string("x"));
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<1>(std::string("0x"));
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<0>(std::string("01"));
    VERIFY( true );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<1>(std::string("x0"), 1);
    VERIFY( true );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }

#if _GLIBCXX_USE_C99_WCHAR
  try {
    std::bitset<1>(std::wstring(L"0x"));
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<1>(std::wstring(L"x0"), 1);
    VERIFY( true );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }
#endif
}

void
test02()
{
#if __cplusplus >= 201103L
  try {
    std::bitset<0>("x");
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<1>("0x", 2);
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<1>("0x", 1);
    VERIFY( true );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<0>("01");
    VERIFY( true );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }

#if _GLIBCXX_USE_C99_WCHAR
  try {
    std::bitset<1>(L"0x", 2);
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<1>(L"0x", 1);
    VERIFY( true );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }
#endif
#endif
}

void
test03()
{
#if __cpp_lib_bitset >= 202202L
  try {
    std::bitset<0>(std::string_view("x"));
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<1>(std::string_view("0x"));
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<0>(std::string_view("01"));
    VERIFY( true );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<1>(std::string_view("x0"), 1);
    VERIFY( true );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }

#if _GLIBCXX_USE_C99_WCHAR
  try {
    std::bitset<1>(std::wstring_view(L"0x"));
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::bitset<1>(std::wstring_view(L"x0"), 1);
    VERIFY( true );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }
#endif
#endif
}

int main()
{
  test01();
  test02();
  test03();
}
