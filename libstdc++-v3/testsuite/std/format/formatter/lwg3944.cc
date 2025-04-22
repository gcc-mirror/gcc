// { dg-do compile { target c++20 } }
// { dg-options "-Wno-unused-result" }

// LWG 3944. Formatters converting sequences of char to sequences of wchar_t

#include <format>
#include <vector>

void test_lwg3944()
{
  // Ill-formed in C++20 and C++23
  const char* cstr = "hello";
  char* str = const_cast<char*>(cstr);
  std::format(L"{}", str); // { dg-error "here" }
  std::format(L"{}",cstr); // { dg-error "here" }

  // Ill-formed in C++20
  std::format(L"{}", "hello"); // { dg-error "here" }
  std::format(L"{}", std::string_view("hello")); // { dg-error "here" }
  std::format(L"{}", std::string("hello")); // { dg-error "here" }
#ifdef __cpp_lib_format_ranges
  // LWG 3944 does not change this, it's still valid.
  std::format(L"{}", std::vector{'h', 'e', 'l', 'l', 'o'});
#endif
}

// { dg-error "std::formatter must be specialized" "" { target *-*-* } 0 }
// { dg-prune-output "use of deleted function" }
// { dg-prune-output "no matching function" }
// { dg-prune-output "has no member named 'parse'" }
// { dg-prune-output "not a constant expression" }
